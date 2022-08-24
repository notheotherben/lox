use std::collections::HashMap;

use crate::{
    ast::{Expr, ExprVisitor, Literal, Stmt, StmtVisitor},
    errors,
    lexer::Token,
    vm::{Chunk, Function, OpCode, VarRef, Value},
    Loc, LoxError,
};

struct Local {
    pub token: Token,
    pub captured: bool,
}

#[derive(Default)]
struct CompilerState {
    pub chunk: Chunk,
    
    pub identifiers: HashMap<String, usize>,

    pub locals: Vec<Local>,
    pub upvalues: Vec<VarRef>,
    pub stack_depth: usize,

    pub break_targets: Vec<usize>,
}

#[derive(Default)]
pub struct Compiler {
    states: Vec<CompilerState>
}

impl Compiler {
    pub fn new() -> Self {
        Compiler { states: vec![CompilerState::default()] }
    }

    pub fn finalize(self) -> Function {
        let mut states = self.states;
        let state = states.pop().unwrap();

        Function::closure(String::default(), 0, state.upvalues, state.chunk)
    }

    fn chunk(&self) -> &Chunk {
        &self.state().chunk
    }

    fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.state_mut().chunk
    }

    fn state(&self) -> &CompilerState {
        self.states.last().unwrap()
    }

    fn state_mut(&mut self) -> &mut CompilerState {
        self.states.last_mut().unwrap()
    }

    fn state_offset(&mut self, offset: usize) -> &mut CompilerState {
        if offset >= self.states.len() {
            return &mut self.states[0];
        }

        let offset = self.states.len() - offset - 1;
        &mut self.states[offset]
    }

    fn identifier(&mut self, name: &str) -> usize {
        let state = self.state_mut();

        let index = state
            .identifiers
            .entry(name.to_string())
            .or_insert_with(|| state.chunk.add_constant(Value::String(name.to_string())));
        *index
    }

    fn define_local(&mut self, name: &Token) {
        self.state_mut().locals.push(Local{
            token: name.clone(),
            captured: false,
        });
    }

    fn add_upvalue(&mut self, up: VarRef, offset: usize) -> usize {
        let state = self.state_offset(offset);

        for (upvalue_idx, upvalue) in state.upvalues.iter().enumerate() {
            if *upvalue == up {
                return upvalue_idx;
            }
        }

        state.upvalues.push(up);
        state.upvalues.len() - 1
    }

    fn get_local(&mut self, token: &Token, offset: usize) -> Option<usize> {
        if offset >= self.states.len() {
            return None;
        }

        for (idx, local) in self.state_offset(offset).locals.iter().enumerate().rev() {
            if local.token.lexeme() == token.lexeme() {
                return Some(idx);
            }
        }

        None
    }

    fn get_upvalue(&mut self, token: &Token, offset: usize) -> Option<usize> {
        if offset + 1 >= self.states.len() {
            return None;
        }

        for (idx, local) in self.state_offset(offset + 1).locals.iter().enumerate().rev() {
            if local.token.lexeme() == token.lexeme() {
                self.state_offset(offset + 1).locals[idx].captured = true;
                return Some(self.add_upvalue(VarRef::Local(idx), offset));
            }
        }

        self.get_upvalue(token, offset + 1).map(|idx| self.add_upvalue(VarRef::Transitive(idx), offset))
    }
}

impl ExprVisitor<Result<(), LoxError>> for Compiler {
    fn visit_assign(&mut self, ident: &Token, value: &Expr) -> Result<(), LoxError> {
        self.visit_expr(value)?;

        if let Some(idx) = self.get_local(ident, 0) {
            self.chunk_mut().write(OpCode::SetLocal(idx), ident.location());
        } else if let Some(idx) = self.get_upvalue(ident, 0) {
            self.chunk_mut().write(OpCode::SetUpvalue(idx), ident.location());
        } else {
            let idx = self.identifier(ident.lexeme());
            self.chunk_mut().write(OpCode::SetGlobal(idx), ident.location());
        }

        Ok(())
    }

    fn visit_binary(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<(), LoxError> {
        self.visit_expr(left)?;
        self.visit_expr(right)?;

        match op {
            Token::Plus(..) => self.chunk_mut().write(OpCode::Add, op.location()),
            Token::Minus(..) => self.chunk_mut().write(OpCode::Subtract, op.location()),
            Token::Star(..) => self.chunk_mut().write(OpCode::Multiply, op.location()),
            Token::Slash(..) => self.chunk_mut().write(OpCode::Divide, op.location()),

            Token::EqualEqual(..) => self.chunk_mut().write(OpCode::Equal, op.location()),
            Token::BangEqual(..) => {
                self.chunk_mut().write(OpCode::Equal, op.location());
                self.chunk_mut().write(OpCode::Not, op.location());
            }
            Token::Greater(..) => {
                self.chunk_mut().write(OpCode::Greater, op.location());
            }
            Token::GreaterEqual(..) => {
                self.chunk_mut().write(OpCode::Less, op.location());
                self.chunk_mut().write(OpCode::Not, op.location());
            }
            Token::Less(..) => self.chunk_mut().write(OpCode::Less, op.location()),
            Token::LessEqual(..) => {
                self.chunk_mut().write(OpCode::Greater, op.location());
                self.chunk_mut().write(OpCode::Not, op.location());
            }
            _ => todo!("{:?}", op),
        }

        Ok(())
    }

    fn visit_call(&mut self, callee: &Expr, args: &[Expr], close: &Token) -> Result<(), LoxError> {
        self.visit_expr(callee)?;

        self.chunk_mut().write(OpCode::Nil, close.location());

        for arg in args {
            self.visit_expr(arg)?;
        }

        self.chunk_mut().write(OpCode::Call(args.len()), close.location());

        Ok(())
    }

    fn visit_get(&mut self, _obj: &Expr, _property: &Token) -> Result<(), LoxError> {
        todo!()
    }

    fn visit_fun_expr(
        &mut self,
        loc: &Loc,
        params: &[Token],
        body: &[Stmt],
    ) -> Result<(), LoxError> {
        self.states.push(CompilerState::default());

        self.define_local(&Token::This(loc.clone()));

        for param in params {
            self.define_local(param);
        }

        self.visit_block(body)?;

        let comp = self.states.pop().unwrap();

        let ptr = self.chunk_mut().add_constant(Value::Function(Function::closure(
            format!("anonymous@{}", loc),
            params.len(),
            comp.upvalues,
            comp.chunk,
        )));
        self.chunk_mut().write(OpCode::Closure(ptr), loc.clone());
        Ok(())
    }

    fn visit_grouping(&mut self, expr: &Expr) -> Result<(), LoxError> {
        self.visit_expr(expr)
    }

    fn visit_literal(&mut self, loc: &Loc, value: &Literal) -> Result<(), LoxError> {
        match value {
            Literal::Nil => self.chunk_mut().write(OpCode::Nil, loc.clone()),
            Literal::Bool(value) => self.chunk_mut().write(
                if *value { OpCode::True } else { OpCode::False },
                loc.clone(),
            ),
            Literal::Number(value) => {
                let ptr = self.chunk_mut().add_constant(Value::Number(*value));

                self.chunk_mut().write(OpCode::Constant(ptr), loc.clone())
            }
            Literal::String(value) => {
                let ptr = self.chunk_mut().add_constant(Value::String(value.clone()));

                self.chunk_mut().write(OpCode::Constant(ptr), loc.clone())
            }
        };

        Ok(())
    }

    fn visit_logical(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<(), LoxError> {
        self.visit_expr(left)?;

        match op {
            Token::Or(..) => {
                self.chunk_mut().write(OpCode::JumpIf(0), op.location());
                let jmp = self.chunk().len() - 1;

                self.chunk_mut().write(OpCode::Pop, op.location());
                self.visit_expr(right)?;
                let target = self.chunk().len();
                self.chunk_mut().overwrite(OpCode::JumpIf(target), jmp);
            },
            Token::And(..) => {
                self.chunk_mut().write(OpCode::JumpIfFalse(0), op.location());
                let jmp = self.chunk().len() - 1;

                self.chunk_mut().write(OpCode::Pop, op.location());
                self.visit_expr(right)?;
                let target = self.chunk().len();
                self.chunk_mut().overwrite(OpCode::JumpIfFalse(target), jmp);
            },
            token => return Err(errors::language(
                op.location(),
                &format!("Received unexpected logical operator {:?}", token),
                "Report this error with the compiler to us on GitHub with sample code to reproduce the error."))
        }

        Ok(())
    }

    fn visit_set(&mut self, _obj: &Expr, _property: &Token, _value: &Expr) -> Result<(), LoxError> {
        todo!()
    }

    fn visit_super(&mut self, _loc: &Loc, _method: &Token) -> Result<(), LoxError> {
        todo!()
    }

    fn visit_this(&mut self, _loc: &Loc) -> Result<(), LoxError> {
        todo!()
    }

    fn visit_unary(&mut self, op: &Token, expr: &Expr) -> Result<(), LoxError> {
        self.visit_expr(expr)?;

        match op {
            Token::Minus(..) => self.chunk_mut().write(OpCode::Negate, op.location()),
            Token::Bang(..) => self.chunk_mut().write(OpCode::Not, op.location()),
            _ => Err(errors::language(
                op.location(),
                "Unrecognized unary operator.",
                "Only numerical negation (`-`) and logical negation (`!`) are supported.",
            ))?,
        };

        Ok(())
    }

    fn visit_var_ref(&mut self, name: &Token) -> Result<(), LoxError> {
        if let Some(idx) = self.get_local(name, 0) {
            self.chunk_mut().write(OpCode::GetLocal(idx), name.location());
        } else if let Some(idx) = self.get_upvalue(name, 0) {
            self.chunk_mut().write(OpCode::GetUpvalue(idx), name.location());
        } else {
            let idx = self.identifier(name.lexeme());
            self.chunk_mut().write(OpCode::GetGlobal(idx), name.location());
        }

        Ok(())
    }
}

impl StmtVisitor<Result<(), LoxError>> for Compiler {
    fn visit_break(&mut self, loc: &Loc) -> Result<(), LoxError> {
        if let Some(&target) = self.state().break_targets.last() {
            self.chunk_mut().write(OpCode::Jump(target), loc.clone());
            Ok(())
        } else {
            Err(errors::language(
                loc.clone(),
                "Found a `break` statement outside a loop statement.",
                "You can only use break from within the body of a `while` or `for` loop.",
            ))
        }
    }

    fn visit_block(&mut self, stmts: &[Stmt]) -> Result<(), LoxError> {
        let parent_locals_len = self.state().locals.len();
        self.state_mut().stack_depth += 1;

        for stmt in stmts {
            self.visit_stmt(stmt)?;
        }

        self.state_mut().stack_depth -= 1;

        let state = self.state_mut();
        while state.locals.len() > parent_locals_len {
            let local = state.locals.pop().unwrap();
            if local.captured {
                state.chunk.write(OpCode::CloseUpvalue, Loc::Unknown);
            } else {
                state.chunk.write(OpCode::Pop, Loc::Unknown);
            }
        }

        Ok(())
    }

    fn visit_class(
        &mut self,
        _name: &Token,
        _superclass: Option<&Expr>,
        _statics: &[Stmt],
        _methods: &[Stmt],
    ) -> Result<(), LoxError> {
        todo!()
    }

    fn visit_expr_stmt(&mut self, expr: &Expr) -> Result<(), LoxError> {
        self.visit_expr(expr)?;
        self.chunk_mut().write(OpCode::Pop, Loc::Unknown);
        Ok(())
    }

    fn visit_fun_def(
        &mut self,
        _ty: crate::ast::FunType,
        name: &Token,
        params: &[Token],
        body: &[Stmt],
    ) -> Result<(), LoxError> {
        self.states.push(CompilerState::default());

        for param in params {
            self.define_local(param);
        }

        self.visit_block(body)?;

        let comp = self.states.pop().unwrap();

        let ident = self.identifier(name.lexeme());
        let ptr = self.chunk_mut().add_constant(Value::Function(Function::closure(
            name.lexeme(),
            params.len(),
            comp.upvalues,
            comp.chunk,
        )));
        self.chunk_mut().write(OpCode::Closure(ptr), name.location());
        self.chunk_mut()
            .write(OpCode::DefineGlobal(ident), name.location());

        Ok(())
    }

    fn visit_if(
        &mut self,
        token: &Token,
        expr: &Expr,
        then_branch: &Stmt,
        else_branch: Option<&Stmt>,
    ) -> Result<(), LoxError> {
        self.visit_expr(expr)?;

        self.chunk_mut().write(OpCode::JumpIfFalse(0), token.location());
        let jmp_else = self.chunk().len() - 1;

        self.visit_stmt(then_branch)?;

        if let Some(else_branch) = else_branch {
            self.chunk_mut().write(OpCode::Jump(0), Loc::Native);
            let jump_end = self.chunk().len() - 1;
            let target = self.chunk().len();
            self.chunk_mut()
                .overwrite(OpCode::JumpIfFalse(target), jmp_else);
            self.chunk_mut().write(OpCode::Pop, Loc::Native);

            self.visit_stmt(else_branch)?;

            let target = self.chunk().len();
            self.chunk_mut()
                .overwrite(OpCode::Jump(target), jump_end);
        } else {
            let target = self.chunk().len();
            self.chunk_mut()
                .overwrite(OpCode::JumpIfFalse(target), jmp_else);
            self.chunk_mut().write(OpCode::Pop, Loc::Native);
        }

        Ok(())
    }

    fn visit_print(&mut self, loc: &Loc, expr: &Expr) -> Result<(), LoxError> {
        self.visit_expr(expr)?;

        self.chunk_mut().write(OpCode::Print, loc.clone());
        Ok(())
    }

    fn visit_return(&mut self, token: &Token, expr: Option<&Expr>) -> Result<(), LoxError> {
        if let Some(expr) = expr {
            self.visit_expr(expr)?;
        } else {
            self.chunk_mut().write(OpCode::Nil, token.location());
        }

        self.chunk_mut().write(OpCode::Return, token.location());

        Ok(())
    }

    fn visit_var_def(&mut self, name: &Token, expr: &Expr) -> Result<(), LoxError> {
        self.visit_expr(expr)?;

        if self.state().stack_depth > 0 {
            self.define_local(name);
        } else {
            let ptr = self.identifier(name.lexeme());
            self.chunk_mut().write(OpCode::DefineGlobal(ptr), name.location());
        }

        Ok(())
    }

    fn visit_while(&mut self, expr: &Expr, body: &Stmt) -> Result<(), LoxError> {
        // The `break` statement needs us to have a pre-registered jump target when
        // evaluating the body of the loop. We handle this by injecting a known jump
        // statement before the loop and then overwriting it with the correct jump
        // target after the loop's body is visited. This results in a trampoline-style
        // jump (i.e. jump to the break target, which jumps to the end of the loop).
        self.chunk_mut().write(OpCode::Jump(0), Loc::Native);
        let start_jmp = self.chunk().len() - 1;
        self.chunk_mut().write(OpCode::Jump(0), Loc::Native);
        let break_jump = self.chunk().len() - 1;
        self.state_mut().break_targets.push(break_jump);

        let start = self.chunk().len();
        self.chunk_mut().overwrite(OpCode::Jump(start), start_jmp);
        self.visit_expr(expr)?;
        self.chunk_mut().write(OpCode::JumpIfFalse(0), Loc::Native);
        let jmp_end = self.chunk().len() - 1;
        //self.chunk_mut().write(OpCode::Pop, Loc::Native);

        self.visit_stmt(body)?;

        self.chunk_mut().write(OpCode::Jump(start), Loc::Native);

        // When the loop condition evaluates falsey, we will jump to here.
        let target = self.chunk().len();
        self.chunk_mut()
            .overwrite(OpCode::JumpIfFalse(target), jmp_end);
        self.chunk_mut().write(OpCode::Pop, Loc::Native);

        // When we run `break` we will jump to this point
        let target = self.chunk().len();
        self.chunk_mut()
            .overwrite(OpCode::Jump(target), break_jump);
        self.state_mut().break_targets.pop();

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::Parser, compiler::compile, lexer::Scanner, vm::VM, CaptureOutput};

    use super::*;

    fn parse(source: &str) -> Vec<Stmt> {
        let lexer = Scanner::new(source);
        let (stmts, errs) = Parser::parse(
            &mut lexer
                .inspect(|t| {
                    if let Err(e) = t {
                        panic!("{}", e);
                    }
                })
                .filter_map(|t| t.ok()),
        );

        if errs.is_empty() {
            stmts
        } else {
            panic!("{:?}", errs);
        }
    }

    macro_rules! run {
        (err: $src:expr => $val:expr) => {{
            let stmts = parse($src);

            let chunk = compile(&stmts).expect("no errors");

            println!("{:?}", chunk);

            let _output = Box::new(CaptureOutput::default());
            let err = VM::default()
                .with_output(_output.clone())
                //.with_debug()
                .call(chunk)
                .expect_err("expected error");
            assert_eq!(format!("{}", err), format!("{}", $val).trim());
        }};

        ($src:expr => $val:expr) => {{
            let stmts = parse($src);

            let chunk = compile(&stmts).expect("no errors");

            println!("{:?}", chunk);

            let output = Box::new(CaptureOutput::default());
            VM::default()
                .with_output(output.clone())
                .with_debug()
                .call(chunk)
                .expect("no errors");
            assert_eq!(output.to_string().trim(), format!("{}", $val).trim());
        }};
    }

    #[test]
    fn unary_and_binary() {
        run!("print -5 + 10;" => 5);
    }

    #[test]
    fn examples() {
        run!("print (-1 + 2) * 3 - -4;" => 7);
        run!("print !(5 - 4 > 3 * 2 == !nil);" => true);
    }

    #[test]
    fn booleans() {
        run!("print true;" => true);
        run!("print !false;" => true);
        run!("print !nil;" => true);
    }

    #[test]
    fn comparisons() {
        run!("print 10 == 10;" => true);
        run!("print 10 != 10;" => false);
        run!("print 10 < 10;" => false);
        run!("print 10 <= 10;" => true);
        run!("print 10 > 10;" => false);
        run!("print 10 >= 10;" => true);
    }

    #[test]
    fn global_variables() {
        run!("var a = 10; print a;" => 10);
        run!(r#"var beverage = "cafe au lait";
        var breakfast = "beignets with " + beverage;
        print breakfast;"# => "beignets with cafe au lait");

        run!("var a = 10; a = 12; print a;" => 12);
    }

    #[test]
    fn local_variables() {
        run!("var a = 10; { var a = 20; print a; } print a;" => "20\n10");
        run!("var a = 10; { var a = 20; { var a = 30; print a; } print a; } print a;" => "30\n20\n10");
    }

    #[test]
    fn test_if() {
        run!("if (true) { print true; }" => true);
        run!("if (false) { print true; }" => "");
        run!("if (true) { print true; } else { print false; }" => true);
        run!("if (false) { print true; } else { print false; }" => false);
    }

    #[test]
    fn logical() {
        run!("print 1 and 2;" => 2);
        run!("print 1 and false;" => false);
        run!("print false and 1;" => false);
        run!("print nil and false;" => "nil");
        run!("print 1 or 2;" => 1);
        run!("print true or false;" => true);
        run!("print false or true;" => true);
        run!("print false or false;" => false);
    }

    #[test]
    fn loops() {
        run!("var i = 0; while (i < 10) { print i; i = i + 1; }" => "0\n1\n2\n3\n4\n5\n6\n7\n8\n9");
        run!("for (var i = 0; i < 10; i = i + 1) { print i; }" => "0\n1\n2\n3\n4\n5\n6\n7\n8\n9");
        run!("while (true) { print 1; break; print 2; }" => 1);
    }

    #[test]
    fn functions() {
        run!("fun foo() { print 1; } print foo;" => "<fn foo>");
        run!("var foo = fun () { print 1; }; print foo;" => "<fn anonymous@'fun' at line 1>");
        run!("fun foo() { print 1; } foo();" => 1);
        run!("fun foo() { print 1; } foo(); foo();" => "1\n1");
        run!("fun foo() { return 1; } print foo();" => 1);
        run!("print clock() > 0;" => true);
        run!(err: "assert(false, \"should fail\");" => "Assertion failed: should fail\n\n  [line 0] in assert()\n  [line 1] in script");
    }

    #[test]
    fn returns() {
        run!("fun foo() { return 1; } print foo();" => 1);
        run!("fun foo() { return; } print foo();" => "nil");
    }

    #[test]
    fn stacktraces() {
        run!(err: r#"
fun a() { b(); }
fun b() { c(); }
fun c() {
    c("too", "many");
}

a();
        "# => ("Invalid number of arguments, got 2 but expected 0.
Make sure that you are passing the correct number of arguments to the function.

  [line 5] in c()
  [line 3] in b()
  [line 2] in a()
  [line 8] in script"))
    }

    #[test]
    fn closures() {
        run!(r#"var x = "global";
        fun outer() {
          var x = "outer";
          fun inner() {
            print x;
          }
          inner();
        }
        outer();"# => "outer");

        run!(r#"
        fun outer() {
            var x = "value";
            fun middle() {
              fun inner() {
                print x;
              }
          
              print "create inner closure";
              return inner;
            }
          
            print "return from outer";
            return middle;
          }
          
          var mid = outer();
          var in = mid();
          in();
        "# => "return from outer\n\
        create inner closure\n\
        value");

        run!(r#"
        fun outer() {
            var x = "outside";
            fun inner() {
              print x;
            }
          
            return inner;
          }
          
          var closure = outer();
          closure();
        "# => "outside");

        run!(r#"
        var globalSet;
        var globalGet;
        
        fun main() {
          var a = "initial";
        
          fun set() { a = "updated"; }
          fun get() { print a; }
        
          globalSet = set;
          globalGet = get;
        }
        
        main();
        globalSet();
        globalGet();
        "# => "updated");

        run!(r#"
        fun makeCounter() {
            var count = 0;

            return fun () {
                count = count + 1;
                return count;
            };
        }

        var c1 = makeCounter();
        var c2 = makeCounter();

        print c1();
        print c1();
        print c1();
        print c2();
        print c1();
        print c2();
        "# => "1\n2\n3\n1\n4\n2")
    }
}
