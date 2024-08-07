use std::collections::HashMap;

use crate::{
    ast::{Expr, ExprVisitor, FunType, Literal, Stmt, StmtVisitor}, compiler::{Chunk, Function, OpCode, Primitive, VarRef}, errors, lexer::Token, Loc, LoxError
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
    pub fun_type: Option<FunType>,
    pub has_superclass: Option<bool>,

    pub break_targets: Vec<usize>,
}

#[derive(Default)]
pub struct Compiler {
    states: Vec<CompilerState>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler { states: vec![CompilerState::default()] }
    }

    pub fn finalize(self) -> Function {
        let mut states = self.states;
        let state = states.pop().unwrap();

        Function::new(String::default(), 0, state.upvalues, state.chunk)
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
            .or_insert_with(|| state.chunk.add_constant(Primitive::String(name.to_string())));
        *index
    }

    fn define_local(&mut self, name: &Token) -> usize {
        self.state_mut().locals.push(Local{
            token: name.clone(),
            captured: false,
        });

        self.state().locals.len() - 1
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
                self.chunk_mut().write(OpCode::GreaterEqual, op.location());
            }
            Token::Less(..) => self.chunk_mut().write(OpCode::Less, op.location()),
            Token::LessEqual(..) => {
                self.chunk_mut().write(OpCode::LessEqual, op.location());
            }
            _ => todo!("{:?}", op),
        }

        Ok(())
    }

    fn visit_call(&mut self, callee: &Expr, args: &[Expr], close: &Token) -> Result<(), LoxError> {
        match callee {
            Expr::Get(obj, property) => {
                // If we're immediately invoking the result of a property access, we can optimize the
                // call path using the OpCode::Invoke instruction.
                let prop = self.identifier(property.lexeme());

                self.visit_expr(obj)?;
   
                for arg in args {
                    self.visit_expr(arg)?;
                }

                self.chunk_mut().write(OpCode::Invoke(prop, args.len()), property.location());
            },
            Expr::Super(loc, property) => {
                let prop = self.identifier(property.lexeme());

                self.visit_var_ref(&Token::This(loc.clone()))?;

                for arg in args {
                    self.visit_expr(arg)?;
                }

                self.visit_var_ref(&Token::Super(loc.clone()))?;
                self.chunk_mut().write(OpCode::InvokeSuper(prop, args.len()), property.location());
            },
            callee => {
                self.visit_expr(callee)?;

                self.chunk_mut().write(OpCode::Nil, close.location());

                for arg in args {
                    self.visit_expr(arg)?;
                }

                self.chunk_mut().write(OpCode::Call(args.len()), close.location());
            }
        }

        Ok(())
    }

    fn visit_get(&mut self, obj: &Expr, property: &Token) -> Result<(), LoxError> {
        self.visit_expr(obj)?;

        let prop = self.identifier(property.lexeme());
        self.chunk_mut().write(OpCode::GetProperty(prop), property.location());
        Ok(())
    }

    fn visit_fun_expr(
        &mut self,
        loc: &Loc,
        params: &[Token],
        body: &[Stmt],
    ) -> Result<(), LoxError> {
        self.states.push(CompilerState {
            fun_type: Some(FunType::Closure),
            ..CompilerState::default()
        });

        self.define_local(&Token::Identifier(loc.clone(), "".to_string()));

        for param in params {
            self.define_local(param);
        }

        self.visit_block(body)?;

        let mut comp = self.states.pop().unwrap();

        // Implicit return (nil)
        comp.chunk.write(OpCode::Nil, Loc::Unknown);
        comp.chunk.write(OpCode::Return, Loc::Unknown);

        let ptr = self.chunk_mut().add_constant(Primitive::Function(Function::new(
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
                let ptr = self.chunk_mut().add_constant(Primitive::Number(*value));

                self.chunk_mut().write(OpCode::Constant(ptr), loc.clone())
            }
            Literal::String(value) => {
                let ptr = self.chunk_mut().add_constant(Primitive::String(value.clone()));

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
                format!("Received unexpected logical operator {:?}", token),
                "Report this error with the compiler to us on GitHub with sample code to reproduce the error."))
        }

        Ok(())
    }

    fn visit_set(&mut self, obj: &Expr, property: &Token, value: &Expr) -> Result<(), LoxError> {
        self.visit_expr(obj)?;
        self.visit_expr(value)?;

        let prop = self.identifier(property.lexeme());
        self.chunk_mut().write(OpCode::SetProperty(prop), property.location());
        Ok(())
    }

    fn visit_super(&mut self, loc: &Loc, method: &Token) -> Result<(), LoxError> {
        let this_token = Token::This(loc.clone());
        let super_token = Token::Super(loc.clone());

        if self.states.iter().rev().any(|state| matches!(state.has_superclass, Some(true))) {
            self.visit_var_ref(&this_token)?;
            self.visit_var_ref(&super_token)?;
            let prop = self.identifier(method.lexeme());
            self.chunk_mut().write(OpCode::GetSuper(prop), method.location());
            Ok(())
        } else {
            Err(errors::language(
                loc.clone(),
                "Cannot use `super` outside of a class method within a subclass.",
                "Make sure that your method is contained within a class that derives from another superclass.",
            ))
        }
    }

    fn visit_this(&mut self, loc: &Loc) -> Result<(), LoxError> {
        let token = Token::This(loc.clone());

        if self.states.iter().rev().any(|state| matches!(state.fun_type, Some(FunType::Method | FunType::Initializer))) {
            self.visit_var_ref(&token)
        } else {
            Err(errors::language(
                loc.clone(),
                "Cannot use `this` outside of a class method.",
                "Make sure that your method is contained within a class.",
            ))
        }
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
                state.chunk.write(OpCode::CloseUpvalue, Loc::Native);
            } else {
                state.chunk.write(OpCode::Pop, Loc::Native);
            }
        }

        Ok(())
    }

    fn visit_class(
        &mut self,
        name: &Token,
        superclass: Option<&Expr>,
        _statics: &[Stmt],
        methods: &[Stmt],
    ) -> Result<(), LoxError> {
        if superclass.is_some() {
            self.states.push(CompilerState {
                has_superclass: Some(true),
                ..CompilerState::default()
            });
        }

        let ident = self.identifier(name.lexeme());
        
        self.chunk_mut().write(OpCode::Class(ident), name.location());
        self.chunk_mut().write(OpCode::DefineGlobal(ident), name.location());
        
        if let Some(superclass) = superclass {
            self.define_local(&Token::This(name.location()));
            self.define_local(&Token::Super(name.location()));
            self.visit_expr(superclass)?;
            self.chunk_mut().write(OpCode::GetGlobal(ident), name.location());
            self.chunk_mut().write(OpCode::Inherit, name.location());
        }

        if !methods.is_empty() {
            self.chunk_mut().write(OpCode::GetGlobal(ident), Loc::Native);

            for method in methods {
                self.visit_stmt(method)?;
            }

            self.chunk_mut().write(OpCode::Pop, Loc::Native);
        }

        if superclass.is_some() {
            let comp = self.states.pop().unwrap();
            let ptr = self.chunk_mut().add_constant(Primitive::Function(Function::new(
                format!("class@{}", name.lexeme()),
                0,
                comp.upvalues,
                comp.chunk,
            )));

            self.chunk_mut().write(OpCode::Closure(ptr), Loc::Native);
            self.chunk_mut().write(OpCode::Nil, Loc::Native);
            self.chunk_mut().write(OpCode::Call(0), Loc::Native);
        }

        Ok(())
    }

    fn visit_expr_stmt(&mut self, expr: &Expr) -> Result<(), LoxError> {
        self.visit_expr(expr)?;
        self.chunk_mut().write(OpCode::Pop, Loc::Native);
        Ok(())
    }

    fn visit_fun_def(
        &mut self,
        ty: crate::ast::FunType,
        name: &Token,
        params: &[Token],
        body: &[Stmt],
    ) -> Result<(), LoxError> {
        self.states.push(CompilerState {
            fun_type: Some(ty),
            ..CompilerState::default()
        });

        if matches!(ty, FunType::Closure) {
            self.define_local(&Token::Identifier(name.location(), "".to_string()));
        } else {
            self.define_local(&Token::This(name.location()));
        }

        for param in params {
            self.define_local(param);
        }

        self.visit_block(body)?;

        let mut comp = self.states.pop().unwrap();

        // Implicit return nil or the instance depending on the function type
        if matches!(ty, FunType::Initializer) {
            comp.chunk.write(OpCode::GetLocal(0), Loc::Unknown);
        } else {
            comp.chunk.write(OpCode::Nil, Loc::Unknown);
        }
        comp.chunk.write(OpCode::Return, Loc::Unknown);

        let ident = self.identifier(name.lexeme());
        
        let ptr = self.chunk_mut().add_constant(Primitive::Function(Function::new(
            name.lexeme(),
            params.len(),
            comp.upvalues,
            comp.chunk,
        )));
        self.chunk_mut().write(OpCode::Closure(ptr), name.location());

        match ty {
            FunType::Closure => self.chunk_mut().write(OpCode::DefineGlobal(ident), name.location()),
            FunType::Method => self.chunk_mut().write(OpCode::Method(ident), name.location()),
            FunType::Initializer => self.chunk_mut().write(OpCode::Method(ident), name.location()),
        };

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

        self.chunk_mut().write(OpCode::Pop, Loc::Native);
        self.visit_stmt(then_branch)?;

        // For the else branch, we need to jump over it after the then branch
        self.chunk_mut().write(OpCode::Jump(0), Loc::Native);
        let jump_end = self.chunk().len() - 1;

        // When we hit the else branch we want to first clear the condition value from the stack
        // by having the else-branch jump to this pop statement.
        let target = self.chunk().len();
        self.chunk_mut().write(OpCode::Pop, Loc::Native);
        self.chunk_mut()
        .overwrite(OpCode::JumpIfFalse(target), jmp_else);

        if let Some(else_branch) = else_branch {
            self.visit_stmt(else_branch)?;
        }
            
        // Finally we want to tell the then-branch how to jump past the else-branch.
        let target = self.chunk().len();
        self.chunk_mut()
            .overwrite(OpCode::Jump(target), jump_end);

        Ok(())
    }

    fn visit_print(&mut self, loc: &Loc, expr: &Expr) -> Result<(), LoxError> {
        self.visit_expr(expr)?;

        self.chunk_mut().write(OpCode::Print, loc.clone());
        Ok(())
    }

    fn visit_return(&mut self, token: &Token, expr: Option<&Expr>) -> Result<(), LoxError> {
        if matches!(self.state().fun_type, Some(FunType::Initializer)) && expr.is_some() {
            return Err(errors::language(
                token.location(),
                "Cannot return a value from an initializer.",
                "Remove the return statement inside your init() function.",
            ));
        }

        if let Some(expr) = expr {
            self.visit_expr(expr)?;
        } else if matches!(self.state().fun_type, Some(FunType::Initializer)) {
            self.chunk_mut().write(OpCode::GetLocal(0), token.location());
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
        self.chunk_mut().write(OpCode::Pop, Loc::Native);

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
