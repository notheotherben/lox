use std::collections::HashMap;

use crate::{vm::{Chunk, Value, OpCode}, ast::{ExprVisitor, StmtVisitor, Literal, Expr, Stmt}, LoxError, lexer::Token, Loc, errors};

#[derive(Default)]
pub struct Compiler {
    pub chunk: Chunk,

    identifiers: HashMap<String, usize>,
    locals: Vec<Token>,
    scope_depth: usize,
}

impl Compiler {
    fn identifier(&mut self, name: &str) -> usize {
        let index = self.identifiers.entry(name.to_string()).or_insert_with(|| self.chunk.add_constant(Value::String(name.to_string())));
        *index
    }

    fn define_local(&mut self, name: &Token) {
        self.locals.push(name.clone());
    }

    fn get_local(&mut self, token: &Token) -> Option<usize> {
        for (idx, local) in self.locals.iter().enumerate().rev() {
            if local.lexeme() == token.lexeme() {
                return Some(idx);
            }
        }

        None
    } 
}

impl ExprVisitor<Result<(), LoxError>> for Compiler {
    fn visit_assign(&mut self, ident: &Token, value: &Expr) -> Result<(), LoxError> {
        self.visit_expr(value)?;

        if let Some(idx) = self.get_local(ident) {
            self.chunk.write(OpCode::SetLocal(idx), ident.location());
        } else {
            let idx = self.identifier(ident.lexeme());
            self.chunk.write(OpCode::SetGlobal(idx), ident.location());
        }

        Ok(())
    }

    fn visit_binary(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<(), LoxError> {
        self.visit_expr(left)?;
        self.visit_expr(right)?;

        match op {
            Token::Plus(..) => self.chunk.write(OpCode::Add, op.location()),
            Token::Minus(..) => self.chunk.write(OpCode::Subtract, op.location()),
            Token::Star(..) => self.chunk.write(OpCode::Multiply, op.location()),
            Token::Slash(..) => self.chunk.write(OpCode::Divide, op.location()),

            Token::EqualEqual(..) => self.chunk.write(OpCode::Equal, op.location()),
            Token::BangEqual(..) => {
                self.chunk.write(OpCode::Equal, op.location());
                self.chunk.write(OpCode::Not, op.location());
            },
            Token::Greater(..) => self.chunk.write(OpCode::Greater, op.location()),
            Token::GreaterEqual(..) => {
                self.chunk.write(OpCode::Less, op.location());
                self.chunk.write(OpCode::Not, op.location());
            },
            Token::Less(..) => self.chunk.write(OpCode::Less, op.location()),
            Token::LessEqual(..) => {
                self.chunk.write(OpCode::Greater, op.location());
                self.chunk.write(OpCode::Not, op.location());
            },
            _ => todo!("{:?}", op),
        }

        Ok(())
    }

    fn visit_call(&mut self, callee: &Expr, args: &[Expr], close: &Token) -> Result<(), LoxError> {
        todo!()
    }

    fn visit_get(&mut self, obj: &Expr, property: &Token) -> Result<(), LoxError> {
        todo!()
    }

    fn visit_fun_expr(&mut self, loc: &Loc, params: &[Token], body: &[Stmt]) -> Result<(), LoxError> {
        todo!()
    }

    fn visit_grouping(&mut self, expr: &Expr) -> Result<(), LoxError> {
        self.visit_expr(expr)
    }

    fn visit_literal(&mut self, loc: &Loc, value: &Literal) -> Result<(), LoxError> {
        match value {
            Literal::Nil => self.chunk.write(OpCode::Nil, loc.clone()),
            Literal::Bool(value) => self.chunk.write(if *value { OpCode::True } else { OpCode::False }, loc.clone()),
            Literal::Number(value) => {
                let ptr = self.chunk.add_constant(Value::Number(*value));

                self.chunk.write(OpCode::Constant(ptr), loc.clone());
            },
            Literal::String(value) => {
                let ptr = self.chunk.add_constant(Value::String(value.clone()));

                self.chunk.write(OpCode::Constant(ptr), loc.clone());
            },
        };

        Ok(())
    }

    fn visit_logical(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<(), LoxError> {
        todo!()
    }

    fn visit_set(&mut self, obj: &Expr, property: &Token, value: &Expr) -> Result<(), LoxError> {
        todo!()
    }

    fn visit_super(&mut self, loc: &Loc, method: &Token) -> Result<(), LoxError> {
        todo!()
    }

    fn visit_this(&mut self, loc: &Loc) -> Result<(), LoxError> {
        todo!()
    }

    fn visit_unary(&mut self, op: &Token, expr: &Expr) -> Result<(), LoxError> {
        self.visit_expr(expr)?;

        match op {
            Token::Minus(..) => self.chunk.write(OpCode::Negate, op.location()),
            Token::Bang(..) => self.chunk.write(OpCode::Not, op.location()),
            _ => Err(errors::language(
                op.location(),
                "Unrecognized unary operator.",
                "Only numerical negation (`-`) and logical negation (`!`) are supported."
            ))?,
        };

        Ok(())
    }

    fn visit_var_ref(&mut self, name: &Token) -> Result<(), LoxError> {
        if let Some(idx) = self.get_local(name) {
            self.chunk.write(OpCode::GetLocal(idx), name.location());
        } else {
            let idx = self.identifier(name.lexeme());
            self.chunk.write(OpCode::GetGlobal(idx), name.location());
        }

        Ok(())
    }
}


impl StmtVisitor<Result<(), LoxError>> for Compiler {
    fn visit_break(&mut self, _loc: &Loc) -> Result<(), LoxError> {
        todo!()
    }

    fn visit_block(&mut self, stmts: &[Stmt]) -> Result<(), LoxError> {
        let parent_locals_len = self.locals.len();
        self.scope_depth += 1;
        
        for stmt in stmts {
            self.visit_stmt(stmt)?;
        }

        self.scope_depth -= 1;
        self.locals.truncate(parent_locals_len);

        Ok(())
    }

    fn visit_class(&mut self, name: &Token, superclass: Option<&Expr>, statics: &[Stmt], methods: &[Stmt]) -> Result<(), LoxError> {
        todo!()
    }

    fn visit_expr_stmt(&mut self, expr: &Expr) -> Result<(), LoxError> {
        self.visit_expr(expr)?;
        self.chunk.write(OpCode::Pop, Loc::Unknown);
        Ok(())
    }

    fn visit_fun_def(&mut self, ty: crate::ast::FunType, name: &Token, params: &[Token], body: &[Stmt]) -> Result<(), LoxError> {
        todo!()
    }

    fn visit_if(&mut self, expr: &Expr, then_branch: &Stmt, else_branch: Option<&Stmt>) -> Result<(), LoxError> {
        todo!()
    }

    fn visit_print(&mut self, loc: &Loc, expr: &Expr) -> Result<(), LoxError> {
        self.visit_expr(expr)?;

        self.chunk.write(OpCode::Print, loc.clone());
        Ok(())
    }

    fn visit_return(&mut self, token: &Token, expr: Option<&Expr>) -> Result<(), LoxError> {
        if let Some(expr) = expr {
            self.visit_expr(expr)?;
        }

        self.chunk.write(OpCode::Return, token.location());

        Ok(())
    }

    fn visit_var_def(&mut self, name: &Token, expr: &Expr) -> Result<(), LoxError> {
        self.visit_expr(expr)?;

        if self.scope_depth > 0 {
            self.define_local(name);
            self.chunk.write(OpCode::DefineLocal, name.location());
        } else {
            let ptr = self.identifier(name.lexeme());
            self.chunk.write(OpCode::DefineGlobal(ptr), name.location());
        }

        Ok(())
    }

    fn visit_while(&mut self, expr: &Expr, body: &Stmt) -> Result<(), LoxError> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::Parser, lexer::Scanner, compiler::compile, CaptureOutput, vm::VM};

    use super::*;

    fn parse(source: &str) -> Vec<Stmt> {
        let lexer = Scanner::new(source);
        let (stmts, errs) = Parser::parse(&mut lexer.inspect(|t| if let Err(e) = t {
            panic!("{}", e);
        }).filter_map(|t| t.ok()));

        if errs.is_empty() {
            stmts
        } else {
            panic!("{:?}", errs);
        }
    }

    macro_rules! run {
        ($src:expr => $val:expr) => {
            {
                let stmts = parse($src);

                let chunk = compile(&stmts).expect("no errors");

                let output = Box::new(CaptureOutput::default());
                VM::default().with_output(output.clone()).interpret(chunk).expect("no errors");
                assert_eq!(output.to_string().trim(), format!("{}", $val).trim());
            }
        };
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
}