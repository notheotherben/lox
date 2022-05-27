use crate::{vm::{Chunk, Value, OpCode}, ast::{ExprVisitor, StmtVisitor, Literal, Expr, Stmt}, LoxError, lexer::Token, Loc, errors};

#[derive(Default)]
pub struct Compiler {
    pub chunk: Chunk,
}

impl ExprVisitor<Result<(), LoxError>> for Compiler {
    fn visit_assign(&mut self, ident: &Token, value: &Expr) -> Result<(), LoxError> {
        todo!()
    }

    fn visit_binary(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<(), LoxError> {
        todo!()
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
        todo!()
    }

    fn visit_literal(&mut self, loc: &Loc, value: &Literal) -> Result<(), LoxError> {
        match value {
            Literal::Nil => todo!(),
            Literal::Bool(_) => todo!(),
            Literal::Number(value) => {
                let ptr = self.chunk.add_constant(Value::Number(*value));

                self.chunk.write(OpCode::Constant(ptr), loc.clone());
            },
            Literal::String(_) => todo!(),
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
            Token::Bang(..) => todo!(),
            _ => Err(errors::language(
                op.location(),
                "Unrecognized unary operator.",
                "Only unary negation (`-`) and logical negation (`!`) are supported."
            ))?,
        };

        Ok(())
    }

    fn visit_var_ref(&mut self, name: &Token) -> Result<(), LoxError> {
        todo!()
    }
}


impl StmtVisitor<Result<(), LoxError>> for Compiler {
    fn visit_break(&mut self, _loc: &Loc) -> Result<(), LoxError> {
        todo!()
    }

    fn visit_block(&mut self, stmts: &[Stmt]) -> Result<(), LoxError> {
        todo!()
    }

    fn visit_class(&mut self, name: &Token, superclass: Option<&Expr>, statics: &[Stmt], methods: &[Stmt]) -> Result<(), LoxError> {
        todo!()
    }

    fn visit_expr_stmt(&mut self, expr: &Expr) -> Result<(), LoxError> {
        self.visit_expr(expr)
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
        todo!()
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
        ($chunk:expr => $val:expr) => {
            {
                let output = Box::new(CaptureOutput::default());
                VM::default().with_output(output.clone()).interpret($chunk).expect("no errors");
                assert_eq!(output.to_string().trim(), format!("{}", $val).trim());
            }
        };
    }

    #[test]
    fn test_stage1() {
        let source = "print -5;";
        let stmts = parse(source);
        assert_eq!(stmts.len(), 1);

        let chunk = compile(&stmts).expect("no errors");

        run!(chunk => -5);
    }
}