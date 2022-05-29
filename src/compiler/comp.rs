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
}