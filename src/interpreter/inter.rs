use crate::{ast::{StmtVisitor, Stmt}, LoxError, errors, analysis::analyze, core::Loc};

use super::{env::Environment, Value, Fun};

pub struct Interpreter {
    pub (super) env: Environment,
    pub (super) breaking: bool,
    pub (super) returning: Option<Value>,
    pub output: Box<dyn std::io::Write>,
}

impl Interpreter {
    pub fn interpret(&mut self, stmts: &[Stmt]) -> Vec<LoxError> {
        let mut errs = analyze(stmts);
        if errs.is_empty() {
            for stmt in stmts.iter() {
                match self.visit_stmt(stmt) {
                    Ok(_) => (),
                    Err(err) => {
                        errs.push(err);
                        break
                    },
                }
            }
        }

        errs
    }

    pub fn with_output(self, output: Box<dyn std::io::Write>) -> Self {
        Self {
            output,
            ..self
        }
    }

    pub fn into_output(self) -> Box<dyn std::io::Write> {
        self.output
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        let mut globals = Environment::new();

        fn id(key: &str) -> crate::lexer::Token {
            crate::lexer::Token::Identifier(Loc::Native, key.to_string())
        }

        globals.define(&id("clock"), Value::Function(Fun::native("native@clock", 0, |_, _| {
            let offset = std::time::SystemTime::now().duration_since(std::time::SystemTime::UNIX_EPOCH)
                .map_err(|_| errors::runtime(
                    Loc::Native,
                    "Failed to get current system time because the system time is currently set to a time earlier than 1970-01-01T00:00:00Z.",
                    "Make sure that you have set your system clock correctly."))?;
                
            Ok(Value::Number(offset.as_secs() as f64))
        })));

        globals.define(&id("assert"), Value::Function(Fun::native("native@assert", 2, |_, args| {
            if !args[0].is_truthy() {
                return Err(errors::user(
                    format!("Assertion failed: {}", args[1]),
                ));
            }

            Ok(Value::Nil)
        })));

        Self {
            env: globals,
            breaking: false,
            returning: None,
            output: Box::new(std::io::stdout()),
        }
    }
}