use std::{fmt::Debug, collections::HashMap};

use crate::{LoxError, errors};

use super::{chunk::Chunk, ops::OpCode, value::Value};

pub struct VM {
    debug: bool,
    output: Box<dyn std::io::Write>,

    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
    
    globals: HashMap<String, Value>,
    locals: Vec<Value>,
}

macro_rules! op_binary {
    ($self:ident ($left:ident, $right:ident), Any : $op:tt => $res:ident) => {
        {
            let $right = $self.pop()?;
            let $left = $self.pop()?;

            #[allow(unused_parens)]
            let result = $op;
            $self.push(Value::$res(result))
        }
    };

    ($self:ident ($left:ident, $right:ident), $($src:ident : $op:tt => $res:ident),+) => {
        {
            let right = $self.pop()?;
            let left = $self.pop()?;

            #[allow(unused_parens)]
            match (left, right) {
                $(
                    (Value::$src($left), Value::$src($right)) => {
                        let result = $op;
                        $self.push(Value::$res(result))
                    }
                )+
                _ => return Err(errors::runtime(
                    $self.chunk.location($self.ip - 1),
                    "Operands must be numbers.",
                    "Make sure that you are passing numbers to the $op operator."
                ))
            }
        }
    };
}

impl VM {
    pub fn interpret(&mut self, chunk: Chunk) -> Result<(), LoxError> {
        self.chunk = chunk;
        self.ip = 0;

        self.run()
    }

    pub fn with_output(self, output: Box<dyn std::io::Write>) -> Self {
        Self { output, ..self }
    }

    pub fn with_debug(self) -> Self {
        Self { debug: true, ..self }
    }

    fn run(&mut self) -> Result<(), LoxError> {
        while let Some(instruction) = self.chunk.code.get(self.ip) {
            self.ip += 1;

            if self.debug {
                println!("{:?}", self);
            }

            match instruction {
                OpCode::Nil => self.stack.push(Value::Nil),
                OpCode::True => self.stack.push(Value::Bool(true)),
                OpCode::False => self.stack.push(Value::Bool(false)),
                OpCode::Constant(idx) => {
                    if let Some(value) = self.chunk.constants.get(*idx) {
                        self.stack.push(value.clone());
                    } else {
                        return Err(errors::runtime(
                            self.chunk.location(self.ip - 1),
                            "Invalid constant index in byte code.",
                            "Make sure that you are passing valid constant indices to the virtual machine."
                        ))
                    }
                },

                OpCode::DefineGlobal(idx) => {
                    if let Some(Value::String(key)) = self.chunk.constants.get(*idx) {
                        let key = key.clone();
                        let value = self.pop()?;
                        self.globals.insert(key, value);
                    } else {
                        return Err(errors::runtime(
                            self.chunk.location(self.ip - 1),
                            "Invalid constant index in byte code.",
                            "Make sure that you are passing valid constant indices to the virtual machine."
                        ))
                    }
                },
                OpCode::GetGlobal(idx) => {
                    if let Some(Value::String(key)) = self.chunk.constants.get(*idx) {
                        let key = key.clone();
                        if let Some(value) = self.globals.get(&key) {
                            self.stack.push(value.clone());
                        } else {
                            return Err(errors::runtime(
                                self.chunk.location(self.ip - 1),
                                &format!("The variable '{}' is not defined.", key),
                                "Make sure that you have defined the variable using the `var` keyword before referencing it."
                            ))
                        }
                    } else {
                        return Err(errors::runtime(
                            self.chunk.location(self.ip - 1),
                            "Invalid constant index in byte code.",
                            "Make sure that you are passing valid constant indices to the virtual machine."
                        ))
                    }
                },
                OpCode::SetGlobal(idx) => {
                    if let Some(Value::String(key)) = self.chunk.constants.get(*idx) {
                        let key = key.clone();
                        let value = self.peek()?;
                        self.globals.insert(key, value);
                    } else {
                        return Err(errors::runtime(
                            self.chunk.location(self.ip - 1),
                            "Invalid constant index in byte code.",
                            "Make sure that you are passing valid constant indices to the virtual machine."
                        ))
                    }
                },

                OpCode::DefineLocal => {
                    let value = self.pop()?;
                    self.locals.push(value);
                },
                OpCode::GetLocal(idx) => {
                    if let Some(value) = self.locals.get(*idx) {
                        self.stack.push(value.clone());
                    } else {
                        return Err(errors::runtime(
                            self.chunk.location(self.ip - 1),
                            "Invalid local index in byte code.",
                            "Make sure that you are passing valid local indices to the virtual machine."
                        ))
                    }
                },
                OpCode::SetLocal(idx) => {
                    let idx = *idx;
                    let value = self.pop()?;

                    if idx >= self.locals.len() {
                        return Err(errors::runtime(
                            self.chunk.location(self.ip - 1),
                            "Invalid local index in byte code.",
                            "Make sure that you are passing valid local indices to the virtual machine."
                        ))
                    }

                    self.locals[idx] = value;
                },
                OpCode::TruncateLocals(len) => {
                    self.locals.truncate(*len);
                }

                OpCode::Add => op_binary!(
                    self(left, right),
                    Number: (left + right) => Number,
                    String: (format!("{}{}", left, right)) => String),
                OpCode::Subtract => op_binary!(self(left, right), Number: (left - right) => Number),
                OpCode::Multiply => op_binary!(self(left, right), Number: (left * right) => Number),
                OpCode::Divide => op_binary!(self(left, right), Number: (left / right) => Number),

                OpCode::Negate => {
                    match self.pop()? {
                        Value::Number(n) => self.stack.push(Value::Number(-n)),
                        _ => return Err(errors::runtime(
                            self.chunk.location(self.ip - 1),
                            "Operand must be a number.",
                            "Make sure that you are passing a number to the negate operator."
                        ))
                    }
                }
                OpCode::Not => {
                    let value = self.pop()?;
                    self.push(Value::Bool(!value.is_truthy()));
                }

                OpCode::Equal => op_binary!(self(left, right), Any: (left == right) => Bool),
                OpCode::Greater => op_binary!(self(left, right), Any: (left > right) => Bool),
                OpCode::Less => op_binary!(self(left, right), Any: (left < right) => Bool),

                OpCode::Pop => { self.pop()?; },
                OpCode::Print => {
                    let value = self.pop()?;
                    writeln!(self.output, "{}", value)?;
                }
                OpCode::Return => {
                    let value = self.pop()?;
                    writeln!(self.output, "{}", value)?;

                    return Ok(());
                },

                OpCode::Jump(ip) => {
                    self.ip = *ip;
                },
                OpCode::JumpIf(ip) => {
                    let ip  = *ip;
                    let value = self.peek()?;
                    if value.is_truthy() {
                        self.ip = ip;
                    }
                },
                OpCode::JumpIfFalse(ip) => {
                    let ip = *ip;
                    let value = self.peek()?;
                    if !value.is_truthy() {
                        self.ip = ip;
                    }
                },
            }
        }

        Ok(())
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Result<Value, LoxError> {
        if let Some(value) = self.stack.pop() {
            Ok(value)
        } else {
            Err(errors::runtime(
                self.chunk.location(self.ip - 1),
                "Attempted to pop with no values on the stack.",
                "Don't try to do this? o.O"))
        }
    }

    fn peek(&self) -> Result<Value, LoxError> {
        if let Some(value) = self.stack.last() {
            Ok(value.clone())
        } else {
            Err(errors::runtime(
                self.chunk.location(self.ip - 1),
                "Attempted to peek with no values on the stack.",
                "Don't try to do this? o.O"))
        }
    }
}

impl Debug for VM {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Chunk: ")?;
        self.chunk.disassemble(self.ip, f)?;

        write!(f, "Stack:")?;
        for value in self.stack.iter() {
            write!(f, "[{}] ", value)?;
        }
        writeln!(f)?;

        write!(f, "Locals:")?;
        for value in self.locals.iter() {
            write!(f, "[{}] ", value)?;
        }
        writeln!(f)?;

        write!(f, "Globals:")?;
        for (key, value) in self.globals.iter() {
            write!(f, "{}=[{}] ", key, value)?;
        }
        writeln!(f)
    }
}

impl Default for VM {
    fn default() -> Self {
        Self {
            debug: false,
            output: Box::new(std::io::stdout()),

            chunk: Chunk::default(),
            ip: 0,
            stack: Vec::new(),

            globals: HashMap::new(),
            locals: Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::CaptureOutput;

    use super::*;

    macro_rules! chunk {
        ($(
            $code:ident $([ $($ty:ident = $val:expr),+ ])?
        ),*) => {
            {
                let mut chunk = Chunk::default();

                $(
                    {
                        let op = OpCode::$code$((
                            $(
                                chunk.add_constant(Value::$ty($val.into())),
                            ),+
                        ))?;

                        chunk.write(op, $crate::Loc::new(0));
                    }
                )*

                chunk
            }
        };
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
    fn test_negate() {
        let chunk = chunk!(
            Constant[Number = 123],
            Negate,
            Return
        );

        run!(chunk => "-123");
    }

    #[test]
    fn test_binary_math() {
        let chunk = chunk!(
            Constant[Number = 1.2],
            Constant[Number = 3.4],
            Add,
            Constant[Number = 5.6],
            Divide,
            Negate,
            Return
        );

        run!(chunk => -((3.4 + 1.2)/5.6));

        let chunk = chunk!(
            Constant[Number = 5],
            Constant[Number = 7],
            Add,
            Constant[Number = 2],
            Subtract,
            Constant[Number = 2],
            Divide,
            Constant[Number = 3],
            Multiply,
            Return
        );

        run!(chunk => 15);
    }

    #[test]
    fn test_boolean() {
        let chunk = chunk!(
            True,
            Not,
            Print
        );

        run!(chunk => false);
    }

    #[test]
    fn test_strings() {
        let chunk = chunk!(
            Constant[String = "st"],
            Constant[String = "ri"],
            Add,
            Constant[String = "ng"],
            Add,
            Print
        );

        run!(chunk => "string");
    }
}