use std::{collections::HashMap, fmt::Debug};

use crate::{errors, Loc, LoxError};

use super::{chunk::Chunk, ops::OpCode, value::Value, Frame, Function};

pub struct VM {
    debug: bool,
    output: Box<dyn std::io::Write>,

    stack: Vec<Value>,

    globals: HashMap<String, Value>,
    frames: Vec<Frame>,
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

    ($self:ident ($frame:ident, $left:ident, $right:ident), $($src:ident : $op:tt => $res:ident),+) => {
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
                    $frame.last_location(),
                    "Operands must be numbers.",
                    "Make sure that you are passing numbers to the $op operator."
                ))
            }
        }
    };
}

impl VM {
    pub fn interpret(&mut self, chunk: Chunk) -> Result<(), LoxError> {
        self.frames.push(Frame::root(chunk));

        self.run()
    }

    pub fn with_output(self, output: Box<dyn std::io::Write>) -> Self {
        Self { output, ..self }
    }

    pub fn with_debug(self) -> Self {
        Self {
            debug: true,
            ..self
        }
    }

    pub fn frame(&self) -> Option<Frame> {
        self.frames.last().cloned()
    }

    fn run(&mut self) -> Result<(), LoxError> {
        while let Some(frame) = self.frame() {
            if let Some(&instruction) = frame.opcode() {
                self.frames.last_mut().unwrap().ip += 1;

                if self.debug {
                    println!("{:?}", self);
                }

                match self.step(frame, instruction) {
                    Ok(()) => (),
                    Err(LoxError::Runtime(_loc, msg, advice)) => {
                        let mut stack = Vec::new();

                        for frame in self.frames.iter().rev() {
                            stack.push(format!("{}", &frame));
                        }

                        return Err(errors::runtime_stacktrace(msg, advice, stack))
                    },
                    Err(err) => return Err(err),
                }
            } else {
                self.stack.truncate(frame.stack_offset);
                self.frames.pop();
            }
        }

        if !self.stack.is_empty() {
            return Err(errors::system(
                "Stack is not empty at the end of the script.",
                "Make sure that you are returning a value from the script."
            ))
        }

        Ok(())
    }

    fn step(&mut self, frame: Frame, instruction: OpCode) -> Result<(), LoxError> {
        match instruction {
            OpCode::Nil => self.stack.push(Value::Nil),
            OpCode::True => self.stack.push(Value::Bool(true)),
            OpCode::False => self.stack.push(Value::Bool(false)),
            OpCode::Constant(idx) => {
                if let Some(value) = frame.constant(idx) {
                    self.stack.push(value.clone());
                } else {
                    return Err(errors::runtime(
                    frame.last_location(),
                    "Invalid constant index in byte code.",
                    "Make sure that you are passing valid constant indices to the virtual machine."
                ));
                }
            }

            OpCode::DefineGlobal(idx) => {
                if let Some(Value::String(key)) = frame.constant(idx) {
                    let key = key.clone();
                    let value = self.pop()?;
                    self.globals.insert(key, value);
                } else {
                    return Err(errors::runtime(
                    frame.last_location(),
                    "Invalid constant index in byte code.",
                    "Make sure that you are passing valid constant indices to the virtual machine."
                ));
                }
            }
            OpCode::GetGlobal(idx) => {
                if let Some(Value::String(key)) = frame.constant(idx) {
                    let key = key.clone();
                    if let Some(value) = self.globals.get(&key) {
                        self.stack.push(value.clone());
                    } else {
                        return Err(errors::runtime(
                        frame.last_location(),
                        &format!("The variable '{}' is not defined.", key),
                        "Make sure that you have defined the variable using the `var` keyword before referencing it."
                    ));
                    }
                } else {
                    return Err(errors::runtime(
                    frame.last_location(),
                    "Invalid constant index in byte code.",
                    "Make sure that you are passing valid constant indices to the virtual machine."
                ));
                }
            }
            OpCode::SetGlobal(idx) => {
                if let Some(Value::String(key)) = frame.constant(idx) {
                    let key = key.clone();
                    let value = self.peek()?;
                    self.globals.insert(key, value);
                } else {
                    return Err(errors::runtime(
                    frame.last_location(),
                    "Invalid constant index in byte code.",
                    "Make sure that you are passing valid constant indices to the virtual machine."
                ));
                }
            }

            OpCode::GetLocal(idx) => {
                if let Some(value) = self.stack.get(
                    self.frames
                        .last()
                        .map(|f| f.stack_offset)
                        .unwrap_or_default()
                        + idx,
                ) {
                    self.stack.push(value.clone());
                } else {
                    return Err(errors::runtime(
                    frame.last_location(),
                    "Invalid local index in byte code.",
                    "Make sure that you are passing valid local indices to the virtual machine."
                ));
                }
            }
            OpCode::SetLocal(idx) => {
                let idx = self
                    .frames
                    .last()
                    .map(|f| f.stack_offset)
                    .unwrap_or_default()
                    + idx;
                let value = self.pop()?;

                if idx >= self.stack.len() {
                    return Err(errors::runtime(
                    frame.last_location(),
                    "Invalid stack index in byte code for local assignment.",
                    "Make sure that you are passing valid stack indices to the virtual machine."
                ));
                }

                self.stack[idx] = value;
            }

            OpCode::Add => op_binary!(
            self(frame, left, right),
            Number: (left + right) => Number,
            String: (format!("{}{}", left, right)) => String),
            OpCode::Subtract => {
                op_binary!(self(frame, left, right), Number: (left - right) => Number)
            }
            OpCode::Multiply => {
                op_binary!(self(frame, left, right), Number: (left * right) => Number)
            }
            OpCode::Divide => {
                op_binary!(self(frame, left, right), Number: (left / right) => Number)
            }

            OpCode::Negate => match self.pop()? {
                Value::Number(n) => self.stack.push(Value::Number(-n)),
                _ => {
                    return Err(errors::runtime(
                        frame.last_location(),
                        "Operand must be a number.",
                        "Make sure that you are passing a number to the negate operator.",
                    ))
                }
            },
            OpCode::Not => {
                let value = self.pop()?;
                self.push(Value::Bool(!value.is_truthy()));
            }

            OpCode::Equal => op_binary!(self(left, right), Any: (left == right) => Bool),
            OpCode::Greater => op_binary!(self(left, right), Any: (left > right) => Bool),
            OpCode::Less => op_binary!(self(left, right), Any: (left < right) => Bool),

            OpCode::Pop => {
                self.pop()?;
            }
            OpCode::Print => {
                let value = self.pop()?;
                writeln!(self.output, "{}", value)?;
            }

            OpCode::Call(call_arity) => {
                if self.frames.len() >= 1000 {
                    return Err(errors::runtime(
                        frame.last_location(),
                        "The maximum call stack size has been exceeded.",
                        "Make sure that you are not calling functions with unbounded recursion.",
                    ));
                }

                match self.stack.get(self.stack.len() - call_arity - 2) {
                    Some(Value::Function(Function::Closure{name, chunk, arity, ..})) => {
                        if *arity != call_arity {
                            return Err(errors::runtime(
                                frame.last_location(),
                                format!("Invalid number of arguments, got {} but expected {}.", call_arity, arity),
                                "Make sure that you are passing the correct number of arguments to the function."
                            ));
                        }

                        let new_frame = Frame::call(name.clone(), chunk.clone(), self.stack.len() - arity - 1);
                        self.frames.push(new_frame);
                    },
                    Some(target) => Err(errors::runtime(
                        frame.last_location(),
                        format!("Unsupported calling target {:?}", target),
                        "Make sure that you are calling a valid function or class constructor."))?,
                    None => Err(errors::runtime(
                        frame.last_location(),
                        "Callee could not be retrieved from the stack based on the known calling convention.",
                        "Please report this issue to us on GitHub with example code."))?,
                };
            }
            OpCode::Return => {
                self.stack.truncate(frame.stack_offset);
                self.frames.pop();
            }

            OpCode::Jump(ip) => {
                self.jump(ip);
            }
            OpCode::JumpIf(ip) => {
                let value = self.peek()?;
                if value.is_truthy() {
                    self.jump(ip);
                }
            }
            OpCode::JumpIfFalse(ip) => {
                let value = self.peek()?;
                if !value.is_truthy() {
                    self.jump(ip);
                }
            }
        };

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
                self.frame()
                    .map(|f| f.last_location())
                    .unwrap_or(Loc::Unknown),
                "Attempted to pop with no values on the stack.",
                "Don't try to do this? o.O",
            ))
        }
    }

    fn peek(&self) -> Result<Value, LoxError> {
        if let Some(value) = self.stack.last() {
            Ok(value.clone())
        } else {
            Err(errors::runtime(
                self.frame()
                    .map(|f| f.last_location())
                    .unwrap_or(Loc::Unknown),
                "Attempted to peek with no values on the stack.",
                "Don't try to do this? o.O",
            ))
        }
    }

    fn jump(&mut self, ip: usize) {
        self.frames.last_mut().unwrap().ip = ip;
    }
}

impl Debug for VM {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "Chunk: {:?}", self.frame().expect("a frame"))?;

        write!(f, "Stack:")?;
        for value in self.stack.iter() {
            write!(f, "[{}] ", value)?;
        }
        writeln!(f)?;

        write!(f, "Locals:")?;
        for value in self.stack.iter().skip(
            self.frames
                .last()
                .map(|f| f.stack_offset)
                .unwrap_or_default(),
        ) {
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

            stack: Vec::new(),

            globals: HashMap::new(),
            frames: Vec::new(),
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
        ($chunk:expr => $val:expr) => {{
            let output = Box::new(CaptureOutput::default());
            VM::default()
                .with_output(output.clone())
                .interpret($chunk)
                .expect("no errors");
            assert_eq!(output.to_string().trim(), format!("{}", $val).trim());
        }};
    }

    #[test]
    fn test_negate() {
        let chunk = chunk!(Constant[Number = 123], Negate, Print);

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
            Print
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
            Print
        );

        run!(chunk => 15);
    }

    #[test]
    fn test_boolean() {
        let chunk = chunk!(True, Not, Print);

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
