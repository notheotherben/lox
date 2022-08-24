use std::{collections::{HashMap, LinkedList}, fmt::Debug, rc::Rc, time::SystemTime};

use crate::{errors, Loc, LoxError};

use super::{chunk::Chunk, ops::OpCode, value::Value, value::{VarRef, Upvalue}, Frame, Function};

pub struct VM {
    debug: bool,
    output: Box<dyn std::io::Write>,

    stack: Vec<Rc<Value>>,
    open_upvalues: LinkedList<Rc<Upvalue>>,

    globals: HashMap<String, Rc<Value>>,
    frames: Vec<Frame>,
}

macro_rules! op_binary {
    ($self:ident ($left:ident, $right:ident), Any : $op:tt => $res:ident) => {
        {
            let $right = $self.pop()?;
            let $left = $self.pop()?;

            #[allow(unused_parens)]
            let result = $op;
            $self.push(Rc::new(Value::$res(result)))
        }
    };

    ($self:ident ($frame:ident, $left:ident, $right:ident), $($src:ident : $op:tt => $res:ident),+) => {
        {
            let right = $self.pop()?;
            let left = $self.pop()?;

            #[allow(unused_parens)]
            match (left.as_ref(), right.as_ref()) {
                $(
                    (Value::$src($left), Value::$src($right)) => {
                        let result = $op;
                        $self.push(Rc::new(Value::$res(result)))
                    }
                )+
                (left, right) => return Err(errors::runtime(
                    $frame.last_location(),
                    format!("Operands must be numbers, but got {} and {}.", left, right),
                    format!("Make sure that you are passing numbers to the {} operator(s).", stringify!($($op),+)),
                ))
            }
        }
    };
}

impl VM {
    pub fn call(&mut self, func: Function) -> Result<(), LoxError> {
        self.frames.push(Frame::root_function(func));

        self.run()
    }

    pub fn interpret(&mut self, chunk: Chunk) -> Result<(), LoxError> {
        self.frames.push(Frame::root_chunk(chunk));

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

    pub fn with_native<N: Into<String>, F: Fn(&[Rc<Value>]) -> Result<Value, LoxError> + 'static>(
        mut self,
        name: N,
        arity: usize,
        fun: F,
    ) -> Self {
        let name = name.into();
        self.globals.insert(
            name.clone(),
            Rc::new(Value::Function(Function::native(name, arity, move |vm| {
                let frame = vm.frame().ok_or_else(|| errors::runtime(Loc::Native, 
                    "No active frame on the virtual machine call stack.",
                    "Report this issue to us on GitHub with example code to reproduce the problem."))?;
                
                fun(&vm.stack[frame.stack_offset + 1..])
            }))),
        );
        self
    }

    pub fn frame(&self) -> Option<Frame> {
        self.frames.last().cloned()
    }

    fn run(&mut self) -> Result<(), LoxError> {
        while let Some(frame) = self.frame() {
            if let Some(&instruction) = frame.opcode() {
                if self.debug {
                    println!("{:?}", self);
                }

                self.frames.last_mut().unwrap().ip += 1;

                match self.step(frame, instruction) {
                    Ok(true) => (),
                    Ok(false) => {
                        return Ok(());
                    },
                    Err(LoxError::Runtime(_loc, msg, advice)) => {
                        let mut stack = Vec::new();

                        for frame in self.frames.iter().rev() {
                            stack.push(format!("{}", &frame));
                        }

                        return Err(errors::runtime_stacktrace(msg, advice, stack));
                    },
                    Err(LoxError::User(msg)) => {
                        let mut stack = Vec::new();

                        for frame in self.frames.iter().rev() {
                            stack.push(format!("{}", &frame));
                        }

                        return Err(errors::user_stacktrace(msg, stack));
                    }
                    Err(err) => return Err(err),
                }
            } else {
                self.close_upvalues(frame.stack_offset)?;
                self.stack.truncate(frame.stack_offset);
                self.frames.pop();
            }
        }

        if !self.stack.is_empty() {
            return Err(errors::system(
                "Stack is not empty at the end of the script.",
                "Make sure that you are returning a value from the script.",
            ));
        }

        Ok(())
    }

    fn step(&mut self, frame: Frame, instruction: OpCode) -> Result<bool, LoxError> {
        match instruction {
            OpCode::Nil => self.stack.push(Rc::new(Value::Nil)),
            OpCode::True => self.stack.push(Rc::new(Value::Bool(true))),
            OpCode::False => self.stack.push(Rc::new(Value::Bool(false))),
            OpCode::Constant(idx) => {
                if let Some(value) = frame.constant(idx) {
                    self.stack.push(Rc::new(value.clone()));
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

            OpCode::GetUpvalue(idx) => {
                if let Some(upvalue) = frame.upvalues.get(idx) {
                    match upvalue.as_ref() {
                        Upvalue::Open(idx) => {
                            let value = self.stack[*idx].clone();
                            self.stack.push(value);
                        }
                        Upvalue::Closed(value) => {
                            self.stack.push(Rc::clone(value));
                        }
                    }
                } else {
                    return Err(errors::runtime(
                        frame.last_location(),
                        "Invalid upvalue index in byte code.",
                        "Make sure that you are passing valid upvalue indices to the virtual machine."
                    ));
                }
            },
            OpCode::SetUpvalue(idx) => {
                let value = self.pop()?;

                if let Some(upvalue) = frame.upvalues.get(idx) {
                    match upvalue.as_ref() {
                        Upvalue::Open(idx) => {
                            self.stack[*idx] = value;
                        }
                        Upvalue::Closed(closure) => {
                            let mut closure = closure.clone();
                            let target = unsafe { Rc::get_mut_unchecked(&mut closure) };
                            *target = Rc::unwrap_or_clone(value);
                        }
                    }
                } else {
                    return Err(errors::runtime(
                        frame.last_location(),
                        "Invalid upvalue index in byte code.",
                        "Make sure that you are passing valid upvalue indices to the virtual machine."
                    ));
                }
            },

            OpCode::GetLocal(idx) => {
                if let Some(value) = self.stack.get(
                    self.frame()
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

                let target = unsafe{ Rc::get_mut_unchecked(self.stack.get_unchecked_mut(idx)) };
                *target = Rc::unwrap_or_clone(value);
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

            OpCode::Negate => match self.pop()?.as_ref() {
                Value::Number(n) => self.stack.push(Rc::new(Value::Number(-n))),
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
                self.push(Rc::new(Value::Bool(!value.is_truthy())));
            }

            OpCode::Equal => op_binary!(self(left, right), Any: (left == right) => Bool),
            OpCode::Greater => op_binary!(self(left, right), Any: (left > right) => Bool),
            OpCode::Less => op_binary!(self(left, right), Any: (left < right) => Bool),

            OpCode::Pop => {
                self.pop()?;
            },
            OpCode::Print => {
                let value = self.pop()?;
                writeln!(self.output, "{}", value)?;
            },

            OpCode::Call(call_arity) => {
                if self.frames.len() >= 1000 {
                    return Err(errors::runtime(
                        frame.last_location(),
                        "The maximum call stack size has been exceeded.",
                        "Make sure that you are not calling functions with unbounded recursion.",
                    ));
                }

                if let Some(value) = self.stack.get(self.stack.len() - call_arity - 2) {
                    match value.as_ref() {
                        Value::Function(Function::ClosedClosure{name, chunk, upvalues, arity, ..}) => {
                            if *arity != call_arity {
                                return Err(errors::runtime(
                                    frame.last_location(),
                                    format!("Invalid number of arguments, got {} but expected {}.", call_arity, arity),
                                    "Make sure that you are passing the correct number of arguments to the function."
                                ));
                            }

                            let new_frame = Frame::call(name.clone(), upvalues.clone(), chunk.clone(), self.stack.len() - arity - 1);
                            self.frames.push(new_frame);
                        },
                        Value::Function(Function::Native { name, arity, fun }) => {
                            if *arity != call_arity {
                                return Err(errors::runtime(
                                    frame.last_location(),
                                    format!("Invalid number of arguments, got {} but expected {}.", call_arity, arity),
                                    "Make sure that you are passing the correct number of arguments to the function."
                                ));
                            }

                            let fun = fun.clone();

                            let stack_top = self.stack.len() - arity - 1;
                            let new_frame = Frame::call(name.clone(), Vec::new(), Rc::new(Chunk::default()), stack_top);
                            self.frames.push(new_frame);
                            let result = fun(self)?;
                            self.frames.pop();
                            self.stack.truncate(stack_top);
                            self.push(Rc::new(result));
                        },
                        Value::Function(Function::OpenClosure { name, .. }) => {
                            return Err(errors::runtime(
                                frame.last_location(),
                                format!("The function '{}' has not been converted into a closure due to a bug in the compiler.", name),
                                "Report this issue to us on GitHub with example code."))
                        }
                        target => return Err(errors::runtime(
                            frame.last_location(),
                            format!("Unsupported calling target {:?}", target),
                            "Make sure that you are calling a valid function or class constructor."))
                        }
                } else {
                    return Err(errors::runtime(
                    frame.last_location(),
                    "Callee could not be retrieved from the stack based on the known calling convention.",
                    "Please report this issue to us on GitHub with example code."))
                }
            },
            OpCode::Closure(idx) => {
                let value = if let Some(value) = frame.constant(idx) {
                    value
                } else {
                    return Err(errors::runtime(
                        frame.last_location(),
                        "Invalid constant index in byte code.",
                        "Make sure that you are passing valid constant indices to the virtual machine."
                    ));
                };

                match value {
                    Value::Function(fun) => {
                        let closure = fun.capture(|ups| {
                            let mut upvalues = Vec::with_capacity(ups.len());
                            for upvalue in ups {
                                match upvalue {
                                    VarRef::Local(idx) => {
                                        let frame_offset = self.frame()
                                            .map(|f| f.stack_offset)
                                            .unwrap_or_default();

                                        let idx = frame_offset + *idx + 1;

                                        let mut target = self.open_upvalues.cursor_front_mut();
                                        while target.current().map(|u| matches!(u.as_ref(), Upvalue::Open(uidx) if *uidx > idx)).unwrap_or_default() {
                                            target.move_next();
                                        }

                                        if target.current().map(|u| matches!(u.as_ref(), Upvalue::Open(uidx) if *uidx == idx)).unwrap_or_default() {
                                            let upvalue = target.current().unwrap().clone();
                                            upvalues.push(upvalue);
                                        } else {
                                            let upvalue = Rc::new(Upvalue::Open(idx));
                                            target.insert_before(upvalue.clone());
                                            upvalues.push(upvalue);
                                        }
                                    },
                                    VarRef::Transitive(idx) => {
                                        let upvalue = frame.upvalues[*idx].clone();
                                        upvalues.push(upvalue);
                                    },
                                }
                            }

                            Ok(upvalues)
                        })?;
                        self.push(Rc::new(Value::Function(closure)));
                    },
                    _ => {
                        return Err(errors::runtime(
                            frame.last_location(),
                            "Attempted to construct a closure over a non-function value. This likely indicates that invalid byte-code is being executed.",
                            "Please report this issue to us on GitHub with example code."))
                    }
                }
            },
            OpCode::CloseUpvalue => {
                self.close_upvalues(self.stack.len() - 1)?;
                self.pop()?;
            },
            OpCode::Return => {
                let result = self.pop()?;
                self.close_upvalues(frame.stack_offset)?;
                self.stack.truncate(frame.stack_offset);
                if self.frames.pop().is_none() {
                    self.pop()?; // pop callee
                    return Ok(false);
                }

                self.push(result);
            },

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

        Ok(true)
    }

    fn push(&mut self, value: Rc<Value>) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Result<Rc<Value>, LoxError> {
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

    fn peek(&self) -> Result<Rc<Value>, LoxError> {
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

    fn close_upvalues(&mut self, stack_top: usize) -> Result<(), LoxError> {
        let mut cursor = self.open_upvalues.cursor_front_mut();
        while cursor.current().map(|u| matches!(u.as_ref(), Upvalue::Open(uidx) if *uidx >= stack_top)).unwrap_or_default() {
            let mut upvalue = cursor.pop_front().unwrap();
            if let Upvalue::Open(idx) = upvalue.as_ref() {
                let value = self.stack.get(*idx)
                    .ok_or_else(|| errors::runtime(
                        Loc::Unknown,
                        format!("Attempted to access local upvalue at a locals index {} which is invalid.", *idx),
                        "Please report this issue to us on GitHub with example code."))?.clone();

                self.stack[*idx] = value.clone();
                let closed = Upvalue::Closed(value);
                let target = unsafe { Rc::get_mut_unchecked(&mut upvalue) };
                *target = closed;
            }
        }

        Ok(())
    }
}

impl Debug for VM {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let frame = self.frame().expect("a frame");

        writeln!(f, "Chunk: {} {:?}", frame.name, frame)?;

        write!(f, "Stack:")?;
        for value in self.stack.iter() {
            write!(f, "[{}] ", value)?;
        }
        writeln!(f)?;

        write!(f, "Locals:")?;
        for value in self.stack.iter().skip(
            self.frame().map(|f| f.stack_offset).unwrap_or_default()
        ) {
            write!(f, "[{}] ", value)?;
        }
        writeln!(f)?;

        write!(f, "Upvalues:")?;
        for upvalue in frame.upvalues.iter() {
            write!(f, "[{}] ", upvalue)?;
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
            open_upvalues: LinkedList::new(),

            globals: HashMap::new(),
            frames: Vec::new(),
        }
        .with_native("clock", 0, |_args| {
            Ok(Value::Number(
                SystemTime::now()
                    .duration_since(SystemTime::UNIX_EPOCH)
                    .map_err(|_| errors::runtime(
                        Loc::Native,
                        "Failed to get current system time because the system time is currently set to a time earlier than 1970-01-01T00:00:00Z.",
                        "Make sure that you have set your system clock correctly."))?
                    .as_secs() as f64,
            ))
        })
        .with_native(
            "assert",
            2,
            |args| {
                if !args[0].is_truthy() {
                    return Err(errors::user(
                        format!("Assertion failed: {}", args[1])));
                }

                Ok(Value::Nil)
            })
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
