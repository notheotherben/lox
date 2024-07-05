use std::{
    collections::LinkedList,
    time::SystemTime,
};

use crate::{
    compiler::{self, Chunk, OpCode, Primitive, VarRef},
    errors, Loc, LoxError,
};

use super::{
    alloc::Alloc, class::Instance, fun::BoundMethod, gc::Allocator, upvalue::Upvalue,
    value::PrimitiveReference, Class, Collectible, Frame, Function, Value, GC,
};

pub struct VM {
    debug: bool,
    output: Box<dyn std::io::Write>,
    gc: GC,

    stack: Vec<Value>,
    open_upvalues: LinkedList<Alloc<Upvalue>>,

    globals: ahash::AHashMap<String, Alloc<Value>>,
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

    ($self:ident ($loc:expr, $left:ident, $right:ident), $($src:ident : $op:tt => $res:ident),+) => {
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
                (left, right) => return Err(errors::runtime(
                    $loc,
                    format!("Operands must be numbers, but got {} and {}.", left, right),
                    format!("Make sure that you are passing numbers to the {} operator(s).", stringify!($($op),+)),
                ))
            }
        }
    };
}

macro_rules! frame_loc {
    ($frames:ident) => {
        $frames
            .last()
            .map(|f| f.last_location())
            .unwrap_or(Loc::Unknown)
    };
}

impl VM {
    pub fn run_function(mut self, func: compiler::Function) -> Result<(), LoxError> {
        let frame = Frame::root_function(func, &mut self.gc);
        self.run(frame)
    }

    pub fn run_chunk(self, chunk: Chunk) -> Result<(), LoxError> {
        self.run(Frame::root_chunk(chunk))
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

    pub fn with_native<N: Into<String>, F: Fn(&[Value]) -> Result<Value, LoxError> + 'static>(
        mut self,
        name: N,
        arity: usize,
        fun: F,
    ) -> Self {
        let name = name.into();
        let function = Value::Function(self.gc.alloc(Function::native(
            name.clone(),
            arity,
            move |vm, frame| fun(&vm.stack[frame.stack_offset + 1..]),
        )));

        self.globals.insert(name, self.gc.alloc(function));
        self
    }

    fn collect(&mut self, frames: &[Frame]) {
        if let Some(stats) = self.gc.collect(|| {
            for slot in self.stack.iter() {
                slot.gc();
            }

            for frame in frames.iter() {
                frame.gc();
            }

            for upvalue in self.open_upvalues.iter() {
                upvalue.gc();
            }

            for global in self.globals.values() {
                global.gc();
            }
        }) {
            if self.debug {
                eprintln!("{}", stats);
            }
        }
    }

    fn run(mut self, frame: Frame) -> Result<(), LoxError> {
        let mut frames = Vec::with_capacity(1000);
        frames.push(frame);

        while !frames.is_empty() {
            if let Some(instruction) = frames.last().and_then(|f| f.opcode()).copied() {
                if self.debug {
                    self.print_debug(&frames)
                }

                frames.last_mut().unwrap().ip += 1;

                match self.step(&mut frames, instruction) {
                    Ok(true) => (),
                    Ok(false) => {
                        return Ok(());
                    }
                    Err(LoxError::Runtime(_loc, msg, advice)) => {
                        let mut stack = Vec::new();

                        for frame in frames.iter().rev() {
                            stack.push(format!("{}", &frame));
                        }

                        return Err(errors::runtime_stacktrace(msg, advice, stack));
                    }
                    Err(LoxError::User(msg)) => {
                        let mut stack = Vec::new();

                        for frame in frames.iter().rev() {
                            stack.push(format!("{}", &frame));
                        }

                        return Err(errors::user_stacktrace(msg, stack));
                    }
                    Err(err) => return Err(err),
                }
            } else if let Some(frame) = frames.pop() {
                self.close_upvalues(frame.stack_offset)?;
                self.stack.truncate(frame.stack_offset);
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

    fn step(&mut self, frames: &mut Vec<Frame>, instruction: OpCode) -> Result<bool, LoxError> {
        match instruction {
            OpCode::Nil => self.stack.push(Value::Nil),
            OpCode::True => self.stack.push(Value::Bool(true)),
            OpCode::False => self.stack.push(Value::Bool(false)),
            OpCode::Constant(idx) => {
                if let Some(value) = frames.last().and_then(|f| f.constant(idx)) {
                    let value = match value {
                        Primitive::Nil => Value::Nil,
                        Primitive::Bool(b) => Value::Bool(*b),
                        Primitive::Number(n) => Value::Number(*n),
                        Primitive::String(s) => Value::String(self.gc.intern(s)),
                        primitive => Value::Primitive(PrimitiveReference::new(primitive)),
                    };

                    self.stack.push(value);
                } else {
                    return Err(errors::runtime(
                        frame_loc!(frames),
                        "Invalid constant index in byte code.",
                        "Make sure that you are passing valid constant indices to the virtual machine."
                    ));
                }
            }

            OpCode::DefineGlobal(idx) => {
                if let Some(Primitive::String(key)) = frames.last().and_then(|f| f.constant(idx)) {
                    let key = key.clone();
                    let value = self.pop()?;
                    let value = self.gc.alloc(value);
                    self.globals.insert(key, value);
                    self.collect(frames);
                } else {
                    return Err(errors::runtime(
                    frame_loc!(frames),
                    "Invalid constant index in byte code.",
                    "Make sure that you are passing valid constant indices to the virtual machine."
                ));
                }
            }
            OpCode::GetGlobal(idx) => {
                if let Some(Primitive::String(key)) = frames.last().and_then(|f| f.constant(idx)) {
                    if let Some(value) = self.globals.get(key) {
                        self.stack.push(value.cloned());
                    } else {
                        return Err(errors::runtime(
                            frame_loc!(frames),
                        format!("The variable '{}' is not defined.", key),
                        "Make sure that you have defined the variable using the `var` keyword before referencing it."
                    ));
                    }
                } else {
                    return Err(errors::runtime(
                        frame_loc!(frames),
                        "Invalid constant index in byte code.",
                        "Make sure that you are passing valid constant indices to the virtual machine."
                    ));
                }
            }
            OpCode::SetGlobal(idx) => {
                if let Some(Primitive::String(key)) = frames.last().and_then(|f| f.constant(idx)) {
                    let key = key.clone();
                    let value = self.peek()?;
                    let value = self.gc.alloc(value.clone());
                    self.globals.insert(key, value);
                    self.collect(frames);
                } else {
                    return Err(errors::runtime(
                        frame_loc!(frames),
                    "Invalid constant index in byte code.",
                    "Make sure that you are passing valid constant indices to the virtual machine."
                ));
                }
            }

            OpCode::GetUpvalue(idx) => {
                if let Some(upvalue) = frames.last().unwrap().upvalues.get(idx) {
                    match upvalue.as_ref() {
                        Upvalue::Closed(value) => self.stack.push(value.cloned()),
                        Upvalue::Open(idx) => {
                            let value = self.stack.get(*idx)
                                .ok_or_else(|| errors::runtime(
                                    frame_loc!(frames),
                                    "Upvalue referenced a stack index which does not exist.",
                                    "Make sure that you are passing valid upvalue indices to the virtual machine."
                                ))?;
                            self.stack.push(value.clone())
                        }
                    }
                } else {
                    return Err(errors::runtime(
                        frame_loc!(frames),
                        "Invalid upvalue index in byte code.",
                        "Make sure that you are passing valid upvalue indices to the virtual machine."
                    ));
                }
            }
            OpCode::SetUpvalue(idx) => {
                let value = self.pop()?;

                if let Some(upvalue) = frames.last().unwrap().upvalues.get(idx) {
                    match upvalue.as_ref() {
                        Upvalue::Closed(val) => {
                            val.replace_value(value);
                        }
                        Upvalue::Open(idx) => self.stack[*idx] = value,
                    }
                } else {
                    return Err(errors::runtime(
                        frame_loc!(frames),
                        "Invalid upvalue index in byte code.",
                        "Make sure that you are passing valid upvalue indices to the virtual machine."
                    ));
                }
            }

            OpCode::GetLocal(idx) => {
                if let Some(value) = self
                    .stack
                    .get(frames.last().map(|f| f.stack_offset).unwrap_or_default() + idx)
                {
                    self.stack.push(value.clone());
                } else {
                    return Err(errors::runtime(
                        frame_loc!(frames),
                    "Invalid local index in byte code.",
                    "Make sure that you are passing valid local indices to the virtual machine."
                ));
                }
            }
            OpCode::SetLocal(idx) => {
                let idx = frames.last().map(|f| f.stack_offset).unwrap_or_default() + idx;

                let value = self.peek()?;

                match self.stack.get(idx) {
                    Some(..) => {
                        self.stack[idx] = value.clone();
                    },
                    None => return Err(errors::runtime(
                        frame_loc!(frames),
                        "Invalid stack index in byte code for local assignment.",
                        "Make sure that you are passing valid stack indices to the virtual machine."
                    ))
                }
            }

            OpCode::GetProperty(idx) => {
                let obj = self.pop()?;
                let value = self.get_property(frames, obj, idx)?;
                self.stack.push(value);
            }
            OpCode::SetProperty(idx) => {
                if let Some(Primitive::String(key)) = frames.last().and_then(|f| f.constant(idx)) {
                    let key = key.clone();

                    let value = self.pop()?;
                    match self.pop()? {
                        Value::Instance(mut instance) => {
                            instance
                                .as_mut()
                                .fields
                                .insert(key, self.gc.alloc(value.clone()));
                            self.push(value);
                        }
                        value => {
                            return Err(errors::runtime(
                                frame_loc!(frames),
                                format!(
                                    "Attempted to set a property on a non-instance value '{}'.",
                                    value
                                ),
                                "Make sure that you are setting properties on instances.",
                            ));
                        }
                    }
                } else {
                    return Err(errors::runtime(
                        frame_loc!(frames),
                        "Invalid constant index in byte code.",
                        "Make sure that you are passing valid constant indices to the virtual machine."
                    ));
                }
            }

            OpCode::Add => {
                let right = self.pop()?;
                let left = self.pop()?;

                match (left, right) {
                    (Value::Number(left), Value::Number(right)) => {
                        self.stack.push(Value::Number(left + right));
                    }
                    (Value::String(left), right) => {
                        self.stack.push(Value::String(self.gc.intern(&format!(
                            "{}{}",
                            left.as_ref(),
                            right
                        ))));
                    }
                    (left, Value::String(right)) => {
                        self.stack.push(Value::String(self.gc.intern(&format!(
                            "{}{}",
                            left,
                            right.as_ref()
                        ))));
                    }
                    (left, right) => {
                        return Err(errors::runtime(
                            frame_loc!(frames),
                            format!(
                                "Operands must be two numbers or two strings, but got {} and {}.",
                                left, right
                            ),
                            "Make sure that you are passing numbers or strings to the + operator.",
                        ));
                    }
                }
            }
            OpCode::Subtract => {
                op_binary!(self(frame_loc!(frames), left, right), Number: (left - right) => Number)
            }
            OpCode::Multiply => {
                op_binary!(self(frame_loc!(frames), left, right), Number: (left * right) => Number)
            }
            OpCode::Divide => {
                op_binary!(self(frame_loc!(frames), left, right), Number: (left / right) => Number)
            }

            OpCode::Negate => match self.pop()? {
                Value::Number(n) => self.stack.push(Value::Number(-n)),
                _ => {
                    return Err(errors::runtime(
                        frame_loc!(frames),
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
            OpCode::GreaterEqual => op_binary!(self(left, right), Any: (left >= right) => Bool),
            OpCode::Less => op_binary!(self(left, right), Any: (left < right) => Bool),
            OpCode::LessEqual => op_binary!(self(left, right), Any: (left <= right) => Bool),

            OpCode::Pop => {
                self.pop()?;
            }
            OpCode::Print => {
                let value = self.pop()?;
                writeln!(self.output, "{}", value)?;
            }

            OpCode::Call(call_arity) => {
                if frames.len() >= 1000 {
                    return Err(errors::runtime(
                        frame_loc!(frames),
                        "The maximum call stack size has been exceeded.",
                        "Make sure that you are not calling functions with unbounded recursion.",
                    ));
                }

                let function = self.stack.get(self.stack.len() - call_arity - 2);
                if function.is_none() {
                    return Err(errors::runtime(
                        frame_loc!(frames),
                        "Callee could not be retrieved from the stack based on the known calling convention.",
                        "Please report this issue to us on GitHub with example code."));
                }

                let function = function.unwrap().clone();

                self.call_function(frames, &function, call_arity, false)?;
            }
            OpCode::Invoke(property, call_arity) => {
                if frames.len() >= 1000 {
                    return Err(errors::runtime(
                        frame_loc!(frames),
                        "The maximum call stack size has been exceeded.",
                        "Make sure that you are not calling functions with unbounded recursion.",
                    ));
                }

                let obj_idx = self.stack.len() - call_arity - 1;

                let obj = self.stack.get(obj_idx).ok_or_else(|| errors::runtime(
                    frame_loc!(frames),
                    "Could not retrieve the object upon which the method should be invoked as it did not exist on the stack.",
                    "Make sure that you are passing valid object references to the virtual machine."
                ))?.clone();

                if let Some(Primitive::String(key)) =
                    frames.last().and_then(|f| f.constant(property))
                {
                    match obj {
                        Value::Instance(instance) => {
                            if let Some(value) = instance.as_ref().fields.get(key) {
                                self.call_function(frames, value.as_ref(), call_arity, true)?;
                            } else if let Some(method) =
                                instance.as_ref().class.as_ref().methods.get(key)
                            {
                                if method.as_ref().arity() != call_arity {
                                    return Err(errors::runtime(
                                        frame_loc!(frames),
                                        format!("Invalid number of arguments, got {} but expected {}.", call_arity, method.as_ref().arity()),
                                        "Make sure that you are passing the correct number of arguments to the function."
                                    ));
                                }

                                if let Function::Closure { .. } = method.as_ref() {
                                    let self_idx = self.stack.len() - call_arity - 1;
                                    self.stack[self_idx] = Value::Instance(instance);
                                    frames.push(Frame::call(*method, self.stack.len(), true));
                                } else {
                                    return Err(errors::runtime(
                                        frame_loc!(frames),
                                        "Attempted to call a non-closure bound method.",
                                        "Make sure that you are calling a closure bound method.",
                                    ));
                                }
                            } else {
                                return Err(errors::runtime(
                                    frame_loc!(frames),
                                    format!("Property '{}' not found on instance.", key),
                                    "Make sure that you are accessing valid properties on instances."
                                ));
                            }
                        }
                        value => {
                            return Err(errors::runtime(
                                frame_loc!(frames),
                                format!(
                                    "Attempted to get a property from a non-instance value '{}'.",
                                    value
                                ),
                                "Make sure that you are accessing properties on instances.",
                            ))
                        }
                    }
                } else {
                    return Err(errors::runtime(
                        frame_loc!(frames),
                        "Invalid constant index in byte code.",
                        "Make sure that you are passing valid constant indices to the virtual machine."
                    ));
                }
            }
            OpCode::InvokeSuper(property, call_arity) => {
                if frames.len() >= 1000 {
                    return Err(errors::runtime(
                        frame_loc!(frames),
                        "The maximum call stack size has been exceeded.",
                        "Make sure that you are not calling functions with unbounded recursion.",
                    ));
                }

                let method = frames
                    .last()
                    .and_then(|f| f.constant(property))
                    .ok_or_else(|| {
                        errors::runtime(
                    frame_loc!(frames),
                    "Could not retrieve method name from the constant pool.",
                    "Make sure that you are passing valid constant indices to the virtual machine."
                )
                    })?;

                let superclass = self.pop()?;
                if let Value::Class(superclass) = superclass {
                    if let Some(function) = superclass.as_ref().methods.get(&method.to_string()) {
                        self.call_function(frames, &Value::Function(*function), call_arity, true)?;
                    } else {
                        return Err(errors::runtime(
                            frame_loc!(frames),
                            format!("Method '{}' not found in superclass '{}'.", method, superclass),
                            "Make sure that the method you are attempting to access exists in the superclass."
                        ));
                    }
                } else {
                    return Err(errors::runtime(
                        frame_loc!(frames),
                        format!("Attempted to invoke a super method on a non-class value '{}'.", superclass),
                        "Make sure that you are passing the correct stack frames to the OP_INVOKE_SUPER opcode."
                    ));
                }
            }
            OpCode::Closure(idx) => {
                let value = if let Some(value) = frames.last().and_then(|f| f.constant(idx)) {
                    value
                } else {
                    return Err(errors::runtime(
                        frame_loc!(frames),
                        "Invalid constant index in byte code.",
                        "Make sure that you are passing valid constant indices to the virtual machine."
                    ));
                };

                match value {
                    Primitive::Function(fun) => {
                        let closure = Function::capture(fun, |uprefs| {
                            let mut upvalues = Vec::with_capacity(uprefs.len());
                            for upref in uprefs {
                                match upref {
                                    VarRef::Local(idx) => {
                                        // We get the current frame's stack offset to calculate the true stack position of the local upvalue
                                        let frame_offset = frames.last()
                                            .map(|f| f.stack_offset)
                                            .unwrap_or_default();

                                        // The local upvalue is in a position offset from the current frame's stack start
                                        let idx = frame_offset + *idx;

                                        // We then start at the beginning of the open_upvalues linked list and move to the first
                                        // upvalue whose index is lower/equal to the current local upvalue index
                                        let mut target = self.open_upvalues.cursor_front_mut();
                                        while matches!(target.current(), Some(u) if matches!(u.as_ref(), Upvalue::Open(i) if *i > idx)) {
                                            target.move_next();
                                        }

                                        match target.current() {
                                            // If we have found the same upvalue index, then we should reference it directly (no need to create a new one)
                                            Some(u) if matches!(u.as_ref(), Upvalue::Open(i) if *i == idx) => {
                                                upvalues.push(*u);
                                            },

                                            // Otherwise we need to create a new open upvalue and insert it before the current upvalue (which would be strictly less than)
                                            _ => {
                                                let upvalue = self.gc.alloc(Upvalue::Open(idx));
                                                target.insert_before(upvalue);
                                                upvalues.push(upvalue);
                                            }
                                        }
                                    },
                                    VarRef::Transitive(idx) => {
                                        // If we are referencing an upvalue from a parent frame, then we simply consume the
                                        // upvalue directly from the parent frame's upvalues list.
                                        let upvalue = frames.last().unwrap().upvalues[*idx];
                                        upvalues.push(upvalue);
                                    },
                                }
                            }

                            Ok(upvalues)
                        })?;
                        let function = self.gc.alloc(closure);
                        self.push(Value::Function(function));
                    },
                    _ => {
                        return Err(errors::runtime(
                            frame_loc!(frames),
                            "Attempted to construct a closure over a non-function value. This likely indicates that invalid byte-code is being executed.",
                            "Please report this issue to us on GitHub with example code."))
                    }
                }
            }
            OpCode::CloseUpvalue => {
                self.close_upvalues(self.stack.len() - 1)?;
                self.pop()?;
                self.collect(frames);
            }
            OpCode::Return => {
                let result = self.pop()?;
                if let Some(frame) = frames.pop() {
                    self.close_upvalues(frame.stack_offset)?;
                    self.stack.truncate(frame.stack_offset);

                    if !frame.fast_call {
                        self.pop()?;
                    }
                }

                self.push(result);
                self.collect(frames);
            }

            OpCode::Class(name) => {
                let name = frames
                    .last()
                    .and_then(|f| f.constant(name))
                    .ok_or_else(|| {
                        errors::runtime(
                    frame_loc!(frames),
                    "Could not retrieve class name from the constant pool.",
                    "Make sure that you are passing valid constant indices to the virtual machine."
                )
                    })?;

                if let Primitive::String(name) = name {
                    let class = self.gc.alloc(Class::new(name));
                    self.push(Value::Class(class));
                    self.collect(frames);
                } else {
                    return Err(errors::runtime(
                        frame_loc!(frames),
                        format!("Received an invalid class name '{}' when a string was expected.", name),
                        "Please report this issue on GitHub with example code reproducing the issue."
                    ));
                }
            }
            OpCode::Inherit => {
                let subclass = self.pop()?;
                let superclass = self.peek()?;

                match (subclass, superclass) {
                    (Value::Class(mut subclass), Value::Class(superclass)) => {
                        subclass.as_mut().inherit(superclass.as_ref());
                    }
                    (Value::Class(subclass), superclass) => {
                        return Err(errors::runtime(
                            frame_loc!(frames),
                            format!("{} attempted to inherit from {} which is not a valid class.", subclass, superclass),
                            "Ensure that the value you are inheriting from is a valid class reference."
                        ));
                    }
                    _ => {
                        return Err(errors::runtime(
                            frame_loc!(frames),
                            "Attempted to inherit from a non-class value.",
                            "Ensure that the value you are inheriting from is a valid class reference."
                        ));
                    }
                }
            }
            OpCode::Method(name) => {
                let name = frames
                    .last()
                    .and_then(|f| f.constant(name))
                    .ok_or_else(|| {
                        errors::runtime(
                    frame_loc!(frames),
                    "Could not retrieve class name from the constant pool.",
                    "Make sure that you are passing valid constant indices to the virtual machine."
                )
                    })?;

                let method = self.pop()?;
                let class = self.peek()?;

                match (class, method) {
                    (Value::Class(mut class), Value::Function(method)) => {
                        class.as_mut().methods.insert(name.to_string(), method);
                    }
                    (class, _) => {
                        return Err(errors::runtime(
                            frame_loc!(frames),
                            format!(
                                "Attempted to add a method to a non-class value '{}'.",
                                class
                            ),
                            "Make sure that you are adding methods to class values.",
                        ));
                    }
                }
            }
            OpCode::GetSuper(idx) => {
                let method = frames.last().and_then(|f| f.constant(idx)).ok_or_else(|| {
                    errors::runtime(
                    frame_loc!(frames),
                    "Could not retrieve method name from the constant pool.",
                    "Make sure that you are passing valid constant indices to the virtual machine."
                )
                })?;

                let superclass = self.pop()?;
                let instance = self.peek()?;

                match (instance, superclass) {
                    (Value::Instance(instance), Value::Class(superclass)) => {
                        if let Some(method) = superclass.as_ref().methods.get(&method.to_string()) {
                            let bound_method = self.gc.alloc(BoundMethod(*instance, *method));
                            self.push(Value::BoundMethod(bound_method));
                        } else {
                            return Err(errors::runtime(
                                frame_loc!(frames),
                                format!("Method '{}' not found in superclass '{}'.", method, superclass),
                                "Make sure that the method you are attempting to access exists in the superclass."
                            ));
                        }
                    }
                    (Value::Instance(_), superclass) => {
                        return Err(errors::runtime(
                            frame_loc!(frames),
                            format!(
                                "Attempted to get a method from a non-class value '{}'.",
                                superclass
                            ),
                            "Make sure that you are getting methods from class values.",
                        ));
                    }
                    (instance, _) => {
                        return Err(errors::runtime(
                            frame_loc!(frames),
                            format!(
                                "Attempted to get a method from a non-instance value '{}'.",
                                instance
                            ),
                            "Make sure that you are getting methods from instance values.",
                        ));
                    }
                }
            }

            OpCode::Jump(ip) => {
                frames.last_mut().unwrap().ip = ip;
            }
            OpCode::JumpIf(ip) => {
                let value = self.peek()?;
                if value.is_truthy() {
                    frames.last_mut().unwrap().ip = ip;
                }
            }
            OpCode::JumpIfFalse(ip) => {
                let value = self.peek()?;
                if !value.is_truthy() {
                    frames.last_mut().unwrap().ip = ip;
                }
            }
        };

        Ok(true)
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Result<Value, LoxError> {
        self.stack.pop().ok_or_else(|| {
            errors::system(
                "Attempted to pop with no values on the stack.",
                "Don't try to do this? o.O",
            )
        })
    }

    fn peek(&self) -> Result<&Value, LoxError> {
        self.stack.last().ok_or_else(|| {
            errors::system(
                "Attempted to peek with no values on the stack.",
                "Don't try to do this? o.O",
            )
        })
    }

    fn call_function(
        &mut self,
        frames: &mut Vec<Frame>,
        function: &Value,
        call_arity: usize,
        fast_call: bool,
    ) -> Result<(), LoxError> {
        match function {
            Value::Function(function) => {
                if function.as_ref().arity() != call_arity {
                    return Err(errors::runtime(
                        frame_loc!(frames),
                        format!("Invalid number of arguments, got {} but expected {}.", call_arity, function.as_ref().arity()),
                        "Make sure that you are passing the correct number of arguments to the function."
                    ));
                }

                match function.as_ref() {
                    Function::Closure { .. } => {
                        frames.push(Frame::call(*function, self.stack.len(), fast_call));
                    }
                    Function::Native { fun, .. } => {
                        let native = fun.clone();

                        frames.push(Frame::call(*function, self.stack.len(), fast_call));

                        let result = native(self, frames.last().unwrap())?;
                        let call_frame = frames.pop().unwrap();
                        self.stack.truncate(call_frame.stack_offset);
                        self.push(result);
                    }
                }
            }
            Value::BoundMethod(bound) => {
                let BoundMethod(instance, function) = bound.as_ref();
                if function.as_ref().arity() != call_arity {
                    return Err(errors::runtime(
                        frame_loc!(frames),
                        format!("Invalid number of arguments, got {} but expected {}.", call_arity, function.as_ref().arity()),
                        "Make sure that you are passing the correct number of arguments to the function."
                    ));
                }

                if let Function::Closure { .. } = function.as_ref() {
                    let self_idx = self.stack.len() - call_arity - 1;
                    self.stack[self_idx] = Value::Instance(*instance);
                    frames.push(Frame::call(*function, self.stack.len(), fast_call));
                } else {
                    return Err(errors::runtime(
                        frame_loc!(frames),
                        "Attempted to call a non-closure bound method.",
                        "Make sure that you are calling a closure bound method.",
                    ));
                }
            }
            Value::Class(class) => {
                let instance = self.gc.alloc(Instance::new(*class));

                if let Some(init) = class.as_ref().methods.get("init") {
                    if init.as_ref().arity() != call_arity {
                        return Err(errors::runtime(
                            frame_loc!(frames),
                            format!("Invalid number of arguments, got {} but expected {}.", call_arity, init.as_ref().arity()),
                            "Make sure that you are passing the correct number of arguments to the function."
                        ));
                    }

                    // Replace the class reference with the instance reference before invoking the init method
                    let callee_idx = self.stack.len() - call_arity - 1;
                    self.stack[callee_idx] = Value::Instance(instance);

                    frames.push(Frame::call(*init, self.stack.len(), fast_call));
                } else if call_arity != 0 {
                    return Err(errors::runtime(
                        frame_loc!(frames),
                        "Attempted to instantiate a class with arguments when no init() function is defined.",
                        "Make sure that you have a valid init() function defined for the class if you wish to provide initialization arguments."
                    ));
                } else {
                    // Remove the class reference and "this" from the stack as we would if we actually ran the function
                    self.stack.truncate(self.stack.len() - 2);
                    self.push(Value::Instance(instance));
                }

                self.collect(frames);
            }
            non_function_value => {
                return Err(errors::runtime(
                    frame_loc!(frames),
                    format!("Attempted to call a non-function value '{}'.", non_function_value),
                    "Make sure that you are attempting to call a function and not another type of value."
                ));
            }
        }

        Ok(())
    }

    fn close_upvalues(&mut self, stack_top: usize) -> Result<(), LoxError> {
        let mut cursor = self.open_upvalues.cursor_front_mut();
        while matches!(cursor.current(), Some(u) if matches!(u.as_ref(), Upvalue::Open(i) if *i >= stack_top))
        {
            let mut upvalue = cursor.remove_current().unwrap();
            if let Upvalue::Open(idx) = upvalue.as_ref() {
                let idx = *idx;

                // Get the value from the stack
                let value = self.stack.get(idx)
                    .ok_or_else(|| errors::runtime(
                        Loc::Unknown,
                        format!("Attempted to access local upvalue at a locals index {} which is invalid.", idx),
                        "Please report this issue to us on GitHub with example code."))?
                    .clone();

                // Move it into the heap
                let value = self.gc.alloc(value);

                // And replace the stack value with a pointer to the heap value
                upvalue.close(value);
                self.stack[idx] = Value::Pointer(value);
            }
        }

        Ok(())
    }

    fn get_property(
        &mut self,
        frames: &[Frame],
        instance: Value,
        idx: usize,
    ) -> Result<Value, LoxError> {
        if let Some(Primitive::String(key)) = frames.last().and_then(|f| f.constant(idx)) {
            match instance {
                Value::Instance(instance) => {
                    if let Some(value) = instance.as_ref().fields.get(key) {
                        Ok(value.cloned())
                    } else if let Some(method) = instance.as_ref().class.as_ref().methods.get(key) {
                        let bound_method = self.gc.alloc(BoundMethod(instance, *method));
                        Ok(Value::BoundMethod(bound_method))
                    } else {
                        Err(errors::runtime(
                            frame_loc!(frames),
                            format!("Property '{}' not found on instance.", key),
                            "Make sure that you are accessing valid properties on instances.",
                        ))
                    }
                }
                value => Err(errors::runtime(
                    frame_loc!(frames),
                    format!(
                        "Attempted to get a property from a non-instance value '{}'.",
                        value
                    ),
                    "Make sure that you are accessing properties on instances.",
                )),
            }
        } else {
            Err(errors::runtime(
                frame_loc!(frames),
                "Invalid constant index in byte code.",
                "Make sure that you are passing valid constant indices to the virtual machine.",
            ))
        }
    }

    fn print_debug(&self, frames: &[Frame]) {
        let frame = frames.last().expect("a frame");

        eprintln!("Chunk: {} {:?}", frame.name, frame);

        eprint!("Stack:");
        for value in self.stack.iter() {
            eprint!("[{}] ", value);
        }
        eprintln!();

        eprint!("Locals:");
        for value in self
            .stack
            .iter()
            .skip(frames.last().map(|f| f.stack_offset).unwrap_or_default())
        {
            eprint!("[{}] ", value);
        }
        eprintln!();

        eprint!("Upvalues:");
        for upvalue in frame.upvalues.iter() {
            eprint!("[{}] ", upvalue);
        }
        eprintln!();

        eprint!("Globals:");
        for (key, value) in self.globals.iter() {
            eprint!("{}=[{}] ", key, value);
        }
        eprintln!()
    }
}

impl Default for VM {
    fn default() -> Self {
        Self {
            debug: false,
            output: Box::new(std::io::stdout()),
            gc: GC::default(),

            stack: Vec::new(),
            open_upvalues: LinkedList::new(),

            globals: ahash::AHashMap::new(),
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
            $code:ident $([ $($param:expr,)* $(const $ty:ident = $val:expr),* ])?
        ),*) => {
            {
                let mut chunk = Chunk::default();

                $(
                    let op = OpCode::$code$((
                        $($param,)*

                        $(
                            chunk.add_constant(Primitive::$ty($val.into())),
                        )*
                    ))?;

                    chunk.write(op, $crate::Loc::new(0));
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
                .run_chunk($chunk)
                .expect("no errors");
            assert_eq!(output.to_string().trim(), format!("{}", $val).trim());
        }};
    }

    #[test]
    fn test_negate() {
        let chunk = chunk!(Constant [const Number = 123], Negate, Print);

        run!(chunk => "-123");
    }

    #[test]
    fn test_binary_math() {
        let chunk = chunk!(
            Constant [const Number = 1.2],
            Constant [const Number = 3.4],
            Add,
            Constant [const Number = 5.6],
            Divide,
            Negate,
            Print
        );

        run!(chunk => -((3.4 + 1.2)/5.6));

        let chunk = chunk!(
            Constant [const Number = 5],
            Constant [const Number = 7],
            Add,
            Constant [const Number = 2],
            Subtract,
            Constant [const Number = 2],
            Divide,
            Constant [const Number = 3],
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
            Constant [const String = "st"],
            Constant [const String = "ri"],
            Add,
            Constant [const String = "ng"],
            Add,
            Print
        );

        run!(chunk => "string");
    }
}
