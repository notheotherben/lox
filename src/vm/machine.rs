use std::{
    collections::LinkedList,
    fmt::{Debug, Formatter},
    time::SystemTime,
};

use crate::{
    compiler::{self, Chunk, OpRunner, Primitive, VarRef},
    errors, Loc, LoxError,
};

use super::{
    alloc::Alloc, callstack::CallStack, class::Instance, fun::BoundMethod, gc::Allocator, upvalue::Upvalue, value::PrimitiveReference, Class, Collectible, Frame, Function, Value, GC
};

pub struct VM {
    debug: bool,
    output: Box<dyn std::io::Write>,
    globals: ahash::AHashMap<String, Alloc<Value>>,
    gc: GC,
}

struct VMState {
    stack: Vec<Value>,

    callframes: CallStack,

    open_upvalues: LinkedList<Alloc<Upvalue>>,
}

macro_rules! op_binary {
    ($self:ident ($left:ident, $right:ident), * : $op:tt => $res:ident) => {
        {
            let $right = $self.stack_pop()?;
            let $left = $self.stack_pop()?;

            #[allow(unused_parens)]
            let result = $op;
            $self.stack.push(Value::$res(result))
        }
    };

    ($self:ident ($left:ident, $right:ident), $($src:ident : $op:tt => $res:ident),+) => {
        {
            let right = $self.stack_pop()?;
            let left = $self.stack_pop()?;

            #[allow(unused_parens)]
            match (left, right) {
                $(
                    (Value::$src($left), Value::$src($right)) => {
                        let result = $op;
                        $self.stack.push(Value::$res(result))
                    }
                )+
                (left, right) => return Err(errors::runtime_stacktrace(
                    format!("Operands must be numbers, but got {} and {}.", left, right),
                    format!("Make sure that you are passing numbers to the {} operator(s).", stringify!($($op),+)),
                    $self.callframes.stacktrace(),
                ))
            }
        }
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

    pub fn with_native<
        N: Into<String>,
        F: Fn(&[Value]) -> Result<Value, LoxError> + Copy + 'static,
    >(
        mut self,
        name: N,
        arity: usize,
        fun: F,
    ) -> Self {
        let name = name.into();

        let function = self.gc.alloc(Function::native(name.clone(), arity, fun));

        self.globals
            .insert(name, self.gc.alloc(Value::Function(function)));
        self
    }

    fn run(mut self, frame: Frame) -> Result<(), LoxError> {
        let mut state = VMState::default();
        state.callframes.push(frame)?;

        while !state.callframes.is_empty() {
            if let Some(op) = state.callframes.active()?.opcode().copied() {
                if self.debug {
                    eprintln!("{:?}", state);
                    eprintln!("{:?}", self);
                }

                state.callframes.active_mut()?.ip += 1;
                match self.visit_op(state, op) {
                    Ok(new_state) => state = new_state,
                    Err(err) => return Err(err),
                }
            } else if let Some(frame) = state.callframes.pop() {
                state.close_upvalues(&mut self.gc, frame.stack_offset)?;
                state.stack.truncate(frame.stack_offset);
            }
        }

        if !state.stack.is_empty() {
            return Err(errors::system(
                "Stack is not empty at the end of the script.",
                "Make sure that you are returning a value from the script.",
            ));
        }

        Ok(())
    }

    fn collect(&mut self, state: &VMState) {
        if let Some(stats) = self.gc.collect(|| {
            state.gc();

            for global in self.globals.values() {
                global.gc();
            }
        }) {
            if self.debug {
                eprintln!("{}", stats);
            }
        }
    }
}

impl Debug for VM {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Globals:")?;
        for (key, value) in self.globals.iter() {
            write!(f, "{}=[{}] ", key, value)?;
        }
        writeln!(f)?;

        writeln!(f, "GC: {}", self.gc)?;

        Ok(())
    }
}

impl Default for VM {
    fn default() -> Self {
        Self {
            debug: false,
            output: Box::new(std::io::stdout()),
            globals: ahash::AHashMap::new(),
            gc: Default::default(),
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

impl VMState {
    fn stack_pop(&mut self) -> Result<Value, LoxError> {
        self.stack.pop().ok_or_else(|| {
            errors::system(
                "Attempted to pop with no values on the stack.",
                "Don't try to do this? o.O",
            )
        })
    }

    fn stack_peek(&self) -> Result<&Value, LoxError> {
        self.stack.last().ok_or_else(|| {
            errors::system(
                "Attempted to peek with no values on the stack.",
                "Don't try to do this? o.O",
            )
        })
    }

    fn get_object_property(&mut self, gc: &mut GC, object: Value, name: usize) -> Result<Value, LoxError> {
        let key = self.callframes.active()?.constant(name)?.as_string()?;
        match object {
            Value::Instance(instance) => {
                if let Some(value) = instance.as_ref().fields.get(key) {
                    Ok(value.copied())
                } else if let Some(method) = instance.as_ref().class.as_ref().methods.get(key) {
                    let bound_method = gc.alloc(BoundMethod(instance, *method));
                    Ok(Value::BoundMethod(bound_method))
                } else {
                    Err(errors::runtime_stacktrace(
                        format!("Property '{}' not found on instance.", key),
                        "Make sure that you are accessing valid properties on instances.",
                        self.callframes.stacktrace(),
                    ))
                }
            }
            value => Err(errors::runtime_stacktrace(
                format!(
                    "Attempted to get a property from a non-instance value '{}'.",
                    value
                ),
                "Make sure that you are accessing properties on instances.",
                self.callframes.stacktrace(),
            )),
        }
    }

    fn close_upvalues(&mut self, gc: &mut GC, stack_top: usize) -> Result<(), LoxError> {
        let mut cursor = self.open_upvalues.cursor_front_mut();
        while matches!(cursor.current(), Some(u) if matches!(u.as_ref(), Upvalue::Open(i) if *i >= stack_top))
        {
            let mut upvalue = cursor.remove_current().unwrap();
            if let Upvalue::Open(idx) = upvalue.as_ref() {
                let idx = *idx;

                // Get the value from the stack
                let value = self.stack.get(idx)
                    .copied()
                    .ok_or_else(|| errors::runtime_stacktrace(
                        format!("Attempted to access local upvalue at a locals index {} which is invalid.", idx),
                        "Please report this issue to us on GitHub with example code.",
                        self.callframes.stacktrace(),
                    ))?;

                // Move it into the heap
                let value = gc.alloc(value);

                // And replace the stack value with a pointer to the heap value
                upvalue.close(value);
                self.stack[idx] = Value::Pointer(value);
            }
        }

        Ok(())
    }

    fn call_function(
        &mut self,
        gc: &mut GC,
        function: &Value,
        call_arity: usize,
        fast_call: bool,
    ) -> Result<(), LoxError> {
        match function {
            Value::Function(function) => {
                if function.as_ref().arity() != call_arity {
                    return Err(errors::runtime_stacktrace(
                        format!("Invalid number of arguments, got {} but expected {}.", call_arity, function.as_ref().arity()),
                        "Make sure that you are passing the correct number of arguments to the function.",
                        self.callframes.stacktrace(),
                    ));
                }

                match function.as_ref() {
                    Function::Closure { .. } => {
                        self.callframes
                            .push(Frame::call(*function, self.stack.len(), fast_call))?;
                    }
                    Function::Native { fun, .. } => {
                        let native = fun.clone();

                        self.callframes
                            .push(Frame::call(*function, self.stack.len(), fast_call))?;

                        let result = native(&self.stack[self.stack.len() - call_arity..]).map_err(|e| {
                            match e {
                                LoxError::Runtime(_, msg, advice) => errors::runtime_stacktrace(msg, advice, self.callframes.stacktrace()),
                                LoxError::User(msg) => errors::user_stacktrace(msg, self.callframes.stacktrace()),
                                e => e
                            }
                        })?;
                        let call_frame = self.callframes.pop().unwrap();
                        self.stack.truncate(call_frame.stack_offset);
                        self.stack.push(result);
                    }
                }
            }
            Value::BoundMethod(bound) => {
                let BoundMethod(instance, function) = bound.as_ref();
                if function.as_ref().arity() != call_arity {
                    return Err(errors::runtime_stacktrace(
                        format!("Invalid number of arguments, got {} but expected {}.", call_arity, function.as_ref().arity()),
                        "Make sure that you are passing the correct number of arguments to the function.",
                        self.callframes.stacktrace(),
                    ));
                }

                if let Function::Closure { .. } = function.as_ref() {
                    let self_idx = self.stack.len() - call_arity - 1;
                    self.stack[self_idx] = Value::Instance(*instance);
                    self.callframes
                        .push(Frame::call(*function, self.stack.len(), fast_call))?;
                } else {
                    return Err(errors::runtime_stacktrace(
                        "Attempted to call a non-closure bound method.",
                        "Make sure that you are calling a closure bound method.",
                        self.callframes.stacktrace(),
                    ));
                }
            }
            Value::Class(class) => {
                let instance = gc.alloc(Instance::new(*class));

                if let Some(init) = class.as_ref().methods.get("init") {
                    if init.as_ref().arity() != call_arity {
                        return Err(errors::runtime_stacktrace(
                            format!("Invalid number of arguments, got {} but expected {}.", call_arity, init.as_ref().arity()),
                            "Make sure that you are passing the correct number of arguments to the function.",
                            self.callframes.stacktrace(),
                        ));
                    }

                    // Replace the class reference with the instance reference before invoking the init method
                    let callee_idx = self.stack.len() - call_arity - 1;
                    self.stack[callee_idx] = Value::Instance(instance);

                    self.callframes
                        .push(Frame::call(*init, self.stack.len(), fast_call))?;
                } else if call_arity != 0 {
                    return Err(errors::runtime_stacktrace(
                        "Attempted to instantiate a class with arguments when no init() function is defined.",
                        "Make sure that you have a valid init() function defined for the class if you wish to provide initialization arguments.",
                        self.callframes.stacktrace(),
                    ));
                } else {
                    // Remove the class reference and "this" from the stack as we would if we actually ran the function
                    self.stack.truncate(self.stack.len() - 2);
                    self.stack.push(Value::Instance(instance));
                }
            }
            non_function_value => {
                return Err(errors::runtime_stacktrace(
                    format!("Attempted to call a non-function value '{}'.", non_function_value),
                    "Make sure that you are attempting to call a function and not another type of value.",
                    self.callframes.stacktrace(),
                ));
            }
        }

        Ok(())
    }
}

impl Default for VMState {
    fn default() -> Self {
        Self {
            stack: Vec::with_capacity(1000),
            callframes: Default::default(),
            open_upvalues:Default::default(),
        }
    }
}

impl Collectible for VMState {
    fn gc(&self) {
        self.callframes.gc();

        for upvalue in self.open_upvalues.iter() {
            upvalue.gc();
        }

        for value in self.stack.iter() {
            value.gc();
        }
    }
}

impl std::fmt::Debug for VMState {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let frame = self.callframes.active().expect("a frame");

        write!(f, "Chunk: {} {:?}", frame.name, frame)?;

        write!(f, "Stack:")?;
        for value in self.stack.iter() {
            write!(f, "[{}] ", value)?;
        }
        writeln!(f)?;

        write!(f, "Locals:")?;
        for value in self.stack.iter().skip(
            self.callframes
                .active()
                .map(|f| f.stack_offset)
                .unwrap_or_default(),
        ) {
            write!(f, "[{}] ", value)?;
        }
        writeln!(f)?;

        write!(f, "Upvalues:")?;
        for upvalue in frame.upvalues.iter() {
            write!(f, "[{}] ", upvalue)?;
        }
        writeln!(f)?;

        Ok(())
    }
}

impl OpRunner<VMState> for VM {
    fn visit_nil(&mut self, mut state: VMState) -> Result<VMState, LoxError> {
        state.stack.push(Value::Nil);
        Ok(state)
    }

    #[inline]
    fn visit_true(&mut self, mut state: VMState) -> Result<VMState, LoxError> {
        state.stack.push(Value::Bool(true));
        Ok(state)
    }

    #[inline]
    fn visit_false(&mut self, mut state: VMState) -> Result<VMState, LoxError> {
        state.stack.push(Value::Bool(false));
        Ok(state)
    }

    #[inline]
    fn visit_constant(&mut self, mut state: VMState, idx: usize) -> Result<VMState, LoxError> {
        let constant = state.callframes.active()?.constant(idx)?;

        let value = match constant {
            Primitive::Nil => Value::Nil,
            Primitive::Bool(b) => Value::Bool(*b),
            Primitive::Number(n) => Value::Number(*n),
            Primitive::String(s) => Value::String(self.gc.intern(s)),
            primitive => Value::Primitive(PrimitiveReference::new(primitive)),
        };

        state.stack.push(value);
        Ok(state)
    }

    #[inline]
    fn visit_define_global(
        &mut self,
        mut state: VMState,
        name: usize,
    ) -> Result<VMState, LoxError> {
        let key = state.callframes.active()?.constant(name)?.as_string()?.clone();
        let value = state.stack_pop()?;
        let value = self.gc.alloc(value);
        self.globals.insert(key, value);
        self.collect(&state);
        Ok(state)
    }

    #[inline]
    fn visit_get_global(&mut self, mut state: VMState, name: usize) -> Result<VMState, LoxError> {
        let key = state.callframes.active()?.constant(name)?.as_string()?;
        if let Some(value) = self.globals.get(key) {
            state.stack.push(value.cloned());
            Ok(state)
        } else {
            Err(errors::runtime_stacktrace(
                format!("Undefined variable '{}'.", key),
                "Make sure that you are accessing valid global variables.",
                state.callframes.stacktrace(),
            ))
        }
    }

    fn visit_set_global(&mut self, state: VMState, name: usize) -> Result<VMState, LoxError> {
        let key = state.callframes.active()?.constant(name)?.as_string()?.clone();
        let value = state.stack_peek()?;
        let value = self.gc.alloc(*value);
        self.globals.insert(key, value);
        self.collect(&state);
        Ok(state)
    }

    fn visit_get_local(&mut self, mut state: VMState, idx: usize) -> Result<VMState, LoxError> {
        if let Some(value) = state.stack.get(state.callframes.active()?.stack_offset + idx) {
            state.stack.push(*value);
            Ok(state)
        } else {
            Err(errors::runtime_stacktrace(
                "Invalid stack index in byte code for local assignment.",
                "Make sure that you are passing valid stack indices to the virtual machine.",
                state.callframes.stacktrace(),
            ))
        }
    }

    fn visit_set_local(&mut self, mut state: VMState, idx: usize) -> Result<VMState, LoxError> {
        let value = *state.stack_peek()?;
        if let Some(slot) = state.stack.get_mut(state.callframes.active()?.stack_offset + idx) {
            *slot = value;
            Ok(state)
        } else {
            Err(errors::runtime_stacktrace(
                "Invalid stack index in byte code for local assignment.",
                "Make sure that you are passing valid stack indices to the virtual machine.",
                state.callframes.stacktrace(),
            ))
        }
    }

    fn visit_get_upvalue(&mut self, mut state: VMState, idx: usize) -> Result<VMState, LoxError> {
        match state.callframes.active()?.upvalue(idx)? {
            Upvalue::Closed(value) => state.stack.push(value.cloned()),
            Upvalue::Open(idx) => {
                let value = state.stack.get(*idx).cloned().ok_or_else(|| errors::runtime_stacktrace(
                    format!("Attempted to access local upvalue at a locals index {} which is invalid.", idx),
                    "Please report this issue to us on GitHub with example code.",
                    state.callframes.stacktrace(),
                ))?;
                state.stack.push(value);
            }
        };

        Ok(state)
    }

    fn visit_set_upvalue(&mut self, mut state: VMState, idx: usize) -> Result<VMState, LoxError> {
        let value = state.stack_pop()?;

        match state.callframes.active()?.upvalue(idx)? {
            Upvalue::Closed(val) => val.replace_value(value),
            Upvalue::Open(idx) => {
                state.stack[*idx] = value;
            }
        };

        Ok(state)
    }

    fn visit_get_property(&mut self, mut state: VMState, name: usize) -> Result<VMState, LoxError> {
        let object = state.stack_pop()?;
        let value = state.get_object_property(&mut self.gc, object, name)?;
        state.stack.push(value);
        Ok(state)
    }

    fn visit_set_property(&mut self, mut state: VMState, name: usize) -> Result<VMState, LoxError> {
        let value = state.stack_pop()?;
        let object = state.stack_pop()?;
        if let Value::Instance(mut instance) = object {
            let key = state.callframes.active()?.constant(name)?.as_string()?.clone();
            instance
                .as_mut()
                .fields
                .insert(key.clone(), self.gc.alloc(value));
            state.stack.push(value);
            self.collect(&state);
            Ok(state)
        } else {
            Err(errors::runtime_stacktrace(
                format!(
                    "Attempted to set a property on a non-instance value '{}'.",
                    object
                ),
                "Make sure that you are setting properties on instances.",
                state.callframes.stacktrace(),
            ))
        }
    }

    fn visit_add(&mut self, mut state: VMState) -> Result<VMState, LoxError> {
        let right = state.stack_pop()?;
        let left = state.stack_pop()?;

        match (left, right) {
            (Value::Number(left), Value::Number(right)) => {
                state.stack.push(Value::Number(left + right));
                Ok(state)
            }
            (Value::String(left), Value::String(right)) => {
                let value = self
                    .gc
                    .intern(&format!("{}{}", left.as_ref(), right.as_ref()));
                state.stack.push(Value::String(value));
                Ok(state)
            }
            (Value::String(left), right) => {
                let value = self.gc.intern(&format!("{}{}", left.as_ref(), right));
                state.stack.push(Value::String(value));
                Ok(state)
            }
            (left, Value::String(right)) => {
                let value = self.gc.intern(&format!("{}{}", left, right.as_ref()));
                state.stack.push(Value::String(value));
                Ok(state)
            }
            (left, right) => Err(errors::runtime_stacktrace(
                format!(
                    "Attempted to add non-numeric values '{}' and '{}'.",
                    left, right
                ),
                "Make sure that you are adding numbers or strings.",
                state.callframes.stacktrace(),
            )),
        }
    }

    fn visit_subtract(&mut self, mut state: VMState) -> Result<VMState, LoxError> {
        op_binary!(state(left, right), Number: (left - right) => Number);
        Ok(state)
    }

    fn visit_multiply(&mut self, mut state: VMState) -> Result<VMState, LoxError> {
        op_binary!(state(left, right), Number: (left * right) => Number);
        Ok(state)
    }

    fn visit_divide(&mut self, mut state: VMState) -> Result<VMState, LoxError> {
        op_binary!(state(left, right), Number: (left / right) => Number);
        Ok(state)
    }

    fn visit_negate(&mut self, mut state: VMState) -> Result<VMState, LoxError> {
        if let Value::Number(value) = state.stack_pop()? {
            state.stack.push(Value::Number(-value));
            Ok(state)
        } else {
            Err(errors::runtime_stacktrace(
                "Attempted to negate a non-numeric value.",
                "Make sure that you are passing a number to the negate operator.",
                state.callframes.stacktrace(),
            ))
        }
    }

    fn visit_not(&mut self, mut state: VMState) -> Result<VMState, LoxError> {
        let value = state.stack_pop()?;
        state.stack.push(Value::Bool(!value.is_truthy()));
        Ok(state)
    }

    fn visit_equal(&mut self, mut state: VMState) -> Result<VMState, LoxError> {
        op_binary!(state(left, right), *: (left == right) => Bool);
        Ok(state)
    }

    fn visit_greater(&mut self, mut state: VMState) -> Result<VMState, LoxError> {
        op_binary!(state(left, right), *: (left > right) => Bool);
        Ok(state)
    }

    fn visit_greater_equal(&mut self, mut state: VMState) -> Result<VMState, LoxError> {
        op_binary!(state(left, right), *: (left >= right) => Bool);
        Ok(state)
    }

    fn visit_less(&mut self, mut state: VMState) -> Result<VMState, LoxError> {
        op_binary!(state(left, right), *: (left < right) => Bool);
        Ok(state)
    }

    fn visit_less_equal(&mut self, mut state: VMState) -> Result<VMState, LoxError> {
        op_binary!(state(left, right), *: (left <= right) => Bool);
        Ok(state)
    }

    fn visit_pop(&mut self, mut state: VMState) -> Result<VMState, LoxError> {
        state.stack_pop()?;
        Ok(state)
    }

    fn visit_print(&mut self, mut state: VMState) -> Result<VMState, LoxError> {
        let value = state.stack_pop()?;
        writeln!(self.output, "{}", value)?;
        Ok(state)
    }

    fn visit_call(&mut self, mut state: VMState, arity: usize) -> Result<VMState, LoxError> {
        if let Some(function) = state.stack.get(state.stack.len() - arity - 2).copied() {
            state.call_function(&mut self.gc, &function, arity, false)?;
            Ok(state)
        } else {
            Err(errors::runtime_stacktrace(
                "Invalid stack index in byte code for function call.",
                "Make sure that you are passing valid stack indices to the virtual machine.",
                state.callframes.stacktrace(),
            ))
        }
    }

    fn visit_invoke(
        &mut self,
        mut state: VMState,
        name: usize,
        call_arity: usize,
    ) -> Result<VMState, LoxError> {
        let obj = state
            .stack
            .get(state.stack.len() - call_arity - 1)
            .copied()
            .ok_or_else(|| {
                errors::runtime_stacktrace(
                    "Invalid stack index in byte code for method invocation.",
                    "Make sure that you are passing valid stack indices to the virtual machine.",
                    state.callframes.stacktrace(),
                )
            })?;

        if let Value::Instance(instance) = obj {
            let key = state.callframes.active()?.constant(name)?.as_string()?;
            if let Some(method) = instance.as_ref().fields.get(key) {
                state.call_function(&mut self.gc, method.as_ref(), call_arity, true)?;
                Ok(state)
            } else if let Some(method) = instance.as_ref().class.as_ref().methods.get(key) {
                if let Function::Closure { arity, .. } = method.as_ref() {
                    if call_arity != *arity {
                        return Err(errors::runtime_stacktrace(
                            format!("Invalid number of arguments, got {} but expected {}.", arity, method.as_ref().arity()),
                            "Make sure that you are passing the correct number of arguments to the method.",
                            state.callframes.stacktrace(),
                        ));
                    }

                    let self_idx = state.stack.len() - call_arity - 1;
                    state.stack[self_idx] = Value::Instance(instance);
                    state
                        .callframes
                        .push(Frame::call(*method, state.stack.len(), true))?;
                    Ok(state)
                } else {
                    Err(errors::runtime_stacktrace(
                        "Attempted to invoke a non-closure bound method.",
                        "Make sure that you are invoking a closure bound method.",
                        state.callframes.stacktrace(),
                    ))
                }
            } else {
                Err(errors::runtime_stacktrace(
                    format!("Method '{}' not found on instance.", key),
                    "Make sure that you are accessing valid methods on instances.",
                    state.callframes.stacktrace(),
                ))
            }
        } else {
            Err(errors::runtime_stacktrace(
                format!(
                    "Attempted to invoke a method on a non-instance value '{}'.",
                    obj
                ),
                "Make sure that you are invoking methods on instances.",
                state.callframes.stacktrace(),
            ))
        }
    }

    fn visit_invoke_super(
        &mut self,
        mut state: VMState,
        name: usize,
        arity: usize,
    ) -> Result<VMState, LoxError> {
        if let Value::Class(superclass) = state.stack_pop()? {
            let method = state.callframes.active()?.constant(name)?.as_string()?;

            let function = superclass.as_ref().methods.get(method).copied()
                .ok_or_else(|| errors::runtime_stacktrace(
                    format!("Method '{}' not found in superclass '{}'.", method, superclass),
                    "Make sure that the method you are attempting to access exists in the superclass.",
                    state.callframes.stacktrace(),
                ))?;

            state.call_function(&mut self.gc, &Value::Function(function), arity, true)?;
            Ok(state)
        } else {
            Err(errors::runtime_stacktrace(
                "Attempted to invoke a super method on a non-class value.",
                "Make sure that you are passing the correct stack frames to the OP_INVOKE_SUPER opcode.",
                state.callframes.stacktrace(),
            ))
        }
    }

    fn visit_closure(&mut self, mut state: VMState, function: usize) -> Result<VMState, LoxError> {
        let function = state.callframes.active()?.constant(function)?.as_function()?;
        let closure = Function::capture(function, |varrefs| {
            let mut upvalues = Vec::with_capacity(varrefs.len());
            for varref in varrefs {
                match varref {
                    VarRef::Transitive(idx) => {
                        let upvalue = state.callframes.active()?.upvalue_alloc(*idx)?;

                        upvalues.push(upvalue);
                    }
                    VarRef::Local(idx) => {
                        let idx = state.callframes.active()?.stack_offset + idx;

                        let mut target = state.open_upvalues.cursor_front_mut();
                        while matches!(target.current(), Some(u) if matches!(u.as_ref(), Upvalue::Open(i) if *i > idx))
                        {
                            target.move_next();
                        }

                        match target.current() {
                            // If we have found the same upvalue index, then we should reference it directly (no need to create a new one)
                            Some(u) if matches!(u.as_ref(), Upvalue::Open(i) if *i == idx) => {
                                upvalues.push(*u);
                            }

                            // Otherwise we need to create a new open upvalue and insert it before the current upvalue (which would be strictly less than)
                            _ => {
                                let upvalue = self.gc.alloc(Upvalue::Open(idx));
                                target.insert_before(upvalue);
                                upvalues.push(upvalue);
                            }
                        }
                    }
                }
            }

            Ok(upvalues)
        })?;

        let closure = self.gc.alloc(closure);
        state.stack.push(Value::Function(closure));
        self.collect(&state);

        Ok(state)
    }

    fn visit_close_upvalue(&mut self, mut state: VMState) -> Result<VMState, LoxError> {
        state.close_upvalues(&mut self.gc, state.stack.len() - 1)?;
        state.stack_pop()?;
        self.collect(&state);
        Ok(state)
    }

    fn visit_return(&mut self, mut state: VMState) -> Result<VMState, LoxError> {
        let value = state.stack_pop()?;
        let call_frame = state.callframes.pop().ok_or_else(|| {
            errors::runtime_stacktrace(
                "Attempted to return from a function with no call frames.",
                "Make sure that you are not returning from the top-level of the program.",
                state.callframes.stacktrace(),
            )
        })?;

        state.close_upvalues(&mut self.gc, call_frame.stack_offset)?;
        state.stack.truncate(call_frame.stack_offset);
        if !call_frame.fast_call {
            state.stack_pop()?;
        }

        state.stack.push(value);

        self.collect(&state);
        Ok(state)
    }

    fn visit_class(&mut self, mut state: VMState, name: usize) -> Result<VMState, LoxError> {
        let name = state.callframes.active()?.constant(name)?.as_string()?.clone();
        let class = self.gc.alloc(Class::new(name));
        state.stack.push(Value::Class(class));
        self.collect(&state);
        Ok(state)
    }

    fn visit_inherit(&mut self, mut state: VMState) -> Result<VMState, LoxError> {
        let subclass = state.stack_pop()?;
        let superclass = state.stack_peek()?;

        if let (Value::Class(mut subclass), Value::Class(superclass)) = (subclass, superclass) {
            subclass.as_mut().inherit(superclass.as_ref());
            Ok(state)
        } else {
            Err(errors::runtime_stacktrace(
                format!(
                    "Attempted to inherit from non-class values '{}', '{}'.",
                    subclass, superclass),
                "Make sure that your bytecode is issuing the OP_INHERIT command with the correct calling convention.",
                state.callframes.stacktrace(),
            ))
        }
    }

    fn visit_method(&mut self, mut state: VMState, name: usize) -> Result<VMState, LoxError> {
        let name = state.callframes.active()?.constant(name)?.as_string()?.clone();
        let method = state.stack_pop()?;
        let class = state.stack_peek()?;

        match (class, method) {
            (Value::Class(mut class), Value::Function(method)) => {
                class.as_mut().methods.insert(name, method);
                Ok(state)
            }
            (class, _) => Err(errors::runtime_stacktrace(
                format!(
                    "Attempted to add a method to a non-class value '{}'.",
                    class
                ),
                "Make sure that you are adding methods to class values.",
                state.callframes.stacktrace(),
            )),
        }
    }

    fn visit_get_super(&mut self, mut state: VMState, name: usize) -> Result<VMState, LoxError> {
        let superclass = state.stack_pop()?;
        let instance = state.stack_peek()?;

        match (instance, superclass) {
            (Value::Instance(instance), Value::Class(superclass)) => {
                let method = state.callframes.active()?.constant(name)?.as_string()?;
                if let Some(method) = superclass.as_ref().methods.get(&method.to_string()) {
                    let bound_method = self.gc.alloc(BoundMethod(*instance, *method));
                    state.stack.push(Value::BoundMethod(bound_method));
                    self.collect(&state);
                    Ok(state)
                } else {
                    Err(errors::runtime_stacktrace(
                        format!("Method '{}' not found in superclass '{}'.", method, superclass),
                        "Make sure that the method you are attempting to access exists in the superclass.",
                        state.callframes.stacktrace(),
                    ))
                }
            }
            (instance, _) => {
                Err(errors::runtime_stacktrace(
                    format!(
                        "Attempted to get a superclass's method from a non-instance value '{}'.",
                        instance
                    ),
                    "Ensure that your bytecode is correctly emitting the OP_GET_SUPER calling convention.",
                    state.callframes.stacktrace(),
                ))
            }
        }
    }

    fn visit_jump(&mut self, mut state: VMState, offset: usize) -> Result<VMState, LoxError> {
        state.callframes.active_mut()?.ip = offset;
        Ok(state)
    }

    fn visit_jump_if(&mut self, mut state: VMState, offset: usize) -> Result<VMState, LoxError> {
        let value = state.stack_peek()?;
        if value.is_truthy() {
            state.callframes.active_mut()?.ip = offset;
        }
        Ok(state)
    }

    fn visit_jump_if_false(
        &mut self,
        mut state: VMState,
        offset: usize,
    ) -> Result<VMState, LoxError> {
        let value = state.stack_peek()?;
        if !value.is_truthy() {
            state.callframes.active_mut()?.ip = offset;
        }
        Ok(state)
    }
}

#[cfg(test)]
mod tests {
    use crate::CaptureOutput;
    use crate::compiler::OpCode;

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
