use crate::LoxError;

use super::OpCode;

pub trait OpRunner<S, R> {
    fn visit_op(&mut self, state: S, op: OpCode) -> Result<R, LoxError> {
        match op {
            OpCode::Nil => self.visit_nil(state),
            OpCode::True => self.visit_true(state),
            OpCode::False => self.visit_false(state),
            OpCode::Constant(idx) => self.visit_constant(state, idx),

            OpCode::DefineGlobal(name) => self.visit_define_global(state, name),
            OpCode::GetGlobal(name) => self.visit_get_global(state, name),
            OpCode::SetGlobal(name) => self.visit_set_global(state, name),
            
            OpCode::GetLocal(idx) => self.visit_get_local(state, idx),
            OpCode::SetLocal(idx) => self.visit_set_local(state, idx),

            OpCode::GetUpvalue(idx) => self.visit_get_upvalue(state, idx),
            OpCode::SetUpvalue(idx) => self.visit_set_upvalue(state, idx),

            OpCode::GetProperty(name) => self.visit_get_property(state, name),
            OpCode::SetProperty(name) => self.visit_set_property(state, name),

            OpCode::Add => self.visit_add(state),
            OpCode::Subtract => self.visit_subtract(state),
            OpCode::Multiply => self.visit_multiply(state),
            OpCode::Divide => self.visit_divide(state),

            OpCode::Negate => self.visit_negate(state),
            OpCode::Not => self.visit_not(state),

            OpCode::Equal => self.visit_equal(state),
            OpCode::Greater => self.visit_greater(state),
            OpCode::GreaterEqual => self.visit_greater_equal(state),
            OpCode::Less => self.visit_less(state),
            OpCode::LessEqual => self.visit_less_equal(state),

            OpCode::Pop => self.visit_pop(state),
            OpCode::Print => self.visit_print(state),

            OpCode::Call(arity) => self.visit_call(state, arity),
            OpCode::Invoke(name, arity) => self.visit_invoke(state, name, arity),
            OpCode::InvokeSuper(name, arity) => self.visit_invoke_super(state, name, arity),
            OpCode::Closure(function) => self.visit_closure(state, function),
            OpCode::CloseUpvalue => self.visit_close_upvalue(state),
            OpCode::Return => self.visit_return(state),

            OpCode::Class(name) => self.visit_class(state, name),
            OpCode::Inherit => self.visit_inherit(state),
            OpCode::Method(name) => self.visit_method(state, name),
            OpCode::GetSuper(name) => self.visit_get_super(state, name),

            OpCode::Jump(offset) => self.visit_jump(state, offset),
            OpCode::JumpIf(offset) => self.visit_jump_if(state, offset),
            OpCode::JumpIfFalse(offset) => self.visit_jump_if_false(state, offset),
        }
    }

    fn visit_nil(&mut self, state: S) -> Result<R, LoxError>;
    fn visit_true(&mut self, state: S) -> Result<R, LoxError>;
    fn visit_false(&mut self, state: S) -> Result<R, LoxError>;
    fn visit_constant(&mut self, state: S, idx: usize) -> Result<R, LoxError>;

    fn visit_define_global(&mut self, state: S, name: usize) -> Result<R, LoxError>;
    fn visit_get_global(&mut self, state: S, name: usize) -> Result<R, LoxError>;
    fn visit_set_global(&mut self, state: S, name: usize) -> Result<R, LoxError>;

    fn visit_get_local(&mut self, state: S, idx: usize) -> Result<R, LoxError>;
    fn visit_set_local(&mut self, state: S, idx: usize) -> Result<R, LoxError>;

    fn visit_get_upvalue(&mut self, state: S, idx: usize) -> Result<R, LoxError>;
    fn visit_set_upvalue(&mut self, state: S, idx: usize) -> Result<R, LoxError>;

    fn visit_get_property(&mut self, state: S, name: usize) -> Result<R, LoxError>;
    fn visit_set_property(&mut self, state: S, name: usize) -> Result<R, LoxError>;

    fn visit_add(&mut self, state: S) -> Result<R, LoxError>;
    fn visit_subtract(&mut self, state: S) -> Result<R, LoxError>;
    fn visit_multiply(&mut self, state: S) -> Result<R, LoxError>;
    fn visit_divide(&mut self, state: S) -> Result<R, LoxError>;
    
    fn visit_negate(&mut self, state: S) -> Result<R, LoxError>;
    fn visit_not(&mut self, state: S) -> Result<R, LoxError>;

    fn visit_equal(&mut self, state: S) -> Result<R, LoxError>;
    fn visit_greater(&mut self, state: S) -> Result<R, LoxError>;
    fn visit_greater_equal(&mut self, state: S) -> Result<R, LoxError>;
    fn visit_less(&mut self, state: S) -> Result<R, LoxError>;
    fn visit_less_equal(&mut self, state: S) -> Result<R, LoxError>;

    fn visit_pop(&mut self, state: S) -> Result<R, LoxError>;
    fn visit_print(&mut self, state: S) -> Result<R, LoxError>;

    fn visit_call(&mut self, state: S, arity: usize) -> Result<R, LoxError>;
    fn visit_invoke(&mut self, state: S, name: usize, arity: usize) -> Result<R, LoxError>;
    fn visit_invoke_super(&mut self, state: S, name: usize, arity: usize) -> Result<R, LoxError>;
    fn visit_closure(&mut self, state: S, function: usize) -> Result<R, LoxError>;
    fn visit_close_upvalue(&mut self, state: S) -> Result<R, LoxError>;
    fn visit_return(&mut self, state: S) -> Result<R, LoxError>;
    
    fn visit_class(&mut self, state: S, name: usize) -> Result<R, LoxError>;
    fn visit_inherit(&mut self, state: S) -> Result<R, LoxError>;
    fn visit_method(&mut self, state: S, name: usize) -> Result<R, LoxError>;
    fn visit_get_super(&mut self, state: S, name: usize) -> Result<R, LoxError>;

    fn visit_jump(&mut self, state: S, offset: usize) -> Result<R, LoxError>;
    fn visit_jump_if(&mut self, state: S, offset: usize) -> Result<R, LoxError>;
    fn visit_jump_if_false(&mut self, state: S, offset: usize) -> Result<R, LoxError>;
}