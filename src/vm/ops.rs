use std::fmt::Display;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpCode {
    Constant(usize),
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Print,
    Return,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            OpCode::Constant(..) => write!(f, "OP_CONSTANT"),
            OpCode::Add => write!(f, "OP_ADD"),
            OpCode::Subtract => write!(f, "OP_SUBTRACT"),
            OpCode::Multiply => write!(f, "OP_MULTIPLY"),
            OpCode::Divide => write!(f, "OP_DIVIDE"),
            OpCode::Negate => write!(f, "OP_NEGATE"),
            OpCode::Print => write!(f, "OP_PRINT"),
            OpCode::Return => write!(f, "OP_RETURN"),
        }
    }
}