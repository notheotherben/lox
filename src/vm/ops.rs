use std::fmt::Display;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpCode {
    Nil,
    True,
    False,
    Constant(usize),
    
    Add,
    Subtract,
    Multiply,
    Divide,
    
    Negate,
    Not,
    
    Equal,
    Greater,
    Less,

    Print,
    Return,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            OpCode::Nil => write!(f, "OP_NIL"),
            OpCode::True => write!(f, "OP_TRUE"),
            OpCode::False => write!(f, "OP_FALSE"),
            OpCode::Constant(..) => write!(f, "OP_CONSTANT"),

            OpCode::Add => write!(f, "OP_ADD"),
            OpCode::Subtract => write!(f, "OP_SUBTRACT"),
            OpCode::Multiply => write!(f, "OP_MULTIPLY"),
            OpCode::Divide => write!(f, "OP_DIVIDE"),
            
            OpCode::Negate => write!(f, "OP_NEGATE"),
            OpCode::Not => write!(f, "OP_NOT"),

            OpCode::Equal => write!(f, "OP_EQUAL"),
            OpCode::Greater => write!(f, "OP_GREATER"),
            OpCode::Less => write!(f, "OP_LESS"),
            
            OpCode::Print => write!(f, "OP_PRINT"),
            OpCode::Return => write!(f, "OP_RETURN"),
        }
    }
}