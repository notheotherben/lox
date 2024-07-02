use std::fmt::Display;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpCode {
    Nil,
    True,
    False,
    Constant(usize),

    DefineGlobal(usize),
    GetGlobal(usize),
    SetGlobal(usize),

    GetUpvalue(usize),
    SetUpvalue(usize),

    GetLocal(usize),
    SetLocal(usize),

    GetProperty(usize),
    SetProperty(usize),
    
    Add,
    Subtract,
    Multiply,
    Divide,
    
    Negate,
    Not,
    
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Pop,
    Print,

    Call(usize),
    Invoke(usize, usize),
    Closure(usize),
    CloseUpvalue,
    Return,

    Class(usize),
    Method(usize),

    Jump(usize),
    JumpIf(usize),
    JumpIfFalse(usize),
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            OpCode::Nil => write!(f, "OP_NIL"),
            OpCode::True => write!(f, "OP_TRUE"),
            OpCode::False => write!(f, "OP_FALSE"),
            OpCode::Constant(..) => write!(f, "OP_CONSTANT"),

            OpCode::DefineGlobal(..) => write!(f, "OP_DEFINE_GLOBAL"),
            OpCode::GetGlobal(..) => write!(f, "OP_GET_GLOBAL"),
            OpCode::SetGlobal(..) => write!(f, "OP_SET_GLOBAL"),

            OpCode::GetUpvalue(..) => write!(f, "OP_GET_UPVALUE"),
            OpCode::SetUpvalue(..) => write!(f, "OP_SET_UPVALUE"),

            OpCode::GetLocal(..) => write!(f, "OP_GET_LOCAL"),
            OpCode::SetLocal(..) => write!(f, "OP_SET_LOCAL"),

            OpCode::GetProperty(..) => write!(f, "OP_GET_PROPERTY"),
            OpCode::SetProperty(..) => write!(f, "OP_SET_PROPERTY"),

            OpCode::Add => write!(f, "OP_ADD"),
            OpCode::Subtract => write!(f, "OP_SUBTRACT"),
            OpCode::Multiply => write!(f, "OP_MULTIPLY"),
            OpCode::Divide => write!(f, "OP_DIVIDE"),
            
            OpCode::Negate => write!(f, "OP_NEGATE"),
            OpCode::Not => write!(f, "OP_NOT"),

            OpCode::Equal => write!(f, "OP_EQUAL"),
            OpCode::Greater => write!(f, "OP_GREATER"),
            OpCode::GreaterEqual => write!(f, "OP_GREATER_EQUAL"),
            OpCode::Less => write!(f, "OP_LESS"),
            OpCode::LessEqual => write!(f, "OP_LESS_EQUAL"),
            
            OpCode::Pop => write!(f, "OP_POP"),
            OpCode::Print => write!(f, "OP_PRINT"),

            OpCode::Call(..) => write!(f, "OP_CALL"),
            OpCode::Invoke(..) => write!(f, "OP_INVOKE"),
            OpCode::Closure(..) => write!(f, "OP_CLOSURE"),
            OpCode::CloseUpvalue => write!(f, "OP_CLOSE_UPVALUE"),
            OpCode::Return => write!(f, "OP_RETURN"),

            OpCode::Class(..) => write!(f, "OP_CLASS"),
            OpCode::Method(..) => write!(f, "OP_METHOD"),

            OpCode::Jump(..) => write!(f, "OP_JUMP"),
            OpCode::JumpIf(..) => write!(f, "OP_JUMP_IF"),
            OpCode::JumpIfFalse(..) => write!(f, "OP_JUMP_IF_FALSE"),
        }
    }
}