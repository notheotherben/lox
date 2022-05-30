use super::Loc;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum LoxError {
    System(String, String),
    Language(Loc, String, String),
    Runtime(Loc, String, String),
    RuntimeStacktrace(String, String, Vec<String>),
    User(String),
    UserStacktrace(String, Vec<String>)
}

impl LoxError {
    pub fn description(&self) -> &str {
        match self {
            LoxError::System(msg, ..) => msg,
            LoxError::Language(_, msg, ..) => msg,
            LoxError::Runtime(_, msg, ..) => msg,
            LoxError::User(msg) => msg,
            LoxError::RuntimeStacktrace(msg, ..) => msg,
            LoxError::UserStacktrace(msg, ..) => msg,
        }
    }
}

pub fn system<M: Into<String>, A: Into<String>>(msg: M, advice: A) -> LoxError {
    LoxError::System(msg.into(), advice.into())
}

pub fn runtime<M: Into<String>, A: Into<String>>(location: Loc, message: M, advice: A) -> LoxError {
    LoxError::Runtime(location, message.into(), advice.into())
}

pub fn runtime_stacktrace<M: Into<String>, A: Into<String>, S: Into<Vec<String>>>(message: M, advice: A, stacktrace: S) -> LoxError {
    LoxError::RuntimeStacktrace(message.into(), advice.into(), stacktrace.into())
}

pub fn language<M: Into<String>, A: Into<String>>(location: Loc, message: M, advice: A) -> LoxError {
    LoxError::Language(location, message.into(), advice.into())
}

pub fn user<M: Into<String>>(message: M) -> LoxError {
    LoxError::User(message.into())
}

pub fn user_stacktrace<M: Into<String>, S: Into<Vec<String>>>(message: M, stacktrace: S) -> LoxError {
    LoxError::UserStacktrace(message.into(), stacktrace.into())
}

impl std::error::Error for LoxError {}

impl std::fmt::Display for LoxError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LoxError::System(message, advice) => write!(f, "{}\n{}", message, advice),
            LoxError::Language(loc, msg, advice) => write!(f, "[{}]: {}\n{}", loc, msg, advice),
            LoxError::Runtime(loc, msg, advice) => write!(f, "[{}] {}\n{}", loc, msg, advice),
            LoxError::User(msg) => write!(f, "{}", msg),
            LoxError::RuntimeStacktrace(msg, advice, stacktrace) => {
                write!(f, "{}\n{}\n\n  {}", msg, advice, stacktrace.join("\n  "))
            },
            LoxError::UserStacktrace(msg, stacktrace) => {
                write!(f, "{}\n\n  {}", msg, stacktrace.join("\n  "))
            }
        }
    }
}

impl From<std::io::Error> for LoxError {
    fn from(e: std::io::Error) -> Self {
        match e.kind() {
            std::io::ErrorKind::NotFound => system(
                "We could not find the file you provided.",
                "Make sure that the file exists and that you have permissions to access it.",
            ),
            std::io::ErrorKind::PermissionDenied => system(
                "You do not have permissions to access the file you provided.",
                "Make sure that you have permissions to access the file.",
            ),
            #[cfg(target_feature = "io_error_more")]
            std::io::ErrorKind::IsADirectory => system(
                "You provided the path to a directory where a file was expected.",
                "Make sure that you provide the path to a file instead of a directory.",
            ),
            #[cfg(target_feature = "io_error_more")]
            std::io::ErrorKind::ResourceBusy => system(
                "The file you provided is currently in use.",
                "Make sure that the file is not currently open in another application.",
            ),
            kind => system(
                &format!("We were unable to open the file you provided due to a {} error.", kind), 
                "Check the internal error message and try searching for a solution online.")
        }
    }
}