human_errors::error_shim!(LoxError);

pub fn source_location(sample: String, line: usize, column: usize) -> SourceLocation {
    SourceLocation { sample, line, column }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLocation {
    sample: String,
    line: usize,
    column: usize,
}

impl std::error::Error for SourceLocation {}

impl std::fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "'{}' at line {}, column {}", &self.sample, self.line, self.column)
    }
}

impl From<std::io::Error> for LoxError {
    fn from(e: std::io::Error) -> Self {
        match e.kind() {
            std::io::ErrorKind::NotFound => user_with_internal(
                "We could not find the file you provided.",
                "Make sure that the file exists and that you have permissions to access it.",
                e,
            ),
            std::io::ErrorKind::PermissionDenied => user_with_internal(
                "You do not have permissions to access the file you provided.",
                "Make sure that you have permissions to access the file.",
                e,
            ),
            #[cfg(target_feature = "io_error_more")]
            std::io::ErrorKind::IsADirectory => user_with_internal(
                "You provided the path to a directory where a file was expected.",
                "Make sure that you provide the path to a file instead of a directory.",
                e,
            ),
            #[cfg(target_feature = "io_error_more")]
            std::io::ErrorKind::ResourceBusy => user_with_internal(
                "The file you provided is currently in use.",
                "Make sure that the file is not currently open in another application.",
                e,
            ),
            kind => system_with_internal(
                &format!("We were unable to open the file you provided due to a {} error.", kind), 
                "Check the internal error message and try searching for a solution online.",
                e)
        }
    }
}