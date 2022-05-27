use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Loc {
    Unknown,
    Eof,
    Native,
    Line {
        line: usize,
    },
    Sample {
        sample: String,
    },
    SampleLine {
        sample: String,
        line: usize,
    },
}

impl Loc {
    pub fn new(line: usize) -> Self {
        Loc::Line { line }
    }

    pub fn with_sample<S: Into<String>>(&self, sample: S) -> Self {
        match self {
            Self::Line { line } | Self::SampleLine { line, .. } => Self::SampleLine { sample: sample.into(), line: *line },
            _ => Self::Sample { sample: sample.into() },
        }
    }
    
    pub fn line(&self) -> usize {
        match self {
            Self::Line { line } | Self::SampleLine { line, .. }  => *line,
            _ => 0,
        }
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Loc::Unknown => write!(f, "unknown"),
            Loc::Eof => write!(f, "end of file"),
            Loc::Native => write!(f, "<native code>"),
            Loc::Line { line } => write!(f, "line {}", line),
            Loc::Sample { sample } => write!(f, "'{}'", sample),
            Loc::SampleLine { sample, line } => write!(f, "'{}' at line {}", sample, line),
        }
    }
}

#[allow(clippy::from_over_into)]
impl Into<Loc> for usize {
    fn into(self) -> Loc {
        Loc::Line{ line: self }
    }
}
