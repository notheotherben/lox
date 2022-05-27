use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Default, Hash)]
pub struct Location {
    line: usize,
    col: usize,
}

impl Location {
    pub fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn col(&self) -> usize {
        self.col
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.col == 0 {
            write!(f, "line {}", self.line)
        } else {
            write!(f, "line {}, column {}", self.line, self.col)
        }
    }
}