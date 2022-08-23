#![feature(arc_unwrap_or_clone, get_mut_unchecked)]

mod core;
pub mod ast;
pub mod analysis;
pub mod compiler;
pub mod lexer;
pub mod interpreter;
pub mod vm;

pub use crate::core::errors::{self, LoxError};
pub use crate::core::{Loc, CaptureOutput};
