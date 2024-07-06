mod alloc;
mod callstack;
mod class;
mod fun;
mod frame;
mod gc;
mod gcpool;
mod machine;
mod stringpool;
mod upvalue;
mod value;

use alloc::Alloc;
use class::{Class, Instance};
use fun::Function;
use frame::Frame;
use gc::{Collectible, GC, Allocator};
pub use machine::VM;
use value::Value;