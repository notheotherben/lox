mod class;
mod fun;
mod frame;
mod gc;
mod machine;
mod upvalue;
mod value;

use class::{Class, Instance};
use fun::Function;
use frame::Frame;
use gc::{Collectible, GC, Alloc, Allocator};
pub use machine::VM;
use value::Value;