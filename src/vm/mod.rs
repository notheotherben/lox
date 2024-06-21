mod class;
mod fun;
mod frame;
mod gc;
mod machine;
mod value;

use class::{Class, Instance};
use fun::Function;
use frame::Frame;
use gc::{Collectible, Collector, GC, Object};
pub use machine::VM;
use value::Value;