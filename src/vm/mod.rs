mod class;
mod fun;
mod frame;
mod gc;
mod machine;
mod value;

pub(self) use class::{Class, Instance};
pub(self) use fun::Function;
pub(self) use frame::Frame;
pub(self) use gc::{Collectible, Collector, GC, Object};
pub use machine::VM;
pub(self) use value::Value;