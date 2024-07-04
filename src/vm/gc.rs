use std::{fmt::Display, mem::size_of_val};


use super::{alloc::Alloc, class::{Class, Instance}, fun::BoundMethod, gcpool::GCPool, stringpool::StringPool, upvalue::Upvalue, Function, Value};

/// A trait which is implemented by the garbage collector to indicate that it can allocate
/// certain types of objects.
///
/// The `Allocator` trait is used to allocate objects on the heap and to mark objects as
/// reachable during the garbage collection process.
pub trait Allocator<T: Collectible> {
    fn alloc(&mut self, value: T) -> Alloc<T>;
}

/// A trait which is implemented by types that can be marked as reachable during the garbage
/// collection process and later garbage collected.
pub trait Collectible {
    fn gc(&self);
    fn size(&self) -> usize {
        size_of_val(self)
    }
}

/// The garbage collector which is responsible for managing the memory of the virtual machine.
///
/// The `GC` struct is responsible for holding and maintaining the various GC pools containing
/// allocated objects, as well as coordinating mark-and-sweep semantics during a GC cycle.
pub struct GC {
    values: GCPool<Value>,
    functions: GCPool<Function>,
    bound_methods: GCPool<BoundMethod>,
    classes: GCPool<Class>,
    instances: GCPool<Instance>,
    upvalues: GCPool<Upvalue>,

    strings: StringPool,

    growth_factor: usize,
    checkpoint: usize,
}

impl GC {
    pub fn collect<M>(&mut self, marker: M) -> Option<GCStats>
    where
        M: FnOnce(),
    {
        if self.allocated_bytes() >= self.checkpoint {
            let stats = self.force_collect(marker);
            Some(stats)
        } else {
            None
        }
    }

    pub fn force_collect<M>(&mut self, scanner: M) -> GCStats
    where
        M: FnOnce(),
    {
        let size_before = self.allocated_bytes();

        scanner();

        self.strings.sweep();

        #[cfg(debug)]
        self.debug_sweep();

        self.sweep();

        self.checkpoint = self.allocated_bytes() * self.growth_factor;

        GCStats {
            allocated: self.allocated_bytes(),
            collected: size_before - self.allocated_bytes(),
        }
    }

    pub fn allocated_bytes(&self) -> usize {
        self.values.allocated_bytes +
        self.functions.allocated_bytes +
        self.bound_methods.allocated_bytes +
        self.classes.allocated_bytes +
        self.instances.allocated_bytes +
        self.upvalues.allocated_bytes
    }

    pub fn intern(&mut self, value: &String) -> Alloc<String> {
        let ptr = self.strings.intern(value);
        Alloc(ptr)
    }

    #[cfg(debug)]
    fn debug_sweep(&self) {
        self.values.debug_sweep();
        self.functions.debug_sweep();
        self.bound_methods.debug_sweep();
        self.classes.debug_sweep();
        self.instances.debug_sweep();
        self.upvalues.debug_sweep();
    }

    fn sweep(&mut self) {
        self.values.sweep();
        self.functions.sweep();
        self.bound_methods.sweep();
        self.classes.sweep();
        self.instances.sweep();
        self.upvalues.sweep();
    }
}

impl Default for GC {
    fn default() -> Self {
        Self {
            values: Default::default(),
            functions: Default::default(),
            bound_methods: Default::default(),
            classes: Default::default(),
            instances: Default::default(),
            upvalues: Default::default(),
            strings: Default::default(),
            growth_factor: 2,
            checkpoint: 1024,
        }
    }

}

impl Allocator<Value> for GC {
    fn alloc(&mut self, value: Value) -> Alloc<Value> {
        self.values.alloc(value)
    }
}

impl Allocator<Function> for GC {
    fn alloc(&mut self, value: Function) -> Alloc<Function> {
        self.functions.alloc(value)
    }
}

impl Allocator<BoundMethod> for GC {
    fn alloc(&mut self, value: BoundMethod) -> Alloc<BoundMethod> {
        self.bound_methods.alloc(value)
    }
}

impl Allocator<Class> for GC {
    fn alloc(&mut self, value: Class) -> Alloc<Class> {
        self.classes.alloc(value)
    }
}

impl Allocator<Instance> for GC {
    fn alloc(&mut self, value: Instance) -> Alloc<Instance> {
        self.instances.alloc(value)
    }
}

impl Allocator<Upvalue> for GC {
    fn alloc(&mut self, value: Upvalue) -> Alloc<Upvalue> {
        self.upvalues.alloc(value)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct GCStats {
    pub allocated: usize,
    pub collected: usize,
}

impl Display for GCStats {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let GCStats { mut allocated, mut collected } = *self;
        let prefixes = ["", "k", "M", "G"];
        let mut allocated_prefix = 0;
        let mut collected_prefix = 0;

        while allocated >= 1024 * 10 && allocated_prefix < prefixes.len() - 1{
            allocated /= 1024;
            allocated_prefix += 1;
        }

        while collected >= 1024 * 10 && collected_prefix < prefixes.len() - 1{
            collected /= 1024;
            collected_prefix += 1;
        }

        write!(f, "GC collected {}{}B ({}{}B remaining)", collected, prefixes[collected_prefix], allocated, prefixes[allocated_prefix])
    }
}

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::*;

    #[test]
    fn test_alloc_size() {
        assert_eq!(size_of::<Alloc<Value>>(), size_of::<&Value>());
    }

    #[test]
    fn test_basic_gc() {
        let mut gc = GC::default();

        {
            let object = gc.alloc(Value::Number(42.0));

            assert_eq!(gc.force_collect(|| {
                object.gc();
            }), GCStats { allocated: size_of::<Value>(), collected: 0 });

            let object2 = gc.alloc(Value::Nil);
            assert_eq!(gc.allocated_bytes(), 2 * size_of::<Value>());

            assert_eq!(gc.force_collect(|| {
                object.gc();
                object2.gc();
            }), GCStats { allocated: 2 * size_of::<Value>(), collected: 0 });

            assert_eq!(gc.force_collect(|| {
                object.gc();
            }), GCStats { allocated: size_of::<Value>(), collected: size_of::<Value>() });

            assert_eq!(object, Value::Number(42.0));
        }

        assert_eq!(gc.allocated_bytes(), size_of::<Value>());

        gc.force_collect(|| {});

        assert_eq!(gc.allocated_bytes(), 0);
    }

    #[test]
    fn test_alloc() {
        let mut gc = GC::default();

        let object = gc.alloc(Value::Number(42.0));

        assert_eq!(object, Value::Number(42.0));

        object.replace_value(Value::Number(43.0));
        assert_eq!(object, Value::Number(43.0));

        gc.force_collect(|| {
            object.gc();
        });

        assert_eq!(object, Value::Number(43.0));
    }

    #[test]
    fn test_upvalue_alloc() {
        let mut gc = GC::default();

        let object = gc.alloc(Upvalue::Open(0));
        let object_copy = object;
        #[allow(clippy::clone_on_copy)]
        let object_clone = object.clone();

        assert_eq!(object, Upvalue::Open(0));
        assert_eq!(object_copy, Upvalue::Open(0));
        assert_eq!(object_clone, Upvalue::Open(0));

        object.replace_value(Upvalue::Closed(gc.alloc(Value::Number(42.0))));
        assert!(matches!(object.as_ref(), Upvalue::Closed(_)));
        assert!(matches!(object_copy.as_ref(), Upvalue::Closed(_)));
        assert!(matches!(object_clone.as_ref(), Upvalue::Closed(_)));

        gc.force_collect(|| {
            object.gc();
        });

        assert!(matches!(object.as_ref(), Upvalue::Closed(v) if matches!(v.as_ref(), Value::Number(42.0))));
        assert!(matches!(object_copy.as_ref(), Upvalue::Closed(v) if matches!(v.as_ref(), Value::Number(42.0))));
        assert!(matches!(object_clone.as_ref(), Upvalue::Closed(v) if matches!(v.as_ref(), Value::Number(42.0))));

        gc.force_collect(|| {});

        assert_eq!(gc.allocated_bytes(), 0);
    }
}
