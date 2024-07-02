use std::{fmt::{Debug, Display}, mem::{size_of, size_of_val}, ptr::NonNull};


use super::{class::{Class, Instance}, Function, value::Upvalue, Value};

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
    classes: GCPool<Class>,
    instances: GCPool<Instance>,
    upvalues: GCPool<Upvalue>,

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

        //#[cfg(debug)]
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
        self.classes.allocated_bytes +
        self.instances.allocated_bytes +
        self.upvalues.allocated_bytes
    }

    fn debug_sweep(&self) {
        self.values.debug_sweep();
        self.functions.debug_sweep();
        self.classes.debug_sweep();
        self.instances.debug_sweep();
        self.upvalues.debug_sweep();
    }

    fn sweep(&mut self) {
        self.values.sweep();
        self.functions.sweep();
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
            classes: Default::default(),
            instances: Default::default(),
            upvalues: Default::default(),
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

/// A wrapper around an `AllocRef` which provides a safe interface to the underlying allocation.
pub struct Alloc<T: Collectible>(NonNull<Allocation<T>>);

impl<T: Collectible> Alloc<T> {
    pub fn ptr_eq(self, other: &Self) -> bool {
        self.0 == other.0
    }

    pub fn replace_value(&self, value: T) {
        unsafe { (*self.0.as_ptr()).value = value };
    }
}

impl<T: Collectible + Copy> Alloc<T> {
    pub fn copied(&self) -> T {
        unsafe { self.0.as_ref().value }
    }
}

impl<T: Collectible + Clone> Alloc<T> {
    pub fn cloned(&self) -> T {
        unsafe { self.0.as_ref().value.clone() }
    }
}

impl<T: Collectible> Copy for Alloc<T> {}

impl<T: Collectible> Clone for Alloc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Collectible> AsRef<T> for Alloc<T> {
    fn as_ref(&self) -> &T {
        unsafe { &self.0.as_ref().value }
    }
}

impl<T: Collectible> AsMut<T> for Alloc<T> {
    fn as_mut(&mut self) -> &mut T {
        unsafe { &mut (*self.0.as_ptr()).value }
    }
}

impl<T: Collectible + PartialEq> PartialEq for Alloc<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl<T: Collectible + PartialEq<T>> PartialEq<T> for Alloc<T> {
    fn eq(&self, other: &T) -> bool {
        self.as_ref() == other
    }
}

impl<T: Collectible + Eq> Eq for Alloc<T> {}

impl<T: Collectible + PartialOrd> PartialOrd for Alloc<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_ref().partial_cmp(other.as_ref())
    }
}

impl<T: Collectible + Ord> PartialOrd<T> for Alloc<T> {
    fn partial_cmp(&self, other: &T) -> Option<std::cmp::Ordering> {
        self.as_ref().partial_cmp(other)
    }
}

impl<T: Collectible + Ord> Ord for Alloc<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_ref().cmp(other.as_ref())
    }
}

impl<T: Collectible + Debug> Debug for Alloc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?} (0x{})", self.as_ref(), self.0.as_ptr() as usize)
    }
}

impl<T: Collectible + Display> Display for Alloc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        unsafe {
            write!(f, "{}", self.0.as_ref().value)
        }
    }
}

impl<T: Collectible> Collectible for Alloc<T> {
    fn gc(&self) {
        unsafe {
            if self.0.as_ref().marked {
                return;
            }

            self.as_ref().gc();

            (*self.0.as_ptr()).marked = true;
        }
    }

    fn size(&self) -> usize {
        size_of::<Self>() + self.as_ref().size()
    }
}

/// An `Allocation` is the internal representation used by the garbage collector to manage
/// allocated objects on the heap.
///
/// The `Allocation` struct is effectively a linked list node which holds the value of a
/// specific allocation as well as an (optional) pointer to the next allocation in the heap.
/// We also keep track of a `marked` flag which is used to indicate whether an object is
/// reachable during a garbage collection cycle. Any objects which are not reachable (i.e.
/// `marked = false`) at the end of a mark cycle will be garbage collected.
#[derive(PartialEq, Eq, PartialOrd)]
pub struct Allocation<T> {
    value: T,
    marked: bool,
    next: Option<NonNull<Allocation<T>>>,
}

struct GCPool<T: Collectible> {
    heap: Option<NonNull<Allocation<T>>>,
    allocated_bytes: usize,
}

impl<T: Collectible> Default for GCPool<T> {
    fn default() -> Self {
        Self {
            heap: None,
            allocated_bytes: Default::default(),
        }
    }
}

impl<T: Collectible> GCPool<T> {
    pub fn alloc(&mut self, value: T) -> Alloc<T> {
        unsafe {
            let size = value.size();
            let alloc = NonNull::new_unchecked(Box::into_raw(Box::new(Allocation {
                value,
                marked: false,
                next: self.heap,
            })));

            self.heap = Some(alloc);

            self.allocated_bytes += size;
            Alloc(alloc)
        }
    }

    pub fn sweep(&mut self) {
        unsafe {
            let mut head: Option<NonNull<Allocation<T>>> = None;
            let mut tail: Option<NonNull<Allocation<T>>> = None;
            let mut current = self.heap;
            while let Some(c) = current {
                if !c.as_ref().marked {
                    // If the current object isn't marked, then move to the next object and drop this one
                    current = c.as_ref().next;
                    let size = c.as_ref().value.size();
                    drop(Box::from_raw(c.as_ptr()));

                    self.allocated_bytes -= size;
                } else {
                    current = c.as_ref().next;
                    (*c.as_ptr()).marked = false;
                    (*c.as_ptr()).next = None;

                    if let Some(t) = tail {
                        // If there is an existing retained allocation, then add the current object to the end
                        (*t.as_ptr()).next = Some(c);
                        tail = Some(c);
                    } else {
                        // If we don't have an existing retained allocation, then set the object as the head
                        head = Some(c);
                        tail = head;
                    }
                }
            }

            self.heap = head;
        }
    }
}

impl<T: Collectible + Debug> GCPool<T> {
    pub fn debug_sweep(&self) {
        unsafe {
            let mut current = self.heap;
            while let Some(c) = current {
                if !c.as_ref().marked {
                    eprintln!("GC: Collecting {:?}", c.as_ref().value);
                }

                current = c.as_ref().next;
            }
        }
    }
}

impl<T: Collectible> Drop for GCPool<T> {
    fn drop(&mut self) {
        unsafe {
            while let Some(object) = self.heap {
                self.heap = object.as_ref().next;
                let size = &object.as_ref().value.size();
                drop(Box::from_raw(object.as_ptr()));
                self.allocated_bytes -= size;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::*;

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
