use std::{fmt::{Debug, Display}, ptr::NonNull};

use super::{value::Upvalue, Value};

/// A trait which is implemented by the garbage collector to indicate that it can allocate
/// certain types of objects.
///
/// The `Allocator` trait is used to allocate objects on the heap and to mark objects as
/// reachable during the garbage collection process.
pub trait Allocator<T: Collectible> {
    fn alloc(&mut self, value: T) -> Alloc<T>;
    fn mark(&mut self, object: Alloc<T>);
}

/// A trait which is implemented by types that can be marked as reachable during the garbage
/// collection process and later garbage collected.
pub trait Collectible {
    fn mark(&self, gc: &mut GC);
}

/// The garbage collector which is responsible for managing the memory of the virtual machine.
///
/// The `GC` struct is responsible for holding and maintaining the various GC pools containing
/// allocated objects, as well as coordinating mark-and-sweep semantics during a GC cycle.
#[derive(Default)]
pub struct GC {
    values: GCPool<Value>,
    upvalues: GCPool<Upvalue>,
}

impl GC {
    pub fn collect<M>(&mut self, marker: M)
    where
        M: FnOnce(&mut GC),
    {
        marker(self);

        self.scan();

        self.sweep();
    }

    pub fn size(&self) -> usize {
        self.values.size + self.upvalues.size
    }

    fn scan(&mut self) {
        while let Some(grey) = self.values.scan_next() {
            grey.mark(self);
        }

        while let Some(grey) = self.upvalues.scan_next() {
            grey.mark(self);
        }
    }

    fn sweep(&mut self) {
        self.values.sweep();
        self.upvalues.sweep();
    }
}

impl Allocator<Value> for GC {
    fn alloc(&mut self, value: Value) -> Alloc<Value> {
        self.values.alloc(value)
    }

    fn mark(&mut self, object: Alloc<Value>) {
        self.values.mark(object);
    }
}

impl Allocator<Upvalue> for GC {
    fn alloc(&mut self, value: Upvalue) -> Alloc<Upvalue> {
        self.upvalues.alloc(value)
    }

    fn mark(&mut self, object: Alloc<Upvalue>) {
        self.upvalues.mark(object);
    }
}

/// A wrapper around an `AllocRef` which provides a safe interface to the underlying allocation.
pub struct Alloc<T>(NonNull<Allocation<T>>);

impl<T> Alloc<T> {
    pub fn replace_value(&self, value: T) {
        unsafe { (*self.0.as_ptr()).value = value };
    }
}

impl<T: Copy> Alloc<T> {
    pub fn copied(&self) -> T {
        unsafe { self.0.as_ref().value }
    }
}

impl<T: Clone> Alloc<T> {
    pub fn cloned(&self) -> T {
        unsafe { self.0.as_ref().value.clone() }
    }
}

impl<T> Copy for Alloc<T> {}

impl<T> Clone for Alloc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> AsRef<T> for Alloc<T> {
    fn as_ref(&self) -> &T {
        unsafe { &self.0.as_ref().value }
    }
}

impl<T> AsMut<T> for Alloc<T> {
    fn as_mut(&mut self) -> &mut T {
        unsafe { &mut (*self.0.as_ptr()).value }
    }
}

impl<T: PartialEq> PartialEq for Alloc<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl<T: PartialEq<T>> PartialEq<T> for Alloc<T> {
    fn eq(&self, other: &T) -> bool {
        self.as_ref() == other
    }
}

impl<T: Eq> Eq for Alloc<T> {}

impl<T: PartialOrd> PartialOrd for Alloc<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_ref().partial_cmp(other.as_ref())
    }
}

impl<T: Ord> PartialOrd<T> for Alloc<T> {
    fn partial_cmp(&self, other: &T) -> Option<std::cmp::Ordering> {
        self.as_ref().partial_cmp(other)
    }
}

impl<T: Ord> Ord for Alloc<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_ref().cmp(other.as_ref())
    }
}

impl<T: Debug> Debug for Alloc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?} (0x{})", self.as_ref(), self.0.as_ptr() as usize)
    }
}

impl<T: Display> Display for Alloc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        unsafe {
            write!(f, "{}{}", if self.0.as_ref().marked { '&' } else { '!' }, self.0.as_ref().value)
        }
    }
}

impl<T: Collectible> Collectible for Alloc<T> {
    fn mark(&self, gc: &mut GC) {
        self.as_ref().mark(gc);
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
    grey_stack: Vec<Alloc<T>>,
    size: usize,
}

impl<T: Collectible> Default for GCPool<T> {
    fn default() -> Self {
        Self {
            heap: None,
            grey_stack: Default::default(),
            size: Default::default(),
        }
    }
}

impl<T: Collectible> GCPool<T> {
    pub fn alloc(&mut self, value: T) -> Alloc<T> {
        unsafe {
            let alloc = NonNull::new_unchecked(Box::into_raw(Box::new(Allocation {
                value,
                marked: false,
                next: self.heap,
            })));

            self.heap = Some(alloc);

            self.size += 1;
            Alloc(alloc)
        }
    }

    pub fn mark(&mut self, object: Alloc<T>) {
        unsafe { (*object.0.as_ptr()).marked = true; }
        self.grey_stack.push(object);
    }

    pub fn scan_next(&mut self) -> Option<Alloc<T>> {
        self.grey_stack.pop()
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
                    drop(Box::from_raw(c.as_ptr()));

                    self.size -= 1;
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

impl<T: Collectible> Drop for GCPool<T> {
    fn drop(&mut self) {
        unsafe {
            while let Some(object) = self.heap {
                self.heap = object.as_ref().next;
                drop(Box::from_raw(object.as_ptr()));
                self.size -= 1;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_gc() {
        let mut gc = GC::default();

        {
            let object = gc.alloc(Value::Number(42.0));
            assert_eq!(gc.size(), 1);

            gc.collect(|gc| {
                gc.mark(object);
            });

            let object2 = gc.alloc(Value::Nil);
            assert_eq!(gc.size(), 2);

            gc.collect(|gc| {
                gc.mark(object);
                gc.mark(object2);
            });
            assert_eq!(gc.size(), 2);

            gc.collect(|gc| {
                gc.mark(object);
            });

            assert_eq!(object, Value::Number(42.0));
            assert_eq!(object2, Value::Nil);
        }

        assert_eq!(gc.size(), 1);

        gc.collect(|_gc| {});

        assert_eq!(gc.size(), 0);
    }

    #[test]
    fn test_alloc() {
        let mut gc = GC::default();

        let object = gc.alloc(Value::Number(42.0));

        assert_eq!(object, Value::Number(42.0));

        object.replace_value(Value::Number(43.0));
        assert_eq!(object, Value::Number(43.0));

        gc.collect(|gc| {
            gc.mark(object);
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

        gc.collect(|gc| {
            gc.mark(object);
        });

        assert!(matches!(object.as_ref(), Upvalue::Closed(v) if matches!(v.as_ref(), Value::Number(42.0))));
        assert!(matches!(object_copy.as_ref(), Upvalue::Closed(v) if matches!(v.as_ref(), Value::Number(42.0))));
        assert!(matches!(object_clone.as_ref(), Upvalue::Closed(v) if matches!(v.as_ref(), Value::Number(42.0))));

        gc.collect(|_gc| {});

        assert_eq!(gc.size(), 0);
    }
}
