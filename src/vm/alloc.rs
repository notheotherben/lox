use std::{fmt::{Debug, Display}, mem::size_of, ptr::NonNull};

use super::gc::Collectible;


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
    pub(crate) value: T,
    pub(crate) marked: bool,
    pub(crate) next: Option<NonNull<Allocation<T>>>,
}

/// A wrapper around an `AllocRef` which provides a safe interface to the underlying allocation.
pub struct Alloc<T: Collectible>(pub(crate) NonNull<Allocation<T>>);

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