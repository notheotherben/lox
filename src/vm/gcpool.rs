use std::ptr::NonNull;

use super::alloc::{Alloc, Allocation};
use super::gc::Collectible;



pub struct GCPool<T: Collectible> {
    heap: Option<NonNull<Allocation<T>>>,
    pub(crate) allocated_bytes: usize,
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

#[cfg(debug)]
impl<T: Collectible + std::fmt::Debug> GCPool<T> {
    pub fn debug_sweep(&self) {
        unsafe {
            let mut current = self.heap;
            while let Some(c) = current {
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
