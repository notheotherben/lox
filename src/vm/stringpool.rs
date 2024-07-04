use std::{collections::HashMap, ptr::NonNull};

use super::alloc::Allocation;

#[derive(Default)]
pub struct StringPool {
    strings: HashMap<String, NonNull<Allocation<String>>>,
    pub(crate) allocated_bytes: usize,
}

impl StringPool {
    pub fn intern(&mut self, value: &String) -> NonNull<Allocation<String>> {
        if let Some(ptr) = self.strings.get(value) {
            return *ptr;
        }

        let size = value.len();
        let string = value.to_string();
        let alloc = NonNull::new(Box::into_raw(Box::new(Allocation {
            value: string,
            marked: false,
            next: None,
        })))
        .unwrap();

        // TODO: We should really not be storing the string twice here...
        self.strings.insert(value.clone(), alloc);

        self.allocated_bytes += size;
        alloc
    }

    pub fn sweep(&mut self) {
        unsafe {
            self.strings.retain(|_, value| {
                if !(*value.as_ptr()).marked {
                    let size = (*value.as_ptr()).value.len();
                    self.allocated_bytes -= size;
                    drop(Box::from_raw(value.as_ptr()));
                    false
                } else {
                    (*value.as_ptr()).marked = false;
                    true
                }
            });
        }
    }
}

impl Drop for StringPool {
    fn drop(&mut self) {
        unsafe {
            for (_, value) in self.strings.drain() {
                let size = value.as_ref().value.len();
                drop(Box::from_raw(value.as_ptr()));
                self.allocated_bytes -= size;
            }
        }
    }
}
