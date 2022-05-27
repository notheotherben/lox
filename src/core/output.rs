use std::{sync::RwLock, rc::Rc};


#[derive(Debug, Clone, Default)]
pub struct CaptureOutput {
    into: Rc<RwLock<String>>,
}

impl std::io::Write for CaptureOutput {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let s = std::str::from_utf8(buf).unwrap();
        *self.into.write().unwrap() += s;
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl std::fmt::Display for CaptureOutput {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.into.read().unwrap())
    }
}
