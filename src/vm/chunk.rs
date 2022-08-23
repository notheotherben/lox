use std::fmt::Display;

use crate::core::Loc;

use super::{ops::OpCode, value::Value, VarRef, Function};

#[derive(Debug, Clone, Default)]
pub struct Chunk {
    pub(super) code: Vec<OpCode>,
    pub(super) constants: Vec<Value>,
    pub(super) locations: Vec<(usize, u8)>,
}

impl Chunk {
    pub fn is_empty(&self) -> bool {
        self.code.is_empty()
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn write<L: Into<Loc>>(&mut self, op: OpCode, loc: L) {
        self.code.push(op);
        let loc = loc.into();

        if let Some(last) = self.locations.last_mut() {
            // Mark unknown/native/EOF locations as the same as the previous location.
            if matches!(loc, Loc::Eof | Loc::Unknown | Loc::Native) || last.0 == loc.line() {
                last.1 += 1;
            } else {
                self.locations.push((loc.line(), 1));
            }
        } else {
            self.locations.push((loc.line(), 1));
        }
    }

    pub fn overwrite(&mut self, op: OpCode, index: usize) {
        self.code[index] = op;
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn location(&self, offset: usize) -> Loc {
        let mut remaining = offset;
        for (line, count) in self.locations.iter() {
            let count = (*count).into();
            if remaining <= count {
                return Loc::new(*line);
            } else {
                remaining -= count;
            }
        }

        Loc::new(0)
    }

    pub fn disassemble(&self, ip: usize, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Some(instruction) = self.code.get(ip) {
            write!(f, "{:04} ", ip)?;

            let loc = self.location(ip);
            if ip > 0 && loc.line() == self.location(ip - 1).line() {
                write!(f, "   | ")?;
            } else {
                write!(f, "{:4} ", loc.line())?;
            }

            match instruction {
                OpCode::Constant(idx) => writeln!(f, "{} {}", instruction, self.constants[*idx]),
                OpCode::DefineGlobal(idx) => writeln!(f, "{} {}", instruction, self.constants[*idx]),
                OpCode::GetGlobal(idx) => writeln!(f, "{} {}", instruction, self.constants[*idx]),
                OpCode::SetGlobal(idx) => writeln!(f, "{} {}", instruction, self.constants[*idx]),

                OpCode::GetUpvalue(idx) => writeln!(f, "{} {}", instruction, *idx),
                OpCode::SetUpvalue(idx) => writeln!(f, "{} {}", instruction, *idx),

                OpCode::GetLocal(idx) => writeln!(f, "{} {}", instruction, *idx),
                OpCode::SetLocal(idx) => writeln!(f, "{} {}", instruction, *idx),

                OpCode::Jump(ip) => writeln!(f, "{} {}", instruction, ip + *ip),
                OpCode::JumpIf(ip) => writeln!(f, "{} {}", instruction, ip),
                OpCode::JumpIfFalse(ip) => writeln!(f, "{} {}", instruction, ip),

                OpCode::Call(arity) => writeln!(f, "{} {}", instruction, arity),
                OpCode::Closure(idx) => {
                    if let Value::Function(Function::OpenClosure { name, upvalues, .. }) = &self.constants[*idx] {
                        writeln!(f, "{} <fn {}>", instruction, name)?;

                        for upvalue in upvalues.iter() {
                            match upvalue {
                                VarRef::Local(idx) => {
                                    writeln!(f, "          |            local {}", idx)?;
                                }
                                VarRef::Transitive(idx) => {
                                    writeln!(f, "          |            upvalue {}", idx)?;
                                }
                            }
                        }
                    } else {
                        writeln!(f, "{} {}", instruction, self.constants[*idx])?;
                    }

                    Ok(())
                },

                op => writeln!(f, "{}", op),
            }
        } else {
            writeln!(f, "END")
        }
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for (i, _) in self.code.iter().enumerate() {
            self.disassemble(i, f)?;
        }

        for constant in self.constants.iter() {
            if let Value::Function(Function::OpenClosure { name, chunk, .. }) = constant {
                writeln!(f, "\n--------- START {} ---------", name)?;
                writeln!(f, "{}", chunk)?;
                writeln!(f, "---------  END {}  ---------\n", name)?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_chunk() {
        let mut chunk = Chunk::default();

        let constant = chunk.add_constant(Value::Number(1.2));
        chunk.write(OpCode::Constant(constant), Loc::new(123));

        chunk.write(OpCode::Return, Loc::new(123));

        assert_eq!(
            format!("{}", chunk),
            "\
            0000  123 OP_CONSTANT 1.2\n\
            0001    | OP_RETURN\n\
            "
        );
    }
}
