use crate::interpreter::KetamineObject;
use crate::parser::{FullIdent};
use std::cell::RefCell;
use std::rc::Rc;

pub type KetamineResult = Result<KetamineObject, KetamineError>;
pub type RefCounted<T> = Rc<RefCell<T>>;

#[derive(Debug)]
pub enum KetamineError {
    UndeclaredVariable(FullIdent),
    TypeError { expected: String, actual: String },
    BreakScope(KetamineObject),
    ArgumentError(String),
}
