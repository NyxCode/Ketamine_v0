use crate::parser::{FullIdent, Function, Ident};
use ordered_float::OrderedFloat;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use crate::interpreter::KetamineObject;

pub type KetamineResult = Result<KetamineObject, KetamineError>;
pub type RefCounted<T> = Rc<RefCell<T>>;

#[derive(Debug)]
pub enum KetamineError {
    UndeclaredVariable(FullIdent),
    TypeError { expected: String, actual: String },
    BreakScope(KetamineObject),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KetamineType {
    Null,
    Number,
    String,
    Array,
    Boolean,
    Dict,
    Function,
    NativeFunction,
}

