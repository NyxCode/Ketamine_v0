use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

use ordered_float::OrderedFloat;

use crate::interpreter::{child_scope, KetamineError, KetamineResult, RefCounted, ScopeRef};
use crate::parser::{Function, AST};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KetamineObject {
    Null,
    Number(OrderedFloat<f64>),
    String(String),
    Array(RefCounted<Vec<KetamineObject>>),
    Boolean(bool),
    Dict(RefCounted<HashMap<String, KetamineObject>>),
    Function(Function),
    NativeFunction(NativeFunction),
}

#[derive(Clone)]
pub struct NativeFunction(pub Rc<dyn Fn(Vec<KetamineObject>) -> KetamineResult>);

// constructors
impl KetamineObject {
    pub fn null() -> KetamineObject {
        KetamineObject::Null
    }

    pub fn number(num: f64) -> KetamineObject {
        KetamineObject::Number(OrderedFloat::from(num))
    }

    pub fn boolean(value: bool) -> KetamineObject {
        KetamineObject::Boolean(value)
    }

    pub fn dict(value: HashMap<String, KetamineObject>) -> KetamineObject {
        KetamineObject::Dict(Rc::new(RefCell::new(value)))
    }

    pub fn array(array: Vec<KetamineObject>) -> KetamineObject {
        KetamineObject::Array(Rc::new(RefCell::new(array)))
    }

    pub fn string(value: String) -> KetamineObject {
        KetamineObject::String(value)
    }

    pub fn native_fn(
        f: impl Fn(Vec<KetamineObject>) -> KetamineResult + 'static,
    ) -> KetamineObject {
        KetamineObject::NativeFunction(NativeFunction(Rc::new(f)))
    }
}

// utility methods
impl KetamineObject {
    pub fn type_name(&self) -> &'static str {
        match self {
            KetamineObject::String(_) => "<string>",
            KetamineObject::Null => "<null>",
            KetamineObject::Number(_) => "<number>",
            KetamineObject::Array(_) => "<array>",
            KetamineObject::Boolean(_) => "<boolean>",
            KetamineObject::Dict(_) => "<object>",
            KetamineObject::Function(_) => "<function>",
            KetamineObject::NativeFunction(_) => "<native>",
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            KetamineObject::Number(_) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            KetamineObject::String(_) => true,
            _ => false,
        }
    }

    pub fn expect_number(&self) -> Result<OrderedFloat<f64>, KetamineError> {
        match self {
            KetamineObject::Number(num) => Ok(*num),
            _ => Err(KetamineError::TypeError {
                expected: "<number>".to_owned(),
                actual: self.type_name().to_owned(),
            }),
        }
    }

    pub fn expect_bool(&self) -> Result<bool, KetamineError> {
        match self {
            KetamineObject::Boolean(value) => Ok(*value),
            _ => Err(KetamineError::TypeError {
                expected: "<boolean>".to_owned(),
                actual: self.type_name().to_owned(),
            }),
        }
    }

    pub fn expect_dict(
        &self,
    ) -> Result<Rc<RefCell<HashMap<String, KetamineObject>>>, KetamineError> {
        match self {
            KetamineObject::Dict(dict) => Ok(dict.clone()),
            _ => Err(KetamineError::TypeError {
                expected: "<object>".to_owned(),
                actual: self.type_name().to_owned(),
            }),
        }
    }
    pub fn expect_string(&self) -> Result<&str, KetamineError> {
        match self {
            KetamineObject::String(string) => Ok(string),
            _ => Err(KetamineError::TypeError {
                expected: "<string>".to_owned(),
                actual: self.type_name().to_owned(),
            }),
        }
    }
}

impl KetamineObject {
    pub fn invoke(&self, scope: &ScopeRef, mut args: Vec<KetamineObject>) -> KetamineResult {
        match self {
            KetamineObject::NativeFunction(function) => function.0(args),
            KetamineObject::Function(function) => {
                let function_scope = child_scope(scope);
                for idx in 0..function.params.len() {
                    let parameter = &function.params[idx];
                    let argument = if args.is_empty() {
                        KetamineObject::null()
                    } else {
                        args.remove(0)
                    };
                    function_scope
                        .deref()
                        .borrow_mut()
                        .new_var(parameter.clone(), argument.clone());
                }

                crate::interpreter::eval(&function_scope, AST::Code(function.code.clone()))
            }
            _ => Err(KetamineError::TypeError {
                expected: "<function>".to_owned(),
                actual: self.type_name().to_owned(),
            }),
        }
    }
}
