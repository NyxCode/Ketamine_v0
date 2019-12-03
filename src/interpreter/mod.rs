use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::ops::Deref;
use std::rc::Rc;

use serde::export::fmt::Error;

pub use object::*;
pub use scope::*;
pub use types::*;

use crate::parser::{
    Array, Assignment, BinaryOperation, BinaryOperator, Call, ForEach, FullIdent, Function, Index,
    Var, While, AST,
};

mod object;
mod scope;
mod types;

impl Debug for NativeFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "native function")
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0.deref(), other.0.deref())
    }
}

impl Eq for NativeFunction {}

pub fn eval(scope: &Rc<RefCell<Scope>>, ast: AST) -> Result<KetamineObject, KetamineError> {
    match ast {
        AST::Code(code) => {
            let mut last_value = None;
            for statement in code.0 {
                match eval(&scope, statement) {
                    Err(KetamineError::BreakScope(value)) => return Ok(value),
                    Err(other) => return Err(other),
                    Ok(x) => last_value = Some(x),
                }
            }
            Ok(last_value.unwrap_or_else(KetamineObject::null))
        }
        AST::Var(Var(ident, value)) => {
            let value = eval(&scope, *value)?;
            scope.deref().borrow_mut().new_var(ident, value);
            Ok(KetamineObject::null())
        }
        AST::Number(float) => Ok(KetamineObject::number(*float)),
        AST::Call(Call { ident, args }) => {
            let args = args
                .into_iter()
                .map(|arg| eval(scope, arg))
                .collect::<Result<Vec<KetamineObject>, KetamineError>>()?;
            let target = scope.deref().borrow().get(&ident)?;
            target.invoke(&scope, args)
        }
        AST::Ident(ident) => scope.deref().borrow().get_var(&ident),
        AST::FullIdent(ident) => scope.deref().borrow().get(&ident),
        AST::BinaryOp(BinaryOperation(lhs, op, rhs)) => {
            let lhs = eval(scope, *lhs)?;
            let rhs = eval(scope, *rhs)?;
            match op {
                BinaryOperator::Add => {
                    if lhs.is_number() && rhs.is_number() {
                        let result = *lhs.expect_number()? + *rhs.expect_number()?;
                        Ok(KetamineObject::number(result))
                    } else {
                        let mut string = lhs.to_string();
                        string.push_str(&rhs.to_string());
                        Ok(KetamineObject::string(string))
                    }
                }
                BinaryOperator::Mod => {
                    let result = *lhs.expect_number()? % *rhs.expect_number()?;
                    Ok(KetamineObject::number(result))
                }
                BinaryOperator::Eq => {
                    let result = lhs == rhs;
                    Ok(KetamineObject::boolean(result))
                }
                BinaryOperator::NotEq => {
                    let result = lhs != rhs;
                    Ok(KetamineObject::boolean(result))
                }

                BinaryOperator::Lt => {
                    let result = *lhs.expect_number()? < *rhs.expect_number()?;
                    Ok(KetamineObject::boolean(result))
                }
                BinaryOperator::Sub => {
                    let result = *lhs.expect_number()? - *rhs.expect_number()?;
                    Ok(KetamineObject::number(result))
                }
                _ => unimplemented!("{:?}", op),
            }
        }
        AST::Function(function) => {
            let ident = function.ident.clone();
            let function: KetamineObject = function.into();
            scope.deref().borrow_mut().new_var(ident, function.clone());
            Ok(function)
        }
        AST::Object(object) => {
            let mut dict = HashMap::new();
            for (k, v) in object.0 {
                dict.insert(k.0, eval(scope, v)?);
            }
            Ok(KetamineObject::dict(dict))
        }
        AST::String(string) => Ok(KetamineObject::string(string)),
        AST::If(clauses) => {
            for clause in clauses.0 {
                let condition = eval(scope, *clause.condition)?.expect_bool()?;
                if condition {
                    return eval(scope, AST::Code(clause.code));
                }
            }
            Ok(KetamineObject::null())
        }
        AST::Boolean(bool) => Ok(KetamineObject::boolean(bool)),
        AST::Null => Ok(KetamineObject::null()),
        AST::Assignment(Assignment(ident, value)) => {
            match ident.0.len() {
                0 => panic!(),
                1 => {
                    let value = eval(scope, *value)?;
                    scope
                        .deref()
                        .borrow_mut()
                        .assign_var(ident.0[0].clone(), value)?;
                }
                _ => {
                    let value = eval(scope, *value)?;
                    scope.deref().borrow_mut().assign(ident, value)?;
                }
            };

            Ok(KetamineObject::null())
        }
        AST::Array(Array(values)) => {
            let mut array = Vec::with_capacity(values.len());
            for value in values {
                array.push(eval(scope, value)?);
            }
            Ok(KetamineObject::array(array))
        }
        AST::Index(Index { receiver, index }) => {
            let receiver = eval(scope, *receiver)?;
            let index = eval(scope, *index)?;
            match receiver {
                KetamineObject::Array(array) => {
                    let index = index.expect_number()?.into_inner();
                    let index = if index.fract() != 0.0 {
                        return Err(KetamineError::TypeError {
                            expected: "<integer>".to_owned(),
                            actual: "<float>".to_owned(),
                        });
                    } else {
                        index as usize
                    };
                    let result = array
                        .deref()
                        .borrow()
                        .get(index)
                        .cloned()
                        .unwrap_or_else(KetamineObject::null);
                    Ok(result)
                }
                _ => unimplemented!(),
            }
        }
        AST::ForEach(ForEach {
            binding,
            iterable,
            code,
        }) => {
            let iterable = eval(scope, *iterable)?;
            match iterable {
                KetamineObject::Array(array) => {
                    for entry in array.deref().borrow().deref().clone() {
                        let child_scope = child_scope(scope);
                        child_scope
                            .deref()
                            .borrow_mut()
                            .new_var(binding.clone(), entry);
                        eval(&child_scope, AST::Code(code.clone()))?;
                    }
                    Ok(KetamineObject::null())
                }
                KetamineObject::Dict(dict) => {
                    let dict = dict.borrow();
                    let entries = dict.iter().map(|(k, v)| {
                        let mut map = HashMap::with_capacity(2);
                        map.insert("key".to_owned(), KetamineObject::String(k.clone()));
                        map.insert("value".to_owned(), v.clone());
                        KetamineObject::dict(map)
                    });
                    for entry in entries {
                        let child_scope = child_scope(scope);
                        child_scope
                            .deref()
                            .borrow_mut()
                            .new_var(binding.clone(), entry);
                        eval(&child_scope, AST::Code(code.clone()))?;
                    }
                    Ok(KetamineObject::null())
                }
                x => Err(KetamineError::TypeError {
                    expected: "<array> | <object>".to_owned(),
                    actual: x.type_name().to_owned(),
                }),
            }
        }
        AST::While(While { condition, code }) => {
            while eval(scope, *condition.clone())?.expect_bool()? {
                eval(&child_scope(scope), AST::Code(code.clone()))?;
            }
            Ok(KetamineObject::null())
        }
        AST::BreakScope(value) => {
            let value = if let Some(value) = value {
                eval(scope, *value)?
            } else {
                KetamineObject::null()
            };
            Err(KetamineError::BreakScope(value))
        }
    }
}

impl From<Function> for KetamineObject {
    fn from(function: Function) -> Self {
        KetamineObject::Function(function)
    }
}

impl Display for KetamineError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            KetamineError::UndeclaredVariable(ident) => {
                write!(f, "undeclared variable or function \"{}\"", ident)
            }
            KetamineError::TypeError { expected, actual } => {
                write!(f, "expected {}, got {}", expected, actual)
            }
            other => write!(f, "{:?}", other),
        }
    }
}

impl Display for FullIdent {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let path = self
            .0
            .iter()
            .map(|ident| ident.0.to_owned())
            .collect::<Vec<_>>()
            .join(".");
        write!(f, "{}", path)
    }
}

impl Display for KetamineObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            KetamineObject::Number(num) => write!(f, "{}", num.into_inner()),
            KetamineObject::Null => write!(f, "null"),
            KetamineObject::String(string) => write!(f, "{}", string),
            KetamineObject::Boolean(boolean) => write!(f, "{}", boolean),
            KetamineObject::Dict(object) => {
                let content = object
                    .deref()
                    .borrow()
                    .iter()
                    .map(|(k, v)| format!("\"{}\": {}", k, v.to_string()))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{{{}}}", content)
            }
            KetamineObject::Function(function) => {
                let params = function
                    .params
                    .iter()
                    .map(|ident| ident.0.clone())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "function {}({}) {{ ... }}", function.ident.0, params)
            }
            KetamineObject::Array(array) => {
                let content = array
                    .deref()
                    .borrow()
                    .iter()
                    .map(KetamineObject::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "[{}]", content)
            }
            _ => unimplemented!(),
        }
    }
}
