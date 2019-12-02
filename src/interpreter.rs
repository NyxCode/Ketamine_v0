use crate::parser::{
    Assignment, BinaryOperation, BinaryOperator, Call, FullIdent, Function, Ident, Var, AST,
};
use ordered_float::OrderedFloat;

use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

pub type KetamineResult = Result<KetamineObjectRef, KetamineError>;

#[derive(Debug)]
pub enum KetamineError {
    Unknown,
    VariableRedefined(Ident),
    UndeclaredVariable(FullIdent),
    TypeError { expected: String, actual: String },
    ArgumentError,
    Returned(KetamineObjectRef),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KetamineObject {
    Null,
    Number(OrderedFloat<f64>),
    String(String),
    Array(Vec<KetamineObject>),
    Boolean(bool),
    Dict(HashMap<String, KetamineObjectRef>),
    Function(Function),
    NativeFunction(fn(Vec<KetamineObjectRef>) -> KetamineResult),
}

pub type KetamineObjectRef = Rc<RefCell<KetamineObject>>;
pub type ScopeRef = Rc<RefCell<Scope>>;

impl KetamineObject {
    pub fn null() -> KetamineObjectRef {
        RefCell::new(KetamineObject::Null).into()
    }

    fn number(num: f64) -> KetamineObjectRef {
        RefCell::new(KetamineObject::Number(OrderedFloat::from(num))).into()
    }

    fn boolean(value: bool) -> KetamineObjectRef {
        RefCell::new(KetamineObject::Boolean(value)).into()
    }

    fn dict(value: HashMap<String, KetamineObjectRef>) -> KetamineObjectRef {
        RefCell::new(KetamineObject::Dict(value)).into()
    }

    fn string(value: String) -> KetamineObjectRef {
        RefCell::new(KetamineObject::String(value)).into()
    }

    fn native_fn(f: fn(Vec<KetamineObjectRef>) -> KetamineResult) -> KetamineObjectRef {
        RefCell::new(KetamineObject::NativeFunction(f)).into()
    }
}

#[derive(Debug)]
pub struct Scope {
    pub parent: Option<Rc<RefCell<Scope>>>,
    pub variables: HashMap<Ident, KetamineObjectRef>,
}

impl Default for Scope {
    fn default() -> Self {
        Scope {
            parent: None,
            variables: HashMap::new(),
        }
    }
}

impl Scope {
    pub fn find_ident(&self, ident: &Ident) -> Option<KetamineObjectRef> {
        if let Some(var) = self.variables.get(ident) {
            Some(var.clone())
        } else if let Some(parent) = &self.parent {
            parent.deref().borrow().find_ident(ident)
        } else {
            None
        }
    }

    pub fn find_full_ident(&self, ident: &FullIdent) -> Option<KetamineObjectRef> {
        let idents = &ident.0;
        let mut base = self.find_ident(&idents[0])?;
        for i in 1..idents.len() {
            base = base.call_getter(&idents[i]).ok()?;
        }
        Some(base)
    }

    pub fn set_ident(&mut self, ident: Ident, value: KetamineObjectRef) {
        self.variables.insert(ident, value);
    }

    pub fn set_full_ident(
        &self,
        ident: &FullIdent,
        value: KetamineObjectRef,
    ) -> Result<(), KetamineError> {
        let idents = &ident.0;
        let mut dict = self
            .find_ident(&ident.0[0])
            .ok_or_else(|| KetamineError::UndeclaredVariable(ident.0[0].clone().into()))?;
        for i in 1..(idents.len() - 1) {
            dict = dict.call_getter(&idents[i])?;
        }
        dict.call_setter(&idents.last().unwrap(), value)
    }

    pub fn native_function(&mut self, name: &str, f: fn(Vec<KetamineObjectRef>) -> KetamineResult) {
        self.variables.insert(Ident(name.to_owned()), KetamineObject::native_fn(f));
    }
}

fn child_scope(parent: &Rc<RefCell<Scope>>) -> Rc<RefCell<Scope>> {
    Rc::new(RefCell::new(Scope {
        parent: Some(parent.clone()),
        ..Scope::default()
    }))
}

#[inline(always)]
pub fn eval(scope: &Rc<RefCell<Scope>>, ast: AST) -> Result<KetamineObjectRef, KetamineError> {
    match ast {
        AST::Code(code) => {
            let mut last_value = None;
            for statement in code.0 {
                match eval(&scope, statement) {
                    Err(KetamineError::Returned(value)) => return Ok(value),
                    Err(other) => return Err(other),
                    Ok(x) => last_value = Some(x),
                }
            }
            Ok(last_value.unwrap_or_else(KetamineObject::null))
        }
        AST::Var(Var(ident, value)) => {
            let value = eval(&scope, *value)?;
            scope.deref().borrow_mut().set_ident(ident, value);
            Ok(KetamineObject::null())
        }
        AST::Number(float) => Ok(KetamineObject::number(*float)),
        AST::Call(Call { ident, args }) => {
            let args = args
                .into_iter()
                .map(|arg| eval(scope, arg))
                .collect::<Result<Vec<KetamineObjectRef>, KetamineError>>()?;
            scope
                .deref()
                .borrow()
                .find_full_ident(&ident)
                .ok_or_else(|| KetamineError::UndeclaredVariable(ident))?
                .call_self(&scope, args)
        }
        AST::Ident(ident) => scope
            .deref()
            .borrow()
            .find_ident(&ident)
            .ok_or_else(|| KetamineError::UndeclaredVariable(ident.into())),
        AST::FullIdent(ident) => scope
            .deref()
            .borrow()
            .find_full_ident(&ident)
            .ok_or_else(|| KetamineError::UndeclaredVariable(ident)),
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
        AST::Return(value) => {
            let value = if let Some(value) = value {
                eval(scope, *value)?
            } else {
                KetamineObject::null()
            };
            Err(KetamineError::Returned(value))
        }
        AST::Function(function) => {
            let ident = function.ident.clone();
            let function: KetamineObjectRef = function.into();
            scope
                .deref()
                .borrow_mut()
                .set_ident(ident, function.clone());
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
            let value = eval(scope, *value)?;
            scope.deref().borrow_mut().set_full_ident(&ident, value)?;
            Ok(KetamineObject::null())
        }
        other => panic!("{:?} unimplemented", other),
    }
}

pub trait KetamineObjectExt {
    fn is_number(&self) -> bool;
    fn is_string(&self) -> bool;
    fn expect_number(&self) -> Result<OrderedFloat<f64>, KetamineError>;
    fn expect_bool(&self) -> Result<bool, KetamineError>;
    fn to_string(&self) -> String;
    fn call_getter(&self, ident: &Ident) -> KetamineResult;
    fn call_setter(&self, ident: &Ident, value: KetamineObjectRef) -> Result<(), KetamineError>;
    fn call_self(&self, scope: &ScopeRef, params: Vec<KetamineObjectRef>) -> KetamineResult;
}

impl KetamineObjectExt for KetamineObjectRef {
    fn is_number(&self) -> bool {
        if let KetamineObject::Number(_) = *self.deref().borrow() {
            true
        } else {
            false
        }
    }

    fn is_string(&self) -> bool {
        if let KetamineObject::String(_) = *self.deref().borrow() {
            true
        } else {
            false
        }
    }

    fn expect_number(&self) -> Result<OrderedFloat<f64>, KetamineError> {
        match *self.deref().borrow() {
            KetamineObject::Number(num) => Ok(num),
            _ => Err(KetamineError::TypeError {
                expected: "number".to_owned(),
                actual: "?".to_owned(),
            }),
        }
    }

    fn expect_bool(&self) -> Result<bool, KetamineError> {
        match *self.deref().borrow() {
            KetamineObject::Boolean(value) => Ok(value),
            _ => Err(KetamineError::TypeError {
                expected: "boolean".to_owned(),
                actual: "?".to_owned(),
            }),
        }
    }

    fn to_string(&self) -> String {
        match &*self.deref().borrow() {
            KetamineObject::Number(num) => num.to_string(),
            KetamineObject::Null => "null".to_string(),
            KetamineObject::String(string) => string.to_owned(),
            KetamineObject::Boolean(boolean) => boolean.to_string(),
            KetamineObject::Dict(object) => {
                let content = object
                    .iter()
                    .map(|(k, v)| format!("\"{}\": {}", k, v.to_string()))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{{}}}", content)
            }
            KetamineObject::Function(function) => {
                let params = function
                    .params
                    .iter()
                    .map(|ident| ident.0.clone())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("function {}({}) {{ ... }}", function.ident.0, params)
            }
            _ => unimplemented!(),
        }
    }

    fn call_getter(&self, ident: &Ident) -> KetamineResult {
        if let KetamineObject::Dict(dict) = &*self.deref().borrow() {
            let value = dict
                .get(&ident.0)
                .ok_or_else(|| KetamineError::UndeclaredVariable(ident.clone().into()))?;
            Ok(value.clone())
        } else {
            Err(KetamineError::UndeclaredVariable(ident.clone().into()))
        }
    }

    fn call_setter(&self, ident: &Ident, value: KetamineObjectRef) -> Result<(), KetamineError> {
        if let KetamineObject::Dict(ref mut dict) = self.deref().borrow_mut().deref_mut() {
            dict.insert(ident.0.clone(), value);
            Ok(())
        } else {
            Err(KetamineError::TypeError {
                expected: "object".to_string(),
                actual: "?".to_string(),
            })
        }
    }

    fn call_self(&self, scope: &ScopeRef, mut args: Vec<KetamineObjectRef>) -> KetamineResult {
        match &self.deref().borrow().deref() {
            KetamineObject::NativeFunction(function) => function(args),
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
                        .set_ident(parameter.clone(), argument.clone());
                }

                eval(&function_scope, AST::Code(function.code.clone()))
            }
            _ => Err(KetamineError::TypeError {
                expected: "function".to_owned(),
                actual: "?".to_owned(),
            }),
        }
    }
}

impl From<KetamineObject> for KetamineObjectRef {
    fn from(obj: KetamineObject) -> Self {
        Rc::new(RefCell::new(obj))
    }
}

impl From<Function> for KetamineObjectRef {
    fn from(function: Function) -> Self {
        KetamineObject::Function(function).into()
    }
}

impl Display for KetamineError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            KetamineError::UndeclaredVariable(ident) =>
                write!(f, "undeclared variable or function \"{}\"", ident),
            KetamineError::TypeError { expected, actual } =>
                write!(f, "expected {}, got {}", expected, actual),
            other => write!(f, "{:?}", other)
        }
    }
}

use std::fmt::Display;
use std::fmt::Formatter;

impl Display for FullIdent {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let path = self.0.iter()
            .map(|ident| ident.0.to_owned())
            .collect::<Vec<_>>().join(".");
        write!(f, "{}", path)
    }
}