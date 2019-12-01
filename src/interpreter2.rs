use crate::parser::{
    parse_source, recursive_parse, BinaryOperation, BinaryOperator, Call, FullIdent, Function,
    Ident, Var, AST,
};
use ordered_float::OrderedFloat;
use pest::state;
use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

#[derive(Debug)]
pub enum KetaminError {
    Unknown,
    VariableRedefined(Ident),
    UndeclaredVariable(FullIdent),
    TypeError { expected: String, actual: String },
    ArgumentError,
    Returned(KetaminObjectRef),
}

pub type KetaminResult = Result<KetaminObjectRef, KetaminError>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KetaminValue {
    Null,
    Number(OrderedFloat<f64>),
    String(String),
    Array(Vec<KetaminObject>),
    Boolean(bool),
    Dict(HashMap<String, KetaminObjectRef>),
    Function(Function),
    NativeFunction(fn(Vec<KetaminObjectRef>) -> KetaminResult),
}

pub type FunctionTable = HashMap<Ident, Rc<Function>>;

pub type KetaminObjectRef = Rc<RefCell<KetaminObject>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KetaminObject {
    pub value: KetaminValue,
    pub methods: FunctionTable,
    pub getters: FunctionTable,
    pub setters: FunctionTable,
}

impl KetaminObject {
    pub fn null() -> KetaminObjectRef {
        RefCell::new(KetaminObject {
            value: KetaminValue::Null,
            methods: HashMap::new(),
            getters: HashMap::new(),
            setters: HashMap::new(),
        })
        .into()
    }

    fn number(num: f64) -> KetaminObjectRef {
        RefCell::new(KetaminObject {
            value: KetaminValue::Number(OrderedFloat::from(num)),
            methods: HashMap::new(),
            getters: HashMap::new(),
            setters: HashMap::new(),
        })
        .into()
    }

    fn boolean(value: bool) -> KetaminObjectRef {
        RefCell::new(KetaminObject {
            value: KetaminValue::Boolean(value),
            methods: HashMap::new(),
            getters: HashMap::new(),
            setters: HashMap::new(),
        })
        .into()
    }

    fn dict(value: HashMap<String, KetaminObjectRef>) -> KetaminObjectRef {
        RefCell::new(KetaminObject {
            value: KetaminValue::Dict(value),
            methods: Default::default(),
            getters: Default::default(),
            setters: Default::default(),
        })
        .into()
    }

    fn string(value: String) -> KetaminObjectRef {
        RefCell::new(KetaminObject {
            value: KetaminValue::String(value),
            methods: Default::default(),
            getters: Default::default(),
            setters: Default::default(),
        })
        .into()
    }
}

impl KetaminObject {
    fn expect_function(&self) -> Result<&Function, KetaminError> {
        if let KetaminValue::Function(function) = &self.value {
            Ok(function)
        } else {
            Err(KetaminError::TypeError {
                expected: "function".to_owned(),
                actual: "?".to_owned(),
            })
        }
    }

    fn call_method(&mut self, method: &Ident, args: Vec<KetaminObject>) -> KetaminResult {
        if let Some(function) = self.methods.get(method) {
            unimplemented!()
        } else {
            Err(KetaminError::UndeclaredVariable(method.clone().into()))
        }
    }

    fn call(&self, scope: &Rc<RefCell<Scope>>, mut args: Vec<KetaminObjectRef>) -> KetaminResult {
        match &self.value {
            KetaminValue::NativeFunction(function) => function(args),
            KetaminValue::Function(function) => {
                let function_scope = child_scope(scope);
                for idx in 0..function.params.len() {
                    let parameter = &function.params[idx];
                    let argument = if args.is_empty() {
                        KetaminObject::null()
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
            _ => Err(KetaminError::TypeError {
                expected: "function".to_owned(),
                actual: "?".to_owned(),
            }),
        }
    }
}

#[derive(Debug)]
pub struct Scope {
    pub parent: Option<Rc<RefCell<Scope>>>,
    pub variables: HashMap<Ident, KetaminObjectRef>,
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
    pub fn with_parent(parent: Rc<RefCell<Scope>>) -> Scope {
        Scope {
            parent: Some(parent),
            ..Scope::default()
        }
    }

    pub fn find_ident(&self, ident: &Ident) -> Option<KetaminObjectRef> {
        if let Some(var) = self.variables.get(ident) {
            Some(var.clone())
        } else if let Some(parent) = &self.parent {
            parent.deref().borrow().find_ident(ident)
        } else {
            None
        }
    }

    pub fn find_full_ident(&self, ident: &FullIdent) -> Option<KetaminObjectRef> {
        let idents = &ident.0;
        let mut base = self.find_ident(&idents[0])?;
        for i in 1..idents.len() {
            base = base.call_getter(&idents[i]).ok()?;
        }
        Some(base)
    }

    pub fn set_ident(&mut self, ident: Ident, value: KetaminObjectRef) {
        self.variables.insert(ident, value);
    }
}

fn child_scope(parent: &Rc<RefCell<Scope>>) -> Rc<RefCell<Scope>> {
    Rc::new(RefCell::new(Scope {
        parent: Some(parent.clone()),
        ..Scope::default()
    }))
}

pub fn eval(scope: &Rc<RefCell<Scope>>, ast: AST) -> Result<KetaminObjectRef, KetaminError> {
    match ast {
        AST::Code(code) => {
            let child_scope = child_scope(scope);
            let mut last_value = None;
            for statement in code.0 {
                match eval(&child_scope, statement) {
                    Err(KetaminError::Returned(value)) => return Ok(value),
                    Err(other) => return Err(other),
                    Ok(x) => last_value = Some(x),
                }
            }
            Ok(last_value.unwrap_or_else(|| KetaminObject::null()))
        }
        AST::Var(Var(ident, value)) => {
            let value = eval(&scope, *value)?;
            scope.deref().borrow_mut().variables.insert(ident, value);
            Ok(KetaminObject::null())
        }
        AST::Number(float) => Ok(KetaminObject::number(*float)),
        AST::Call(Call { ident, args }) => {
            let args = args
                .into_iter()
                .map(|arg| eval(scope, arg))
                .collect::<Result<Vec<KetaminObjectRef>, KetaminError>>()?;
            scope
                .deref()
                .borrow()
                .find_full_ident(&ident)
                .ok_or_else(|| KetaminError::UndeclaredVariable(ident))?
                .deref()
                .borrow()
                .call(scope, args)
        }
        AST::Ident(ident) => scope
            .deref()
            .borrow()
            .find_ident(&ident)
            .ok_or_else(|| KetaminError::UndeclaredVariable(ident.into())),
        AST::FullIdent(ident) => scope
            .deref()
            .borrow()
            .find_full_ident(&ident)
            .ok_or_else(|| KetaminError::UndeclaredVariable(ident)),
        AST::BinaryOp(BinaryOperation(lhs, op, rhs)) => {
            let lhs = eval(scope, *lhs)?;
            let rhs = eval(scope, *rhs)?;
            match op {
                BinaryOperator::Add => {
                    if lhs.is_number() && rhs.is_number() {
                        let result = *lhs.expect_number()? + *rhs.expect_number()?;
                        Ok(KetaminObject::number(result))
                    } else {
                        let mut string = lhs.to_string();
                        string.push_str(&rhs.to_string());
                        Ok(KetaminObject::string(string))
                    }
                }
                BinaryOperator::Mod => {
                    let result = *lhs.expect_number()? % *rhs.expect_number()?;
                    Ok(KetaminObject::number(result))
                }
                BinaryOperator::Eq => {
                    let result = lhs == rhs;
                    Ok(KetaminObject::boolean(result))
                }
                BinaryOperator::Lt => {
                    let result = *lhs.expect_number()? < *rhs.expect_number()?;
                    Ok(KetaminObject::boolean(result))
                }
                BinaryOperator::Sub => {
                    let result = *lhs.expect_number()? - *rhs.expect_number()?;
                    Ok(KetaminObject::number(result))
                }
                _ => unimplemented!("{:?}", op),
            }
        }
        AST::Return(value) => {
            let value = if let Some(value) = value {
                eval(scope, *value)?
            } else {
                KetaminObject::null()
            };
            Err(KetaminError::Returned(value))
        }
        AST::Function(function) => {
            let ident = function.ident.clone();
            let function: KetaminObjectRef = function.into();
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
            Ok(KetaminObject::dict(dict))
        }
        AST::String(string) => Ok(KetaminObject::string(string)),
        AST::If(clauses) => {
            for clause in clauses.0 {
                let condition = eval(scope, *clause.condition)?.expect_bool()?;
                if condition {
                    return eval(scope, AST::Code(clause.code));
                }
            }
            Ok(KetaminObject::null())
        }
        AST::Boolean(bool) => Ok(KetaminObject::boolean(bool)),
        other => panic!("{:?} unimplemented", other),
    }
}

pub trait KetaminObjectExt {
    fn is_number(&self) -> bool;
    fn is_string(&self) -> bool;
    fn expect_number(&self) -> Result<OrderedFloat<f64>, KetaminError>;
    fn expect_bool(&self) -> Result<bool, KetaminError>;
    fn to_string(&self) -> String;
    fn call_getter(&self, ident: &Ident) -> KetaminResult;
}

impl KetaminObjectExt for KetaminObjectRef {
    fn is_number(&self) -> bool {
        if let KetaminValue::Number(_) = self.deref().borrow().value {
            true
        } else {
            false
        }
    }

    fn is_string(&self) -> bool {
        if let KetaminValue::String(_) = self.deref().borrow().value {
            true
        } else {
            false
        }
    }

    fn expect_number(&self) -> Result<OrderedFloat<f64>, KetaminError> {
        match self.deref().borrow().value {
            KetaminValue::Number(num) => Ok(num.clone()),
            _ => Err(KetaminError::TypeError {
                expected: "number".to_owned(),
                actual: "?".to_owned(),
            }),
        }
    }

    fn expect_bool(&self) -> Result<bool, KetaminError> {
        match self.deref().borrow().value {
            KetaminValue::Boolean(value) => Ok(value),
            _ => Err(KetaminError::TypeError {
                expected: "boolean".to_owned(),
                actual: "?".to_owned(),
            }),
        }
    }

    fn to_string(&self) -> String {
        match &self.deref().borrow().value {
            KetaminValue::Number(num) => num.to_string(),
            KetaminValue::Null => "null".to_string(),
            KetaminValue::String(string) => string.to_owned(),
            KetaminValue::Boolean(boolean) => boolean.to_string(),
            KetaminValue::Dict(object) => {
                let content = object
                    .iter()
                    .map(|(k, v)| format!("\"{}\": {}", k, v.to_string()))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{{}}}", content)
            }
            KetaminValue::Function(function) => {
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

    fn call_getter(&self, ident: &Ident) -> KetaminResult {
        if let KetaminValue::Dict(dict) = &self.deref().borrow().value {
            let value = dict
                .get(&ident.0)
                .ok_or_else(|| KetaminError::UndeclaredVariable(ident.clone().into()))?;
            Ok(value.clone())
        } else {
            Err(KetaminError::UndeclaredVariable(ident.clone().into()))
        }
    }
}

impl From<KetaminObject> for KetaminObjectRef {
    fn from(obj: KetaminObject) -> Self {
        Rc::new(RefCell::new(obj))
    }
}

impl From<Function> for KetaminObjectRef {
    fn from(function: Function) -> Self {
        KetaminObject {
            value: KetaminValue::Function(function),
            methods: Default::default(),
            getters: Default::default(),
            setters: Default::default(),
        }
        .into()
    }
}
