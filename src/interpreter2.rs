use ordered_float::OrderedFloat;
use std::collections::HashMap;
use crate::parser::{Ident, Function, parse_source, AST, Var, Call, FullIdent, BinaryOperation, BinaryOperator, recursive_parse};
use std::rc::Rc;
use pest::state;
use std::borrow::{BorrowMut, Borrow};
use std::cell::{RefCell, Ref};
use std::ops::{Deref, DerefMut};

#[derive(Debug)]
pub enum KetamineError {
    Unknown,
    VariableRedefined(Ident),
    UndeclaredVariable(FullIdent),
    TypeError {
        expected: String,
        actual: String,
    },
    ArgumentError,
    Returned(KetamineObjectRef),
}

pub type KetamineResult = Result<KetamineObjectRef, KetamineError>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KetamineValue {
    Null,
    Number(OrderedFloat<f64>),
    String(String),
    Array(Vec<KetamineObject>),
    Boolean(bool),
    Dict(HashMap<String, KetamineObjectRef>),
    Function(Function),
    NativeFunction(fn(Vec<KetamineObjectRef>) -> KetamineResult),
}


pub type FunctionTable = HashMap<Ident, Rc<Function>>;

pub type KetamineObjectRef = Rc<RefCell<KetamineObject>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KetamineObject {
    pub value: KetamineValue,
    pub methods: FunctionTable,
    pub getters: FunctionTable,
    pub setters: FunctionTable,
}

impl KetamineObject {
    pub fn null() -> KetamineObjectRef {
        RefCell::new(KetamineObject {
            value: KetamineValue::Null,
            methods: HashMap::new(),
            getters: HashMap::new(),
            setters: HashMap::new(),
        }).into()
    }

    fn number(num: f64) -> KetamineObjectRef {
        RefCell::new(KetamineObject {
            value: KetamineValue::Number(OrderedFloat::from(num)),
            methods: HashMap::new(),
            getters: HashMap::new(),
            setters: HashMap::new(),
        }).into()
    }

    fn boolean(value: bool) -> KetamineObjectRef {
        RefCell::new(KetamineObject {
            value: KetamineValue::Boolean(value),
            methods: HashMap::new(),
            getters: HashMap::new(),
            setters: HashMap::new(),
        }).into()
    }

    fn dict(value: HashMap<String, KetamineObjectRef>) -> KetamineObjectRef {
        RefCell::new(KetamineObject {
            value: KetamineValue::Dict(value),
            methods: Default::default(),
            getters: Default::default(),
            setters: Default::default(),
        }).into()
    }

    fn string(value: String) -> KetamineObjectRef {
        RefCell::new(KetamineObject {
            value: KetamineValue::String(value),
            methods: Default::default(),
            getters: Default::default(),
            setters: Default::default(),
        }).into()
    }
}

impl KetamineObject {
    fn expect_function(&self) -> Result<&Function, KetamineError> {
        if let KetamineValue::Function(function) = &self.value {
            Ok(function)
        } else {
            Err(KetamineError::TypeError {
                expected: "function".to_owned(),
                actual: "?".to_owned(),
            })
        }
    }

    fn call_method(&mut self, method: &Ident, args: Vec<KetamineObject>) -> KetamineResult {
        if let Some(function) = self.methods.get(method) {
            unimplemented!()
        } else {
            Err(KetamineError::UndeclaredVariable(method.clone().into()))
        }
    }

    fn call(&mut self, scope: &Rc<RefCell<Scope>>, mut args: Vec<KetamineObjectRef>) -> KetamineResult {
        match &self.value {
            KetamineValue::NativeFunction(function) => {
                function(args)
            }
            KetamineValue::Function(function) => {
                let mut function_scope = child_scope(scope);
                for idx in 0..function.params.len() {
                    let parameter = &function.params[idx];
                    let argument = if args.is_empty() {
                        KetamineObject::null()
                    } else {
                        args.remove(0)
                    };
                    function_scope.deref().borrow_mut().set_ident(parameter.clone(), argument.clone());
                }

                eval(&function_scope, AST::Code(function.code.clone()))
            }
            _ => Err(KetamineError::TypeError {
                expected: "function".to_owned(),
                actual: "?".to_owned(),
            })
        }
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
    pub fn with_parent(parent: Rc<RefCell<Scope>>) -> Scope {
        Scope {
            parent: Some(parent),
            ..Scope::default()
        }
    }

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
}

fn child_scope(parent: &Rc<RefCell<Scope>>) -> Rc<RefCell<Scope>> {
    Rc::new(RefCell::new(
        Scope {
            parent: Some(parent.clone()),
            ..Scope::default()
        }
    ))
}

pub fn eval(scope: &Rc<RefCell<Scope>>, ast: AST) -> Result<KetamineObjectRef, KetamineError> {
    match ast {
        AST::Code(code) => {
            let child_scope = child_scope(scope);
            let mut last_value = None;
            for statement in code.0 {
                match eval(&child_scope, statement) {
                    Err(KetamineError::Returned(value)) => return Ok(value),
                    Err(other) => return Err(other),
                    Ok(x) => last_value = Some(x)
                }
            }
            Ok(last_value.unwrap_or_else(|| KetamineObject::null()))
        }
        AST::Var(Var(ident, value)) => {
            let value = eval(&scope, *value)?;
            scope.deref().borrow_mut().variables.insert(ident, value);
            Ok(KetamineObject::null())
        }
        AST::Number(float) => {
            Ok(KetamineObject::number(*float))
        }
        AST::Call(Call { ident, args }) => {
            let args = args.into_iter()
                .map(|arg| eval(scope, arg))
                .collect::<Result<Vec<KetamineObjectRef>, KetamineError>>()?;
            scope.deref().borrow().find_full_ident(&ident)
                .ok_or_else(|| KetamineError::UndeclaredVariable(ident))?
                .deref().borrow_mut()
                .call(scope, args)
        }
        AST::Ident(ident) => {
            scope.deref().borrow()
                .find_ident(&ident)
                .ok_or_else(|| KetamineError::UndeclaredVariable(ident.into()))
        }
        AST::FullIdent(ident) => {
            scope.deref().borrow().find_full_ident(&ident)
                .ok_or_else(|| KetamineError::UndeclaredVariable(ident))
        }
        AST::BinaryOp(BinaryOperation(lhs, op, rhs)) => {
            let lhs = eval(scope, *lhs)?;
            let rhs = eval(scope, *rhs)?;
            match op {
                BinaryOperator::Add => {
                    if lhs.is_number() && rhs.is_number() {
                        let result = *lhs.expect_number()? + *rhs.expect_number()?;
                        Ok(KetamineObject::number(result))
                    } else {
                        let mut string= lhs.to_string();
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
                _ => unimplemented!()
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
            scope.deref().borrow_mut().set_ident(ident, function.clone());
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
                    return eval(scope, AST::Code(clause.code))
                }
            }
            Ok(KetamineObject::null())
        },
        AST::Boolean(bool) => Ok(KetamineObject::boolean(bool)),
        other => panic!("{:?} unimplemented", other)
    }
}

pub trait KetamineObjectExt {
    fn is_number(&self) -> bool;
    fn is_string(&self) -> bool;
    fn expect_number(&self) -> Result<OrderedFloat<f64>, KetamineError>;
    fn expect_bool(&self) -> Result<bool, KetamineError>;
    fn to_string(&self) -> String;
    fn call_getter(&self, ident: &Ident) -> KetamineResult;

}

impl KetamineObjectExt for KetamineObjectRef {
    fn is_number(&self) -> bool {
        if let KetamineValue::Number(_) = self.deref().borrow().value {
            true
        } else {
            false
        }
    }

    fn is_string(&self) -> bool {
        if let KetamineValue::String(_) = self.deref().borrow().value {
            true
        } else {
            false
        }
    }


    fn expect_number(&self) -> Result<OrderedFloat<f64>, KetamineError> {
        match self.deref().borrow().value {
            KetamineValue::Number(num) => Ok(num.clone()),
            _ => Err(KetamineError::TypeError {
                expected: "number".to_owned(),
                actual: "?".to_owned(),
            })
        }
    }

    fn expect_bool(&self) -> Result<bool, KetamineError> {
        match self.deref().borrow().value {
            KetamineValue::Boolean(value) => Ok(value),
            _ => Err(KetamineError::TypeError {
                expected: "boolean".to_owned(),
                actual: "?".to_owned(),
            })
        }
    }

    fn to_string(&self) -> String {
        match &self.deref().borrow().value {
            KetamineValue::Number(num) => num.to_string(),
            KetamineValue::Null => "null".to_string(),
            KetamineValue::String(string) => string.to_owned(),
            KetamineValue::Boolean(boolean) => boolean.to_string(),
            KetamineValue::Dict(object) => {
                let content = object.iter()
                    .map(|(k, v)| format!("\"{}\": {}", k, v.to_string()))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{{}}}", content)
            }
            KetamineValue::Function(function) => {
                let params = function.params.iter()
                    .map(|ident| ident.0.clone())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("function {}({}) {{ ... }}", function.ident.0, params)
            }
            _ => unimplemented!()
        }
    }

    fn call_getter(&self, ident: &Ident) -> KetamineResult {
        if let KetamineValue::Dict(dict) = &self.deref().borrow().value {
            let value = dict.get(&ident.0).
                ok_or_else(|| KetamineError::UndeclaredVariable(ident.clone().into()))?;
            Ok(value.clone())
        } else {
            Err(KetamineError::UndeclaredVariable(ident.clone().into()))
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
        KetamineObject {
            value: KetamineValue::Function(function),
            methods: Default::default(),
            getters: Default::default(),
            setters: Default::default(),
        }.into()
    }
}