use crate::interpreter::{RefCounted, KetamineObject, KetamineResult, KetamineError};
use std::collections::HashMap;
use crate::parser::{Ident, FullIdent};
use std::ops::Deref;
use std::rc::Rc;
use std::cell::RefCell;
use crate::interpreter::KetamineError::UndeclaredVariable;

#[derive(Debug)]
pub struct Scope {
    pub parent: Option<RefCounted<Scope>>,
    pub variables: HashMap<Ident, KetamineObject>,
}

pub type ScopeRef = Rc<RefCell<Scope>>;


impl Default for Scope {
    fn default() -> Self {
        Scope {
            parent: None,
            variables: HashMap::new(),
        }
    }
}

impl Scope {
    pub fn get_var(&self, ident: &Ident) -> KetamineResult {
        if let Some(var) = self.variables.get(ident) {
            Ok(var.clone())
        } else if let Some(parent) = &self.parent {
            parent.deref().borrow().get_var(ident)
        } else {
            Err(KetamineError::UndeclaredVariable(ident.clone().into()))
        }
    }

    pub fn new_var(&mut self, ident: Ident, value: KetamineObject) {
        self.variables.insert(ident, value);
    }

    pub fn assign_var(&mut self, ident: Ident, value: KetamineObject) -> Result<(), KetamineError> {
        if self.variables.contains_key(&ident) {
            self.variables.insert(ident, value);
            Ok(())
        } else if let Some(parent) = &self.parent {
            parent.deref().borrow_mut().assign_var(ident, value)?;
            Ok(())
        } else {
            Err(KetamineError::UndeclaredVariable(ident.into()))
        }
    }

    pub fn get(&self, ident: &FullIdent) -> KetamineResult {
        let idents = &ident.0;
        let mut base = self.get_var(&idents[0])?;
        for child_ident in &idents[1..] {
            base = base.expect_dict()?
                .deref().borrow()
                .get(&child_ident.0)
                .cloned()
                .ok_or_else(|| KetamineError::UndeclaredVariable(ident.clone()))?;
        }
        Ok(base)
    }

    pub fn assign(
        &mut self,
        ident: FullIdent,
        value: KetamineObject,
    ) -> Result<(), KetamineError> {
        let idents = &ident.0;
        let base_ident = &idents[0];

        match idents.len() {
            1 => {
                // we are only setting a variable
                self.assign_var(ident.0[0].clone(), value)?;
                Ok(())
            }
            _ => {
                // we are setting a property of an object, e.g "object.property = 1"
                let mut dict = self.get_var(base_ident)?.expect_dict()?;
                for child_ident in &idents[1..(idents.len() - 1)] {
                    let child_dict = dict.deref().borrow().get(&child_ident.0)
                        .ok_or_else(|| UndeclaredVariable(ident.clone()))?
                        .expect_dict()?;
                    dict = child_dict;
                }
                dict.deref().borrow_mut().insert(idents.last().unwrap().0.clone(), value);
                Ok(())
            }
        }
    }

    pub fn native_fn(
        &mut self,
        name: &str,
        f: impl Fn(Vec<KetamineObject>) -> KetamineResult + 'static,
    ) {
        self.variables
            .insert(Ident(name.to_owned()), KetamineObject::native_fn(f));
    }
}

pub fn child_scope(parent: &RefCounted<Scope>) -> RefCounted<Scope> {
    Rc::new(RefCell::new(Scope {
        parent: Some(parent.clone()),
        ..Scope::default()
    }))
}
