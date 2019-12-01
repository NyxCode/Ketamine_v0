#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

use crate::interpreter2::KetaminObjectExt;
use crate::interpreter2::{KetaminObject, KetaminObjectRef, KetaminResult, Scope};
use crate::parser::{Code, Function, Ident, ParseResult, AST};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

mod interpreter2;
mod parser;

fn main() -> ParseResult<()> {
    let ast = parser::parse_source(
        r#"
            var dict = {a: 1};
            print(dict);
            dict.a = 2;
            print(dict);
        "#,
    )?;
    let scope = Rc::new(RefCell::new(Scope {
        parent: None,
        variables: HashMap::new(),
    }));

    {
        fn print(args: Vec<KetaminObjectRef>) -> KetaminResult {
            println!(
                "{}",
                args.iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            );
            Ok(KetaminObject::null())
        }

        scope.deref().borrow_mut().set_ident(
            Ident("print".to_owned()),
            Rc::new(RefCell::new(KetaminObject::NativeFunction(print))),
        );
    }

    interpreter2::eval(&scope, ast).expect("evaluation failed!");

    Ok(())
}
