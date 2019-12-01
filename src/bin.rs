#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

use crate::interpreter2::KetaminObjectExt;
use crate::interpreter2::{KetaminObject, KetaminObjectRef, KetaminResult, KetaminValue, Scope};
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
            function fib(n) {
                return if n < 3 {
                    1;
                } else {
                    fib(n - 1) + fib(n - 2);
                };
            }

            print(fib(25));
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
            Rc::new(RefCell::new(KetaminObject {
                value: KetaminValue::NativeFunction(print),
                methods: Default::default(),
                getters: Default::default(),
                setters: Default::default(),
            })),
        );
    }

    interpreter2::eval(&scope, ast).expect("evaluation failed!");

    Ok(())
}
