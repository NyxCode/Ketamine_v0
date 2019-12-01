#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

use crate::parser::{ParseResult, AST, Ident, Function, Code};
use std::rc::Rc;
use crate::interpreter2::{Scope, KetaminObject, KetaminValue, KetaminObjectRef, KetaminResult};
use std::collections::HashMap;
use std::cell::RefCell;
use std::ops::Deref;
use crate::interpreter2::KetaminObjectExt;

mod parser;
mod interpreter2;

fn main() -> ParseResult<()> {
    let ast = parser::parse_source(
        r#"
            function greet(person) {
                print(
                    "Hallo,",
                    if person.gender == "male" {
                        "Herr";
                    } else if person.gender == "female" {
                        "Frau";
                    },
                    person.first_name, person.last_name + "!"
                );
            }

            var myself = {
                gender: "male",
                first_name: "Moritz",
                last_name: "Bischof"
            };
            greet(myself);
        "#,
    )?;
    let scope = Rc::new(RefCell::new(Scope {
        parent: None,
        variables: HashMap::new(),
    }));

    {
        fn print(args: Vec<KetaminObjectRef>) -> KetaminResult {
            println!("{}", args.iter().map(|arg| arg.to_string()).collect::<Vec<_>>().join(" "));
            Ok(KetaminObject::null())
        }

        scope.deref().borrow_mut().set_ident(Ident("print".to_owned()),
        Rc::new(RefCell::new(KetaminObject {
            value: KetaminValue::NativeFunction(print),
            methods: Default::default(),
            getters: Default::default(),
            setters: Default::default()
        })));

    }

    interpreter2::eval(&scope, ast).expect("evaluation failed!");

    Ok(())
}
