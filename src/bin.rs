#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

use crate::interpreter::KetamineObjectExt;
use crate::interpreter::{KetamineObject, KetamineObjectRef, KetamineResult, Scope};
use crate::parser::ParseResult;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::io::{stdin, stdout, Write};

mod interpreter;
mod parser;

fn main() -> ParseResult<()> {
    fn print(args: Vec<KetamineObjectRef>) -> KetamineResult {
        println!(
            "{}",
            args.iter()
                .map(|arg| arg.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        );
        Ok(KetamineObject::null())
    }

    fn exit(_: Vec<KetamineObjectRef>) -> KetamineResult {
        std::process::exit(0);
    }

    let mut scope = Scope {
        parent: None,
        variables: HashMap::new(),
    };

    scope.native_function("print", print);
    scope.native_function("exit", exit);
    let scope = Rc::new(RefCell::new(scope));

    let mut line = String::new();
    loop  {
        line.clear();
        print!("> ");
        stdout().flush().unwrap();
        stdin().read_line(&mut line).unwrap();
        match parser::parse_source(&line.trim_matches('\n')) {
            Ok(ast) => match interpreter::eval(&scope, ast) {
                Err(e) => println!(" --> execution error: {}", e),
                Ok(res) => println!(" --> {}", res.to_string())
            },
            Err(e) => {
                println!("{}\n", e);
                continue;
            }
        };

    }
}
