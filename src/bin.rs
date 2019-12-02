#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

use crate::interpreter::{Scope, KetamineObject};
use crate::parser::ParseResult;
use std::cell::RefCell;

use std::io::{stdin, stdout, Write};
use std::process::exit;
use std::rc::Rc;

mod interpreter;
mod parser;
mod stdlib;

fn main() -> ParseResult<()> {
    let mut scope = Scope::default();
    scope.native_fn("exit", |_| exit(0));
    scope.native_fn("print", stdlib::print);
    let scope = Rc::new(RefCell::new(scope));

    let mut line = String::new();
    loop {
        line.clear();
        print!("> ");
        stdout().flush().unwrap();
        stdin().read_line(&mut line).unwrap();
        match parser::parse_source(&line.trim_matches('\n')) {
            Ok(ast) => match interpreter::eval(&scope, ast) {
                Err(e) => println!(" --> execution error: {}", e),
                Ok(KetamineObject::Null) => (),
                Ok(other) => println!(" --> {}", other.to_string()),
            },
            Err(e) => {
                println!("{}\n", e);
                continue;
            }
        };
    }
}
