use std::ops::Deref;

use crate::interpreter::{KetamineError, KetamineObject, KetamineResult};

fn expect_len<T>(args: &[T], n: usize) -> Result<(), KetamineError> {
    if args.len() != n {
        let msg = format!("expected {} arguments, got {}", n, args.len());
        Err(KetamineError::ArgumentError(msg))
    } else {
        Ok(())
    }
}

fn expect_usize(float: f64) -> Result<usize, KetamineError> {
    if float.fract() == 0.0 {
        Ok(float as usize)
    } else {
        Err(KetamineError::TypeError {
            expected: "<integer>".to_owned(),
            actual: "<float>".to_owned(),
        })
    }
}

pub fn print(args: Vec<KetamineObject>) -> KetamineResult {
    println!(
        "{}",
        args.iter()
            .map(KetamineObject::to_string)
            .collect::<Vec<_>>()
            .join(" ")
    );
    Ok(KetamineObject::null())
}

pub fn contains(mut args: Vec<KetamineObject>) -> KetamineResult {
    expect_len(&args, 2)?;
    let search = args.pop().unwrap();
    let search_in = args.pop().unwrap();

    let result = match search_in {
        KetamineObject::Array(array) => array.deref().borrow().contains(&search),
        KetamineObject::String(string) => string.contains(&search.to_string()),
        _ => false,
    };

    Ok(KetamineObject::boolean(result))
}

pub fn substring(mut args: Vec<KetamineObject>) -> KetamineResult {
    expect_len(&args, 3)?;
    let end = expect_usize(*args.pop().unwrap().expect_number()?)?;
    let start = expect_usize(*args.pop().unwrap().expect_number()?)?;
    let string = args.pop().unwrap().expect_string()?.to_owned();
    let end = end.min(string.len());
    let start = start.max(0);
    Ok(KetamineObject::String(string[start..end].to_owned()))
}
