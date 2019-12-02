use crate::interpreter::{KetamineObject, KetamineResult};

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
