#![feature(proc_macro, wasm_custom_section, wasm_import_module)]

extern crate wasm_bindgen;
extern crate j8t;

use wasm_bindgen::prelude::*;

// #[wasm_bindgen]
// extern {
//     fn alert(s: &str);
// }

#[wasm_bindgen]
pub fn j8tw(code: &str) -> String {
    let mut trace = j8t::Trace::new(false);
    let inv = j8t::Invocation {
        filename: String::from("input.js"),
        input: Vec::from(code),
        fmt: false,
        rename: j8t::Rename::Off,
    };

    let mut output = Vec::<u8>::new();
    if let Err(err) = j8t::run(&mut trace, inv, &mut output) {
        return format!("{}", err);
    }
    
    return String::from_utf8_lossy(&output).into();
}
