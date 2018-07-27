#![feature(proc_macro, wasm_custom_section, wasm_import_module)]

extern crate j8t;
extern crate wasm_bindgen;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    // Provided by playground.js.
    fn now_ms() -> usize;
}

static mut LAST_ERR: Option<j8t::ParseError> = None;

#[wasm_bindgen]
pub fn last_err_str() -> String {
    unsafe {
        if let Some(ref err) = LAST_ERR {
            return err.msg.clone();
        }
    }
    return String::new();
}
#[wasm_bindgen]
pub fn last_err_start() -> usize {
    unsafe {
        if let Some(ref err) = LAST_ERR {
            return err.at.start;
        }
    }
    return 0;
}
#[wasm_bindgen]
pub fn last_err_end() -> usize {
    unsafe {
        if let Some(ref err) = LAST_ERR {
            return err.at.end;
        }
    }
    return 0;
}

#[wasm_bindgen]
pub fn run(code: &str) -> String {
    let mut trace = j8t::Trace::new(false, Some(Box::new(|| now_ms())));
    let inv = j8t::Invocation {
        filename: String::from("input.js"),
        input: Vec::from(code),
        fmt: false,
        rename: j8t::Rename::Off,
    };

    let mut output = Vec::<u8>::new();
    if let Err(err) = j8t::run(&mut trace, &inv, &mut output) {
        unsafe {
            LAST_ERR = Some(err);
        }
        return String::from("ERROR");
    }
    unsafe {
        LAST_ERR = None;
    }

    let res = String::from_utf8_lossy(&output).to_string();
    // TODO: return trace info too.
    // res.push_str(&trace.to_string());
    res
}
