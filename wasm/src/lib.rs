#![feature(proc_macro, wasm_custom_section, wasm_import_module)]

extern crate j8t;
extern crate wasm_bindgen;

#[macro_use]
extern crate serde_derive;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    // Provided by playground.js.
    fn now_ms() -> usize;
}

#[derive(Default, Serialize)]
struct Pos {
    line: usize,
    col: usize,
}

#[derive(Default, Serialize)]
struct Error {
    msg: String,
    start: Pos,
    end: Pos,
}

#[derive(Serialize)]
struct Result {
    output: String,
    error: Error,
}

fn err_from_j8t(input: &[u8], err: j8t::ParseError) -> Error {
    let mut start = Pos::default();
    let mut end = Pos::default();

    // TODO: j8t returns start/end offsets as byte offsets into UTF8, but
    // JS wants start/end positions as UTF-16 indexes.
    let mut line = 1;
    let mut line_start = 0;
    let mut col = 1;
    for (i, c) in input.iter().chain([0, 0].iter()).enumerate() {
        if i == err.at.start {
            start = Pos {
                line: line,
                col: col,
            };
        }
        if i == err.at.end {
            end = Pos {
                line: line,
                col: col,
            };
        }
        match *c as char {
            '\n' => {
                line += 1;
                col = 1;
                line_start = i + 1;
            }
            _ => col += 1,
        }
    }

    Error {
        msg: err.msg,
        start: start,
        end: end,
    }
}

#[wasm_bindgen]
pub fn run(code: &str) -> JsValue {
    let mut trace = j8t::Trace::new(false, Some(Box::new(|| now_ms())));
    let inv = j8t::Invocation {
        filename: String::from("input.js"),
        input: Vec::from(code),
        fmt: false,
        rename: j8t::Rename::Off,
    };

    let mut output = Vec::<u8>::new();
    let result = match j8t::run(&mut trace, &inv, &mut output) {
        Err(err) => Result {
            output: String::default(),
            error: err_from_j8t(&inv.input, err),
        },
        Ok(()) => Result {
            output: String::from_utf8_lossy(&output).to_string(),
            error: Error::default(),
        },
    };
    JsValue::from_serde(&result).unwrap()
}
