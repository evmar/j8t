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
    let mut p = j8t::parse::Parser::new(code.as_bytes());
    let mut module = match p.module() {
        Ok(stmts) => stmts,
        Err(err) => {
            let mut buf = Vec::<u8>::new();
            err.print(&p.lexer, &mut buf).unwrap();
            return String::from_utf8_lossy(&buf).into();
        }
    };

    j8t::trans::deblock(&mut module);

    let mut output = Vec::<u8>::new();
    {
        let mut writer = j8t::gen::Writer::new(&mut output);
        //writer.disable_asi = fmt;
        writer.module(&module).unwrap();
    }
    
    return String::from_utf8_lossy(&output).into();
}
