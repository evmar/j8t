/*
 * Copyright 2018 Google LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use ast;
use gen;
use parse;

pub fn must_parse(input: &str) -> ast::Module {
    parse::Parser::new(input.as_bytes()).module().unwrap()
}

fn must_gen(module: &ast::Module) -> String {
    let mut buf: Vec<u8> = Vec::new();
    {
        let mut w = gen::Writer::new(&mut buf);
        w.disable_asi = true;
        w.module(&module).unwrap();
    }
    String::from_utf8(buf).unwrap()
}

pub fn ast_eq(input: &ast::Module, expected: &str) {
    let input_str = must_gen(input);
    let output_str = must_gen(&must_parse(expected));
    assert_eq!(input_str, output_str);
}
