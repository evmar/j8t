/*
 * Copyright 2017 Google LLC
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
use trans::visit;

const NAME_GEN_ALPHABET: &'static [u8] = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$";

struct NameGen {
    i: usize,
}

impl NameGen {
    fn new() -> NameGen {
        NameGen { i: 0 }
    }
    fn clone(&self) -> NameGen {
        NameGen { i: self.i }
    }
    fn new_name(&mut self) -> String {
        let mut i = self.i;
        self.i += 1;
        let mut name: String = String::new();
        name.push(NAME_GEN_ALPHABET[i % NAME_GEN_ALPHABET.len()] as char);
        i /= NAME_GEN_ALPHABET.len();
        let ext_len = NAME_GEN_ALPHABET.len() + 10;
        while i > 0 {
            let ci = i % ext_len;
            i /= ext_len;
            name.push(if ci < NAME_GEN_ALPHABET.len() {
                NAME_GEN_ALPHABET[ci]
            } else {
                b"01234567890"[ci - NAME_GEN_ALPHABET.len()]
            } as char);
        }
        return name;
    }
}

fn rename_scope(gen: &mut NameGen, scope: &mut ast::Scope, debug: bool) {
    for (i, s) in scope.bindings.iter_mut().enumerate() {
        let new_name = if debug {
            format!("{}{}", s.name.borrow(), i)
        } else {
            gen.new_name()
        };
        *s.name.borrow_mut() = new_name;
    }
}

fn rename_func(gen: &mut NameGen, func: &mut ast::Func, debug: bool) {
    let mut gen = gen.clone();
    rename_scope(&mut gen, &mut func.scope, debug);
    for stmt in func.body.iter_mut() {
        rename_stmt(&mut gen, stmt, debug);
    }
}

fn rename_stmt(gen: &mut NameGen, stmt: &mut ast::Stmt, debug: bool) {
    match *stmt {
        ast::Stmt::Function(ref mut fun) => {
            rename_func(gen, &mut fun.func, debug);
            // TODO: expressions, e.g. function f(a=(function()...)) {}
        }
        _ => {
            visit::stmt_stmt(stmt, |s| rename_stmt(gen, s, debug));
        }
    }
    visit::stmt_expr(stmt, |e| rename_expr(gen, e, debug));
}

fn rename_expr(gen: &mut NameGen, expr: &mut ast::Expr, debug: bool) {
    match *expr {
        ast::Expr::Function(ref mut fun) => rename_func(gen, &mut fun.func, debug),
        _ => visit::expr_expr(expr, |(_, e)| rename_expr(gen, e, debug)),
    }
}

pub fn rename(module: &mut ast::Module, debug: bool) {
    let mut gen = NameGen::new();
    rename_scope(&mut gen, &mut module.scope, debug);
    for stmt in module.stmts.iter_mut() {
        rename_stmt(&mut gen, stmt, debug);
    }
}
