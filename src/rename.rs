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
use visit;

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
        let mut name: Vec<u8> = Vec::new();

        let ext_len = NAME_GEN_ALPHABET.len() + 10;
        while i >= NAME_GEN_ALPHABET.len() {
            let ci = i % ext_len;
            i /= ext_len;
            name.push(if ci < NAME_GEN_ALPHABET.len() {
                NAME_GEN_ALPHABET[ci]
            } else {
                b"01234567890"[ci - NAME_GEN_ALPHABET.len()]
            });
        }
        name.push(NAME_GEN_ALPHABET[i % NAME_GEN_ALPHABET.len()]);
        name.reverse();

        return String::from_utf8_lossy(&name).into();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn name_gen() {
        let mut n = NameGen::new();
        assert_eq!(n.new_name(), "a");
        assert_eq!(n.new_name(), "b");
        assert_eq!(n.new_name(), "c");
        n.i = 26 + 26;
        assert_eq!(n.new_name(), "_");
        assert_eq!(n.new_name(), "$");
        assert_eq!(n.new_name(), "a0");
        assert_eq!(n.new_name(), "a1");
        n.i = 26 + 26 + 2 + 10;
        assert_eq!(n.new_name(), "ba");
    }
}

struct V<'a> {
    gen: &'a mut NameGen,
    debug: bool,
}

impl<'a> visit::Visit for V<'a> {
    fn expr(&mut self, en: &mut ast::ExprNode) {
        rename_expr(self.gen, en, self.debug);
    }
    fn stmt(&mut self, s: &mut ast::Stmt) {
        rename_stmt(self.gen, s, self.debug);
    }
}

fn rename_scope(gen: &mut NameGen, scope: &mut ast::Scope, debug: bool) {
    for (i, s) in scope.iter().enumerate() {
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
            visit::stmt(
                stmt,
                &mut V {
                    gen: gen,
                    debug: debug,
                },
            );
        }
    }
}

fn rename_expr(gen: &mut NameGen, en: &mut ast::ExprNode, debug: bool) {
    match en.expr {
        ast::Expr::Function(ref mut fun) => rename_func(gen, &mut fun.func, debug),
        _ => visit::expr(
            en,
            &mut V {
                gen: gen,
                debug: debug,
            },
        ),
    }
}

pub fn rename(module: &mut ast::Module, debug: bool) {
    let mut gen = NameGen::new();
    rename_scope(&mut gen, &mut module.scope, debug);
    for stmt in module.stmts.iter_mut() {
        rename_stmt(&mut gen, stmt, debug);
    }
}
