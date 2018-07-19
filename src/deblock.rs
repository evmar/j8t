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
use std;
use visit;

fn consumes_dangling_else(stmt: &ast::Stmt) -> bool {
    match *stmt {
        ast::Stmt::If(ref i) => {
            if let Some(ref else_) = i.else_ {
                consumes_dangling_else(else_)
            } else {
                true
            }
        }
        ast::Stmt::Block(_) => false,
        ast::Stmt::While(ref w) => consumes_dangling_else(&w.body),
        ast::Stmt::For(ref f) => consumes_dangling_else(&f.body),
        ast::Stmt::ForInOf(ref f) => consumes_dangling_else(&f.body),
        ast::Stmt::Label(ref l) => consumes_dangling_else(&l.stmt),

        ast::Stmt::Var(_)
        | ast::Stmt::Empty
        | ast::Stmt::Expr(_)
        | ast::Stmt::Switch(_)
        | ast::Stmt::Continue(_)
        | ast::Stmt::Break(_)
        | ast::Stmt::Return(_)
        | ast::Stmt::Throw(_)
        | ast::Stmt::Try(_)
        | ast::Stmt::DoWhile(_)
        | ast::Stmt::Function(_)
        | ast::Stmt::Class(_) => false,
    }
}

fn is_block(s: &ast::Stmt) -> bool {
    match *s {
        ast::Stmt::Block(_) => true,
        _ => false,
    }
}

struct Deblock {}
impl visit::Visit for Deblock {
    fn expr(&mut self, en: &mut ast::ExprNode) {
        match en.expr {
            ast::Expr::Function(ref mut func) => {
                visit::func(&mut func.func, self);
            }
            _ => visit::expr(en, self),
        }
    }

    fn stmt(&mut self, stmt: &mut ast::Stmt) {
        if let ast::Stmt::Try(_) = stmt {
            // A try-catch statement syntactically must have braces.
            // Special-case it here.
        } else {
            visit::stmt(stmt, self);
        }

        *stmt = match *stmt {
            // An "if" statement with an "else" must be careful to brace if the
            // body can consume the "else".
            ast::Stmt::If(ref mut i) => {
                if !is_block(&i.iftrue) && i.else_.is_some() && consumes_dangling_else(&i.iftrue) {
                    let mut e = ast::Stmt::Empty;
                    std::mem::swap(&mut e, &mut i.iftrue);
                    i.iftrue = ast::Stmt::Block(vec![e]);
                }
                return;
            }
            ast::Stmt::Try(ref mut try) => {
                // Don't visit the contained blocks directly, but rather only visit the
                // children of the blocks.
                visit::stmt(&mut try.block, self);
                if let Some((_, ref mut catch)) = try.catch {
                    visit::stmt(catch, self);
                }
                if let Some(ref mut finally) = try.finally {
                    visit::stmt(finally, self);
                }
                return;
            }
            ast::Stmt::Block(ref mut stmts) => {
                if stmts.len() != 1 {
                    return;
                }
                stmts.pop().unwrap()
            }
            _ => return,
        }
    }
}

pub fn deblock(module: &mut ast::Module) {
    let mut d = Deblock {};
    visit::module(module, &mut d);
}

#[cfg(test)]
mod tests {
    use ast;
    use test_util::{must_parse, ast_eq};

    fn deblock(input: &str) -> ast::Module {
        let mut module = must_parse(input);
        super::deblock(&mut module);
        module
    }

    #[test]
    fn deblock_if() {
        // Simplest deblocking positive case.
        ast_eq(
            &deblock("if (a) { return b(c) }"),
            "if (a) return b(c)"
        );

        // Dangling else.
        ast_eq(
            &deblock("if (a) { if (b) c; } else d;"),
            "if (a) { if (b) c; } else d"
        );
    }

    #[test]
    fn try() {
        // Can't remove blocks on try/catch.
        ast_eq(
            &deblock("try { x; } catch (e) { y; }"),
            "try { x } catch (e) { y }"
        );
    }

    #[test]
    fn nested() {
        ast_eq(
            &deblock("if (a) { if (b) { c; } else if (d) { e } } else if (g) { h }"),
            "if (a) {
  if (b) c;
  else if (d) e
}
else if (g) h"
        );
    }

    #[test]
    fn class() {
        // Can't remove blocks on class/methods.
        ast_eq(
            &deblock("class C { f() { if (a) { b; } } }"),
            "class C { f() { if (a) b } }"
        );
    }
}
