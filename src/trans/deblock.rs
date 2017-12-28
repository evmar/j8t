/**
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
use trans::visit;
use trans::visit::Visit;

struct Deblock {}

fn consumes_dangling_else(stmt: &ast::Stmt) -> bool {
    match *stmt {
        ast::Stmt::If(ref i) => {
            if i.else_.is_none() {
                return true;
            }
            consumes_dangling_else(&i.iftrue)
        }
        ast::Stmt::Block(_) => false,
        ast::Stmt::While(ref w) => consumes_dangling_else(&w.body),
        ast::Stmt::For(ref f) => consumes_dangling_else(&f.body),
        ast::Stmt::ForInOf(ref f) => consumes_dangling_else(&f.body),
        ast::Stmt::Label(ref l) => consumes_dangling_else(&l.stmt),

        ast::Stmt::Var(_) |
        ast::Stmt::Empty |
        ast::Stmt::Expr(_) |
        ast::Stmt::Switch(_) |
        ast::Stmt::Continue(_) |
        ast::Stmt::Break(_) |
        ast::Stmt::Return(_) |
        ast::Stmt::Throw(_) |
        ast::Stmt::Try(_) |
        ast::Stmt::DoWhile(_) |
        ast::Stmt::Function(_) => false,
    }
}

fn is_block(s: &ast::Stmt) -> bool {
    match *s {
        ast::Stmt::Block(_) => true,
        _ => false,
    }
}

impl visit::Visit for Deblock {
    fn expr(&mut self, expr: &mut ast::Expr) {
        visit::expr_children(self, expr);
    }

    fn stmt(&mut self, stmt: &mut ast::Stmt) {
        visit::stmt_children(self, stmt);

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
    let mut deblock = Deblock {};
    for s in module.stmts.iter_mut() {
        deblock.stmt(s);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str;
    use parse::Parser;
    use gen::Writer;

    fn parse(input: &str) -> ast::Module {
        Parser::new(input.as_bytes()).module().unwrap()
    }

    fn gen(module: &ast::Module) -> String {
        let mut buf: Vec<u8> = Vec::new();
        {
            let mut w = Writer::new(&mut buf);
            w.module(module).unwrap();
        }
        String::from_utf8(buf).unwrap()
    }

    #[test]
    fn test_deblock() {
        let mut sts = parse("if (a) { return b(c) }");
        deblock(&mut sts);
        println!("{}", gen(&sts));

        let mut sts = parse("if (a) { if (b) c; } else d;");
        deblock(&mut sts);
        println!("{}", gen(&sts));
    }
}
