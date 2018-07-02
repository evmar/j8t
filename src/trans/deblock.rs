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
use trans::visit;

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

fn deblock_expr(en: &mut ast::ExprNode) {
    match en.expr {
        ast::Expr::Function(ref mut func) => {
            for s in func.func.body.iter_mut() {
                deblock_stmt(s, /* parent is try */ false);
            }
        }
        _ => visit::expr_expr(en, |e| deblock_expr(e)),
    }
}

fn deblock_stmt(stmt: &mut ast::Stmt, parent_is_try: bool) {
    // A try-catch statement syntactically must have braces.
    // Special-case it here.
    // TODO: one idea is to have Stmt::Block for syntactic blocks,
    // and Stmt::List or whatever for cases where it's a list of statements
    // that are not semantically a block. This might also help with handling
    // lexical scope (where blocks might handle differently than braced syntax).
    let is_try = if let ast::Stmt::Try(_) = *stmt {
        true
    } else {
        false
    };

    visit::stmt_expr(stmt, deblock_expr);
    visit::stmt_stmt(stmt, |s| deblock_stmt(s, is_try));

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
            if stmts.len() != 1 || parent_is_try {
                return;
            }
            stmts.pop().unwrap()
        }
        _ => return,
    }
}

pub fn deblock(module: &mut ast::Module) {
    for s in module.stmts.iter_mut() {
        deblock_stmt(s, /* parent is try */ false);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use gen;
    use parse;
    use std::str;

    fn deblock_to_string(input: &str) -> String {
        let mut module = parse::Parser::new(input.as_bytes()).module().unwrap();
        deblock(&mut module);
        let mut buf: Vec<u8> = Vec::new();
        {
            let mut w = gen::Writer::new(&mut buf);
            w.module(&module).unwrap();
        }
        String::from_utf8(buf).unwrap()
    }

    #[test]
    fn deblock_if() {
        assert_eq!(
            deblock_to_string("if (a) { return b(c) }"),
            "if(a)return b(c)"
        );

        assert_eq!(
            deblock_to_string("if (a) { if (b) c; } else d;"),
            "if(a){if(b)c}else d"
        );
    }

    #[test]
    fn try() {
        assert_eq!(
            deblock_to_string("try { x; } catch (e) { y; }"),
            "try{x}catch(e){y}"
        );
    }

    #[test]
    fn nested() {
        assert_eq!(
            deblock_to_string("if (a) { if (b) { c; } else if (d) { e } } else if (g) { h }"),
            "if(a){if(b)c;else if(d)e}else if(g)h"
        );
    }

    #[test]
    fn class() {
        assert_eq!(
            deblock_to_string("class C { f() { if (a) { b; } } }"),
            "class C{f(){if(a)b}}"
        );
    }
}
