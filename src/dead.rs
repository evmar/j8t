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
use std;
use visit;

struct SymStatus {
    /// Symbols where we observed their value was read.
    read: std::collections::HashSet<ast::SymId>,
}

struct Eval<'a> {
    syms: &'a mut SymStatus,
}

impl<'a> visit::Visit for Eval<'a> {
    fn expr(&mut self, en: &mut ast::ExprNode) {
        match en.expr {
            ast::Expr::Ident(ref mut sym) => {
                self.syms.read.insert(sym.id);
            }
            _ => visit::expr(en, self),
        }
    }

    fn stmt(&mut self, stmt: &mut ast::Stmt) {
        visit::stmt(stmt, self);
    }
}

struct Dead<'a> {
    syms: &'a mut SymStatus,
}
impl<'a> Dead<'a> {
    fn is_dead(&self, sym: &ast::RefSym) -> bool {
        !self.syms.read.contains(&sym.id)
    }

    fn trim_expr(&self, en: &mut ast::ExprNode) -> Option<ast::ExprNode> {
        if let ast::Expr::Assign(ref mut e1, ref mut e2) = en.expr {
            if let ast::Expr::Ident(ref mut sym) = e1.expr {
                if self.is_dead(sym) {
                    // x = expr where x is unused; replace with expr.
                    let mut new = Box::new(ast::ExprNode::empty());
                    std::mem::swap(&mut new, e2);
                    return Some(*new);
                }
            }
        }
        None
    }

    fn trim_stmt(&self, stmt: &mut ast::Stmt) -> Option<ast::Stmt> {
        match *stmt {
            ast::Stmt::Function(ref f) => {
                if let Some(ref name) = f.name {
                    //println!("{:?}", f.name);
                    if self.is_dead(name) {
                        return Some(ast::Stmt::Empty);
                    }
                }
            }
            _ => {}
        };
        None
    }
}

impl<'a> visit::Visit for Dead<'a> {
    fn expr(&mut self, en: &mut ast::ExprNode) {
        if let Some(new) = self.trim_expr(en) {
            println!("dead: {}", en.expr.kind());
            *en = new;
        } else {
            visit::expr(en, self);
        }
    }

    fn stmt(&mut self, stmt: &mut ast::Stmt) {
        if let Some(new) = self.trim_stmt(stmt) {
            println!("dead: {}", stmt.kind());
            *stmt = new;
        } else {
            visit::stmt(stmt, self);
        }
    }
}

pub fn dead(module: &mut ast::Module) {
    let mut syms = SymStatus {
        read: std::collections::HashSet::new(),
    };
    {
        let mut e = Eval { syms: &mut syms };
        visit::module(module, &mut e);
    }
    {
        let mut d = Dead { syms: &mut syms };
        visit::module(module, &mut d);
    }
}
