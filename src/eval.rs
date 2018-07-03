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
use trans::visit;

struct Eval {}

impl visit::Visit for Eval {
    fn expr(&mut self, en: &mut ast::ExprNode) {
        match en.expr {
            ast::Expr::Ident(ref mut sym) => {
                sym.borrow_mut().read = true;
            }
            ast::Expr::Assign(ref mut e1, ref mut e2) => {
                match e1.expr {
                    ast::Expr::Ident(ref mut sym) => {
                        sym.borrow_mut().write = true;
                    }
                    _ => visit::expr(e1, self),
                }
                visit::expr(e2, self);
            }
            _ => visit::expr(en, self),
        }
    }

    fn stmt(&mut self, stmt: &mut ast::Stmt) {
        match *stmt {
            ast::Stmt::Var(ref mut decls) => {
                for decl in decls.decls.iter_mut() {
                    match decl.pattern {
                        ast::BindingPattern::Name(ref mut name) => {
                            println!("decl {:?}", name);
                        }
                        _ => unimplemented!(),
                    }
                    if let Some(ref mut en) = decl.init {
                        self.expr(en);
                    }
                }
            }
            _ => {
                visit::stmt(stmt, self);
            }
        }
    }
}

fn is_dead(s: &ast::RefSym) -> bool {
    !s.borrow().read
}

struct Dead {}
impl visit::Visit for Dead {
    fn expr(&mut self, en: &mut ast::ExprNode) {
        let new = match en.expr {
            ast::Expr::Assign(ref mut e1, ref mut e2) => {
                match e1.expr {
                    ast::Expr::Ident(ref mut sym) => {
                        if is_dead(sym) {
                            // x = expr where x is unused; replace with expr.
                            let mut new =
                                Box::new(ast::ExprNode::new(ast::Span::new(0, 0), ast::Expr::Null));
                            std::mem::swap(&mut new, e2);
                            Some(new)
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            _ => None,
        };
        if let Some(new) = new {
            println!("dead: {}", en.expr.kind());
            *en = *new;
        } else {
            visit::expr(en, self);
        }
    }
    fn stmt(&mut self, stmt: &mut ast::Stmt) {
        let new = match *stmt {
            ast::Stmt::Function(ref f) => {
                if let Some(ref name) = f.name {
                    //println!("{:?}", f.name);
                    if is_dead(name) {
                        Some(ast::Stmt::Empty)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        };
        if let Some(new) = new {
            println!("dead: {}", stmt.kind());
            *stmt = new;
        } else {
            visit::stmt(stmt, self);
        }
    }
}

pub fn eval(module: &mut ast::Module) {
    let mut e = Eval {};
    visit::module(module, &mut e);
    let mut d = Dead {};
    visit::module(module, &mut d);
}
