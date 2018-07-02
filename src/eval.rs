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

trait Visit {
    fn expr(&mut self, expr: &mut ast::Expr);
    fn stmt(&mut self, stmt: &mut ast::Stmt);
}

fn visit_func<V: Visit>(func: &mut ast::Func, v: &mut V) {
    for s in func.body.iter_mut() {
        v.stmt(s);
    }
}

fn visit_expr<V: Visit>(expr: &mut ast::Expr, v: &mut V) {
    match *expr {
        ast::Expr::EmptyParens
        | ast::Expr::This
        | ast::Expr::Ident(_)
        | ast::Expr::Null
        | ast::Expr::Undefined
        | ast::Expr::Bool(_)
        | ast::Expr::Number(_)
        | ast::Expr::String(_) => {}

        // Array(Vec<ExprNode>),
        // // The parse of "...a", which can only occur in arrow functions and
        // // in array literals.
        // Spread(Box<ExprNode>),
        // Object(Box<Object>),
        // Function(Box<Function>),
        ast::Expr::Class(ref mut c) => {
            for m in c.methods.iter_mut() {
                visit_func(&mut m.func, v);
            }
        }
        // ArrowFunction(Box<ArrowFunction>),
    // Regex(Box<Regex>),
    // Template(Box<Template>),

    // // 12.3 Left-Hand-Side Expressions
    // Index(Box<ExprNode>, Box<ExprNode>),
    // Field(Box<ExprNode>, String),
    // New(Box<ExprNode>),
        ast::Expr::Call(ref mut c) => {
            v.expr(&mut c.func.1);
            for a in c.args.iter_mut() {
                v.expr(&mut a.1);
            }
        }

        // // Various other operators.
        // Unary(UnOp, Box<ExprNode>),
        // Binary(Box<Binary>),
        // TypeOf(Box<ExprNode>),
        // Ternary(Box<Ternary>),
        ast::Expr::Assign(ref mut e1, ref mut e2) => {
            v.expr(&mut e1.1);
            v.expr(&mut e2.1);
        }
        _ => unimplemented!("{}", expr.kind()),
    }
}

fn visit_stmt<V: Visit>(stmt: &mut ast::Stmt, v: &mut V) {
    match *stmt {
        // ast::Stmt::Block(Vec<Stmt>),
        ast::Stmt::Var(ref mut decls) => {
            for d in decls.decls.iter_mut() {
                if let Some(ref mut e) = d.init {
                    v.expr(&mut e.1);
                }
            }
        }
        // ast::Stmt::Empty,
        ast::Stmt::Expr((_, ref mut e)) => v.expr(e),
        // ast::Stmt::If(Box<If>),
        // ast::Stmt::While(Box<While>),
        // ast::Stmt::DoWhile(Box<While>),
        // ast::Stmt::For(Box<For>),
        // ast::Stmt::ForInOf(Box<ForInOf>),
        // ast::Stmt::Switch(Box<Switch>),
        // ast::Stmt::Continue(Option<String>),
        // ast::Stmt::Break(Option<String>),
        // ast::Stmt::Return(Option<Box<Expr>>),
        // ast::Stmt::Label(Box<Label>),
        // ast::Stmt::Throw(Box<Expr>),
        // ast::Stmt::Try(Box<Try>),
        ast::Stmt::Function(ref mut f) => {
            visit_func(&mut f.func, v);
        }
        // ast::Stmt::Class(Box<Class>),
        _ => unimplemented!("{}", stmt.kind()),
    }
}

fn visit_module<V: Visit>(module: &mut ast::Module, v: &mut V) {
    for stmt in module.stmts.iter_mut() {
        v.stmt(stmt);
    }
}

struct Eval {}
impl Visit for Eval {
    fn expr(&mut self, expr: &mut ast::Expr) {
        match *expr {
            ast::Expr::Ident(ref mut sym) => {
                sym.borrow_mut().read = true;
            }
            ast::Expr::Assign(ref mut e1, ref mut e2) => {
                match e1.1 {
                    ast::Expr::Ident(ref mut sym) => {
                        sym.borrow_mut().write = true;
                    }
                    _ => visit_expr(&mut e1.1, self),
                }
                visit_expr(&mut e2.1, self);
            }
            _ => visit_expr(expr, self),
        }
    }

    fn stmt(&mut self, stmt: &mut ast::Stmt) {
        match *stmt {
            ast::Stmt::Var(ref mut decls) => {
                for decl in decls.decls.iter_mut() {
                    match decl.pattern {
                        ast::BindingPattern::Name(ref mut name) => {
                            //println!("decl {:?}", name);
                        }
                        _ => unimplemented!(),
                    }
                    if let Some((_, ref mut init)) = decl.init {
                        self.expr(init);
                    }
                }
            }
            _ => {
                visit_stmt(stmt, self);
            }
        }
    }
}

fn is_dead(s: &ast::RefSym) -> bool {
    !s.borrow().read
}

struct Dead {}
impl Visit for Dead {
    fn expr(&mut self, expr: &mut ast::Expr) {
        let new = match *expr {
            ast::Expr::Assign(ref mut e1, ref mut e2) => {
                match e1.1 {
                    ast::Expr::Ident(ref mut sym) => {
                        if is_dead(sym) {
                            let mut new = Box::new((ast::Span::new(0,0), ast::Expr::Null));
                            std::mem::swap(&mut new, e2);
                            Some(new.1)
                        } else {
                            None
                        }
                    }
                    _ => None
                }
            }
            _ => None
        };
        if let Some(new) = new {
            println!("dead: {}", expr.kind());
            *expr = new;
        } else {
            visit_expr(expr, self);
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
            visit_stmt(stmt, self);
        }
    }
}

pub fn eval(module: &mut ast::Module) {
    let mut e = Eval {};
    visit_module(module, &mut e);
    let mut d = Dead {};
    visit_module(module, &mut d);
}
