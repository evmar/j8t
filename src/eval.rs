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

trait Visit {
    fn expr(&mut self, expr: &ast::Expr);
    fn stmt(&mut self, stmt: &ast::Stmt);
}

fn visit_func<V: Visit>(func: &ast::Func, v: &mut V) {
    for s in func.body.iter() {
        v.stmt(s);
    }
}

fn visit_expr<V: Visit>(expr: &ast::Expr, v: &mut V) {
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
        ast::Expr::Class(ref c) => {
            for m in c.methods.iter() {
                visit_func(&m.func, v);
            }
        }
        // ArrowFunction(Box<ArrowFunction>),
    // Regex(Box<Regex>),
    // Template(Box<Template>),

    // // 12.3 Left-Hand-Side Expressions
    // Index(Box<ExprNode>, Box<ExprNode>),
    // Field(Box<ExprNode>, String),
    // New(Box<ExprNode>),
        ast::Expr::Call(ref c) => {
            v.expr(&c.func.1);
            for a in c.args.iter() {
                v.expr(&a.1);
            }
        }

        // // Various other operators.
        // Unary(UnOp, Box<ExprNode>),
        // Binary(Box<Binary>),
        // TypeOf(Box<ExprNode>),
        // Ternary(Box<Ternary>),
        ast::Expr::Assign(ref e1, ref e2) => {
            v.expr(&e1.1);
            v.expr(&e2.1);
        }
        _ => unimplemented!("{}", expr.kind()),
    }
}
fn visit_stmt<V: Visit>(stmt: &ast::Stmt, v: &mut V) {
    match *stmt {
        // ast::Stmt::Block(Vec<Stmt>),
        // ast::Stmt::Var(Box<VarDecls>),
        // ast::Stmt::Empty,
        ast::Stmt::Expr((_, ref e)) => v.expr(e),
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
        ast::Stmt::Function(ref f) => {
            visit_func(&f.func, v);
        }
        // ast::Stmt::Class(Box<Class>),
        _ => unimplemented!("{}", stmt.kind()),
    }
}

struct Eval {}
impl Visit for Eval {
    fn expr(&mut self, expr: &ast::Expr) {
        visit_expr(expr, self);
    }

    fn stmt(&mut self, stmt: &ast::Stmt) {
        match *stmt {
            ast::Stmt::Var(ref decls) => {
                for decl in decls.decls.iter() {
                    match decl.pattern {
                        ast::BindingPattern::Name(ref name) => println!("decl {:?}", name),
                        _ => unimplemented!(),
                    }
                    if let Some((_, ref init)) = decl.init {
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

impl Eval {
    fn module(&mut self, module: &ast::Module) {
        for stmt in module.stmts.iter() {
            self.stmt(stmt);
        }
    }
}

pub fn eval(module: &ast::Module) {
    let mut e = Eval {};
    e.module(module);
}
