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

pub trait Visit {
    fn expr(&mut self, expr: &mut ast::ExprNode);
    fn stmt(&mut self, stmt: &mut ast::Stmt);
}

pub fn func<V: Visit>(func: &mut ast::Func, v: &mut V) {
    for s in func.body.iter_mut() {
        v.stmt(s);
    }
}

pub fn expr<V: Visit>(en: &mut ast::ExprNode, v: &mut V) {
    match en.expr {
        ast::Expr::EmptyParens
        | ast::Expr::This
        | ast::Expr::Ident(_)
        | ast::Expr::Null
        | ast::Expr::Undefined
        | ast::Expr::Bool(_)
        | ast::Expr::Number(_)
        | ast::Expr::String(_) => {}

        ast::Expr::Array(ref mut es) => {
            for e in es.iter_mut() {
                v.expr(e);
            }
        }
        // The parse of "...a", which can only occur in arrow functions and
        // in array literals.
        ast::Expr::Spread(ref mut expr) => v.expr(expr),
        ast::Expr::Object(ref mut obj) => {
            for prop in obj.props.iter_mut() {
                v.expr(&mut prop.value);
            }
        }
        ast::Expr::Function(ref mut fun) => func(&mut fun.func, v),
        ast::Expr::Class(ref mut c) => {
            for m in c.methods.iter_mut() {
                func(&mut m.func, v);
            }
        }
        ast::Expr::ArrowFunction(ref mut fun) => match fun.body {
            ast::ArrowBody::Expr(ref mut en) => v.expr(en),
            ast::ArrowBody::Stmts(ref mut sts) => {
                for s in sts.iter_mut() {
                    v.stmt(s);
                }
            }
        },
        ast::Expr::Regex(_) => {}
        ast::Expr::Template(ref _literal) => {}

        // 12.3 Left-Hand-Side Expressions
        ast::Expr::Index(ref mut e1, ref mut e2) => {
            v.expr(e1);
            v.expr(e2);
        }
        ast::Expr::Field(ref mut e, _) => v.expr(e),
        ast::Expr::New(ref mut e) => v.expr(e),
        ast::Expr::Call(ref mut c) => {
            v.expr(&mut c.func);
            for a in c.args.iter_mut() {
                v.expr(a);
            }
        }

        // Various other operators.
        ast::Expr::Unary(_, ref mut e) => v.expr(e),
        ast::Expr::Binary(ref mut bin) => {
            v.expr(&mut bin.lhs);
            v.expr(&mut bin.rhs);
        }
        ast::Expr::TypeOf(ref mut e) => {
            v.expr(e);
        }
        ast::Expr::Ternary(ref mut t) => {
            v.expr(&mut t.condition);
            v.expr(&mut t.iftrue);
            v.expr(&mut t.iffalse);
        }
        ast::Expr::Assign(ref mut e1, ref mut e2) => {
            v.expr(e1);
            v.expr(e2);
        }
    }
}

pub fn stmt<V: Visit>(stmt: &mut ast::Stmt, v: &mut V) {
    match *stmt {
        ast::Stmt::Block(ref mut block) => {
            for s in block.stmts.iter_mut() {
                v.stmt(s);
            }
        }
        ast::Stmt::Var(ref mut decls) => {
            for d in decls.decls.iter_mut() {
                if let Some(ref mut e) = d.init {
                    v.expr(e);
                }
            }
        }
        ast::Stmt::Empty => {}
        ast::Stmt::Expr(ref mut e) => v.expr(e),
        ast::Stmt::If(ref mut i) => {
            v.expr(&mut i.cond);
            v.stmt(&mut i.iftrue);
            if let Some(ref mut else_) = i.else_ {
                v.stmt(else_);
            }
        }
        ast::Stmt::While(ref mut w) => {
            v.expr(&mut w.cond);
            v.stmt(&mut w.body);
        }
        ast::Stmt::DoWhile(ref mut w) => {
            v.stmt(&mut w.body);
            v.expr(&mut w.cond);
        }
        ast::Stmt::For(ref mut for_) => {
            match for_.init {
                ast::ForInit::Empty => {}
                ast::ForInit::Expr(ref mut e) => v.expr(e),
                ast::ForInit::Decls(ref mut decls) => {
                    for decl in decls.decls.iter_mut() {
                        if let Some(ref mut en) = decl.init {
                            v.expr(en);
                        }
                    }
                }
            };
            if let Some(ref mut c) = for_.cond {
                v.expr(c);
            }
            if let Some(ref mut c) = for_.iter {
                v.expr(c);
            }
            v.stmt(&mut for_.body);
        }
        ast::Stmt::ForInOf(ref mut for_) => {
            v.expr(&mut for_.expr);
            v.stmt(&mut for_.body);
        }
        ast::Stmt::Switch(ref mut sw) => {
            v.expr(&mut sw.expr);
            for c in sw.cases.iter_mut() {
                if let Some(ref mut e) = c.expr {
                    v.expr(e);
                }
                for s in c.stmts.iter_mut() {
                    v.stmt(s);
                }
            }
        }
        ast::Stmt::Continue(_) => {}
        ast::Stmt::Break(_) => {}
        ast::Stmt::Return(ref mut e) => {
            if let Some(en) = e {
                v.expr(en);
            }
        }
        ast::Stmt::Label(ref mut l) => v.stmt(&mut l.stmt),
        ast::Stmt::Throw(ref mut e) => v.expr(e),
        ast::Stmt::Try(ref mut t) => {
            v.stmt(&mut t.block);
            if let Some((_, ref mut catch)) = t.catch {
                v.stmt(catch);
            }
            if let Some(ref mut finally) = t.finally {
                v.stmt(finally);
            }
        }
        ast::Stmt::Function(ref mut f) => {
            func(&mut f.func, v);
        }
        ast::Stmt::Class(ref mut class) => {
            for m in class.methods.iter_mut() {
                func(&mut m.func, v);
            }
        }
    }
}

pub fn module<V: Visit>(module: &mut ast::Module, v: &mut V) {
    for stmt in module.stmts.iter_mut() {
        v.stmt(stmt);
    }
}
