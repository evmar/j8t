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

pub trait Visit {
    fn expr(&mut self, expr: &mut ast::Expr);
    fn stmt(&mut self, stmt: &mut ast::Stmt);
}

pub fn expr_children(v: &mut Visit, expr: &mut ast::Expr) {
    match *expr {
        ast::Expr::This |
        ast::Expr::Ident(_) |
        ast::Expr::Null |
        ast::Expr::Undefined |
        ast::Expr::Bool(_) |
        ast::Expr::Number(_) |
        ast::Expr::String(_) |
        ast::Expr::Regex(_) => {}

        ast::Expr::Array(ref mut es) => {
            for e in es.iter_mut() {
                v.expr(e);
            }
        }
        ast::Expr::Object(ref mut obj) => {
            obj.props.iter_mut().for_each(|p| v.expr(&mut p.value));
        }
        ast::Expr::Function(ref mut fun) => {
            for s in fun.body.iter_mut() {
                v.stmt(s);
            }
        }
        ast::Expr::Index(ref mut e1, ref mut e2) => {
            v.expr(e1);
            v.expr(e2);
        }
        ast::Expr::Field(ref mut e, _) => v.expr(e),
        ast::Expr::New(ref mut e) => v.expr(e),
        ast::Expr::Call(ref mut c) => {
            v.expr(&mut c.func);
            for e in c.args.iter_mut() {
                v.expr(e);
            }
        }
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

pub fn stmt_children(v: &mut Visit, stmt: &mut ast::Stmt) {
    match *stmt {
        ast::Stmt::Block(ref mut stmts) => {
            for s in stmts.iter_mut() {
                v.stmt(s);
            }
        }
        ast::Stmt::If(ref mut if_) => {
            v.expr(&mut if_.cond);
            v.stmt(&mut if_.iftrue);
            if let Some(ref mut else_) = if_.else_ {
                v.stmt(else_);
            }
        }
        ast::Stmt::While(ref mut wh) => v.stmt(&mut wh.body),
        ast::Stmt::DoWhile(ref mut wh) => v.stmt(&mut wh.body),
        ast::Stmt::For(ref mut for_) => v.stmt(&mut for_.body),
        ast::Stmt::ForInOf(ref mut for_) => v.stmt(&mut for_.body),
        ast::Stmt::Switch(ref mut sw) => {
            for c in sw.cases.iter_mut() {
                for s in c.stmts.iter_mut() {
                    v.stmt(s);
                }
            }
        }
        ast::Stmt::Label(ref mut l) => v.stmt(&mut l.stmt),
        ast::Stmt::Try(ref mut t) => {
            v.stmt(&mut t.block);
            if let Some((_, ref mut catch)) = t.catch {
                v.stmt(catch);
            }
            if let Some(ref mut finally) = t.finally {
                v.stmt(finally);
            }
        }
        ast::Stmt::Function(ref mut fun) => {
            for s in fun.body.iter_mut() {
                v.stmt(s);
            }
        }

        ast::Stmt::Expr(ref mut e) => v.expr(e),
        ast::Stmt::Return(ref mut e) => {
            if let Some(ref mut e) = *e {
                v.expr(e);
            }
        }
        ast::Stmt::Throw(ref mut e) => v.expr(e),

        ast::Stmt::Var(ref mut decls) => {
            for d in decls.decls.iter_mut() {
                v.expr(&mut d.name);
                if let Some(ref mut init) = d.init {
                    v.expr(init);
                }
            }
        }
        ast::Stmt::Empty |
        ast::Stmt::Continue(_) |
        ast::Stmt::Break(_) => {}
    }
}
