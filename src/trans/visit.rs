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
    fn expr(&mut self, expr: &mut ast::Expr);
    fn stmt(&mut self, stmt: &mut ast::Stmt);
}

pub fn expr_expr<F: FnMut(&mut ast::Expr)>(expr: &mut ast::Expr, mut f: F) {
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
                f(&mut e.1);
            }
        }
        ast::Expr::Object(ref mut obj) => {
            obj.props.iter_mut().for_each(|p| f(&mut p.value.1));
        }
        ast::Expr::Function(_) => panic!("caller must handle functions"),
        ast::Expr::Index(ref mut e1, ref mut e2) => {
            f(&mut e1.1);
            f(&mut e2.1);
        }
        ast::Expr::Field(ref mut e, _) => f(&mut e.1),
        ast::Expr::New(ref mut e) => f(&mut e.1),
        ast::Expr::Call(ref mut c) => {
            f(&mut c.func.1);
            for e in c.args.iter_mut() {
                f(&mut e.1);
            }
        }
        ast::Expr::Unary(_, ref mut e) => f(&mut e.1),
        ast::Expr::Binary(ref mut bin) => {
            f(&mut bin.lhs.1);
            f(&mut bin.rhs.1);
        }
        ast::Expr::TypeOf(ref mut e) => {
            f(&mut e.1);
        }
        ast::Expr::Ternary(ref mut t) => {
            f(&mut t.condition.1);
            f(&mut t.iftrue.1);
            f(&mut t.iffalse.1);
        }
        ast::Expr::Assign(ref mut e1, ref mut e2) => {
            f(&mut e1.1);
            f(&mut e2.1);
        }
    }
}

pub fn stmt_expr_forinit<F: FnMut(&mut ast::Expr)>(init: &mut ast::ForInit, f: &mut F) {
    match *init {
        ast::ForInit::Empty => {}
        ast::ForInit::Expr(ref mut e) => f(e),
        ast::ForInit::Decls(ref mut decls) => {
            for d in decls.decls.iter_mut() {
                if let Some(ref mut init) = d.init {
                    f(init);
                }
            }
        }
    }
}

pub fn stmt_expr<F: FnMut(&mut ast::Expr)>(stmt: &mut ast::Stmt, mut f: F) {
    match *stmt {
        ast::Stmt::If(ref mut if_) => {
            f(&mut if_.cond);
        }
        ast::Stmt::While(ref mut w) => f(&mut w.cond),
        ast::Stmt::DoWhile(ref mut w) => f(&mut w.cond),
        ast::Stmt::For(ref mut for_) => {
            // TODO: init
            stmt_expr_forinit(&mut for_.init, &mut f);
            if let Some(ref mut c) = for_.cond {
                f(c);
            }
            if let Some(ref mut c) = for_.iter {
                f(c);
            }
        }
        ast::Stmt::ForInOf(ref mut for_) => {
            stmt_expr_forinit(&mut for_.init, &mut f);
        }
        ast::Stmt::Switch(ref mut sw) => f(&mut sw.expr),
        ast::Stmt::Expr(ref mut e) => f(&mut e.1),
        ast::Stmt::Return(ref mut e) => {
            if let Some(ref mut e) = *e {
                f(e);
            }
        }
        ast::Stmt::Throw(ref mut e) => f(e),

        ast::Stmt::Var(ref mut decls) => {
            for d in decls.decls.iter_mut() {
                if let Some(ref mut init) = d.init {
                    f(init);
                }
            }
        }

        ast::Stmt::Block(_) |
        ast::Stmt::Empty |
        ast::Stmt::Continue(_) |
        ast::Stmt::Break(_) |
        ast::Stmt::Label(_) |
        ast::Stmt::Try(_) |
        ast::Stmt::Function(_) => {}
    }
}

pub fn stmt_stmt<F: FnMut(&mut ast::Stmt)>(stmt: &mut ast::Stmt, mut f: F) {
    match *stmt {
        ast::Stmt::Block(ref mut stmts) => {
            for s in stmts.iter_mut() {
                f(s);
            }
        }
        ast::Stmt::If(ref mut if_) => {
            f(&mut if_.iftrue);
            if let Some(ref mut else_) = if_.else_ {
                f(else_);
            }
        }
        ast::Stmt::While(ref mut wh) => f(&mut wh.body),
        ast::Stmt::DoWhile(ref mut wh) => f(&mut wh.body),
        ast::Stmt::For(ref mut for_) => f(&mut for_.body),
        ast::Stmt::ForInOf(ref mut for_) => f(&mut for_.body),
        ast::Stmt::Switch(ref mut sw) => {
            for c in sw.cases.iter_mut() {
                for s in c.stmts.iter_mut() {
                    f(s);
                }
            }
        }
        ast::Stmt::Label(ref mut l) => f(&mut l.stmt),
        ast::Stmt::Try(ref mut t) => {
            f(&mut t.block);
            if let Some((_, ref mut catch)) = t.catch {
                f(catch);
            }
            if let Some(ref mut finally) = t.finally {
                f(finally);
            }
        }
        ast::Stmt::Function(ref mut fun) => {
            for s in fun.body.iter_mut() {
                f(s);
            }
        }

        ast::Stmt::Return(_) |
        ast::Stmt::Throw(_) |
        ast::Stmt::Var(_) |
        ast::Stmt::Expr(_) |
        ast::Stmt::Empty |
        ast::Stmt::Continue(_) |
        ast::Stmt::Break(_) => {}
    }
}
