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

pub fn expr_expr<F: FnMut(&mut ast::ExprNode)>(expr: &mut ast::Expr, mut f: F) {
    match *expr {
        ast::Expr::EmptyParens => unreachable!(),
        ast::Expr::This
        | ast::Expr::Ident(_)
        | ast::Expr::Null
        | ast::Expr::Undefined
        | ast::Expr::Bool(_)
        | ast::Expr::Number(_)
        | ast::Expr::String(_)
        | ast::Expr::Regex(_) => {}

        ast::Expr::Array(ref mut es) => for e in es.iter_mut() {
            f(e);
        },
        ast::Expr::Object(ref mut obj) => {
            obj.props.iter_mut().for_each(|p| f(&mut p.value));
        }
        ast::Expr::Function(_) | ast::Expr::ArrowFunction(_) => {
            panic!("caller must handle functions")
        }
        ast::Expr::Class(_) => {
            panic!("caller must handle class")
        }
        ast::Expr::Index(ref mut e1, ref mut e2) => {
            f(e1);
            f(e2);
        }
        ast::Expr::Field(ref mut e, _) => f(e),
        ast::Expr::New(ref mut e) => f(e),
        ast::Expr::Call(ref mut c) => {
            f(&mut c.func);
            for e in c.args.iter_mut() {
                f(e);
            }
        }
        ast::Expr::Unary(_, ref mut e) => f(e),
        ast::Expr::Binary(ref mut bin) => {
            f(&mut bin.lhs);
            f(&mut bin.rhs);
        }
        ast::Expr::TypeOf(ref mut e) => {
            f(e);
        }
        ast::Expr::Ternary(ref mut t) => {
            f(&mut t.condition);
            f(&mut t.iftrue);
            f(&mut t.iffalse);
        }
        ast::Expr::Assign(ref mut e1, ref mut e2) => {
            f(e1);
            f(e2);
        }
    }
}

pub fn stmt_expr_forinit<F: FnMut(&mut ast::Expr)>(init: &mut ast::ForInit, f: &mut F) {
    match *init {
        ast::ForInit::Empty => {}
        ast::ForInit::Expr(ref mut e) => f(e),
        ast::ForInit::Decls(ref mut decls) => for decl in decls.decls.iter_mut() {
            if let Some((_, ref mut init)) = decl.init {
                f(init);
            }
        },
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
            f(&mut for_.expr.1);
        }
        ast::Stmt::Switch(ref mut sw) => f(&mut sw.expr),
        ast::Stmt::Expr(ref mut e) => f(&mut e.1),
        ast::Stmt::Return(ref mut e) => {
            if let Some(ref mut e) = *e {
                f(e);
            }
        }
        ast::Stmt::Throw(ref mut e) => f(e),

        ast::Stmt::Var(ref mut decls) => for decl in decls.decls.iter_mut() {
            if let Some((_, ref mut init)) = decl.init {
                f(init);
            }
        },

        ast::Stmt::Block(_)
        | ast::Stmt::Empty
        | ast::Stmt::Continue(_)
        | ast::Stmt::Break(_)
        | ast::Stmt::Label(_)
        | ast::Stmt::Try(_)
        | ast::Stmt::Function(_)
        | ast::Stmt::Class(_) => {}
    }
}

pub fn stmt_stmt<F: FnMut(&mut ast::Stmt)>(stmt: &mut ast::Stmt, mut f: F) {
    match *stmt {
        ast::Stmt::Block(ref mut stmts) => for s in stmts.iter_mut() {
            f(s);
        },
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
        ast::Stmt::Switch(ref mut sw) => for c in sw.cases.iter_mut() {
            for s in c.stmts.iter_mut() {
                f(s);
            }
        },
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
        ast::Stmt::Function(ref mut fun) => for s in fun.body.iter_mut() {
            f(s);
        },
        ast::Stmt::Class(ref mut class) => for s in class.methods.iter_mut() {
            panic!("class methods");
        },

        ast::Stmt::Return(_)
        | ast::Stmt::Throw(_)
        | ast::Stmt::Var(_)
        | ast::Stmt::Expr(_)
        | ast::Stmt::Empty
        | ast::Stmt::Continue(_)
        | ast::Stmt::Break(_) => {}
    }
}
