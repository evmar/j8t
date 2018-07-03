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

        // Array(Vec<ExprNode>),
        // // The parse of "...a", which can only occur in arrow functions and
        // // in array literals.
        // Spread(Box<ExprNode>),
        // Object(Box<Object>),
        // Function(Box<Function>),
        ast::Expr::Class(ref mut c) => {
            for m in c.methods.iter_mut() {
                func(&mut m.func, v);
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
            v.expr(&mut c.func);
            for a in c.args.iter_mut() {
                v.expr(a);
            }
        }

        // // Various other operators.
        // Unary(UnOp, Box<ExprNode>),
        // Binary(Box<Binary>),
        // TypeOf(Box<ExprNode>),
        // Ternary(Box<Ternary>),
        ast::Expr::Assign(ref mut e1, ref mut e2) => {
            v.expr(e1);
            v.expr(e2);
        }
        _ => unimplemented!("{}", en.expr.kind()),
    }
}

pub fn stmt<V: Visit>(stmt: &mut ast::Stmt, v: &mut V) {
    match *stmt {
        ast::Stmt::Block(ref mut stmts) => {
            for s in stmts.iter_mut() {
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
        // ast::Stmt::While(Box<While>),
        // ast::Stmt::DoWhile(Box<While>),
        // ast::Stmt::For(Box<For>),
        // ast::Stmt::ForInOf(Box<ForInOf>),
        // ast::Stmt::Switch(Box<Switch>),
        // ast::Stmt::Continue(Option<String>),
        // ast::Stmt::Break(Option<String>),
        ast::Stmt::Return(ref mut e) => {
            if let Some(en) = e {
                v.expr(en);
            }
        }
        // ast::Stmt::Label(Box<Label>),
        // ast::Stmt::Throw(Box<Expr>),
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
        _ => unimplemented!("{}", stmt.kind()),
    }
}

pub fn module<V: Visit>(module: &mut ast::Module, v: &mut V) {
    for stmt in module.stmts.iter_mut() {
        v.stmt(stmt);
    }
}

/*
pub fn expr_expr<F: FnMut(&mut ast::ExprNode)>(en: &mut ast::ExprNode, mut f: F) {
    match en.expr {
        ast::Expr::EmptyParens => unreachable!(),
        ast::Expr::This
        | ast::Expr::Ident(_)
        | ast::Expr::Null
        | ast::Expr::Undefined
        | ast::Expr::Bool(_)
        | ast::Expr::Number(_)
        | ast::Expr::String(_)
        | ast::Expr::Regex(_) => {}
        ast::Expr::Template(_) => unimplemented!(),

        ast::Expr::Array(ref mut es) => for e in es.iter_mut() {
            f(e);
        },
        ast::Expr::Spread(ref mut e) => f(e),
        ast::Expr::Object(ref mut obj) => {
            obj.props.iter_mut().for_each(|p| f(&mut p.value));
        }
        ast::Expr::Function(_) | ast::Expr::ArrowFunction(_) => {
            panic!("caller must handle functions")
        }
        ast::Expr::Class(_) => panic!("caller must handle class"),
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

pub fn stmt_expr_forinit<F: FnMut(&mut ast::ExprNode)>(init: &mut ast::ForInit, f: &mut F) {
    match *init {
        ast::ForInit::Empty => {}
        ast::ForInit::Expr(ref mut e) => f(e),
        ast::ForInit::Decls(ref mut decls) => for decl in decls.decls.iter_mut() {
            if let Some(ref mut en) = decl.init {
                f(en);
            }
        },
    }
}

pub fn stmt_expr<F: FnMut(&mut ast::ExprNode)>(stmt: &mut ast::Stmt, mut f: F) {
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
            f(&mut for_.expr);
        }
        ast::Stmt::Switch(ref mut sw) => f(&mut sw.expr),
        ast::Stmt::Expr(ref mut e) => f(e),
        ast::Stmt::Return(ref mut e) => {
            if let Some(ref mut e) = *e {
                f(e);
            }
        }
        ast::Stmt::Throw(ref mut e) => f(e),

        ast::Stmt::Var(ref mut decls) => for decl in decls.decls.iter_mut() {
            if let Some(ref mut en) = decl.init {
                f(en);
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
        ast::Stmt::Function(ref mut fun) => for s in fun.func.body.iter_mut() {
            f(s);
        },
        ast::Stmt::Class(ref mut class) => for m in class.methods.iter_mut() {
            for s in m.func.body.iter_mut() {
                f(s);
            }
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
*/
