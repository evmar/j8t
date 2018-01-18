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

use std::rc::Rc;
use ast;
use trans::visit;

fn decl_names(decls: &ast::VarDecls, scope: &mut ast::Scope) {
    for decl in decls.decls.iter() {
        match decl.name {
            ast::Expr::Ident(ref sym) => {
                scope.bindings.push(sym.clone());
            }
            _ => panic!("vardecl {:?}", decl),
        }
    }
}

fn var_declared_names(stmt: &ast::Stmt, scope: &mut ast::Scope) {
    // Follows the definition of VarDeclaredNames in the spec.
    match *stmt {
        ast::Stmt::Block(ref stmts) => {
            for s in stmts {
                var_declared_names(s, scope);
            }
        }
        ast::Stmt::Var(ref decls) => {
            decl_names(decls, scope);
        }
        ast::Stmt::If(ref if_) => {
            var_declared_names(&if_.iftrue, scope);
            if let Some(ref else_) = if_.else_ {
                var_declared_names(else_, scope);
            }
        }
        ast::Stmt::While(ref while_) => {
            var_declared_names(&while_.body, scope);
        }
        ast::Stmt::DoWhile(ref do_) => {
            var_declared_names(&do_.body, scope);
        }
        ast::Stmt::For(ref for_) => {
            match for_.init {
                ast::ForInit::Empty |
                ast::ForInit::Expr(_) => {}
                ast::ForInit::Decls(ref decls) => {
                    decl_names(decls, scope);
                }
            }
            var_declared_names(&for_.body, scope);
        }
        ast::Stmt::ForInOf(ref forinof) => {
            match forinof.init {
                ast::ForInit::Empty |
                ast::ForInit::Expr(_) => {}
                ast::ForInit::Decls(ref decls) => {
                    decl_names(decls, scope);
                }
            }
            var_declared_names(&forinof.body, scope);
        }
        ast::Stmt::Switch(ref sw) => {
            for case in sw.cases.iter() {
                for stmt in case.stmts.iter() {
                    var_declared_names(&stmt, scope);
                }
            }
        }
        ast::Stmt::Try(ref try) => {
            var_declared_names(&try.block, scope);
            if let Some((ref param, ref catch)) = try.catch {
                // TODO: not part of the spec, how does param get in scope?
                if let ast::Expr::Ident(ref sym) = *param {
                    scope.bindings.push(sym.clone());
                } else {
                    unimplemented!("binding pattern");
                }
                var_declared_names(catch, scope);
            }
        }
        ast::Stmt::Label(ref label) => {
            var_declared_names(&label.stmt, scope);
        }
        ast::Stmt::Function(ref func) => {
            // TODO: this is not part of the spec, how do functions get hoisted?
            if let Some(ref name) = func.name {
                scope.bindings.push(name.clone());
            }
        }
        ast::Stmt::Empty | ast::Stmt::Expr(_) |
        ast::Stmt::Continue(_) | ast::Stmt::Break(_) |
        ast::Stmt::Return(_) | ast::Stmt::Throw(_) => {}
    }
}

fn collect_scope(stmts: &[ast::Stmt], scope: &mut ast::Scope) {
    for s in stmts.iter() {
        var_declared_names(s, scope);
    }
}

struct Env<'a> {
    scope: ast::Scope,
    parent: Option<&'a Env<'a>>,
}

impl<'a> Env<'a> {
    fn resolve(&self, name: &str) -> Option<Rc<ast::Symbol>> {
        let mut s: &Env<'a> = self;
        loop {
            if let Some(sym) = s.scope.resolve(name) {
                return Some(sym);
            }
            if let Some(parent) = s.parent {
                s = parent;
            } else {
                return None;
            }
        }
    }

    // Note tricky type here! The new scope's type param is a
    // lifetime that self outlives.
    fn new_scope<'b, 's: 'b>(&'s self) -> Env<'b> {
        Env {
            scope: ast::Scope::new(),
            parent: Some(self),
        }
    }
}

struct Visit {
    all_syms: Vec<Rc<ast::Symbol>>,
}

impl Visit {
    fn function<'a>(&mut self, env: &Env<'a>, func: &mut ast::Function, expr: bool) {
        let mut visit = env.new_scope();
        if let Some(ref name) = func.name {
            // The function name is itself in scope within the function,
            // for cases like:
            //   let x = (function foo() { ... foo(); });
            // See note 2 in 14.1.21.
            if expr {
                visit.scope.bindings.push(name.clone());
            }
        }
        let mut args = ast::Symbol::new("arguments");
        Rc::get_mut(&mut args).unwrap().renameable = false;
        visit.scope.bindings.push(args);
        for p in func.params.iter() {
            visit.scope.bindings.push(p.clone());
        }
        collect_scope(&mut func.body, &mut visit.scope);
        for s in func.body.iter_mut() {
            self.stmt(&visit, s);
        }
        for s in visit.scope.bindings.iter() {
            if !s.renameable {
                continue;
            }
            self.all_syms.push(s.clone());
        }
        func.scope = visit.scope;
    }

    fn expr<'a>(&mut self, env: &Env<'a>, span: &mut ast::Span, expr: &mut ast::Expr) {
        match *expr {
            ast::Expr::Ident(ref mut sym) => {
                let resolved = env.resolve(&*sym.name.borrow());
                if let Some(new) = resolved {
                    *sym = new;
                } else {
                    panic!("could not resolve {:?} {:?}", sym.name.borrow(), span);
                }
            }
            ast::Expr::Function(ref mut func) => {
                self.function(
                    env,
                    func,
                    /* expression */
                    true,
                );
            }
            // ast::Expr::TypeOf(ref mut expr) => {
            //     // Look for e.g.
            //     //   typeof exports
            //     // which may refer to a global.
            //     match **expr {
            //         ast::Expr::Ident(ref mut sym) => {
            //             *sym = self.resolve(&sym.name, /* infer_global */ true);
            //         }
            //         _ => self.expr(expr)
            //     }
            // }
            _ => {
                visit::expr_expr(expr, |&mut (ref mut s, ref mut e)| self.expr(env, s, e));
            }
        }
    }

    fn stmt<'a>(&mut self, env: &Env<'a>, stmt: &mut ast::Stmt) {
        match *stmt {
            ast::Stmt::Function(ref mut func) => {
                self.function(
                    env,
                    func,
                    /* expression */
                    false,
                );
            }
            _ => {
                visit::stmt_expr(stmt, |e| {
                    let mut s = ast::Span::new(0, 0);
                    self.expr(env, &mut s, e);
                });
                visit::stmt_stmt(stmt, |s| self.stmt(env, s));
            }
        }
    }
}

pub fn scope(externs: &ast::Scope, module: &mut ast::Module) {
    let mut globals = Visit { all_syms: vec![] };
    let externs = ast::Scope { bindings: externs.bindings.clone() };
    let gv = Env {
        scope: externs,
        parent: None,
    };

    let mut v = gv.new_scope();
    collect_scope(&mut module.stmts, &mut v.scope);
    for s in module.stmts.iter_mut() {
        globals.stmt(&v, s);
    }
    module.scope = v.scope;

    for (i, s) in globals.all_syms.iter_mut().enumerate() {
        let new_name = format!("{}{}", s.name.borrow(), i);
        // let new_name = format!("s{}", i);
        *s.name.borrow_mut() = new_name;
    }
}
