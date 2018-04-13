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
use parse::Parser;
use trans::visit;

const EXTERNS: &'static str = r#"
var Array;
var ArrayBuffer;
var Boolean;
var Date;
var Document;
var DocumentFragment;
var Element;
var Error;
var Event;
var Function;
var HTMLBodyElement;
var HTMLElement;
var HTMLFrameElement;
var HTMLFrameSetElement;
var HTMLIFrameElement;
var HTMLMediaElement;
var Hammer;
var IDBCursor;
var IDBDatabase;
var IDBOpenDBRequest;
var IDBRequest;
var IDBTransaction;
var Infinity;
var Intl;
var JSON;
var Map;
var Math;
var MessageChannel;
var NaN;
var Node;
var Number;
var Object;
var Promise;
var Proxy;
var RegExp;
var Set;
var String;
var Symbol;
var TypeError;
var Uint8Array;
var WeakMap;
var XMLHttpRequest;
var Zone;
var console;
var decodeURIComponent;
var document;
var encodeURIComponent;
var eval;
var exports;
var getComputedStyle;
var global;
var isFinite;
var isNaN;
var module;
var parseFloat;
var parseInt;
var setImmediate;
var setTimeout;
var window;
"#;

fn load_externs() -> ast::Scope {
    let mut scope = ast::Scope::new();
    let mut p = Parser::new(EXTERNS.as_bytes());
    let module = p.module().unwrap();
    for s in module.stmts {
        match s {
            ast::Stmt::Var(decls) => for d in decls.decls {
                match d.pattern {
                    ast::BindingPattern::Name(sym) => {
                        scope.bindings.push(sym);
                    }
                    _ => panic!("bad externs"),
                }
            },
            _ => panic!("bad externs"),
        }
    }
    return scope;
}

fn decl_names(decls: &ast::VarDecls, scope: &mut ast::Scope) {
    for decl in decls.decls.iter() {
        match decl.pattern {
            ast::BindingPattern::Name(ref sym) => {
                scope.bindings.push(sym.clone());
            }
            _ => panic!("vardecl {:?}", decl),
        }
    }
}

fn var_declared_names(stmt: &ast::Stmt, scope: &mut ast::Scope) {
    // Follows the definition of VarDeclaredNames in the spec.
    match *stmt {
        ast::Stmt::Block(ref stmts) => for s in stmts {
            var_declared_names(s, scope);
        },
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
                ast::ForInit::Empty | ast::ForInit::Expr(_) => {}
                ast::ForInit::Decls(ref decls) => {
                    decl_names(decls, scope);
                }
            }
            var_declared_names(&for_.body, scope);
        }
        ast::Stmt::ForInOf(ref forinof) => {
            if forinof.decl_type.is_some() {
                match forinof.loop_var {
                    ast::BindingPattern::Name(ref sym) => {
                        scope.bindings.push(sym.clone());
                    }
                    _ => unimplemented!("forinof"),
                }
            }
            var_declared_names(&forinof.body, scope);
        }
        ast::Stmt::Switch(ref sw) => for case in sw.cases.iter() {
            for stmt in case.stmts.iter() {
                var_declared_names(&stmt, scope);
            }
        },
        ast::Stmt::Try(ref try) => {
            var_declared_names(&try.block, scope);
            if let Some((ref pattern, ref catch)) = try.catch {
                // TODO: not part of the spec, how does decl get in scope?
                match *pattern {
                    ast::BindingPattern::Name(ref sym) => {
                        scope.bindings.push(sym.clone());
                    }
                    _ => unimplemented!("binding pattern"),
                }
                var_declared_names(catch, scope);
            }
            if let Some(ref finally) = try.finally {
                var_declared_names(finally, scope);
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
        ast::Stmt::Class(ref class) => {
            // TODO: class bindings.
        }
        ast::Stmt::Empty
        | ast::Stmt::Expr(_)
        | ast::Stmt::Continue(_)
        | ast::Stmt::Break(_)
        | ast::Stmt::Return(_)
        | ast::Stmt::Throw(_) => {}
    }
}

fn collect_scope(stmts: &[ast::Stmt], scope: &mut ast::Scope) {
    for s in stmts.iter() {
        var_declared_names(s, scope);
    }
}

const NAME_GEN_ALPHABET: &'static [u8] = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$";

struct NameGen {
    i: usize,
}

impl NameGen {
    fn new() -> NameGen {
        NameGen { i: 0 }
    }
    fn clone(&self) -> NameGen {
        NameGen { i: self.i }
    }
    fn new_name(&mut self) -> String {
        let mut i = self.i;
        self.i += 1;
        let mut name: String = String::new();
        name.push(NAME_GEN_ALPHABET[i % NAME_GEN_ALPHABET.len()] as char);
        i /= NAME_GEN_ALPHABET.len();
        let ext_len = NAME_GEN_ALPHABET.len() + 10;
        while i > 0 {
            let ci = i % ext_len;
            i /= ext_len;
            name.push(if ci < NAME_GEN_ALPHABET.len() {
                NAME_GEN_ALPHABET[ci]
            } else {
                b"01234567890"[ci - NAME_GEN_ALPHABET.len()]
            } as char);
        }
        return name;
    }
}

struct Env<'p> {
    scope: ast::Scope,
    parent: Option<&'p Env<'p>>,
    name_gen: NameGen,
}

impl<'p> Env<'p> {
    fn resolve<'a, 'b>(&'a self, sym: &Rc<ast::Symbol>) -> Option<Rc<ast::Symbol>> {
        let mut s: &Env<'p> = self;
        loop {
            if let Some(sym) = s.scope.resolve(sym) {
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
            name_gen: self.name_gen.clone(),
        }
    }

    fn rename(&mut self, debug: bool) {
        for (i, s) in self.scope.bindings.iter_mut().enumerate() {
            let new_name = if debug {
                format!("{}{}", s.name.borrow(), i)
            } else {
                self.name_gen.new_name()
            };
            *s.name.borrow_mut() = new_name;
        }
    }
}

struct Visit<'a> {
    all_syms: Vec<Rc<ast::Symbol>>,
    globals: &'a mut ast::Scope,
    debug_rename: bool,
}

impl<'a> Visit<'a> {
    fn function<'e>(&mut self, env: &Env<'e>, func: &mut ast::Function, expr: bool) {
        let mut env = env.new_scope();
        if let Some(ref name) = func.name {
            // The function name is itself in scope within the function,
            // for cases like:
            //   let x = (function foo() { ... foo(); });
            // See note 2 in 14.1.21.
            if expr {
                env.scope.bindings.push(name.clone());
            }
        }
        let mut args = ast::Symbol::new("arguments");
        Rc::get_mut(&mut args).unwrap().renameable = false;
        env.scope.bindings.push(args);
        for param in func.params.iter() {
            match *param {
                (ast::BindingPattern::Name(ref name), _) => {
                    env.scope.bindings.push(name.clone());
                }
                _ => unimplemented!(),
            }
        }
        collect_scope(&mut func.body, &mut env.scope);
        for s in func.body.iter_mut() {
            self.stmt(&env, s);
        }
        for s in env.scope.bindings.iter() {
            if !s.renameable {
                continue;
            }
            self.all_syms.push(s.clone());
        }
        env.rename(self.debug_rename);
        func.scope = env.scope;
    }

    fn resolve<'e>(
        &mut self,
        env: &Env<'e>,
        sym: &mut Rc<ast::Symbol>,
        create_global: bool,
    ) -> bool {
        if let Some(new) = env.resolve(&sym) {
            *sym = new;
            return true;
        }
        if let Some(new) = self.globals.resolve(&sym) {
            *sym = new;
            return true;
        }
        if create_global {
            eprintln!("inferred global: {}", sym.name.borrow());
            let mut new = ast::Symbol::new(sym.name.borrow().clone());
            Rc::get_mut(&mut new).unwrap().renameable = false;
            self.globals.bindings.push(new.clone());
            *sym = new;
            return true;
        }
        return false;
    }

    fn expr<'e>(&mut self, env: &Env<'e>, span: &mut ast::Span, expr: &mut ast::Expr) {
        match *expr {
            ast::Expr::Ident(ref mut sym) => {
                if !self.resolve(env, sym, false) {
                    panic!("could not resolve {:?} {:?}", sym.name.borrow(), span);
                }
                return;
            }
            ast::Expr::Function(ref mut func) => {
                self.function(env, func, /* expression */ true);
                return;
            }
            ast::Expr::TypeOf(ref mut expr) => {
                // Look for e.g.
                //   typeof exports
                // which may refer to a global.
                if let ast::Expr::Ident(ref mut sym) = expr.1 {
                    self.resolve(env, sym, true);
                    return;
                }
            }
            _ => {}
        }
        visit::expr_expr(expr, |&mut (ref mut s, ref mut e)| self.expr(env, s, e));
    }

    fn stmt<'e>(&mut self, env: &Env<'e>, stmt: &mut ast::Stmt) {
        match *stmt {
            ast::Stmt::Function(ref mut func) => {
                self.function(env, func, /* expression */ false);
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

pub fn scope(module: &mut ast::Module, debug: bool) {
    let mut externs = load_externs();
    let mut visit = Visit {
        all_syms: vec![],
        globals: &mut externs,
        debug_rename: debug,
    };
    let mut env = Env {
        scope: ast::Scope::new(),
        parent: None,
        name_gen: NameGen::new(),
    };

    collect_scope(&mut module.stmts, &mut env.scope);
    for s in module.stmts.iter_mut() {
        visit.stmt(&env, s);
    }
    module.scope = env.scope;
}
