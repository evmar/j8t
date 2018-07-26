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
use parse::Parser;
use std::rc::Rc;
use visit;
use visit::Visit;

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

/// decl_names adds all the new names declared in a BindingPattern to a Scope.
fn pattern_declared_names(pat: &ast::BindingPattern, scope: &mut ast::Scope) {
    match *pat {
        ast::BindingPattern::Name(ref sym) => {
            scope.bindings.push(sym.clone());
        }
        ast::BindingPattern::Array(ref pat) => {
            for (ref pat, _) in pat.elems.iter() {
                pattern_declared_names(pat, scope);
            }
        }
        ast::BindingPattern::Object(ref pat) => {
            for (ref _name, ref element) in pat.props.iter() {
                let (ref pat, _) = element;
                pattern_declared_names(pat, scope);
            }
        }
    }
}

/// declared_names gathers all 'var'/let declared names in a statement
/// and adds them to a Scope.  Note that in the spec they treat VarDeclaredNames
/// separately from LexicallyDeclaredNames, and we might need to as well.
fn declared_names(stmt: &ast::Stmt, scope: &mut ast::Scope) {
    // Follows the definition of VarDeclaredNames in the spec.
    match *stmt {
        ast::Stmt::Block(ref stmts) => for s in stmts {
            declared_names(s, scope);
        },
        ast::Stmt::Var(ref decls) => {
            for decl in decls.decls.iter() {
                pattern_declared_names(&decl.pattern, scope);
            }
        }
        ast::Stmt::If(ref if_) => {
            declared_names(&if_.iftrue, scope);
            if let Some(ref else_) = if_.else_ {
                declared_names(else_, scope);
            }
        }
        ast::Stmt::While(ref while_) => {
            declared_names(&while_.body, scope);
        }
        ast::Stmt::DoWhile(ref do_) => {
            declared_names(&do_.body, scope);
        }
        ast::Stmt::For(ref for_) => {
            match for_.init {
                ast::ForInit::Empty | ast::ForInit::Expr(_) => {}
                ast::ForInit::Decls(ref decls) => {
                    for decl in decls.decls.iter() {
                        pattern_declared_names(&decl.pattern, scope);
                    }
                }
            }
            declared_names(&for_.body, scope);
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
            declared_names(&forinof.body, scope);
        }
        ast::Stmt::Switch(ref sw) => for case in sw.cases.iter() {
            for stmt in case.stmts.iter() {
                declared_names(&stmt, scope);
            }
        },
        ast::Stmt::Try(ref try) => {
            declared_names(&try.block, scope);
            if let Some((ref pattern, ref catch)) = try.catch {
                // TODO: not part of the spec, how does decl get in scope?
                // Answer: 13.15.7 Runtime Semantics: CatchClauseEvaluation,
                // it should be in its own scope.
                match *pattern {
                    ast::BindingPattern::Name(ref sym) => {
                        scope.bindings.push(sym.clone());
                    }
                    _ => unimplemented!("binding pattern"),
                }
                declared_names(catch, scope);
            }
            if let Some(ref finally) = try.finally {
                declared_names(finally, scope);
            }
        }
        ast::Stmt::Label(ref label) => {
            declared_names(&label.stmt, scope);
        }
        ast::Stmt::Function(ref func) => {
            // TODO: this is not part of the spec, how do functions get hoisted?
            if let Some(ref name) = func.name {
                scope.bindings.push(name.clone());
            }
        }
        ast::Stmt::Class(ref class) => {
            if let Some(ref name) = class.name {
                scope.bindings.push(name.clone());
            }
        }
        ast::Stmt::Empty
        | ast::Stmt::Expr(_)
        | ast::Stmt::Continue(_)
        | ast::Stmt::Break(_)
        | ast::Stmt::Return(_)
        | ast::Stmt::Throw(_) => {}
    }
}

struct Bind {
    scopes: Vec<ast::Scope>,
    warnings: Vec<String>,
}

impl Bind {
    /// Resolve a symbol against the current scopes, overwriting the symbol if found.
    fn resolve(&mut self, sym: &mut ast::RefSym, create_global: bool) {
        for scope in self.scopes.iter().rev() {
            if let Some(r) = scope.resolve(sym) {
                *sym = r;
                return;
            }
        }

        {
            let mut sym = sym.borrow_mut();
            if create_global {
                self.warnings.push(format!("inferred global: {}", sym.name));
            } else {
                self.warnings.push(format!(
                    "global referenced without declaration: {}",
                    sym.name
                ));
            }
            sym.renameable = false;
        }
        self.scopes[0].bindings.push(sym.clone());
    }

    fn function(&mut self, func: &mut ast::Function, expr: bool) {
        self.func(func.name.clone(), &mut func.func, expr);
    }

    fn func(&mut self, name: Option<ast::RefSym>, func: &mut ast::Func, expr: bool) {
        let mut scope = ast::Scope::new();
        if let Some(name) = name {
            // The function name is itself in scope within the function,
            // for cases like:
            //   let x = (function foo() { ... foo(); });
            // See note 2 in 14.1.21.
            if expr {
                scope.bindings.push(name);
            }
        }
        let args = ast::Symbol::new("arguments");
        args.borrow_mut().renameable = false;
        scope.bindings.push(args);
        for (ref pat, _) in func.params.iter() {
            pattern_declared_names(pat, &mut scope);
        }
        for s in func.body.iter_mut() {
            declared_names(s, &mut scope);
        }

        self.scopes.push(scope);
        for s in func.body.iter_mut() {
            self.stmt(s);
        }
        scope = self.scopes.pop().unwrap();

        // See if anyone used 'arguments', and if not, drop it from the scope.
        // Maybe it'd be better to leave arguments out and only create
        // it if it's needed, hm.
        let args = scope
            .bindings
            .iter()
            .position(|s| s.borrow().name == "arguments")
            .unwrap();
        if Rc::strong_count(&scope.bindings[args]) == 1 {
            scope.bindings.swap_remove(args);
        }
        func.scope = scope;
    }

    fn module(&mut self, module: &mut ast::Module) {
        let mut scope = ast::Scope::new();
        for s in module.stmts.iter_mut() {
            declared_names(s, &mut scope);
        }
        self.scopes.push(scope);
        for s in module.stmts.iter_mut() {
            self.stmt(s);
        }
        module.scope = self.scopes.pop().unwrap();
    }
}

impl visit::Visit for Bind {
    fn expr(&mut self, en: &mut ast::ExprNode) {
        match en.expr {
            ast::Expr::Ident(ref mut sym) => {
                self.resolve(sym, false);
                return;
            }
            ast::Expr::Function(ref mut func) => {
                self.function(func, /* expression */ true);
                return;
            }
            ast::Expr::TypeOf(ref mut en) => {
                // Look for e.g.
                //   typeof exports
                // which may refer to a global.
                if let ast::Expr::Ident(ref mut sym) = en.expr {
                    self.resolve(sym, true);
                    return;
                }
            }
            _ => {}
        }
        visit::expr(en, self);
    }

    fn stmt(&mut self, stmt: &mut ast::Stmt) {
        match *stmt {
            ast::Stmt::Function(ref mut func) => {
                self.function(func, /* expression */ false);
            }
            _ => visit::stmt(stmt, self),
        }
    }
}

pub fn bind(module: &mut ast::Module) -> Vec<String> {
    let mut bind = Bind {
        scopes: Vec::new(),
        warnings: Vec::new(),
    };
    bind.scopes.push(load_externs());
    bind.module(module);
    assert_eq!(bind.scopes.len(), 1);
    return bind.warnings;
}
