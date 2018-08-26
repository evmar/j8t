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

fn inline_iife(stmt: &mut ast::Stmt) -> bool {
    // This function has a lot of tricky lifetimes.
    // It seems very similar to some of the discussion on
    //   https://github.com/rust-lang/rust/issues/16223
    // which suggests that NLL will help.
    let block = {
        // Match stmt into a call expression.
        let call = match stmt {
            ast::Stmt::Expr(ast::ExprNode {
                expr: ast::Expr::Call(call),
                ..
            }) => &mut **call,
            _ => return false,
        };
        // Match the call into an IIFE call.
        // This cannot be part of the above match because 'call' is a box above
        // and patterns can't look into boxes.
        let (func, args) = match call {
            ast::Call {
                func:
                    ast::ExprNode {
                        expr: ast::Expr::Function(ref mut f),
                        ..
                    },
                ref mut args,
                ..
            } => (&mut f.func, args),
            _ => return false,
        };
        // TODO: arrows?
        // TODO: default args?

        let mut new_body = Vec::new();
        std::mem::swap(&mut new_body, &mut func.body);
        let decls = func.params
            .drain(..)
            .zip(args.drain(..))
            .map(|((pat, _), arg)| ast::VarDecl {
                pattern: pat,
                init: Some(arg),
            })
            .collect();
        let new_let = ast::Stmt::Var(Box::new(ast::VarDecls {
            typ: ast::VarDeclType::Let,
            decls: decls,
        }));
        let mut scope = ast::Scope::new();
        std::mem::swap(&mut scope, &mut func.scope);
        new_body.insert(0, new_let);
        Box::new(ast::Block {
            scope: scope,
            stmts: new_body,
        })
    };
    *stmt = ast::Stmt::Block(block);
    return true;
}

struct Eval {}

impl Eval {
    fn stmts(&mut self, stmts: &mut Vec<ast::Stmt>) {
        for stmt in stmts.iter_mut() {
            inline_iife(stmt);
        }
    }
}

pub fn eval(module: &mut ast::Module) {
    let mut e = Eval {};
    e.stmts(&mut module.stmts);
}

#[cfg(test)]
mod tests {
    use ast;
    use test_util::*;

    fn eval(input: &str) -> ast::Module {
        let mut module = must_parse(input);
        super::eval(&mut module);
        module
    }

    #[test]
    fn iife_simple() {
        ast_eq(&eval("(function (x) {x})(3)"), "{let x = 3; x}");
    }

    #[test]
    fn iife_return() {
        // TODO
        // ast_eq(&eval("(function (x) { return x })(3)"), "{let x = 3; x}");
    }
}
