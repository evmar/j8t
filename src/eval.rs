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
    let rewrite = {
        let en = match stmt {
            ast::Stmt::Expr(ref mut en) => en,
            _ => return false,
        };
        let ast::Call {
            ref mut func,
            ref mut args,
        } = match en.expr {
            ast::Expr::Call(ref mut c) => &mut **c,
            _ => return false,
        };
        // TODO: arrows?
        let ast::Func {
            ref mut params,
            ref mut body,
            ..
        } = match func.expr {
            ast::Expr::Function(ref mut f) => &mut f.func,
            _ => return false,
        };
        // TODO: default args?

        let mut new_body = Vec::new();
        std::mem::swap(&mut new_body, body);
        let decls = params
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
        new_body.insert(0, new_let);
        new_body
    };
    *stmt = ast::Stmt::Block(rewrite);
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
}
