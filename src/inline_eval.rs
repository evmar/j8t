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
use visit;
use visit::Visit;

/// InlineEval inlines eval("x") to just "x".
/// TODO: do we really want to do that?
struct InlineEval {}

impl visit::Visit for InlineEval {
    fn expr(&mut self, _en: &mut ast::ExprNode) {}

    fn stmt(&mut self, _stmt: &mut ast::Stmt) {}
}

pub fn inline_eval(module: &mut ast::Module) {
    let mut ie = InlineEval {};
    for s in module.stmts.iter_mut() {
        ie.stmt(s);
    }
}

#[cfg(test)]
mod tests {
    use ast;
    use test_util::*;

    fn inline_eval(input: &str) -> ast::Module {
        let mut module = must_parse(input);
        super::inline_eval(&mut module);
        module
    }

    #[test]
    fn basic() {
        ast_eq(&inline_eval("eval('x = 3')"), "x = 3");
    }

}
