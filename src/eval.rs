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

struct Visit {
}
impl Visit {
    fn expr(&mut self, stmt: &ast::Expr) {
    }

    fn stmt(&mut self, stmt: &ast::Stmt) {
        match *stmt {
            ast::Stmt::Var(ref decls) => {
                for decl in decls.decls.iter() {
                    match decl.pattern {
                        ast::BindingPattern::Name(ref name) => println!("decl {:?}", name),
                        _ => unimplemented!(),
                    }
                    if let Some((_, ref init)) = decl.init {
                        self.expr(init);
                    }
                }
            }
            _ => unimplemented!("eval {}", stmt.kind()),
        }
    }

    fn module(&mut self, module: &ast::Module) {
        for stmt in module.stmts.iter() {
            self.stmt(stmt);
        }
    }
}

pub fn eval(module: &ast::Module) {
    let mut v = Visit{};
    v.module(module);
}
