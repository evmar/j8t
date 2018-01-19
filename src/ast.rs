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
use std::cell::RefCell;

pub use lex::{Span};

#[derive(Debug)]
pub struct Symbol {
    pub name: RefCell<String>,
    pub renameable: bool,
}

impl Symbol {
    pub fn new<S: Into<String>>(name: S) -> Rc<Symbol> {
        Rc::new(Symbol {
            name: RefCell::new(String::from(name.into())),
            renameable: true,
        })
    }
}

#[derive(Debug)]
pub struct Scope {
    pub bindings: Vec<Rc<Symbol>>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope { bindings: Vec::new() }
    }
    pub fn resolve(&self, sym: &Rc<Symbol>) -> Option<Rc<Symbol>> {
        let name = sym.name.borrow();
        self.bindings
            .iter()
            .find(|s| *s.name.borrow() == *name)
            .map(|t| t.clone())
    }
}

#[derive(Debug)]
pub enum Expr {
    // 12.2 Primary Expression
    This,
    Ident(Rc<Symbol>),
    Null,
    Undefined, // Note: not part of the grammar, hmm.
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<ExprNode>),
    Object(Box<Object>),
    Function(Box<Function>),
    Regex(Box<Regex>),

    // 12.3 Left-Hand-Side Expressions
    Index(Box<ExprNode>, Box<ExprNode>),
    Field(Box<ExprNode>, String),
    New(Box<ExprNode>),
    Call(Box<Call>),

    // Various other operators.
    Unary(UnOp, Box<ExprNode>),
    Binary(Box<Binary>),
    TypeOf(Box<ExprNode>),
    Ternary(Box<Ternary>),
    Assign(Box<ExprNode>, Box<ExprNode>),
}

pub type ExprNode = (Span, Expr);

#[derive(Debug)]
pub struct Object {
    pub props: Vec<Property>,
}

// Property forms:
// 1) a   (short for a: a)
// 2) a: b
// 3) 'a': b
// 4) 0: b  (numeric key)
// 5) [a]: b
// 6) a(b) {}
// 7) get/set a(b) {}
// 8) a=b (used to cover alternative syntax for bindings)

#[derive(Debug)]
pub enum PropertyKey {
    String(String),
    Number(f64),
}

#[derive(Debug)]
pub struct Property {
    pub name: PropertyKey,
    pub value: ExprNode,
}

#[derive(Debug)]
pub struct Function {
    pub scope: Scope,
    pub name: Option<Rc<Symbol>>,
    pub params: Vec<Rc<Symbol>>,
    pub body: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Regex {
    // TODO: parse into body and flags?
    pub literal: String,
}

#[derive(Debug)]
pub struct Call {
    pub func: ExprNode,
    pub args: Vec<ExprNode>,
}

pub use ops::UnOp;
pub use ops::BinOp;

#[derive(Debug)]
pub struct Binary {
    pub op: BinOp,
    pub lhs: ExprNode,
    pub rhs: ExprNode,
}

#[derive(Debug)]
pub struct Ternary {
    pub condition: ExprNode,
    pub iftrue: ExprNode,
    pub iffalse: ExprNode,
}

#[derive(Debug)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Var(Box<VarDecls>),
    Empty,
    Expr(ExprNode),
    If(Box<If>),
    While(Box<While>),
    DoWhile(Box<While>),
    For(Box<For>),
    ForInOf(Box<ForInOf>),
    Switch(Box<Switch>),
    Continue(Option<String>),
    Break(Option<String>),
    Return(Option<Box<Expr>>),
    Label(Box<Label>),
    Throw(Box<Expr>),
    Try(Box<Try>),

    Function(Box<Function>),
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(Copy)]
pub enum VarDeclType {
    Var,
    Const,
    Let,
}

impl VarDeclType {
    pub fn to_string(&self) -> &'static str {
        match self {
            &VarDeclType::Var => "var",
            &VarDeclType::Const => "const",
            &VarDeclType::Let => "let",
        }
    }
}

#[derive(Debug)]
pub struct VarDecl {
    pub name: Expr,
    pub init: Option<Expr>,
}

#[derive(Debug)]
pub struct VarDecls {
    pub typ: VarDeclType,
    pub decls: Vec<VarDecl>,
}

#[derive(Debug)]
pub struct If {
    pub cond: Expr,
    pub iftrue: Stmt,
    pub else_: Option<Stmt>,
}

#[derive(Debug)]
pub struct While {
    pub cond: Expr,
    pub body: Stmt,
}

#[derive(Debug)]
pub enum ForInit {
    Empty,
    Expr(Expr),
    Decls(VarDecls),
}

#[derive(Debug)]
pub struct For {
    pub init: ForInit,
    pub cond: Option<Expr>,
    pub iter: Option<Expr>,
    pub body: Stmt,
}

#[derive(Debug)]
pub struct ForInOf {
    pub init: ForInit,
    pub body: Stmt,
}

#[derive(Debug)]
pub struct Case {
    pub expr: Option<Expr>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Switch {
    pub expr: Expr,
    pub cases: Vec<Case>,
}

#[derive(Debug)]
pub struct Label {
    pub label: String,
    pub stmt: Stmt,
}

#[derive(Debug)]
pub struct Try {
    pub block: Stmt,
    pub catch: Option<(Expr, Stmt)>,
    pub finally: Option<Stmt>,
}

#[derive(Debug)]
pub struct Module {
    pub scope: Scope,
    pub stmts: Vec<Stmt>,
}
