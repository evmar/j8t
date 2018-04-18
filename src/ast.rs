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

pub use lex::Span;

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
        Scope {
            bindings: Vec::new(),
        }
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
    // The parse of "()", which is used only in parsing arrow functions,
    // and which should not make it out of the parsing layer.
    EmptyParens,

    // 12.2 Primary Expression
    This,
    Ident(Rc<Symbol>),
    Null,
    Undefined, // Note: not part of the grammar, hmm.
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<ExprNode>),
    // The parse of "...a", which can only occur in arrow functions and
    // in array literals.
    Spread(Box<ExprNode>),
    Object(Box<Object>),
    Function(Box<Function>),
    Class(Box<Class>),
    ArrowFunction(Box<ArrowFunction>),
    Regex(Box<Regex>),
    Template(Box<Template>),

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
pub enum PropertyName {
    String(String),
    Number(f64),
    Computed(ExprNode),
}

#[derive(Debug)]
pub struct Property {
    pub name: PropertyName,
    pub value: ExprNode,
}

#[derive(Debug)]
pub struct ObjectBindingPattern {
    pub props: Vec<(PropertyName, BindingElement)>,
}

#[derive(Debug)]
pub struct ArrayBindingPattern {
    pub elems: Vec<BindingElement>,
}

#[derive(Debug)]
pub enum BindingPattern {
    Name(Rc<Symbol>),
    Object(ObjectBindingPattern),
    Array(ArrayBindingPattern),
}

pub type BindingElement = (BindingPattern, Option<ExprNode>);

// Attributes shared by functions and methods.
#[derive(Debug)]
pub struct FunctionMethod {
    pub scope: Scope,
    pub async: bool,
    pub params: Vec<BindingElement>,
    pub body: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Function {
    pub name: Option<Rc<Symbol>>,
    pub body: FunctionMethod,
}

#[derive(Debug)]
pub struct Method {
    pub is_static: bool,
    pub name: PropertyName,
    pub body: FunctionMethod,
}

#[derive(Debug)]
pub enum ArrowBody {
    Expr(ExprNode),
    Stmts(Vec<Stmt>),
}

#[derive(Debug)]
pub struct ArrowFunction {
    pub params: Vec<BindingElement>,
    pub body: ArrowBody,
}

#[derive(Debug)]
pub struct Class {
    pub name: Option<Rc<Symbol>>,
    pub extends: Option<ExprNode>,
    pub methods: Vec<Method>,
}

#[derive(Debug)]
pub struct Regex {
    // TODO: parse into body and flags?
    pub literal: String,
}

#[derive(Debug)]
pub struct Template {
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
    Class(Box<Class>),
}

#[derive(Debug, Clone, Copy)]
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
    pub pattern: BindingPattern,
    pub init: Option<ExprNode>,
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
    Expr(ExprNode),
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
pub enum InOf {
    In,
    Of,
}

#[derive(Debug)]
pub struct ForInOf {
    pub decl_type: Option<VarDeclType>,
    pub loop_var: BindingPattern,
    pub in_of: InOf,
    pub expr: ExprNode,
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
    pub catch: Option<(BindingPattern, Stmt)>,
    pub finally: Option<Stmt>,
}

#[derive(Debug)]
pub struct Module {
    pub scope: Scope,
    pub stmts: Vec<Stmt>,
}
