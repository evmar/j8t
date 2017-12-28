/**
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

use std;
use std::rc::Rc;
use lex;
use lex::{Tok, Token, Span, LexError};
use ast;
use ast::{Expr, Stmt};

pub struct Parser<'a> {
    pub lexer: lex::Lexer<'a>,
}

#[derive(Debug)]
pub struct ParseError {
    msg: String,
    at: Span,
}

impl ParseError {
    fn from_lexerror(LexError { msg, pos }: LexError) -> ParseError {
        ParseError {
            msg,
            at: Span {
                start: pos,
                end: pos + 1,
            },
        }
    }

    pub fn print(&self, l: &lex::Lexer) {
        let lex::Context {
            line,
            col,
            source_line,
        } = l.scan.context(self.at.start);
        eprintln!("ERROR:{}:{}: {}", line, col, self.msg);
        eprintln!("{}", std::str::from_utf8(source_line).unwrap());
        let mut mark = String::new();
        for _ in 0..col - 1 {
            mark.push(' ');
        }
        for _ in self.at.start..self.at.end {
            mark.push('^');
        }
        eprintln!("{}", mark);
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

fn is_for_in_of_head(expr: &mut Expr) -> Option<ast::ForInOf> {
    match expr {
        &mut ast::Expr::Binary(ref mut bin) if bin.op == ast::BinOp::In => {
            let mut lhs = ast::Expr::Ident(ast::Symbol::new(""));
            std::mem::swap(&mut bin.lhs, &mut lhs);
            let mut rhs = ast::Expr::Ident(ast::Symbol::new(""));
            std::mem::swap(&mut bin.rhs, &mut rhs);
            return Some(ast::ForInOf {
                typ: None,
                decl: lhs,
                iter: rhs,
                body: Stmt::Empty,
            });
        }
        _ => None,
    }
}

fn decls_from_expr(expr: Expr, decls: &mut Vec<ast::VarDecl>) {
    match expr {
        ast::Expr::Ident(_) => {
            decls.push(ast::VarDecl {
                name: expr,
                init: None,
            });
        }
        ast::Expr::Assign(lhs, rhs) => {
            decls.push(ast::VarDecl {
                name: *lhs,
                init: Some(*rhs),
            });
        }
        ast::Expr::Binary(bin) => {
            let bin = *bin;
            if bin.op == ast::BinOp::Comma {
                decls_from_expr(bin.lhs, decls);
                decls_from_expr(bin.rhs, decls);
            } else {
                panic!("no decls on {:?}", bin);
            }
        }
        _ => panic!("no decls on {:?}", expr),
    }
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a [u8]) -> Parser<'a> {
        Parser { lexer: lex::Lexer::new(input) }
    }

    fn parse_error<S: Into<String>>(&self, got: Token, expected: S) -> ParseError {
        ParseError {
            msg: format!("expected {}, got {:?}", expected.into(), got),
            at: got.span,
        }
    }

    fn lex_read(&mut self) -> ParseResult<Token> {
        self.lexer.read().map_err(ParseError::from_lexerror)
    }

    fn lex_peek(&mut self) -> ParseResult<Tok> {
        self.lexer.peek().map_err(ParseError::from_lexerror)
    }

    fn expect(&mut self, tok: Tok) -> ParseResult<()> {
        let token = self.lex_read()?;
        if token.tok == tok {
            Ok(())
        } else {
            Err(self.parse_error(token, format!("{:?}", tok)))
        }

    }

    fn array_literal(&mut self) -> ParseResult<Vec<Expr>> {
        let mut elems: Vec<Expr> = Vec::new();
        loop {
            match self.lex_peek()? {
                Tok::RSquare => break,
                Tok::Comma => {
                    // elision TODO
                    self.lex_read()?;
                }
                _ => {
                    elems.push(try!(self.expr()));
                    if self.lex_peek()? == Tok::Comma {
                        self.lex_read()?;
                    } else {
                        break;
                    }
                }
            }
        }
        try!(self.expect(Tok::RSquare));
        Ok(elems)
    }

    fn object_literal(&mut self) -> ParseResult<ast::Object> {
        let mut props: Vec<ast::Property> = Vec::new();
        loop {
            let tok = self.lex_peek()?;
            if tok == Tok::Ident || tok == Tok::String || tok.is_kw() {
                let token = self.lex_read()?;
                let name = self.lexer.text(token);
                try!(self.expect(Tok::Colon));
                let value = try!(self.expr_prec(3));
                props.push(ast::Property {
                    name: name,
                    value: value,
                });
                if self.lex_peek()? == Tok::Comma {
                    self.lex_read()?;
                } else {
                    break;
                }
            } else {
                break;
            }

        }
        try!(self.expect(Tok::RBrace));
        Ok(ast::Object { props: props })
    }

    // 12.2 Primary Expression
    // TODO: we need to allow this to fail to handle 'case' clauses in switch properly.
    // Need a primary_expr_opt() that this calls.
    fn primary_expr(&mut self) -> ParseResult<Expr> {
        let token = self.lex_read()?;
        Ok(match token.tok {
            Tok::This => Expr::This,
            Tok::Ident => {
                let text = self.lexer.text(token);
                match text.as_str() {
                    "null" => Expr::Null,
                    "undefined" => Expr::Undefined,
                    "true" => Expr::Bool(true),
                    "false" => Expr::Bool(false),
                    _ => Expr::Ident(ast::Symbol::new(text))
                }
            }
            Tok::Number => {
                if let lex::TokData::Number(n) = token.data {
                    Expr::Number(n)
                } else {
                    unreachable!();
                }
            }
            Tok::String => Expr::String(String::from(self.lexer.text(token))),
            Tok::LSquare => Expr::Array(try!(self.array_literal())),
            Tok::LBrace => Expr::Object(Box::new(try!(self.object_literal()))),
            Tok::Function => Expr::Function(Box::new(try!(self.function()))),
            Tok::LParen => {
                let r = try!(self.expr());
                try!(self.expect(Tok::RParen));
                r
            }
            Tok::Div => {
                let literal = match self.lexer.read_regex() {
                    Err(err) => panic!(err),
                    Ok(literal) => literal,
                };
                Expr::Regex(Box::new(ast::Regex { literal: String::from(literal) }))
            }
            _ => {
                return Err(self.parse_error(token, "primary expression"));
            }
        })
    }

    // 14.1 Function Definitions
    fn function(&mut self) -> ParseResult<ast::Function> {
        let name = match self.lex_peek()? {
            Tok::Ident => {
                let token = self.lex_read()?;
                Some(ast::Symbol::new(self.lexer.text(token)))
            }
            _ => None,
        };

        try!(self.expect(Tok::LParen));
        let mut params: Vec<Rc<ast::Symbol>> = Vec::new();
        loop {
            if self.lex_peek()? == Tok::Ident {
                let token = self.lex_read()?;
                params.push(ast::Symbol::new(self.lexer.text(token)));
                if self.lex_peek()? == Tok::Comma {
                    self.lex_read()?;
                    continue;
                }
            }
            break;
        }
        try!(self.expect(Tok::RParen));

        try!(self.expect(Tok::LBrace));
        let mut body: Vec<Stmt> = Vec::new();
        while self.lex_peek()? != Tok::RBrace {
            body.push(try!(self.stmt()));
        }
        try!(self.expect(Tok::RBrace));

        Ok(ast::Function {
            name: name,
            params: params,
            body: body,
        })
    }

    fn function_call(&mut self, func: Expr) -> ParseResult<Expr> {
        let mut params: Vec<Expr> = Vec::new();
        loop {
            if self.lex_peek()? == Tok::RParen {
                break;
            }
            params.push(try!(self.expr_prec(3)));
            if self.lex_peek()? == Tok::Comma {
                self.lex_read()?;
                continue;
            }
            break;
        }
        try!(self.expect(Tok::RParen));
        Ok(Expr::Call(Box::new(ast::Call {
            func: func,
            args: params,
        })))
    }

    fn expr_prec(&mut self, prec: usize) -> ParseResult<Expr> {
        // prec is precedence:
        // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence

        // Parse a unary preceding op, or the primary expression itself.
        let token = self.lex_read()?;
        let mut expr = match token.tok {
            Tok::Not | Tok::BNot | Tok::Plus | Tok::Minus | Tok::PlusPlus | Tok::MinusMinus |
            Tok::Void | Tok::Delete if prec <= 16 => {
                let expr = try!(self.expr_prec(16));
                Expr::Unary(ast::UnOp::from_tok(token.tok), Box::new(expr))
            }
            Tok::TypeOf if prec <= 16 => {
                let expr = try!(self.expr_prec(16));
                Expr::TypeOf(Box::new(expr))
            }
            Tok::New if prec <= 18 => {
                let expr = try!(self.expr_prec(18));
                Expr::New(Box::new(expr))
            }
            _ => {
                self.lexer.back(token);
                try!(self.primary_expr())
            }
        };

        // Parse any following unary/binary ops.
        loop {
            let token = self.lex_read()?;
            match token.tok {
                Tok::Comma if prec <= 0 => {
                    let rhs = try!(self.expr_prec(0));
                    expr = Expr::Binary(Box::new(ast::Binary {
                        op: ast::BinOp::Comma,
                        lhs: expr,
                        rhs: rhs,
                    }));
                }
                Tok::Eq if prec <= 3 => {
                    let rhs = try!(self.expr_prec(3));
                    expr = Expr::Assign(Box::new(expr), Box::new(rhs));
                }
                Tok::PlusEq | Tok::MinusEq | Tok::StarEq | Tok::PercentEq | Tok::StarStarEq |
                Tok::LTLTEq | Tok::GTGTEq | Tok::GTGTGTEq | Tok::AndEq | Tok::OrEq |
                Tok::CaratEq if prec <= 3 => {
                    let rhs = try!(self.expr_prec(3));
                    expr = Expr::Binary(Box::new(ast::Binary {
                        op: ast::BinOp::from_tok(token.tok),
                        lhs: expr,
                        rhs: rhs,
                    }));
                }
                Tok::Question if prec <= 4 => {
                    let iftrue = try!(self.expr_prec(3));
                    try!(self.expect(Tok::Colon));
                    let iffalse = try!(self.expr_prec(3));
                    expr = Expr::Ternary(Box::new(ast::Ternary {
                        condition: expr,
                        iftrue: iftrue,
                        iffalse: iffalse,
                    }));
                }
                Tok::OrOr if prec <= 5 => {
                    let rhs = try!(self.expr_prec(5));
                    expr = Expr::Binary(Box::new(ast::Binary {
                        op: ast::BinOp::OrOr,
                        lhs: expr,
                        rhs: rhs,
                    }));
                }
                Tok::AndAnd if prec <= 6 => {
                    let rhs = try!(self.expr_prec(6));
                    expr = Expr::Binary(Box::new(ast::Binary {
                        op: ast::BinOp::AndAnd,
                        lhs: expr,
                        rhs: rhs,
                    }));
                }
                Tok::BOr if prec <= 7 => {
                    let rhs = try!(self.expr_prec(7));
                    expr = Expr::Binary(Box::new(ast::Binary {
                        op: ast::BinOp::BOr,
                        lhs: expr,
                        rhs: rhs,
                    }));
                }
                Tok::Xor if prec <= 8 => {
                    let rhs = try!(self.expr_prec(8));
                    expr = Expr::Binary(Box::new(ast::Binary {
                        op: ast::BinOp::Xor,
                        lhs: expr,
                        rhs: rhs,
                    }));
                }
                Tok::BAnd if prec <= 9 => {
                    let rhs = try!(self.expr_prec(9));
                    expr = Expr::Binary(Box::new(ast::Binary {
                        op: ast::BinOp::BAnd,
                        lhs: expr,
                        rhs: rhs,
                    }));
                }
                Tok::EqEq | Tok::NEq | Tok::EqEqEq | Tok::NEqEq if prec <= 10 => {
                    let rhs = try!(self.expr_prec(11));
                    expr = Expr::Binary(Box::new(ast::Binary {
                        op: ast::BinOp::from_tok(token.tok),
                        lhs: expr,
                        rhs: rhs,
                    }));
                }
                Tok::LT | Tok::LTE | Tok::GT | Tok::GTE | Tok::In | Tok::InstanceOf
                    if prec <= 11 => {
                    let rhs = try!(self.expr_prec(11));
                    expr = Expr::Binary(Box::new(ast::Binary {
                        op: ast::BinOp::from_tok(token.tok),
                        lhs: expr,
                        rhs: rhs,
                    }));
                }
                Tok::LTLT | Tok::GTGT | Tok::GTGTGT if prec <= 12 => {
                    let rhs = try!(self.expr_prec(12));
                    expr = Expr::Binary(Box::new(ast::Binary {
                        op: ast::BinOp::from_tok(token.tok),
                        lhs: expr,
                        rhs: rhs,
                    }));
                }
                Tok::Plus | Tok::Minus if prec <= 13 => {
                    let rhs = try!(self.expr_prec(14));
                    expr = Expr::Binary(Box::new(ast::Binary {
                        op: ast::BinOp::from_tok(token.tok),
                        lhs: expr,
                        rhs: rhs,
                    }));
                }
                Tok::Star | Tok::Div | Tok::Percent if prec <= 14 => {
                    let rhs = try!(self.expr_prec(15));
                    expr = Expr::Binary(Box::new(ast::Binary {
                        op: ast::BinOp::from_tok(token.tok),
                        lhs: expr,
                        rhs: rhs,
                    }));
                }
                Tok::PlusPlus if prec <= 17 => {
                    expr = Expr::Unary(ast::UnOp::PostPlusPlus, Box::new(expr))
                }
                Tok::MinusMinus if prec <= 17 => {
                    expr = Expr::Unary(ast::UnOp::PostMinusMinus, Box::new(expr))
                }
                Tok::Dot if prec <= 19 => {
                    let token = self.lex_read()?;
                    if token.tok != Tok::Ident && !token.tok.is_kw() {
                        return Err(self.parse_error(token, "member"));
                    }
                    let field = self.lexer.text(token);
                    expr = Expr::Field(Box::new(expr), field);
                }
                Tok::LSquare if prec <= 19 => {
                    let index = try!(self.expr());
                    try!(self.expect(Tok::RSquare));
                    expr = Expr::Index(Box::new(expr), Box::new(index));
                }
                Tok::LParen if prec <= 19 => {
                    expr = try!(self.function_call(expr));
                }
                _ => {
                    self.lexer.back(token);
                    return Ok(expr);
                }
            }
        }
    }

    pub fn expr(&mut self) -> ParseResult<Expr> {
        self.expr_prec(0)
    }

    fn bindings(&mut self) -> ParseResult<Vec<ast::VarDecl>> {
        let mut decls: Vec<ast::VarDecl> = Vec::new();
        loop {
            let token = self.lex_read()?;
            let name = match token.tok {
                Tok::Ident => self.lexer.text(token),
                _ => {
                    return Err(self.parse_error(token, "binding name"));
                }
            };
            let init = if self.lex_peek()? == Tok::Eq {
                self.lex_read()?;
                Some(try!(self.expr()))
            } else {
                None
            };
            decls.push(ast::VarDecl {
                name: ast::Expr::Ident(ast::Symbol::new(name)),
                init: init,
            });

            if self.lex_peek()? == Tok::Comma {
                self.lex_read()?;
                continue;
            } else {
                break;
            }
        }
        Ok(decls)
    }

    fn if_stmt(&mut self) -> ParseResult<ast::If> {
        try!(self.expect(Tok::LParen));
        let cond = try!(self.expr());
        try!(self.expect(Tok::RParen));
        let iftrue = try!(self.stmt());
        let else_ = if self.lex_peek()? == Tok::Else {
            self.lex_read()?;
            Some(try!(self.stmt()))
        } else {
            None
        };
        Ok(ast::If {
            cond: cond,
            iftrue: iftrue,
            else_: else_,
        })
    }

    fn while_stmt(&mut self) -> ParseResult<ast::While> {
        self.expect(Tok::LParen)?;
        let cond = self.expr()?;
        self.expect(Tok::RParen)?;
        let body = self.stmt()?;
        Ok(ast::While {
            cond: cond,
            body: body,
        })
    }

    fn do_while_stmt(&mut self) -> ParseResult<ast::While> {
        let body = self.stmt()?;
        self.expect(Tok::While)?;
        self.expect(Tok::LParen)?;
        let cond = self.expr()?;
        self.expect(Tok::RParen)?;
        self.expect_semi()?;
        Ok(ast::While {
            cond: cond,
            body: body,
        })
    }

    // Read a variable binding left hand side.
    fn binding(&mut self) -> ParseResult<String> {
        let token = self.lex_read()?;
        match token.tok {
            Tok::Ident => Ok(self.lexer.text(token)),
            // TODO: binding patterns.
            _ => Err(self.parse_error(token, "binding")),
        }
    }

    fn for_stmt(&mut self) -> ParseResult<Stmt> {
        // This is subtle because of the many possible forms of a 'for' statement.
        // See the test.

        self.expect(Tok::LParen)?;

        let tok = self.lex_peek()?;
        let init = if tok == Tok::Semi {
            ast::ForInit::Empty
        } else {
            let decl_type = if tok == Tok::Var {
                self.lex_read()?;
                Some(ast::VarDeclType::Var)
            } else {
                None
            };
            let mut expr = self.expr()?;
            if let Some(mut forin) = is_for_in_of_head(&mut expr) {
                self.expect(Tok::RParen)?;
                forin.typ = decl_type;
                forin.body = try!(self.stmt());
                return Ok(Stmt::ForInOf(Box::new(forin)));
            }
            if let Some(decl_type) = decl_type {
                let mut decls: Vec<ast::VarDecl> = Vec::new();
                decls_from_expr(expr, &mut decls);
                ast::ForInit::Decls(ast::VarDecls {
                    typ: decl_type,
                    decls: decls,
                })
            } else {
                ast::ForInit::Expr(expr)
            }
        };
        self.expect(Tok::Semi)?;

        // for (a;b;c) loop.  Lexer is now pointed at 'b'.
        let cond = if self.lex_peek()? != Tok::Semi {
            Some(try!(self.expr()))
        } else {
            None
        };
        try!(self.expect(Tok::Semi));

        let iter = if self.lex_peek()? != Tok::RParen {
            Some(try!(self.expr()))
        } else {
            None
        };
        try!(self.expect(Tok::RParen));
        let body = try!(self.stmt());
        Ok(Stmt::For(Box::new(ast::For {
            init: init,
            cond: cond,
            iter: iter,
            body: body,
        })))
    }

    fn switch(&mut self) -> ParseResult<ast::Switch> {
        try!(self.expect(Tok::LParen));
        let expr = try!(self.expr());
        try!(self.expect(Tok::RParen));
        try!(self.expect(Tok::LBrace));
        let mut cases: Vec<ast::Case> = Vec::new();
        loop {
            cases.push(match self.lex_peek()? {
                Tok::Case => {
                    self.lex_read()?;
                    let expr = try!(self.expr());
                    try!(self.expect(Tok::Colon));
                    let stmts = try!(self.stmts());
                    ast::Case {
                        expr: Some(expr),
                        stmts: stmts,
                    }
                }
                Tok::Default => {
                    self.lex_read()?;
                    try!(self.expect(Tok::Colon));
                    let stmts = try!(self.stmts());
                    ast::Case {
                        expr: None,
                        stmts: stmts,
                    }
                }
                _ => {
                    break;
                }
            });
        }
        try!(self.expect(Tok::RBrace));
        Ok(ast::Switch {
            expr: expr,
            cases: cases,
        })
    }

    fn try(&mut self) -> ParseResult<ast::Try> {
        let block = try!(self.block());

        let catch = if self.lex_peek()? == Tok::Catch {
            self.lex_read()?;
            try!(self.expect(Tok::LParen));
            let catch_expr = try!(self.binding());
            try!(self.expect(Tok::RParen));
            let catch_block = try!(self.block());
            Some((catch_expr, catch_block))
        } else {
            None
        };

        let finally = if self.lex_peek()? == Tok::Finally {
            self.lex_read()?;
            let fin_block = try!(self.block());
            Some(fin_block)
        } else {
            None
        };

        Ok(ast::Try {
            block: block,
            catch: catch,
            finally: finally,
        })
    }

    // Attempt to read a semicolon, returning true if succeeded.
    fn asi_semi(&mut self) -> ParseResult<bool> {
        let token = self.lex_read()?;
        Ok(match token.tok {
            Tok::Semi => true,
            // Attempt ASI if not a semi.
            _ if token.saw_newline => {
                self.lexer.back(token);
                true
            }
            Tok::RBrace | Tok::EOF => {
                self.lexer.back(token);
                true
            }
            _ => {
                self.lexer.back(token);
                false
            }
        })
    }

    fn expect_semi(&mut self) -> ParseResult<()> {
        if !self.asi_semi()? {
            let token = self.lex_read()?;
            return Err(self.parse_error(token, "semicolon"));
        }
        Ok(())
    }

    // 13 ECMAScript Language: Statements and Declarations
    pub fn stmt(&mut self) -> ParseResult<Stmt> {
        let token = self.lex_read()?;
        let stmt = match token.tok {
            // Declaration
            Tok::Function => Stmt::Function(Box::new(try!(self.function()))),
            // Statement
            Tok::LBrace => {
                let body = try!(self.stmts());
                try!(self.expect(Tok::RBrace));
                Stmt::Block(body)
            }
            Tok::Var => {
                let decls = try!(self.bindings());
                try!(self.expect_semi());
                Stmt::Var(Box::new(ast::VarDecls {
                    typ: ast::VarDeclType::Var,
                    decls: decls,
                }))
            }
            Tok::Semi => Stmt::Empty,
            Tok::If => Stmt::If(Box::new(try!(self.if_stmt()))),
            Tok::While => Stmt::While(Box::new(try!(self.while_stmt()))),
            Tok::Do => Stmt::DoWhile(Box::new(try!(self.do_while_stmt()))),
            Tok::For => try!(self.for_stmt()),
            Tok::Switch => Stmt::Switch(Box::new(try!(self.switch()))),
            Tok::Break | Tok::Continue => {
                let target = if self.asi_semi()? {
                    None
                } else {
                    let token = self.lex_read()?;
                    if token.tok != Tok::Ident {
                        return Err(self.parse_error(token, "label"));
                    }
                    let label = self.lexer.text(token);
                    self.expect_semi()?;
                    Some(label)
                };
                if token.tok == Tok::Break {
                    Stmt::Break(target)
                } else {
                    Stmt::Continue(target)
                }
            }
            Tok::Return => {
                if self.asi_semi()? {
                    Stmt::Return(None)
                } else {
                    let expr = try!(self.expr());
                    try!(self.expect_semi());
                    Stmt::Return(Some(Box::new(expr)))
                }
            }
            Tok::Throw => Stmt::Throw(Box::new(try!(self.expr()))),
            Tok::Try => Stmt::Try(Box::new(try!(self.try()))),
            t => {
                if t == Tok::Ident && self.lex_peek()? == Tok::Colon {
                    // Note: we have to read two tokens(!) to spot a label statement.
                    let label = self.lexer.text(token);
                    self.lex_read()?;
                    let stmt = try!(self.stmt());
                    return Ok(Stmt::Label(Box::new(ast::Label {
                        label: label,
                        stmt: stmt,
                    })));
                }
                self.lexer.back(token);
                let expr = try!(self.expr());
                try!(self.expect_semi());
                Stmt::Expr(Box::new(expr))
            }
        };
        Ok(stmt)
    }

    pub fn stmts(&mut self) -> ParseResult<Vec<Stmt>> {
        let mut body: Vec<Stmt> = Vec::new();
        loop {
            match self.lex_peek()? {
                // TODO: why 'case' here? because otherwise we read error on cases in switch.
                Tok::RBrace | Tok::EOF | Tok::Case | Tok::Default => break,
                _ => {
                    body.push(try!(self.stmt()));
                }
            }
        }
        Ok(body)
    }

    // In some cases, the syntax only allows a block but for AST simplicity
    // we represent it as a single Stmt::Block.
    fn block(&mut self) -> ParseResult<Stmt> {
        try!(self.expect(Tok::LBrace));
        let block = try!(self.stmts());
        try!(self.expect(Tok::RBrace));
        Ok(Stmt::Block(block))
    }

    pub fn module(&mut self) -> ParseResult<ast::Module> {
        Ok(ast::Module{stmts:self.stmts()?})
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    fn parse_expr(input: &str) -> Expr {
        println!("parse_expr: {:?}", input);
        let mut p = Parser::new(input.as_bytes());
        let expr = match p.expr() {
            Err(err) => {
                err.print(&p.lexer);
                panic!("err");
            }
            Ok(expr) => expr,
        };
        println!("result: {:?}", expr);
        p.expect(Tok::EOF).unwrap();
        expr
    }

    fn parse(input: &str) -> Vec<Stmt> {
        println!("parse: {:?}", input);
        let mut p = Parser::new(input.as_bytes());
        let stmts = match p.stmts() {
            Err(err) => {
                err.print(&p.lexer);
                panic!("err");
            }
            Ok(stmt) => stmt,
        };
        println!("result: {:?}", stmts);
        p.expect(Tok::EOF).unwrap();
        stmts
    }

    #[test]
    fn test_expr() {
        parse_expr("abc");
    }

    #[test]
    fn test_assoc() {
        let expr = parse_expr("a + b + c");
        match expr {
            Expr::Binary(ref bin) => {
                match bin.lhs {
                    Expr::Binary(_) => {}
                    _ => panic!("fail"),
                }
            }
            _ => panic!("fail"),
        }
    }

    #[test]
    fn test_binop() {
        parse_expr("a * b + c * d + e * f * g + h");

        parse_expr("i + j * k + l");

        parse_expr("(i + j) * k + l");
    }

    #[test]
    fn test_field() {
        parse_expr("a.b.c");
        //parse_expr("a.3");
    }

    #[test]
    fn test_unary() {
        parse_expr("a + typeof typeof a.b + a");
    }

    #[test]
    fn test_umd() {
        parse_expr(
            "function (global, factory) {
	typeof exports === 'object' && typeof module !== 'undefined' ? module.exports = factory() :
	typeof define === 'function' && define.amd ? define(factory) :
    (global.x = factory());
}",
        );
    }

    #[test]
    fn test_asi() {
        parse(
            "{ a
b } c",
        );
    }

    #[test]
    fn test_asi_case() {
        parse(
            "
      switch (c) {
        case 1: a; break // c1
        case 2: b; break // c2
      }",
        );
    }

    #[test]
    fn test_asi_return() {
        parse(
            "return
return 3
{ return }
{ return 3 }
return",
        );
    }

    #[test]
    fn test_for() {
        parse("for (;;);");
        parse("for (var x = 3; a; b);");
        parse("for (var x = 3, y = 4; a; b);");
        parse("for (x = 3; a; b);");
        parse("for (x = 3, y = 4; a; b);");
        parse("for (var x in a);");
        parse("for (x in a);");
    }

    #[test]
    fn test_kws() {
        parse("foo.extends = bar.case;");
    }

    #[test]
    fn test_label() {
        parse("foo: bar;");
    }
}
