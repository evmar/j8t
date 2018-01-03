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

use std::io;
use std::io::Write;
use ast;
use ast::Expr;

trait Prec {
    fn prec(&self) -> i8;
}

impl Prec for ast::BinOp {
    fn prec(&self) -> i8 {
        match self {
            &ast::BinOp::StarStar => 15,

            &ast::BinOp::Star |
            &ast::BinOp::Div |
            &ast::BinOp::Percent => 14,

            &ast::BinOp::Plus |
            &ast::BinOp::Minus => 13,

            &ast::BinOp::LTLT |
            &ast::BinOp::GTGT |
            &ast::BinOp::GTGTGT => 12,

            &ast::BinOp::LT |
            &ast::BinOp::GT |
            &ast::BinOp::LTE |
            &ast::BinOp::GTE |
            &ast::BinOp::In |
            &ast::BinOp::InstanceOf => 11,

            &ast::BinOp::EqEq |
            &ast::BinOp::NEq |
            &ast::BinOp::EqEqEq |
            &ast::BinOp::NEqEq => 10,

            &ast::BinOp::BAnd => 9,
            &ast::BinOp::Xor => 8,
            &ast::BinOp::BOr => 7,
            &ast::BinOp::AndAnd => 6,
            &ast::BinOp::OrOr => 5,

            &ast::BinOp::Eq |
            &ast::BinOp::PlusEq |
            &ast::BinOp::MinusEq |
            &ast::BinOp::StarEq |
            &ast::BinOp::PercentEq |
            &ast::BinOp::StarStarEq |
            &ast::BinOp::LTLTEq |
            &ast::BinOp::GTGTEq |
            &ast::BinOp::GTGTGTEq |
            &ast::BinOp::AndEq |
            &ast::BinOp::OrEq |
            &ast::BinOp::CaratEq |
            &ast::BinOp::DivEq => 3,

            &ast::BinOp::Comma => 0,
        }
    }
}

pub struct Writer<'a> {
    w: &'a mut Write,
    ofs: usize,
    want_semi: bool,
    statement_start: usize,
    last_char: u8,

    pub disable_asi: bool,
}

type Result = io::Result<()>;

impl<'a> Write for Writer<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        if self.want_semi {
            if buf[0] as char != '}' {
                self.w.write(b";")?;
            }
            self.want_semi = false;
        }
        self.ofs += buf.len();
        self.last_char = buf[buf.len() - 1];
        self.w.write(buf)
    }
    fn flush(&mut self) -> io::Result<()> {
        self.w.flush()
    }
}

#[derive(PartialEq)]
enum CharClass {
    Space,
    Letter,
    Punc,
}

fn class(ch: u8) -> CharClass {
    match ch as char {
        'a'...'z' | 'A'...'Z' | '0'...'9' | '_' | '$' => CharClass::Letter,
        ' ' | '\n' | '\t' | '\0' => CharClass::Space,
        _ => CharClass::Punc,
    }
}

fn punc_needs_space(last: u8, next: u8) -> bool {
    // TODO: reuse trie from genscan to compute exactly the set of disallowed pairs?
    // That would do the wrong thing on e.g. "x++ + 3" (i.e. x+++3), hmm.
    match (last as char, next as char) {
        ('+', '+') => true,
        _ => false,
    }
}

fn obj_prop_needs_quote(name: &str) -> bool {
    // TODO: reserved words(?)
    name.bytes().any(|c| class(c) != CharClass::Letter)
}

impl<'a> Writer<'a> {
    pub fn new(w: &'a mut Write) -> Writer<'a> {
        Writer {
            w: w,
            ofs: 0,
            want_semi: false,
            statement_start: 0,
            last_char: 0,
            disable_asi: false,
        }
    }

    fn semi(&mut self) -> Result {
        if self.disable_asi {
            self.token(";")?;
        } else {
            self.want_semi = true;
            self.last_char = ';' as u8;
        }
        Ok(())
    }

    // Emit a token, guaranteeing that it doesn't parse as part of the previous one.
    fn token(&mut self, t: &str) -> Result {
        let first = t.as_bytes()[0];
        let needs_space = match (class(self.last_char), class(first)) {
            (CharClass::Space, _) => false,
            (CharClass::Letter, CharClass::Letter) => true,
            (CharClass::Punc, CharClass::Punc) if punc_needs_space(self.last_char, first) => true,
            _ => false,
        };
        if needs_space {
            write!(self, " ")?;
        }
        self.write(t.as_bytes())?;
        Ok(())
    }

    #[inline(always)]
    fn wrap<F: Fn(&mut Writer) -> Result>(&mut self, left: char, right: char, f: F) -> Result {
        self.token(&left.to_string())?;
        f(self)?;
        self.token(&right.to_string())?;
        Ok(())
    }

    #[inline(always)]
    fn paren<F: Fn(&mut Writer) -> Result>(&mut self, f: F) -> Result {
        self.wrap('(', ')', f)
    }

    #[inline(always)]
    fn maybe_paren<F: Fn(&mut Writer) -> Result>(&mut self, paren: bool, f: F) -> Result {
        if paren {
            self.wrap('(', ')', f)
        } else {
            f(self)
        }
    }

    #[inline(always)]
    fn brace<F: Fn(&mut Writer) -> Result>(&mut self, f: F) -> Result {
        self.wrap('{', '}', f)
    }

    fn comma<T, F: Fn(&mut Writer, &T) -> Result>(&mut self, ts: &[T], f: F) -> Result {
        for (i, t) in ts.iter().enumerate() {
            if i > 0 {
                self.token(",")?;
            }
            f(self, t)?;
        }
        Ok(())
    }

    fn function(&mut self, f: &ast::Function) -> Result {
        self.token("function")?;
        if let Some(ref name) = f.name {
            self.token(&name.name)?;
        }
        self.function_from_paren(f)
    }

    fn function_from_paren(&mut self, f: &ast::Function) -> Result {
        self.paren(|w| w.comma(&f.params, |w, p| w.token(&p.name)))?;
        self.brace(|w| {
            for s in f.body.iter() {
                w.stmt(s)?;
            }
            Ok(())
        })?;
        Ok(())
    }

    fn write_char(&mut self, c: char) -> Result {
        let buf: [u8; 1] = [c as u8];
        self.write_all(&buf[..])?;
        Ok(())
    }

    fn quoted(&mut self, s: &str) -> Result {
        // TODO: consider the memchr crate.
        let quote_char = if s.find('"').is_some() { '\'' } else { '"' };
        self.write_char(quote_char)?;
        for c in s.as_bytes() {
            match *c as char {
                '\n' => self.write_all(b"\\n")?,
                '\r' => self.write_all(b"\\r")?,
                '\t' => self.write_all(b"\\t")?,
                '"' if quote_char == '"' => self.write_all(b"\\\"")?,
                '\'' if quote_char == '\'' => self.write_all(b"\\'")?,
                '\\' => self.write_all(b"\\\\")?,
                c => self.write_char(c)?,
            }
        }
        self.write_char(quote_char)?;
        Ok(())
    }

    fn expr(&mut self, e: &Expr, prec: i8) -> Result {
        match e {
            &ast::Expr::This => self.token("this")?,
            &ast::Expr::Ident(ref s) => self.token(&s.name)?,
            &ast::Expr::Null => self.token("null")?,
            &ast::Expr::Undefined => self.token("undefined")?,
            &ast::Expr::Bool(b) => self.token(if b { "true" } else { "false" })?,
            &ast::Expr::Number(ref n) => self.token(&format!("{}", n))?,
            &ast::Expr::String(ref s) => self.quoted(s)?,
            &ast::Expr::Array(ref arr) => {
                self.wrap('[', ']', |w| w.comma(arr, |w, e| w.expr(e, 0)))?;
            }
            &ast::Expr::Object(ref obj) => {
                self.brace(|w| {
                    w.comma(&obj.props, |w, p| {
                        let quoted = obj_prop_needs_quote(&p.name);
                        if quoted {
                            w.token(&format!("{:?}", p.name))?;
                        } else {
                            w.token(&p.name)?;
                        }
                        match p.value {
                            ast::Expr::Ident(ref v) if v.name == p.name => {
                                // omit; implied by property name.
                            },
                            ast::Expr::Function(ref f) if !quoted => {
                                w.function_from_paren(f)?;
                            },
                            _ => {
                                w.token(":")?;
                                w.expr(&p.value, 0)?;
                            }
                        };
                        Ok(())
                    })
                })?;
            }
            &ast::Expr::Function(ref f) => {
                let needs_parens = self.ofs == self.statement_start;
                self.maybe_paren(needs_parens, |w| w.function(f))?;
            }
            &ast::Expr::Regex(ref regex) => self.token(&regex.literal)?,
            &ast::Expr::Index(ref expr, ref index) => {
                self.maybe_paren(prec > 19, |w| {
                    w.expr(expr, 19)?;
                    w.wrap('[', ']', |w| w.expr(index, -1))?;
                    Ok(())
                })?;
            }
            &ast::Expr::Field(ref expr, ref field) => {
                self.maybe_paren(prec > 19, |w| {
                    w.expr(expr, 19)?;
                    w.token(".")?;
                    w.token(field)?;
                    Ok(())
                })?;
            }
            &ast::Expr::New(ref expr) => {
                self.maybe_paren(prec > 18, |w| {
                    w.token("new")?;
                    w.expr(expr, 18)?;
                    Ok(())
                })?;
            }
            &ast::Expr::Call(ref call) => {
                self.maybe_paren(prec > 19, |w| {
                    w.expr(&call.func, 19)?;
                    w.paren(|w| w.comma(&call.args, |w, e| w.expr(e, 0)))?;
                    Ok(())
                })?;
            }
            &ast::Expr::Unary(ref op, ref expr) => {
                match op {
                    &ast::UnOp::PostPlusPlus |
                    &ast::UnOp::PostMinusMinus => {
                        self.maybe_paren(prec > 17, |w| {
                            w.expr(expr, 17)?;
                            w.token(&op.to_string())?;
                            Ok(())
                        })?;
                    }
                    _ => {
                        self.maybe_paren(prec > 16, |w| {
                            w.token(&op.to_string())?;
                            w.expr(expr, 16)?;
                            Ok(())
                        })?;
                    }
                }
            }
            &ast::Expr::Binary(ref bin) => {
                let p = bin.op.prec();
                self.maybe_paren(prec > p, |w| {
                    w.expr(&bin.lhs, p)?;
                    w.token(&bin.op.to_string())?;
                    w.expr(&bin.rhs, p)?;
                    Ok(())
                })?;
            }
            &ast::Expr::TypeOf(ref expr) => {
                self.token("typeof")?;
                self.expr(expr, 16)?;
            }
            &ast::Expr::Ternary(ref t) => {
                self.maybe_paren(prec > 4, |w| {
                    w.expr(&t.condition, 5)?;
                    w.token("?")?;
                    w.expr(&t.iftrue, 3)?;
                    w.token(":")?;
                    w.expr(&t.iffalse, 3)?;
                    Ok(())
                })?;
            }
            &ast::Expr::Assign(ref lhs, ref rhs) => {
                self.maybe_paren(prec > 3, |w| {
                    w.expr(lhs, 4)?;
                    w.token("=")?;
                    w.expr(rhs, 3)?;
                    Ok(())
                })?;
            }
        }
        Ok(())
    }

    pub fn stmt(&mut self, s: &ast::Stmt) -> Result {
        self.statement_start = self.ofs;
        match s {
            &ast::Stmt::Block(ref stmts) => {
                self.brace(|w| {
                    for s in stmts.iter() {
                        w.stmt(s)?;
                    }
                    Ok(())
                })?;
            }
            &ast::Stmt::Var(ref decl) => {
                self.token("var")?;
                for (i, decl) in decl.decls.iter().enumerate() {
                    if i > 0 {
                        self.token(",")?;
                    }
                    self.expr(&decl.name, -1)?;
                    if let Some(ref init) = decl.init {
                        self.token("=")?;
                        self.expr(init, -1)?;
                    }
                }
                self.semi()?;
            }
            &ast::Stmt::Empty => self.semi()?,
            &ast::Stmt::Expr(ref e) => {
                self.expr(e, -1)?;
                self.semi()?;
            }
            &ast::Stmt::If(ref i) => {
                self.token("if")?;
                self.paren(|w| w.expr(&i.cond, -1))?;
                self.stmt(&i.iftrue)?;
                if let Some(ref else_) = i.else_ {
                    self.token("else")?;
                    self.stmt(else_)?;
                }
            }
            &ast::Stmt::While(ref wh) => {
                self.token("while")?;
                self.paren(|w| w.expr(&wh.cond, -1))?;
                self.stmt(&wh.body)?;
            }
            &ast::Stmt::DoWhile(ref wh) => {
                self.token("do")?;
                self.stmt(&wh.body)?;
                self.token("while")?;
                self.paren(|w| w.expr(&wh.cond, -1))?;
                self.semi()?;
            }
            &ast::Stmt::For(ref f) => {
                self.token("for")?;
                self.paren(|w| {
                    match f.init {
                        ast::ForInit::Empty => {}
                        ast::ForInit::Expr(ref e) => w.expr(e, -1)?,
                        ast::ForInit::Decls(ref decls) => {
                            if decls.decls.len() > 0 {
                                w.token(decls.typ.to_string())?;
                            }
                            w.comma(&decls.decls, |w, d| {
                                w.expr(&d.name, -1)?;
                                if let Some(ref init) = d.init {
                                    w.token("=")?;
                                    w.expr(init, -1)?;
                                }
                                Ok(())
                            })?;
                        }
                    }
                    w.token(";")?;
                    if let Some(ref cond) = f.cond {
                        w.expr(cond, -1)?;
                    }
                    w.token(";")?;
                    if let Some(ref iter) = f.iter {
                        w.expr(iter, -1)?;
                    }
                    Ok(())
                })?;
                self.stmt(&f.body)?;
            }
            &ast::Stmt::ForInOf(ref f) => {
                self.token("for")?;
                self.paren(|w| {
                    match f.init {
                        ast::ForInit::Empty => {}
                        ast::ForInit::Expr(ref e) => w.expr(e, -1)?,
                        ast::ForInit::Decls(ref decls) => {
                            w.comma(&decls.decls, |w, d| {
                                w.token(decls.typ.to_string())?;
                                w.expr(&d.name, -1)?;
                                if let Some(ref init) = d.init {
                                    w.token("in")?;
                                    w.expr(init, -1)?;
                                }
                                Ok(())
                            })?;
                        }
                    }
                    Ok(())
                })?;
                self.stmt(&f.body)?;
            }
            &ast::Stmt::Switch(ref switch) => {
                self.token("switch")?;
                self.paren(|w| w.expr(&switch.expr, -1))?;
                self.brace(|w| {
                    for c in &switch.cases {
                        match c.expr {
                            Some(ref c) => {
                                w.token("case")?;
                                w.expr(c, -1)?;
                            }
                            None => w.token("default")?,
                        }
                        w.token(":")?;
                        w.stmts(&c.stmts)?;
                    }
                    Ok(())
                })?;
            }
            &ast::Stmt::Break(ref label) => {
                self.token("break")?;
                if let &Some(ref label) = label {
                    self.token(label)?;
                }
                self.semi()?;
            }
            &ast::Stmt::Continue(ref label) => {
                self.token("continue")?;
                if let &Some(ref label) = label {
                    self.token(label)?;
                }
                self.semi()?;
            }
            &ast::Stmt::Return(ref expr) => {
                self.token("return")?;
                if let &Some(ref expr) = expr {
                    self.expr(expr, -1)?;
                }
                self.semi()?;
            }
            &ast::Stmt::Label(ref label) => {
                self.token(&label.label)?;
                self.token(":")?;
                self.stmt(&label.stmt)?;
            }
            &ast::Stmt::Throw(ref expr) => {
                self.token("throw")?;
                self.expr(expr, -1)?;
                self.semi()?;
            }
            &ast::Stmt::Try(ref try) => {
                self.token("try")?;
                self.stmt(&try.block)?;
                if let Some((ref v, ref stmt)) = try.catch {
                    self.token("catch")?;
                    self.paren(|w| w.expr(v, -1))?;
                    self.stmt(stmt)?;
                }
                if let Some(ref finally) = try.finally {
                    self.token("finally")?;
                    self.stmt(finally)?;
                }
            }
            &ast::Stmt::Function(ref f) => self.function(f)?,
        }
        Ok(())
    }

    pub fn stmts(&mut self, stmts: &[ast::Stmt]) -> Result {
        for s in stmts.iter() {
            self.stmt(&s)?;
        }
        Ok(())
    }

    pub fn module(&mut self, module: &ast::Module) -> Result {
        self.stmts(&module.stmts)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str;
    use parse::Parser;

    fn codegen(input: &str) -> String {
        let parse = Parser::new(input.as_bytes()).stmts().unwrap();
        println!("{:?}", parse);
        let mut buf: Vec<u8> = Vec::new();
        {
            let mut w = Writer::new(&mut buf);
            w.stmts(&parse).unwrap();
        }
        let out = String::from_utf8(buf).unwrap();
        println!("{:?}", out);
        out
    }

    #[test]
    fn test_write() {
        assert_eq!(codegen("foo + bar"), "foo+bar");
    }

    #[test]
    fn test_prec() {
        assert_eq!(codegen("a = b = c"), "a=b=c");
        assert_eq!(codegen("(a = b) = c"), "(a=b)=c");

        assert_eq!(
            codegen("a = ((b = c) ? (d = e) : (f = g))"),
            "a=(b=c)?d=e:f=g"
        );

        // assert_eq!(codegen("new (!a).b(c, (d = e))[f]"), "new (!a).b(c,d=e)[f]");
    }

    #[test]
    fn test_func_expr() {
        assert_eq!(codegen("function(){}"), "function(){}");
        assert_eq!(codegen("(function(){})()"), "(function(){})()");
    }

    #[test]
    fn test_whitespace() {
        assert_eq!(codegen("1 += +2;"), "1+=+2");
        assert_eq!(codegen("return typeof a;"), "return typeof a");
        assert_eq!(codegen("if (a) b; else if (c) d;"), "if(a)b;else if(c)d");
    }

    #[test]
    fn test_switch() {
        assert_eq!(
            codegen("switch (a) { case b: case 3: z; default: q; }"),
            "switch(a){case b:case 3:z;default:q}"
        );
    }

    #[test]
    fn test_obj_props() {
        assert_eq!(
            codegen("x = {a:0,'b':0,'c?':0,'e f':0};"),
            "x={a:0,b:0,\"c?\":0,\"e f\":0}"
        );
    }

    #[test]
    fn test_regex() {
        assert_eq!(codegen("x = /a/g;"), "x=/a/g");
    }

    #[test]
    fn test_plusplus() {
        assert_eq!(codegen("++x--"), "++x--");
    }

    #[test]
    fn test_comma() {
        assert_eq!(codegen("var x = 3, y = 4;x=3,y=4;"), "var x=3,y=4;x=3,y=4");
    }

    #[test]
    fn test_for() {
        // assert_eq!(codegen("for (;;);"), "for(;;)");
        // assert_eq!(codegen("for (var x = 3; a; b);"), "for(var x=3;a;b)");
        assert_eq!(
            codegen("for (var x = 3, y = 4; a; b);"),
            "for(var x=3,y=4;a;b)"
        );
        assert_eq!(codegen("for (x = 3; a; b);"), "for(x=3;a;b)");
        assert_eq!(codegen("for (x = 3, y = 4; a; b);"), "for(x=3,y=4;a;b)");
        assert_eq!(codegen("for (var x in a);"), "for(var x in a)");
        assert_eq!(codegen("for (x in a);"), "for(x in a)");
    }

    #[test]
    fn test_number() {
        assert_eq!(codegen("1"), "1");
        assert_eq!(codegen("1.1"), "1.1");
        assert_eq!(codegen("0xb"), "11");
        assert_eq!(codegen("1e3"), "1000");
        // assert_eq!(codegen("1000"), "1e3");
    }

    #[test]
    fn test_string() {
        assert_eq!(codegen("'a\\nb'"), "\"a\\nb\"");
        assert_eq!(codegen(r#""a'b""#), r#""a'b""#);
        assert_eq!(codegen(r#"'a"b'"#), r#"'a"b'"#);
    }

    #[test]
    fn test_vars() {
        assert_eq!(codegen("var x = 3, y = 4;"), "var x=3,y=4");
    }

    #[test]
    fn test_object() {
        // TODO: parens around obj literal.
        assert_eq!(codegen(r"({
  plain: 0,
  'string': 1,
  pun,
  func() {},
  explicit_func: function() {},
  'with space': function() {},
});"), "{plain:0,string:1,pun,func(){},explicit_func(){},\"with space\":function(){}}");
    }
}
