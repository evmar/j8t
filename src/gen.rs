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
use ast::{Expr, ExprNode};
use std::io;
use std::io::Write;

trait Prec {
    fn prec(&self) -> i8;
}

impl Prec for ast::BinOp {
    fn prec(&self) -> i8 {
        match *self {
            ast::BinOp::StarStar => 15,

            ast::BinOp::Star | ast::BinOp::Div | ast::BinOp::Percent => 14,

            ast::BinOp::Plus | ast::BinOp::Minus => 13,

            ast::BinOp::LTLT | ast::BinOp::GTGT | ast::BinOp::GTGTGT => 12,

            ast::BinOp::LT
            | ast::BinOp::GT
            | ast::BinOp::LTE
            | ast::BinOp::GTE
            | ast::BinOp::In
            | ast::BinOp::InstanceOf => 11,

            ast::BinOp::EqEq | ast::BinOp::NEq | ast::BinOp::EqEqEq | ast::BinOp::NEqEq => 10,

            ast::BinOp::BAnd => 9,
            ast::BinOp::Xor => 8,
            ast::BinOp::BOr => 7,
            ast::BinOp::AndAnd => 6,
            ast::BinOp::OrOr => 5,

            ast::BinOp::Eq
            | ast::BinOp::PlusEq
            | ast::BinOp::MinusEq
            | ast::BinOp::StarEq
            | ast::BinOp::PercentEq
            | ast::BinOp::StarStarEq
            | ast::BinOp::LTLTEq
            | ast::BinOp::GTGTEq
            | ast::BinOp::GTGTGTEq
            | ast::BinOp::AndEq
            | ast::BinOp::OrEq
            | ast::BinOp::CaratEq
            | ast::BinOp::DivEq => 3,

            ast::BinOp::Comma => 0,
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
    name.len() == 0 || name.bytes().any(|c| class(c) != CharClass::Letter)
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

    fn sym(&mut self, s: &ast::RefSym) -> Result {
        let str = &*s.borrow().name;
        self.token(str)
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
        self.func_from_name(f)
    }

    fn func_from_name(&mut self, f: &ast::Function) -> Result {
        if let Some(ref name) = f.name {
            self.sym(&name)?;
        }
        self.func_from_paren(&f.func)
    }

    fn func_from_paren(&mut self, f: &ast::Func) -> Result {
        self.paren(|w| {
            w.comma(&f.params, |w, &(ref param, ref init)| {
                w.binding_pattern(param)?;
                w.maybe_init(init)?;
                Ok(())
            })
        })?;
        self.brace(|w| {
            w.dump_scope(&f.scope)?;
            for s in f.body.iter() {
                w.stmt(s)?;
            }
            Ok(())
        })?;
        Ok(())
    }

    // Return the property name as a string if it's allowed to be bunned.
    fn property_name<'p>(&mut self, prop: &'p ast::PropertyName) -> io::Result<Option<&'p str>> {
        Ok(match *prop {
            ast::PropertyName::String(ref s) => {
                let quoted = obj_prop_needs_quote(s);
                if quoted {
                    self.token(&format!("{:?}", s))?;
                    None
                } else {
                    self.token(s)?;
                    Some(s)
                }
            }
            ast::PropertyName::Number(ref n) => {
                self.token(&format!("{}", n))?;
                None
            }
            ast::PropertyName::Computed(ref prop) => {
                self.wrap('[', ']', |w| w.exprn(prop, 0))?;
                None
            }
        })
    }

    fn class(&mut self, c: &ast::Class) -> Result {
        self.token("class")?;
        if let Some(ref name) = c.name {
            self.sym(name)?;
        }
        self.brace(|w| {
            for method in c.methods.iter() {
                if method.is_static {
                    w.token("static")?;
                }
                w.property_name(&method.name)?;
                w.func_from_paren(&method.func)?;
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

    fn template(&mut self, template: &ast::Template) -> Result {
        self.write_char('`')?;
        self.write(template.literal.as_bytes())?;
        self.write_char('`')?;
        Ok(())
    }

    fn exprn(&mut self, e: &ExprNode, prec: i8) -> Result {
        self.expr(&e.1, prec)
    }
    fn expr(&mut self, e: &Expr, prec: i8) -> Result {
        match *e {
            ast::Expr::EmptyParens => unreachable!(),
            ast::Expr::This => self.token("this")?,
            ast::Expr::Ident(ref s) => self.sym(&s)?,
            ast::Expr::Null => self.token("null")?,
            ast::Expr::Undefined => self.token("undefined")?,
            ast::Expr::Bool(b) => self.token(if b { "true" } else { "false" })?,
            ast::Expr::Number(ref n) => self.token(&format!("{}", n))?,
            ast::Expr::String(ref s) => self.quoted(s)?,
            ast::Expr::Array(ref arr) => {
                self.wrap('[', ']', |w| w.comma(arr, |w, e| w.exprn(e, 0)))?;
            }
            ast::Expr::Spread(ref expr) => {
                self.token("...")?;
                self.exprn(expr, 19)?;
            }
            ast::Expr::Object(ref obj) => {
                self.brace(|w| {
                    w.comma(&obj.props, |w, p| {
                        // Write the property name, and grab the name string if
                        // it's allowed to be punned.
                        let name = w.property_name(&p.name)?;
                        let wrote = if let Some(n) = name {
                            match p.value.1 {
                                ast::Expr::Ident(ref v) if *n == *v.borrow().name => {
                                    // omit; implied by property name.
                                    true
                                }
                                ast::Expr::Function(ref f) => {
                                    if let Some(ref fname) = f.name {
                                        if *n == *fname.borrow().name {
                                            // TODO: this always clobbers the function name.
                                            // Do we care?
                                            w.func_from_paren(&f.func)?;
                                            true
                                        } else {
                                            false
                                        }
                                    } else {
                                        false
                                    }
                                }
                                _ => false,
                            }
                        } else {
                            false
                        };
                        if !wrote {
                            w.token(":")?;
                            w.exprn(&p.value, 0)?;
                        }
                        Ok(())
                    })
                })?;
            }
            ast::Expr::Function(ref f) => {
                let needs_parens = self.ofs == self.statement_start;
                self.maybe_paren(needs_parens, |w| w.function(f))?;
            }
            ast::Expr::ArrowFunction(ref f) => {
                self.maybe_paren(prec > 3, |w| {
                    let needs_parens = f.params.len() == 1 && f.params[0].0.is_name();
                    w.maybe_paren(needs_parens, |w| {
                        w.comma(&f.params, |w, &(ref pat, ref init)| {
                            w.binding_pattern(pat)?;
                            w.maybe_init(init)?;
                            Ok(())
                        })
                    })?;
                    w.token("=>")?;
                    match f.body {
                        ast::ArrowBody::Expr(ref expr) => w.exprn(expr, 3),
                        ast::ArrowBody::Stmts(ref stmts) => w.brace(|w| {
                            for s in stmts.iter() {
                                w.stmt(s)?;
                            }
                            Ok(())
                        }),
                    }
                })?;
            }
            ast::Expr::Class(ref class) => self.class(class)?,
            ast::Expr::Regex(ref regex) => self.token(&regex.literal)?,
            ast::Expr::Template(ref template) => self.template(template)?,
            ast::Expr::Index(ref expr, ref index) => {
                self.maybe_paren(prec > 19, |w| {
                    w.exprn(expr, 19)?;
                    w.wrap('[', ']', |w| w.exprn(index, -1))?;
                    Ok(())
                })?;
            }
            ast::Expr::Field(ref expr, ref field) => {
                self.maybe_paren(prec > 19, |w| {
                    w.exprn(expr, 19)?;
                    w.token(".")?;
                    w.token(field)?;
                    Ok(())
                })?;
            }
            ast::Expr::New(ref expr) => {
                self.maybe_paren(prec > 18, |w| {
                    w.token("new")?;
                    w.exprn(expr, 18)?;
                    Ok(())
                })?;
            }
            ast::Expr::Call(ref call) => {
                self.maybe_paren(prec > 19, |w| {
                    w.exprn(&call.func, 19)?;
                    w.paren(|w| w.comma(&call.args, |w, e| w.exprn(e, 0)))?;
                    Ok(())
                })?;
            }
            ast::Expr::Unary(ref op, ref expr) => match *op {
                ast::UnOp::PostPlusPlus | ast::UnOp::PostMinusMinus => {
                    self.maybe_paren(prec > 17, |w| {
                        w.exprn(expr, 17)?;
                        w.token(&op.to_string())?;
                        Ok(())
                    })?;
                }
                _ => {
                    self.maybe_paren(prec > 16, |w| {
                        w.token(&op.to_string())?;
                        w.exprn(expr, 16)?;
                        Ok(())
                    })?;
                }
            },
            ast::Expr::Binary(ref bin) => {
                let p = bin.op.prec();
                self.maybe_paren(prec > p, |w| {
                    w.exprn(&bin.lhs, p)?;
                    w.token(&bin.op.to_string())?;
                    w.exprn(&bin.rhs, p)?;
                    Ok(())
                })?;
            }
            ast::Expr::TypeOf(ref expr) => {
                self.token("typeof")?;
                self.exprn(expr, 16)?;
            }
            ast::Expr::Ternary(ref t) => {
                self.maybe_paren(prec > 4, |w| {
                    w.exprn(&t.condition, 5)?;
                    w.token("?")?;
                    w.exprn(&t.iftrue, 3)?;
                    w.token(":")?;
                    w.exprn(&t.iffalse, 3)?;
                    Ok(())
                })?;
            }
            ast::Expr::Assign(ref lhs, ref rhs) => {
                self.maybe_paren(prec > 3, |w| {
                    w.exprn(lhs, 4)?;
                    w.token("=")?;
                    w.exprn(rhs, 3)?;
                    Ok(())
                })?;
            }
        }
        Ok(())
    }

    fn maybe_init(&mut self, init: &Option<ExprNode>) -> Result {
        if let Some(ref init) = *init {
            self.token("=")?;
            self.exprn(init, 3)?;
        }
        Ok(())
    }

    fn binding_pattern(&mut self, pattern: &ast::BindingPattern) -> Result {
        match *pattern {
            ast::BindingPattern::Name(ref name) => self.sym(name)?,
            ast::BindingPattern::Object(ref obj) => self.brace(|w| {
                w.comma(&obj.props, |w, &(ref name, (ref pat, ref init))| {
                    w.property_name(name)?;
                    w.token(":")?;
                    w.binding_pattern(pat)?;
                    w.maybe_init(init)?;
                    Ok(())
                })
            })?,
            ast::BindingPattern::Array(ref arr) => self.wrap('[', ']', |w| {
                w.comma(&arr.elems, |w, &(ref pat, ref init)| {
                    w.binding_pattern(pat)?;
                    w.maybe_init(init)?;
                    Ok(())
                })
            })?,
        }
        Ok(())
    }

    fn var_decl(&mut self, decl: &ast::VarDecl) -> Result {
        self.binding_pattern(&decl.pattern)?;
        if let Some((_, ref init)) = decl.init {
            self.token("=")?;
            self.expr(init, -1)?;
        }
        Ok(())
    }

    pub fn stmt(&mut self, s: &ast::Stmt) -> Result {
        self.statement_start = self.ofs;
        match *s {
            ast::Stmt::Block(ref stmts) => {
                self.brace(|w| {
                    for s in stmts.iter() {
                        w.stmt(s)?;
                    }
                    Ok(())
                })?;
            }
            ast::Stmt::Var(ref decl) => {
                self.token("var")?;
                self.comma(&decl.decls, |w, decl| w.var_decl(decl))?;
                self.semi()?;
            }
            ast::Stmt::Empty => self.semi()?,
            ast::Stmt::Expr(ref e) => {
                self.exprn(e, -1)?;
                self.semi()?;
            }
            ast::Stmt::If(ref i) => {
                self.token("if")?;
                self.paren(|w| w.expr(&i.cond, -1))?;
                self.stmt(&i.iftrue)?;
                if let Some(ref else_) = i.else_ {
                    self.token("else")?;
                    self.stmt(else_)?;
                }
            }
            ast::Stmt::While(ref wh) => {
                self.token("while")?;
                self.paren(|w| w.expr(&wh.cond, -1))?;
                self.stmt(&wh.body)?;
            }
            ast::Stmt::DoWhile(ref wh) => {
                self.token("do")?;
                self.stmt(&wh.body)?;
                self.token("while")?;
                self.paren(|w| w.expr(&wh.cond, -1))?;
                self.semi()?;
            }
            ast::Stmt::For(ref f) => {
                self.token("for")?;
                self.paren(|w| {
                    match f.init {
                        ast::ForInit::Empty => {}
                        ast::ForInit::Expr(ref e) => w.exprn(e, -1)?,
                        ast::ForInit::Decls(ref decls) => {
                            if decls.decls.len() > 0 {
                                w.token(decls.typ.to_string())?;
                            }
                            w.comma(&decls.decls, |w, d| w.var_decl(d))?;
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
            ast::Stmt::ForInOf(ref f) => {
                self.token("for")?;
                self.paren(|w| {
                    if let Some(decl_type) = f.decl_type {
                        w.token(decl_type.to_string())?;
                    }
                    w.binding_pattern(&f.loop_var)?;
                    w.token("in")?; // TODO
                    w.exprn(&f.expr, -1)?;
                    Ok(())
                })?;
                self.stmt(&f.body)?;
            }
            ast::Stmt::Switch(ref switch) => {
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
            ast::Stmt::Break(ref label) => {
                self.token("break")?;
                if let &Some(ref label) = label {
                    self.token(label)?;
                }
                self.semi()?;
            }
            ast::Stmt::Continue(ref label) => {
                self.token("continue")?;
                if let &Some(ref label) = label {
                    self.token(label)?;
                }
                self.semi()?;
            }
            ast::Stmt::Return(ref expr) => {
                self.token("return")?;
                if let &Some(ref expr) = expr {
                    self.expr(expr, -1)?;
                }
                self.semi()?;
            }
            ast::Stmt::Label(ref label) => {
                self.token(&label.label)?;
                self.token(":")?;
                self.stmt(&label.stmt)?;
            }
            ast::Stmt::Throw(ref expr) => {
                self.token("throw")?;
                self.expr(expr, -1)?;
                self.semi()?;
            }
            ast::Stmt::Try(ref try) => {
                self.token("try")?;
                self.stmt(&try.block)?;
                if let Some((ref decl, ref stmt)) = try.catch {
                    self.token("catch")?;
                    self.paren(|w| w.binding_pattern(decl))?;
                    self.stmt(stmt)?;
                }
                if let Some(ref finally) = try.finally {
                    self.token("finally")?;
                    self.stmt(finally)?;
                }
            }
            ast::Stmt::Function(ref f) => self.function(f)?,
            ast::Stmt::Class(ref c) => self.class(c)?,
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
        self.dump_scope(&module.scope)?;
        self.stmts(&module.stmts)
    }

    pub fn dump_scope(&mut self, scope: &ast::Scope) -> Result {
        if false {
            self.write(b"/*\n")?;
            for b in scope.bindings.iter() {
                self.write(format!("{}\n", b.borrow().name).as_bytes())?;
            }
            self.write(b"*/\n")?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parse::Parser;
    use std::str;

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
    fn write() {
        assert_eq!(codegen("foo + bar"), "foo+bar");
    }

    #[test]
    fn prec() {
        assert_eq!(codegen("a = b = c"), "a=b=c");
        assert_eq!(codegen("(a = b) = c"), "(a=b)=c");

        assert_eq!(
            codegen("a = ((b = c) ? (d = e) : (f = g))"),
            "a=(b=c)?d=e:f=g"
        );

        // assert_eq!(codegen("new (!a).b(c, (d = e))[f]"), "new (!a).b(c,d=e)[f]");
    }

    #[test]
    fn func_expr() {
        assert_eq!(codegen("function(){}"), "function(){}");
        assert_eq!(codegen("(function(){})()"), "(function(){})()");
    }

    #[test]
    fn whitespace() {
        assert_eq!(codegen("1 += +2;"), "1+=+2");
        assert_eq!(codegen("return typeof a;"), "return typeof a");
        assert_eq!(codegen("if (a) b; else if (c) d;"), "if(a)b;else if(c)d");
    }

    #[test]
    fn switch() {
        assert_eq!(
            codegen("switch (a) { case b: case 3: z; default: q; }"),
            "switch(a){case b:case 3:z;default:q}"
        );
    }

    #[test]
    fn obj_props() {
        assert_eq!(
            codegen("x = {a:0,'b':0,'c?':0,'e f':0};"),
            "x={a:0,b:0,\"c?\":0,\"e f\":0}"
        );
    }

    #[test]
    fn regex() {
        assert_eq!(codegen("x = /a/g;"), "x=/a/g");
    }

    #[test]
    fn plusplus() {
        assert_eq!(codegen("++x--"), "++x--");
    }

    #[test]
    fn comma() {
        assert_eq!(codegen("var x = 3, y = 4;x=3,y=4;"), "var x=3,y=4;x=3,y=4");
    }

    #[test]
    fn for_variants() {
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
    fn number() {
        assert_eq!(codegen("1"), "1");
        assert_eq!(codegen("1.1"), "1.1");
        assert_eq!(codegen("0xb"), "11");
        assert_eq!(codegen("1e3"), "1000");
        // assert_eq!(codegen("1000"), "1e3");
    }

    #[test]
    fn string() {
        assert_eq!(codegen("'a\\nb'"), "\"a\\nb\"");
        assert_eq!(codegen(r#""a'b""#), r#""a'b""#);
        assert_eq!(codegen(r#"'a"b'"#), r#"'a"b'"#);
    }

    #[test]
    fn vars() {
        assert_eq!(codegen("var x = 3, y = 4;"), "var x=3,y=4");
    }

    #[test]
    fn object() {
        // TODO: parens around obj literal.
        assert_eq!(
            codegen(
                r"({
  plain: 0,
  0: number,
  'string': 1,
  pun,
  func() {},
  explicit_func: function() {},
  'with space': function() {},
});",
            ),
            "{plain:0,0:number,string:1,pun,func(){},explicit_func:function(){},\"with space\":function(){}}"
        );
    }
}
