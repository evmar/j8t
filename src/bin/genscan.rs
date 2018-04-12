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

const LICENSE: &'static str = r#"/*
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
"#;

struct Trie {
    edges: Vec<(u8, Trie)>,
    matchers: Vec<String>,
    action: Option<String>,
}

impl Trie {
    fn new() -> Trie {
        Trie {
            edges: Vec::new(),
            matchers: Vec::new(),
            action: None,
        }
    }
    fn insert(&mut self, mut tok: std::str::Bytes, action: String) {
        match tok.next() {
            Some(c) => {
                for &mut (ec, ref mut t) in self.edges.iter_mut() {
                    if ec == c {
                        t.insert(tok, action);
                        return;
                    }
                }
                let mut t = Trie::new();
                t.insert(tok, action);
                self.edges.push((c, t));
            }
            None => {
                self.action = Some(action);
            }
        }
    }
    fn sort(&mut self) {
        self.edges.sort_by(|a, b| a.0.cmp(&b.0));
        for &mut (_, ref mut e) in self.edges.iter_mut() {
            e.sort();
        }
    }
}

struct CodeGen<'a> {
    w: &'a mut Write,
}

type Result = io::Result<()>;

impl<'a> Write for CodeGen<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.w.write(buf)
    }
    fn flush(&mut self) -> io::Result<()> {
        self.w.flush()
    }
}

impl<'a> CodeGen<'a> {
    fn brace<F: FnOnce(&mut CodeGen) -> Result>(&mut self, f: F) -> Result {
        writeln!(self.w, "{{")?;
        f(self)?;
        writeln!(self.w, "}}")?;
        Ok(())
    }

    fn write_match<F: FnOnce(&mut CodeGen) -> Result>(&mut self, expr: &str, f: F) -> Result {
        write!(self.w, "match {} ", expr)?;
        self.brace(f)?;
        Ok(())
    }
}

fn gen_match(w: &mut CodeGen, t: &Trie) -> io::Result<()> {
    if t.edges.len() > 0 {
        w.write_match("s.read() as char", |w| {
            for &(c, ref next) in &t.edges {
                writeln!(w, "{:?} => {{", c as char)?;
                gen_match(w, next)?;
                writeln!(w, "}}")?;
            }
            for matcher in &t.matchers {
                writeln!(w, "{}", matcher)?;
            }
            if let Some(ref action) = t.action {
                writeln!(w, "_ => {{")?;
                writeln!(w, "s.back();")?;
                writeln!(w, "{}", action)?;
                writeln!(w, "}}")?;
            } else {
                writeln!(w, "c => panic!(\"xxx {{:?}}\", c)")?;
            }
            Ok(())
        })?;
    } else {
        if let Some(ref action) = t.action {
            writeln!(w, "{}", action)?;
        } else {
            panic!("no action");
        }
    }
    Ok(())
}

fn gen_trie(w: &mut CodeGen, t: &Trie) -> io::Result<()> {
    writeln!(
        w,
        "pub fn sc(s: &mut Scanner, data: &mut TokData) -> Result<Tok>"
    )?;
    w.brace(|w| {
        write!(w, "Ok(")?;
        gen_match(w, t)?;
        write!(w, ")")
    })
}

type StrTab = &'static [(&'static str, &'static str)];

// 11.7 Punctuators
const TOKS: StrTab = &[
    ("{", "LBrace"),
    ("(", "LParen"),
    (")", "RParen"),
    ("[", "LSquare"),
    ("]", "RSquare"),
    (".", "Dot"),
    ("...", "Ellipsis"),
    (";", "Semi"),
    ("?", "Question"),
    (":", "Colon"),
    ("}", "RBrace"),
];

const UNOPS: StrTab = &[
    ("!", "Not"),
    ("~", "BNot"),
    ("++", "PlusPlus"),
    ("--", "MinusMinus"),
];

const BINOPS: StrTab = &[
    ("=", "Eq"),
    ("<", "LT"),
    (">", "GT"),
    ("<=", "LTE"),
    (">=", "GTE"),
    ("==", "EqEq"),
    ("!=", "NEq"),
    ("===", "EqEqEq"),
    ("!==", "NEqEq"),
    ("+", "Plus"),
    ("-", "Minus"),
    ("*", "Star"),
    ("%", "Percent"),
    ("**", "StarStar"),
    ("<<", "LTLT"),
    (">>", "GTGT"),
    (">>>", "GTGTGT"),
    ("&", "BAnd"),
    ("|", "BOr"),
    ("^", "Xor"),
    ("&&", "AndAnd"),
    ("||", "OrOr"),
    ("+=", "PlusEq"),
    ("-=", "MinusEq"),
    ("*=", "StarEq"),
    ("%=", "PercentEq"),
    ("**=", "StarStarEq"),
    ("<<=", "LTLTEq"),
    (">>=", "GTGTEq"),
    (">>>=", "GTGTGTEq"),
    ("&=", "AndEq"),
    ("|=", "OrEq"),
    ("^=", "CaratEq"),
    ("/", "Div"),
    ("/=", "DivEq"),
    (",", "Comma"),
];

const KWS: StrTab = &[
    ("await", "Await"),
    ("break", "Break"),
    ("case", "Case"),
    ("catch", "Catch"),
    ("class", "Class"),
    ("const", "Const"),
    ("continue", "Continue"),
    ("debugger", "Debugger"),
    ("default", "Default"),
    ("delete", "Delete"),
    ("do", "Do"),
    ("else", "Else"),
    ("export", "Export"),
    ("extends", "Extends"),
    ("finally", "Finally"),
    ("for", "For"),
    ("function", "Function"),
    ("if", "If"),
    ("import", "Import"),
    ("in", "In"),
    ("instanceof", "InstanceOf"),
    ("let", "Let"),
    ("new", "New"),
    ("return", "Return"),
    ("super", "Super"),
    ("switch", "Switch"),
    ("this", "This"),
    ("throw", "Throw"),
    ("try", "Try"),
    ("typeof", "TypeOf"),
    ("var", "Var"),
    ("void", "Void"),
    ("while", "While"),
    ("with", "With"),
    ("yield", "Yield"),
];

fn gen_kwhash(w: &mut CodeGen) -> Result {
    let mut by_len: Vec<Vec<(&'static str, &'static str)>> = Vec::new();
    for &(kw, action) in KWS {
        let len = kw.len();
        while len >= by_len.len() {
            by_len.push(Vec::new());
        }
        by_len[len].push((kw, action));
    }

    writeln!(w, "pub fn kw(text: &[u8]) -> Tok")?;
    w.brace(|w| {
        w.write_match("text.len()", |w| {
            for (i, ref kws) in by_len.iter().enumerate() {
                if kws.len() == 0 {
                    continue;
                }
                writeln!(w, "{} => {{", i)?;
                for &(kw, action) in kws.iter() {
                    writeln!(w, "if text == {:?}.as_bytes() {{", kw)?;
                    writeln!(w, "return Tok::{};", action)?;
                    writeln!(w, "}}")?;
                }
                writeln!(w, "}}")?;
            }
            writeln!(w, "_ => {{}}")
        })?;
        writeln!(w, "return Tok::Ident")
    })
}

fn gen_scan() -> Result {
    let mut f = std::fs::File::create("src/lex/scan.rs")?;
    let mut writer = CodeGen { w: &mut f };
    let w = &mut writer;

    writeln!(w, "{}", LICENSE)?;
    writeln!(w, "// GENERATED by genscan.rs")?;
    writeln!(w, "use lex::scanner::{{Result, Scanner}};")?;
    writeln!(w, "use lex::hand;")?;
    writeln!(w, "#[derive(Copy)]")?;
    writeln!(w, "#[derive(Clone)]")?;
    writeln!(w, "#[derive(Debug)]")?;
    writeln!(w, "#[derive(PartialEq)]")?;
    writeln!(w, "pub enum Tok")?;
    w.brace(|w| {
        writeln!(w, "EOF,")?;
        for &(_, action) in TOKS.iter().chain(UNOPS).chain(BINOPS).chain(KWS) {
            writeln!(w, "{},", action)?;
        }
        writeln!(w, "Comment,")?;
        writeln!(w, "String,")?;
        writeln!(w, "Number,")?;
        writeln!(w, "Ident,")
    })?;

    writeln!(w, "impl Tok")?;
    w.brace(|w| {
        writeln!(w, "pub fn is_kw(&self) -> bool")?;
        w.brace(|w| {
            w.write_match("self", |w| {
                writeln!(w, "&Tok::EOF => false,")?;
                for &(_, action) in TOKS.iter().chain(UNOPS).chain(BINOPS) {
                    writeln!(w, "&Tok::{} => false,", action)?;
                }
                for &(_, action) in KWS.iter() {
                    writeln!(w, "&Tok::{} => true,", action)?;
                }
                writeln!(w, "&Tok::Comment => false,")?;
                writeln!(w, "&Tok::String => false,")?;
                writeln!(w, "&Tok::Number => false,")?;
                writeln!(w, "&Tok::Ident => false,")
            })
        })
    })?;

    writeln!(w, "#[derive(Debug)]")?;
    writeln!(w, "pub enum TokData {{")?;
    writeln!(w, "    None,")?;
    writeln!(w, "    String(String),")?;
    writeln!(w, "    Number(f64),")?;
    writeln!(w, "}}")?;

    let mut trie = Trie::new();
    let mut eof = Trie::new();
    eof.action = Some(String::from("Tok::EOF"));
    trie.edges.push((0, eof));
    for &(tok, action) in TOKS.iter().chain(UNOPS).chain(BINOPS) {
        trie.insert(tok.bytes(), format!("Tok::{}", action));
    }
    trie.insert(
        "/*".bytes(),
        String::from("hand::block_comment(s)?; Tok::Comment"),
    );
    trie.insert(
        "//".bytes(),
        String::from("hand::line_comment(s); Tok::Comment"),
    );
    trie.insert(
        "'".bytes(),
        String::from("*data = TokData::String(hand::quoted(s, '\\'')?); Tok::String"),
    );
    trie.insert(
        "\"".bytes(),
        String::from("*data = TokData::String(hand::quoted(s, '\"')?); Tok::String"),
    );
    trie.matchers.push(String::from(
        "'0'...'9' => { s.back(); *data = TokData::Number(hand::number(s)); Tok::Number }",
    ));
    trie.matchers.push(String::from(
        "'a'...'z' | 'A'...'Z' | '_' | '$' => { hand::ident(s); Tok::Ident }",
    ));
    trie.sort();
    gen_trie(w, &mut trie)?;

    gen_kwhash(w)?;
    Ok(())
}

fn gen_ops() -> io::Result<()> {
    let mut f = std::fs::File::create("src/ops.rs")?;
    let mut writer = CodeGen { w: &mut f };
    let w = &mut writer;

    writeln!(w, "{}", LICENSE)?;
    writeln!(w, "// GENERATED by genscan.rs")?;
    writeln!(w, "use std::fmt;")?;
    writeln!(w, "use lex::Tok;")?;

    let extra_unops: StrTab = &[
        ("-", "Minus"),
        ("+", "Plus"),
        ("--", "PostMinusMinus"), // XXX see hack below.
        ("++", "PostPlusPlus"),
        ("delete", "Delete"),
        ("typeof", "TypeOf"),
        ("void", "Void"),
    ];
    let unops: Vec<(&'static str, &'static str)> =
        UNOPS.iter().chain(extra_unops).map(|o| *o).collect();
    let extra_binops: StrTab = &[("in", "In"), ("instanceof", "InstanceOf")];
    let binops: Vec<(&'static str, &'static str)> =
        BINOPS.iter().chain(extra_binops).map(|o| *o).collect();

    for &(name, ops) in &[("UnOp", unops.as_slice()), ("BinOp", binops.as_slice())] {
        writeln!(w, "#[derive(Debug)]")?;
        writeln!(w, "#[derive(PartialEq)]")?;
        writeln!(w, "pub enum {}", name)?;
        w.brace(|w| {
            for &(_, action) in ops.iter() {
                writeln!(w, "{},", action)?;
            }
            Ok(())
        })?;

        writeln!(w, "impl fmt::Display for {}", name)?;
        w.brace(|w| {
            writeln!(w, "fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result")?;
            w.brace(|w| {
                writeln!(w, "f.write_str(")?;
                w.write_match("self", |w| {
                    for &(t, action) in ops.iter() {
                        writeln!(w, "&{}::{} => {:?},", name, action, t)?;
                    }
                    Ok(())
                })?;
                writeln!(w, ")")
            })
        })?;

        writeln!(w, "impl {}", name)?;
        w.brace(|w| {
            writeln!(w, "pub fn from_tok(t: Tok) -> {}", name)?;
            w.brace(|w| {
                w.write_match("t", |w| {
                    for &(_, action) in ops.iter() {
                        if action == "PostPlusPlus" || action == "PostMinusMinus" {
                            continue;
                        }
                        writeln!(w, "Tok::{} => {}::{},", action, name, action)?;
                    }
                    writeln!(w, "_ => panic!(\"non-{} {{:?}}\", t)", name)
                })
            })
        })?;
    }
    Ok(())
}

fn main() {
    gen_scan().unwrap();
    gen_ops().unwrap();
}
