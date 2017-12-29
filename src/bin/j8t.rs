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

extern crate j8t;
extern crate getopts;

use j8t::ast;
use j8t::lex::Tok;
use j8t::parse::*;
use j8t::gen;
use j8t::trans;
use j8t::eval;
use std::io;
use std::io::Read;
use std::io::Write;

#[allow(dead_code)]
fn sizes() {
    println!("tok is {} bytes", std::mem::size_of::<Tok>());
    println!("vec is {} bytes", std::mem::size_of::<Vec<ast::Expr>>());
    println!("box is {} bytes", std::mem::size_of::<Box<ast::Expr>>());
    println!("binop is {} bytes", std::mem::size_of::<ast::BinOp>());
    println!("string is {} bytes", std::mem::size_of::<String>());
    println!("expr is {} bytes", std::mem::size_of::<ast::Expr>());
    println!("stmt is {} bytes", std::mem::size_of::<ast::Stmt>());
}

struct FmtWrite {
    child: std::process::Child,
    w: Option<Box<Write>>,
}

impl FmtWrite {
    fn new() -> io::Result<FmtWrite> {
        let mut c = std::process::Command::new("clang-format")
            .arg("-assume-filename=js.js")
            .arg("-style=Google")
            .stdin(std::process::Stdio::piped())
            .spawn()?;
        let w = Box::new(c.stdin.take().unwrap());
        Ok(
            (FmtWrite {
                 child: c,
                 w: Some(w),
             }),
        )
    }
}

impl Write for FmtWrite {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        if let Some(ref mut w) = self.w {
            return w.write(buf);
        }
        Ok(0)
    }
    fn flush(&mut self) -> io::Result<()> {
        if let Some(ref mut w) = self.w {
            w.flush()?;
        }
        self.w = None;
        self.child.wait().unwrap();
        Ok(())
    }
}

fn measure<T, F: FnMut() -> T>(mut f: F) -> (u64, T) {
    let start = std::time::Instant::now();
    let r = f();
    let dur = std::time::Instant::now().duration_since(start);
    let time = dur.as_secs() * 1000 + (dur.subsec_nanos() as u64 / 1_000_000);
    (time, r)
}

fn load_externs() -> ast::Scope {
    let mut scope = ast::Scope::new();
    let mut input = Vec::<u8>::new();
    std::fs::File::open("externs.js")
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();
    let mut p = Parser::new(input.as_slice());
    let module = p.module().unwrap();
    for s in module.stmts {
        match s {
            ast::Stmt::Var(decls) => {
                for d in decls.decls {
                    match d.name {
                        ast::Expr::Ident(sym) => {
                            scope.bindings.push(sym);
                        }
                        _ => panic!("bad externs"),
                    }
                }
            }
            _ => panic!("bad externs"),
        }
    }
    return scope;
}

fn real_main() -> bool {
    //sizes();

    let args: Vec<String> = std::env::args().collect();
    let mut opts = getopts::Options::new();
    opts.optflag("h", "help", "");
    opts.optflag("", "timing", "");
    opts.optflag("", "fmt", "");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => {
            eprintln!("error: {}", f.to_string());
            return false;
        }
    };
    if matches.free.len() != 1 {
        eprintln!("specify input");
        return false;
    }
    let infile = &matches.free[0];
    let timing = matches.opt_present("timing");
    let fmt = matches.opt_present("fmt");

    let mut input = Vec::<u8>::new();
    std::fs::File::open(infile)
        .unwrap()
        .read_to_end(&mut input)
        .unwrap();

    let mut p = Parser::new(input.as_slice());
    let (t, mut module) = match measure(|| p.module()) {
        (t, Ok(stmts)) => (t, stmts),
        (_, Err(err)) => {
            err.print(&p.lexer);
            return false;
        }
    };
    if timing {
        eprintln!("parse: {}ms", t);
    }

    let externs = load_externs();
    let (t, _) = measure(|| { eval::scope(&externs, &mut module); });
    if timing {
        eprintln!("scope: {}ms", t);
    }

    let (t, _) = measure(|| { trans::deblock(&mut module); });
    if timing {
        eprintln!("deblock: {}ms", t);
    }

    let mut w: Box<Write> = if fmt {
        Box::new(FmtWrite::new().unwrap())
    } else {
        Box::new(std::io::BufWriter::new(std::io::stdout()))
    };
    let (t, _) = measure(|| {
        let mut writer = gen::Writer::new(&mut w);
        writer.disable_asi = fmt;
        writer.module(&module).unwrap();
    });
    if timing {
        eprintln!("gen: {}ms", t);
    }
    w.flush().unwrap();
    return true;
}

fn main() {
    std::process::exit(if real_main() { 0 } else { 1 });
}
