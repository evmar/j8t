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

extern crate getopts;
extern crate j8t;

use std::io;
use std::io::Read;
use std::io::Write;

#[allow(dead_code)]
fn sizes() {
    println!("tok is {} bytes", std::mem::size_of::<j8t::lex::Tok>());
    println!("token is {} bytes", std::mem::size_of::<j8t::lex::Token>());
    println!(
        "vec is {} bytes",
        std::mem::size_of::<Vec<j8t::ast::Expr>>()
    );
    println!(
        "box is {} bytes",
        std::mem::size_of::<Box<j8t::ast::Expr>>()
    );
    println!("binop is {} bytes", std::mem::size_of::<j8t::ast::BinOp>());
    println!("string is {} bytes", std::mem::size_of::<String>());
    println!("expr is {} bytes", std::mem::size_of::<j8t::ast::Expr>());
    println!(
        "exprnode is {} bytes",
        std::mem::size_of::<j8t::ast::ExprNode>()
    );
    println!("stmt is {} bytes", std::mem::size_of::<j8t::ast::Stmt>());
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
        Ok(FmtWrite {
            child: c,
            w: Some(w),
        })
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

fn parse_options(args: &[String]) -> std::result::Result<(j8t::Trace, j8t::Invocation), String> {
    let mut parser = getopts::Options::new();
    parser.optflag("h", "help", "");
    parser.optflag("", "timing", "");
    parser.optflag("", "fmt", "");
    parser.optflagopt("", "rename", "", "MODE");
    parser.optflag("", "exp", "");
    let matches = match parser.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => {
            return Err(format!("error: {}", f.to_string()));
        }
    };
    if matches.free.len() != 1 {
        return Err("specify input".to_string());
    }
    let filename = &matches.free[0];
    let timing = matches.opt_present("timing");
    let fmt = matches.opt_present("fmt");
    let rename = match matches.opt_str("rename") {
        None => j8t::Rename::On,
        Some(ref s) if s == "debug" => j8t::Rename::Debug,
        Some(ref s) if s == "off" => j8t::Rename::Off,
        Some(ref s) if s == "on" => j8t::Rename::On,
        Some(ref s) => {
            return Err(format!("bad --rename: {}", s));
        }
    };
    let exp = matches.opt_present("exp");
    Ok((
        j8t::Trace::new(timing, None),
        j8t::Invocation {
            filename: filename.to_string(),
            input: Vec::new(),
            fmt: fmt,
            rename: rename,
            exp: exp,
        },
    ))
}

fn real_main() -> bool {
    //sizes();
    let args: Vec<String> = std::env::args().collect();
    let (mut trace, mut inv) = match parse_options(&args) {
        Ok(r) => r,
        Err(err) => {
            eprintln!("{}", err);
            return false;
        }
    };

    trace.measure("read", || {
        std::fs::File::open(&inv.filename)
            .unwrap()
            .read_to_end(&mut inv.input)
            .unwrap();
    });

    let mut w: Box<Write> = if inv.fmt {
        Box::new(FmtWrite::new().unwrap())
    } else {
        Box::new(std::io::BufWriter::new(std::io::stdout()))
    };

    if let Err(err) = j8t::run(&mut trace, &inv, &mut w) {
        eprintln!("{}", err.pretty(&inv.input));
    }

    return true;
}

fn main() {
    std::process::exit(if real_main() { 0 } else { 1 });
}
