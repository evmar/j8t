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

struct Trace {
    log: bool,
    points: Vec<(usize, String)>,
}

impl Trace {
    fn new(log: bool) -> Trace {
        Trace {
            log: log,
            points: Vec::new(),
        }
    }

    fn measure<R, F: FnMut() -> R>(&mut self, msg: &str, mut f: F) -> R {
        let start = std::time::Instant::now();
        let r = f();
        let dur = std::time::Instant::now().duration_since(start);
        let time = (dur.as_secs() * 1000 + (dur.subsec_nanos() as u64 / 1_000_000)) as usize;
        if self.log {
            eprintln!("{} {}ms", msg, time);
        }
        self.points.push((time, msg.into()));
        r
    }
}

#[derive(PartialEq)]
enum Rename {
    Off,
    On,
    Debug,
}

struct Invocation {
    infile: String,
    timing: bool,
    fmt: bool,
    rename: Rename,
}

fn parse_options(args: &[String]) -> std::result::Result<Invocation, String> {
    let mut parser = getopts::Options::new();
    parser.optflag("h", "help", "");
    parser.optflag("", "timing", "");
    parser.optflag("", "fmt", "");
    parser.optflagopt("", "rename", "", "MODE");
    let matches = match parser.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => {
            return Err(format!("error: {}", f.to_string()));
        }
    };
    if matches.free.len() != 1 {
        return Err("specify input".to_string());
    }
    let infile = &matches.free[0];
    let timing = matches.opt_present("timing");
    let fmt = matches.opt_present("fmt");
    let rename = match matches.opt_str("rename") {
        None => Rename::Off,
        Some(ref s) if s == "debug" => Rename::Debug,
        Some(ref s) if s == "off" => Rename::On,
        Some(ref s) => {
            return Err(format!("bad --rename: {}", s));
        }
    };
    Ok(Invocation {
        infile: infile.to_string(),
        timing: timing,
        fmt: fmt,
        rename: rename,
    })
}

fn real_main() -> bool {
    //sizes();
    let args: Vec<String> = std::env::args().collect();
    let options = match parse_options(&args) {
        Ok(o) => o,
        Err(err) => {
            eprintln!("{}", err);
            return false;
        }
    };

    let mut trace = Trace::new(options.timing);

    let mut input = Vec::<u8>::new();
    trace.measure("read", || {
        std::fs::File::open(&options.infile)
            .unwrap()
            .read_to_end(&mut input)
            .unwrap();
    });

    let mut p = j8t::Parser::new(input.as_slice());
    let mut module = match trace.measure("parse", || p.module()) {
        Ok(stmts) => stmts,
        Err(err) => {
            print!("{}", err.pretty(&p.lexer));
            return false;
        }
    };

    let warnings = trace.measure("bind", || j8t::bind(&mut module));
    for w in warnings {
        println!("warn: {}", w);
    }

    trace.measure("eval", || j8t::eval(&mut module));

    if options.rename != Rename::Off {
        trace.measure("rename", || {
            j8t::rename(&mut module, options.rename == Rename::Debug)
        });
    }

    trace.measure("deblock", || j8t::deblock(&mut module));

    let mut w: Box<Write> = if options.fmt {
        Box::new(FmtWrite::new().unwrap())
    } else {
        Box::new(std::io::BufWriter::new(std::io::stdout()))
    };
    trace.measure("write", || {
        {
            let mut writer = j8t::Writer::new(&mut w);
            writer.disable_asi = options.fmt;
            writer.module(&module).unwrap();
        }
        w.flush().unwrap();
    });
    return true;
}

fn main() {
    std::process::exit(if real_main() { 0 } else { 1 });
}
