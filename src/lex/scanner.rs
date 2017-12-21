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

// Source-level iterator (Scanner) and file context (for error messages).

use std;

pub struct Scanner<'a> {
    pub input: &'a [u8],
    pub pos: usize,
}

pub struct Context<'a> {
    pub source_line: &'a [u8],
    pub line: usize,
    pub col: usize,
}

#[derive(Debug)]
pub struct LexError {
    pub msg: String,
    pub pos: usize,
}

pub type Result<T> = std::result::Result<T, LexError>;

impl<'a> Scanner<'a> {
    pub fn new(input: &'a [u8]) -> Scanner<'a> {
        Scanner {
            input: input,
            pos: 0,
        }
    }

    pub fn err<S: Into<String>>(&self, msg: S) -> LexError {
        LexError {
            msg: msg.into(),
            pos: if self.pos > 0 { self.pos - 1 } else { self.pos },
        }
    }

    pub fn read(&mut self) -> u8 {
        let c = self.peek();
        self.pos += 1;
        c
    }

    pub fn back(&mut self) {
        if self.pos > 0 {
            self.pos -= 1;
        }
    }

    pub fn peek(&mut self) -> u8 {
        if self.pos >= self.input.len() {
            return 0;
        }
        self.input[self.pos]
    }

    pub fn next(&mut self) {
        self.pos += 1;
    }

    pub fn lit(&mut self, s: &str) -> bool {
        for c in s.bytes() {
            if self.read() != c {
                return false;
            }
        }
        return true;
    }

    pub fn context(&self, pos: usize) -> Context {
        let mut scan = Scanner {
            input: self.input,
            pos: 0,
        };
        let mut line = 1;
        let mut line_start = 0;
        let mut col = 1;
        for i in 0..pos {
            match scan.read() as char {
                '\n' => {
                    line += 1;
                    col = 1;
                    line_start = i + 1;
                }
                _ => col += 1,
            }
        }
        let mut line_end = line_start;
        for i in pos..std::cmp::min(pos + 80, self.input.len()) {
            match scan.read() as char {
                '\n' | '\0' => break,
                _ => {
                    line_end = i;
                }
            }
        }
        return Context {
            source_line: &self.input[line_start..line_end],
            line: line,
            col: col,
        };
    }
}
