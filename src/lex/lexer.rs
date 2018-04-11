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

// The Lexer type, which reads JS tokens from an input Scanner.

use lex::scanner;
use lex::Tok;
use lex::hand;
use lex::scan;
use lex::scan::TokData;
use lex::scanner::Result;
use std;

#[derive(Clone, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span {
            start: start,
            end: end,
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
    pub saw_newline: bool,
    pub data: scan::TokData,
}

pub struct Lexer<'a> {
    pub scan: scanner::Scanner<'a>,
    pub lookahead: std::collections::VecDeque<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a [u8]) -> Lexer<'a> {
        Lexer {
            scan: self::scanner::Scanner::new(input),
            lookahead: std::collections::VecDeque::new(),
        }
    }

    pub fn read(&mut self) -> Result<Token> {
        if let Some(t) = self.lookahead.pop_front() {
            return Ok(t);
        }

        let mut saw_newline = false;
        loop {
            if hand::whitespace(&mut self.scan) {
                saw_newline = true;
            }
            let start = self.scan.pos;
            let mut data: scan::TokData = scan::TokData::None;
            let mut tok = scan::sc(&mut self.scan, &mut data)?;
            let end = self.scan.pos;
            if tok == Tok::Comment {
                continue;
            }
            if tok == Tok::Ident {
                tok = scan::kw(&self.scan.input[start..end]);
            }
            return Ok(Token {
                tok: tok,
                span: Span {
                    start: start,
                    end: end,
                },
                saw_newline: saw_newline,
                data: data,
            });
        }
    }

    pub fn peek(&mut self) -> Result<Tok> {
        if self.lookahead.is_empty() {
            let token = self.read()?;
            self.lookahead.push_back(token);
        }
        Ok(self.lookahead.front().unwrap().tok)
    }

    pub fn back(&mut self, token: Token) {
        self.lookahead.push_front(token);
    }

    pub fn span_text(&self, span: &Span) -> &'a str {
        // TODO: unsafe?
        if span.start < self.scan.input.len() {
            std::str::from_utf8(&self.scan.input[span.start..span.end]).unwrap()
        } else {
            std::str::from_utf8(&self.scan.input[0..0]).unwrap()
        }
    }

    pub fn text(&mut self, token: Token) -> String {
        if token.tok == Tok::String {
            if let TokData::String(s) = token.data {
                s
            } else {
                unreachable!();
            }
        } else {
            String::from(self.span_text(&token.span))
        }
    }

    pub fn read_regex(&mut self) -> Result<&'a str> {
        let start = self.scan.pos - 1;
        hand::regex(&mut self.scan);
        let end = self.scan.pos;
        Ok(self.span_text(&Span {
            start: start,
            end: end,
        }))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Tok;
    fn next(&mut self) -> Option<Tok> {
        match self.read().unwrap().tok {
            Tok::EOF => None,
            t => Some(t),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dots() {
        let l = Lexer::new("a.b.c".as_bytes());
        let toks: Vec<Tok> = l.collect();
        assert_eq!(
            toks.as_slice(),
            &[Tok::Ident, Tok::Dot, Tok::Ident, Tok::Dot, Tok::Ident]
        );
    }

    #[test]
    fn test_keywords() {
        let l = Lexer::new("func function".as_bytes());
        let toks: Vec<Tok> = l.collect();
        assert_eq!(toks.as_slice(), &[Tok::Ident, Tok::Function]);
    }
}
