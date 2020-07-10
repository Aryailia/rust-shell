//run: cargo test parser_tests -- --nocapture
#![allow(dead_code, unused_imports)]

mod helpers;

use async_std::task;
use futures::{stream, stream::Stream, StreamExt};
use helpers::{split_inclusive, OwnedToOption, TextGridWalk};

//enum Token {
//    ident,
//}

//const FLAG_COMMENT_STARTED: u8 = 0x01;
const DQUOTE_BEGIN: Flag = Flag(0x04);
//const FLAG_NON_NEWLINE_WHITESPACE: u8 = 0x08;

struct Flag(u8);
use std::ops::BitOr;

impl Flag {
    fn new() -> Self {
        Flag(0)
    }
    fn reset(&mut self) {
        *self = Self::new()
    }
    fn is(&self, rhs: Self) -> bool {
        self.0 & rhs.0 > 0
    }
    fn set(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
    fn unset(self, rhs: Self) -> Self {
        Self(self.0 & !rhs.0)
    }
}

impl BitOr for Flag {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        Self(self.0 & rhs.0)
    }
}

use std::borrow::Cow;

#[derive(Debug)]
enum Lexeme<'a> {
    Text(Cow<'a, str>),
    Comment(&'a str),
    Separator,
    EndOfCommand,
}

struct LexerState<'a> {
    buffer: &'a str,
    buffer_walker: TextGridWalk<'a>,
    token_start: usize,
    flags: Flag,
}

impl<'a> LexerState<'a> {
    fn new(buffer: &'a str) -> Self {
        Self {
            buffer,
            buffer_walker: TextGridWalk::new(buffer),
            token_start: 0,
            flags: Flag::new(),
        }
    }

    // Token methods
    fn exclusive_break(&mut self) -> Option<&str> {
        let walker = &mut self.buffer_walker;
        let start = self.token_start;
        let index = walker.row_start_index + walker.col_index;
        self.token_start = index + 1;
        (start < index).to_some(&self.buffer[start..index])
    }

    fn inclusive_break(&mut self) -> &str {
        let walker = &mut self.buffer_walker;
        let start_index = self.token_start;
        let end_pos = walker.row_start_index + walker.col_index + 1;
        //assert!(start_index < end_pos)
        self.token_start = end_pos;
        &self.buffer[start_index..end_pos]
    }

    fn rest_of_line(&mut self) -> &str {
        let walker = &mut self.buffer_walker;
        self.token_start += walker.cur_line.len();
        // NOTE: 'walker.walk_to_line_end()' does not update 'walker.col_index'
        walker.walk_to_line_end()
    }

    fn is(&self, rhs: Flag) -> bool {
        self.flags.is(rhs)
    }
    fn is_not(&self, rhs: Flag) -> bool {
        !self.flags.is(rhs)
    }

    fn reset_flags(&mut self) {
        self.flags.reset();
    }

    // Do not parse shebang
    fn lex(&mut self) {
        while let Some(ch) = self.buffer_walker.next() {
            //if col == 0 {
            //    println!("Line: {:?}", line);
            //}
            match ch {
                '\\' => {
                    let walker = &mut self.buffer_walker;
                    let cur_index = walker.row_start_index + walker.col_index;
                    emit(self.exclusive_break());
                    // POSIX does not specify what to do with dangling POSIX
                    // NOTE: Backslash before EOF is intepreted literally in
                    // dash
                    let walker = &mut self.buffer_walker;
                    let i = match walker.next() {
                        // Index of the next character
                        // same as 'walker.row_start_index + walker.col_index'
                        Some(_) => self.token_start,  // exclusive_break() sets
                        // Danging backslash is returned
                        None => cur_index,
                    };
                    self.token_start += 1; // +2 total due to 'walker.next()'
                    let escape_target = &self.buffer[i..i + 1];
                    emit(Lexeme::Text(escape_target.into()));
                }

                '\n' | '\r' => {
                    if let Some(x) = self.exclusive_break() {
                        emit(Lexeme::Text(x.into()));
                        emit(Lexeme::EndOfCommand);
                    } // 'None' means just whitespace before
                }

                //' ' if self.is_not(DQUOTE_BEGIN) => {
                //    emit(self.exclusive_break())
                //}
                '#' if self.is_not(DQUOTE_BEGIN) => {
                    emit(Lexeme::Comment(self.rest_of_line()));
                }
                _ => {}
            }
        }
    }
}

fn emit<T: std::fmt::Debug>(token: T) {
    println!("Token: {:?}", token);
}

async fn lexer(mut stream_of_scripts: impl Stream<Item = &str> + Unpin) {
    while let Some(body) = stream_of_scripts.next().await {
        // Maybe parse shebang?
        let mut str_lexer = LexerState::new(body);
        str_lexer.lex();
    }
}

#[cfg(test)]
mod parser_tests {
    use super::*;

    const SCRIPT: &str = r##"#!/bin/ashell
main() {
  # yo
  asdf='hello'
  printf %s\\n ${asdf}
}


"##;
    //#[test]
    fn grid() {
        TextGridWalk::new(SCRIPT).for_each(|a| println!("{:?}", a));
    }

    #[test]
    fn lexer_test() {
        task::block_on(run(SCRIPT));
    }

    async fn run(input: &str) {
        println!("I am doing things\n====");
        let script = stream::iter(vec![input]);
        lexer(script).await;
    }
}
