//run: cargo test parser_tests -- --nocapture
#![allow(dead_code, unused_imports)]

mod helpers;

use async_std::task;
use futures::{stream, stream::Stream, StreamExt};
use helpers::{split_inclusive, TextGridWalk};

//enum Token {
//    ident,
//}

enum Lexeme {
    Text,
    SingleQuote,
    DoubleQuote,
    Comment,
    Separator,
    EndOfCommand,
}

//const FLAG_COMMENT_STARTED: u8 = 0x01;
const SINGLE_QUOTE_STARTED: Flag = Flag(0x02);
const DOUBLE_QUOTE_STARTED: Flag = Flag(0x04);
const ESCAPED: Flag = Flag(0x08);
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

    fn emit_exclusive_token(&mut self) {
        let walker = &mut self.buffer_walker;
        let index = walker.row_start_index + walker.col_index;
        if self.token_start < index {
            emit(&self.buffer[self.token_start..index])
        }
        //emit("exclusive\n");
        self.token_start = index + 1;
    }

    fn emit_inclusive_token(&mut self) {
        let walker = &mut self.buffer_walker;
        let pos = walker.row_start_index + walker.col_index + 1;
        //assert!(self.token_start < pos)
        //println!("debug: {} {}", self.token_start, pos);
        emit(&self.buffer[self.token_start..pos]);
        self.token_start = pos;
    }

    fn emit_rest_of_line(&mut self) {
        let walker = &mut self.buffer_walker;
        // NOTE: 'walker.walk_to_line_end()' does not update 'walker.col_index'
        emit(walker.walk_to_line_end());
        // Does not matter if walk_to_next_line() returned None (means we end)
        self.token_start += walker.cur_line.len();
    }
    fn emit_end_of_statement(&self) {
        emit("\n");
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
                //'\\' if self.is(ESCAPED) => {
                //    self.emit_inclusive_token();
                //}
                // @TODO does not work if last character is backslash
                '\\' => {
                    self.emit_exclusive_token();
                    self.flags.set(ESCAPED);
                }
                '\n' | '\r' if self.is(ESCAPED) => {
                    self.emit_inclusive_token();
                }
                '\n' | '\r' => {
                    //print!("{}", );
                    self.emit_exclusive_token();
                    self.emit_end_of_statement();
                }

                '#' if self.is_not(ESCAPED | SINGLE_QUOTE_STARTED | DOUBLE_QUOTE_STARTED) => {
                    self.emit_rest_of_line();
                }
                _ => {}
            }
        }
    }
}

fn emit(str_view: &str) {
    println!("Token: \"{}\", ", str_view);
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




//async fn lexer2(mut stream_of_scripts: impl Stream<Item = &str> + Unpin) {
//    while let Some(body) = stream_of_scripts.next().await {
//        // State variables
//        //let mut escaped = true;
//        let mut state = LexerState::new(body);
//        let mut line_index = 0;
//
//        let mut line_iter = split_inclusive(body, '\n').enumerate();
//        while let Some((_row, line)) = line_iter.next() {
//            let mut char_iter = line.chars().enumerate();
//
//            // 'line_index' should increment for col
//            // as we might process entire line
//            while let Some((col, ch)) = char_iter.next() {
//                match ch {
//                    '\\' if state.is(ESCAPED) => {
//                        //println!("hello {}", &state.flags.0);
//                        state.emit_token_end(line_index, col);
//                        //token_skip(start, end, processed_len);
//                    }
//                    // @TODO does not work if last character is backslash
//                    '\\' => {
//                        state.emit_exclusive_token(line_index, col);
//                        state.flags.set(ESCAPED);
//                    }
//                    '\n' | '\r' if state.is(ESCAPED) => {
//                        state.emit_token_end(line_index, col);
//                    }
//                    '\n' | '\r' => {
//                        state.emit_exclusive_token(line_index, col);
//                        state.emit_end_of_statement();
//                    }
//
//                    '#' if state.is_not(ESCAPED | SINGLE_QUOTE_STARTED | DOUBLE_QUOTE_STARTED) => {
//                        state.emit_rest_of_line(col);
//                        break;
//                    }
//                    _ => {}
//                }
//            }
//            //print!("line: {}", line);
//            print!("\n====\n");
//            line_index += line.len();
//        }
//    }
//}

