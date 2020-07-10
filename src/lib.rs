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
const UNTERMINATED: Flag = Flag(0x02);
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
#[derive(Debug)]
enum Lexeme2 {
    Text(String),
    Comment(String),
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

    // On Flags
    fn is(&self, rhs: Flag) -> bool {
        self.flags.is(rhs)
    }
    fn is_not(&self, rhs: Flag) -> bool {
        !self.flags.is(rhs)
    }

    fn reset_flags(&mut self) {
        self.flags.reset();
    }

    // Token methods
    // 'None' means just whitespace before
    fn exclusive_break(&mut self) -> Option<&str> {
        let walker = &mut self.buffer_walker;
        let start = self.token_start;
        let index = walker.row_start_index + walker.col_index;
        self.token_start = index + 1;
        if start < index {
            Some(&self.buffer[start..index])
        } else {
            None
        }
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

    // Main Logic
    //fn lex_attempt2<F: FnMut(Lexeme<'a>)>(&mut self, _ch: char, mut emit: &mut F) {
    //    emit(Lexeme::Comment(&self.buffer[0..1]));
    //}

    fn lex_attempt1<F: Fn(Lexeme)>(&mut self, ch: char, emit: F) {
        match ch {
            '\\' => {
                let walker = &mut self.buffer_walker;
                let cur_index = walker.row_start_index + walker.col_index;
                if let Some(x) = self.exclusive_break() {
                    emit(Lexeme::Text(x.into()));
                }
                // POSIX does not specify what to do with dangling POSIX
                // NOTE: Backslash before EOF is intepreted literally in
                // dash
                let walker = &mut self.buffer_walker;
                let i = match walker.next() {
                    // Index of the next character
                    // same as 'walker.row_start_index + walker.col_index'
                    Some(_) => self.token_start, // exclusive_break() sets
                    // Danging backslash is returned
                    None => cur_index,
                };
                self.token_start += 1; // +2 total due to 'walker.next()'
                let escape_target = &self.buffer[i..i + 1];
                emit(Lexeme::Text(escape_target.into()));
            }

            // @TODO: How to handle abitrary '\r' in middle of a line
            '\n' | '\r' => {
                if let Some(x) = self.exclusive_break() {
                    emit(Lexeme::Text(x.into()));
                    self.rest_of_line(); // for CRLF
                }
                emit(Lexeme::EndOfCommand);

                // @TODO: actually benchmark this
                // @TODO: loop over peek and delete contiguous whitespace
                //while let Some(_) = self.buffer_walker.next() {
                //    let peek = self.buffer_walker.peek();
                //}
            }

            '\'' => {
                if let Some(x) = self.exclusive_break() {
                    emit(Lexeme::Text(x.into()));
                }
                let mut found = false;
                while let Some(peek) = self.buffer_walker.next() {
                    if peek == '\'' {
                        if let Some(x) = self.exclusive_break() {
                            emit(Lexeme::Text(x.into()));
                        } else {
                            emit(Lexeme::Text("".into()));
                        }
                        found = true;
                        break;
                    }
                }
                if !found {
                    panic!("Unterminated single quote string")
                }
            }

            // https://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html
            // https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap07.html
            // This should listen to LC_CTYPE, but there is a POSIX locale
            // In POSIX local, space and tab are the only <blank>
            ' ' | '\t' if self.is_not(DQUOTE_BEGIN) => {
                if let Some(x) = self.exclusive_break() {
                    emit(Lexeme::Text(x.into()));
                }
                emit(Lexeme::Separator);
            }
            '#' if self.is_not(DQUOTE_BEGIN) => {
                emit(Lexeme::Comment(self.rest_of_line()));
            }
            _ => {}
        }
    }

    fn exclusive(&'a mut self) -> Option<&'a str> {
        let start = self.token_start;
        let index = self.buffer_walker.row_start_index + self.buffer_walker.col_index;
        self.token_start = index + 1;
        if start < index {
            Some(&self.buffer[start..index])
        } else {
            None
        }
    }

    /***************************************************************************
    // @IMPORTANT: This is the most working version
    // works when '#' code is placed directly in match, but not when
    // relegated into a function
    // This is because self.$func_call borrows, limiting the lifetime
    // to the match statement.
    // This will probably cause problems down the line anyway when receveing
    // jobs from later stages in the compile phase
    ***************************************************************************/
    fn lex_attempt2<F>(&mut self, ch: char, emit: &mut F)
    where
        F: FnMut(Lexeme<'a>),
    {
        match ch {
            '#' => {
                self.token_start += self.buffer_walker.cur_line.len();
                self.buffer_walker.char_source = "".chars();

                emit(Lexeme::Comment(
                    &self.buffer_walker.cur_line[self.buffer_walker.col_index..],
                ));
                //emit(Lexeme::Comment(self.rest()));
            }
            //' ' | '\t' => {
            //    if let Some(x) = a {
            //        emit(Lexeme::Text(x.into()));
            //    }
            //    emit(Lexeme::Separator);
            //}
            _ => {}
        }
        //emit(Lexeme::Comment(self.buffer));
    }
}

//fn lex_attempt3<'a, F>(state: &'a mut LexerState<'a>, ch: char, emit: &mut F)
//where
//    F: FnMut(Lexeme<'a>),
//{
//    match ch {
//        '#' => {
//            state.token_start += state.buffer_walker.cur_line.len();
//            state.buffer_walker.char_source = "".chars();
//
//            emit(Lexeme::Comment(
//                &state.buffer_walker.cur_line[state.buffer_walker.col_index..]
//            ));
//            //emit(Lexeme::Comment(self.rest()));
//        }
//        ' ' | '\t' => {
//            let a: Option<&'a str> = state.exclusive_break();
//            match a {
//                Some(x) => emit(Lexeme::<'a>::Comment(x)),
//                None => {}
//            }
//            emit(Lexeme::Separator);
//        }
//        _ =>{}
//    }
//    //emit(Lexeme::Comment(state.buffer));
//}
//
async fn lexer<'a, T, F>(mut stream_of_scripts: T, mut emit: F)
where
    T: Stream<Item = &'a str> + Unpin,
    F: FnMut(Lexeme<'a>),
{
    while let Some(body) = stream_of_scripts.next().await {
        // Maybe parse shebang?
        let mut str_lexer: LexerState<'a> = LexerState::new(body);
        while let Some(ch) = str_lexer.buffer_walker.next() {
            //if col == 0 {
            //    println!("Line: {:?}", line);
            //}
            str_lexer.lex_attempt2(ch, &mut emit);
            //lex_attempt3(&mut str_lexer, ch, &mut emit);
        }
    }
}

async fn file_stream_lex<'a, T, F>(mut stream_of_scripts: T, mut emit: F)
where
    T: Stream<Item = &'a str> + Unpin,
    F: FnMut(Lexeme2),
{
    while let Some(body) = stream_of_scripts.next().await {
        // Maybe parse shebang?
        let mut str_lexer: LexerState<'a> = LexerState::new(body);
        while let Some(ch) = str_lexer.buffer_walker.next() {
            //if col == 0 {
            //    println!("Line: {:?}", line);
            //}
            match ch {
                '#' => emit(Lexeme2::Comment(str_lexer.buffer.into())),
                _ => {}
            }
            //emit(Lexeme2::Comment(str_lexer.buffer.into()));
        }
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
  echo '' ''
}


"##;
    fn emit1(token: Lexeme) {
        match token {
            Lexeme::EndOfCommand => println!(";"),
            Lexeme::Comment(x) => println!("{:?}", Lexeme::Comment(x)),
            Lexeme::Separator => print!("| "),
            x => print!("{:?} ", x),
        }
    }
    fn emit2(token: Lexeme) {
        println!("Token: {:?}", token);
    }

    #[test]
    fn lexer_test() {
        //let (producer, consumer) = futures::channel::mpsc::unbounded::<Lexeme>();

        task::block_on(async {
            println!("I am doing things\n====");
            let script_stream = stream::iter(vec![SCRIPT]);
            let mut token_list: Vec<Lexeme2> = Vec::with_capacity(100);

            //lexer(script_stream, |token| {
            //    token_list.push(token);
            //})
            //.await;
            file_stream_lex(script_stream, |token| {
                token_list.push(token);
            })
            .await;
            token_list
                .into_iter()
                .for_each(|token| println!("{:?}", token));
        });
    }

    //#[test]
    //fn grid() {
    //    TextGridWalk::new(SCRIPT).for_each(|a| println!("{:?}", a));
    //}

    //#[test]
    //fn hello() {
    //    let (sender, mut receiver) = futures::channel::mpsc::unbounded::<Lexeme>();
    //    task::block_on(async {
    //        sender.unbounded_send(Lexeme::Text("hello".into())).unwrap();
    //        sender.unbounded_send(Lexeme::Text("asdf".into())).unwrap();
    //    });
    //    task::block_on(async {
    //        while let Some(a) = receiver.next().await {
    //            println!("{:?}", a);
    //        }
    //    });
    //}
}
