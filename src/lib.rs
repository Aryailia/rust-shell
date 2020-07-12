//run: cargo test parser_tests -- --nocapture
#![allow(dead_code, unused_imports)]

mod helpers;

use async_std::task;
use futures::{stream, stream::Stream, StreamExt};
use helpers::{OwnedToOption, TextGridWalk};

//const COMMENTED: Flag = Flag(0x01);
//const WHITESPACE: Flag = Flag(0x02);
//const SINGLE_QUOTED: Flag = Flag(0x04);
//const DOUBLE_QUOTED: Flag = Flag(0x08);
//const FLAG_NON_NEWLINE_WHITESPACE: u8 = 0x08;

#[derive(Debug)]
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
    fn unset(&mut self, rhs: Self) {
        self.0 &= !rhs.0
    }
}

impl BitOr for Flag {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        Self(self.0 | rhs.0)
    }
}

use std::borrow::Cow;

#[derive(Debug, PartialEq)]
enum Lexeme {
    Text(String),
    Comment(String),
    CommentStart, // Might remove this in favour of just Comment
    Separator,
    EndOfCommand,
    Debug(String),
}

////////////////////////////////////////////////////////////////////////////////
struct LexerState<'a> {
    buffer: &'a str,
    token_start: usize,
    flags: Flag,
}

impl<'a> LexerState<'a> {
    fn new(buffer: &'a str) -> Self {
        Self {
            buffer,
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
    fn text_till(&self, index: usize) -> Lexeme {
        Lexeme::Text(self.buffer[self.token_start..index].into())
    }
}

type Walker<'a> = std::iter::Peekable<TextGridWalk<'a>>;

// https://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html
// '2.3.0' Is Token Recognition
fn file_lex<F: FnMut(Lexeme)>(body: &str, emit: &mut F) {
    let mut state = LexerState::new(body);
    let mut walker = TextGridWalk::new(body);
    // @TODO (_, _, tuple) for error handling
    while let Some((index, ch, _)) = walker.next() {
        match ch {
            '\'' => {  // Single quote
                if state.token_start < index { // Do not emit ''
                    emit(state.text_till(index));
                }
                state.token_start = index + 1;  // skip opening quote
                if let Some(index) = walker.next_while(|c| c == '\'') {
                    emit(state.text_till(index));   // This can emit ''
                    state.token_start = index + 1;  // skip closing quote
                } else {
                    println!("Error, unterminated single quote");
                }
            }
            // @TODO: double quotes
            // @TODO: parens
            // @TODO: curly braces
            // @TODO: here strings
            // @TODO: operators
            // @TODO: $


            '\\' => { // If end of file, just return the current backslash
                if state.token_start < index {
                    emit(state.text_till(index));
                }
                // POSIX does not specify what to do with dangling POSIX
                // NOTE: Backslash before EOF is interpreted literally in Dash
                let next = walker.peek().map(|(i, _, _)| i).unwrap_or(index);
                state.token_start = next + 1;
                emit(Lexeme::Text(state.buffer[next..next + 1].into()));
            }

            // @VOLATILE: make sure this happens before handling blanks
            // In POSIX, only newlines end commands (and ; &) see 2.9.3
            // Carriage-return is a non-space
            '\n' => {
                if state.token_start < index {
                    emit(state.text_till(index));
                }
                let after_last_semantic_space = walker.peek_till(|c| {
                    is_blank(c) || c == '\n'
                }).unwrap_or(index + 1);
                state.token_start = after_last_semantic_space;
                emit(Lexeme::EndOfCommand);
            }
            // @VOLATILE: make sure this happens after handling blanks
            c if is_blank(c) => {
                if state.token_start < index {
                    emit(state.text_till(index));
                }
                let cur = walker.peek_till(is_blank).unwrap_or(index + 1);
                state.token_start = cur;
                emit(Lexeme::Separator);
            }


            // @DESIGN: Allow 'CommentStart, Text("")' to make it down the line
            //          Might change this decision though...
            '#' => {  // Comments
                if state.token_start < index {
                    emit(state.text_till(index));
                }
                state.token_start = index + 1;  // Skip pound
                emit(Lexeme::CommentStart);
                // Skip till peek() is '\n'
                let cur = walker.peek_till(|c| c != '\n').unwrap_or(index + 1);
                emit(state.text_till(cur));  // Can emit ''
                state.token_start = cur + 1;  // Skip newline
            }
            _ => {}
        }
    }
}

// Test for whitespace characters
// https://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html
// https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap07.html
// In POSIX locale, <blank> class only includes space and tab
//
// This does mean that "echo\r\n" is interpreted as "echo\r"
//
// @TODO Should this listen to LC_CTYPE, not sure what a POSIX locale entails
// @TODO add extra options for ignoring closing "\r"?
fn is_blank(ch: char) -> bool {
    match ch {
        ' ' => true,
        '\t' => true,
        _ => false,
    }
}

async fn job_stream_lex<'a, T, F>(mut job_stream: T, mut emit: F)
where
    T: Stream<Item = &'a str> + Unpin,
    F: FnMut(Lexeme),
{
    while let Some(body) = job_stream.next().await {
        // Maybe parse shebang?
        file_lex(body, &mut emit);
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
#

"##;

    fn emit1(token: Lexeme) {
        match token {
            Lexeme::EndOfCommand => println!(";"),
            Lexeme::Comment(x) => println!("C({:?})", x),
            Lexeme::Separator => print!("| "),
            Lexeme::Debug(x) => print!("{}", x),
            x => print!("{:?} ", x),
        }
    }
    fn emit2(token: Lexeme) {
        println!("Token: {:?}", token);
    }

    #[test]
    fn development() {
        //let (producer, consumer) = futures::channel::mpsc::unbounded::<Lexeme>();

        let mut token_list: Vec<Lexeme> = Vec::with_capacity(100);
        task::block_on(async {
            println!("I am doing things\n====");
            let script_stream = stream::iter(vec![SCRIPT]);

            job_stream_lex(script_stream, |token| {
                token_list.push(token);
            })
            .await;
        });
        token_list.into_iter().for_each(emit1);
    }

    #[test]
    fn lexer_test() {
        let mut token_list: Vec<Lexeme> = Vec::with_capacity(100);
        task::block_on(async {
            //println!("I am doing things\n====");
            let script_stream = stream::iter(vec![SCRIPT]);

            job_stream_lex(script_stream, |token| {
                token_list.push(token);
            })
            .await;
        });
        assert_eq!(
            token_list,
            vec![
                Lexeme::CommentStart,
                Lexeme::Text("!/bin/ashell".into()),
                Lexeme::EndOfCommand,
                Lexeme::Text("main()".into()),
                Lexeme::Separator,
                Lexeme::Text("{".into()),
                Lexeme::EndOfCommand,
                Lexeme::CommentStart,
                Lexeme::Text(" yo".to_string()),
                Lexeme::EndOfCommand,
                Lexeme::Text("asdf=".into()),
                Lexeme::Text("hello".into()),
                Lexeme::EndOfCommand,
                Lexeme::Text("printf".into()),
                Lexeme::Separator,
                Lexeme::Text("%s".into()),
                Lexeme::Text("\\".into()),
                Lexeme::Text("n".into()),
                Lexeme::Separator,
                Lexeme::Text("${asdf}".into()),
                Lexeme::EndOfCommand,
                Lexeme::Text("echo".into()),
                Lexeme::Separator,
                Lexeme::Text("".into()),
                Lexeme::Separator,
                Lexeme::Text("".into()),
                Lexeme::EndOfCommand,
                Lexeme::Text("}".into()),
                Lexeme::EndOfCommand,
                Lexeme::CommentStart,
                Lexeme::Text("".into()),
                Lexeme::EndOfCommand,
            ]
        )
    }

    //#[test]
    //fn grid() {
    //    TextGridWalk::new(SCRIPT).for_each(|a| println!("{:?}", a));
    //}
}
