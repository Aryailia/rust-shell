//run: cargo test parser_tests -- --nocapture
#![allow(dead_code, unused_imports)]

mod helpers;

use async_std::task;
use futures::{stream, stream::Stream, StreamExt};
use helpers::{OwnedToOption, TextGridWalk};

//const COMMENTED: Flag = Flag(0x01);
const HERE_DOCUMENT_OPEN: Flag = Flag(0x01);
const HERE_DOCUMENT_BODY: Flag = Flag(0x02);
const STRIP_TABS_FOR_HERE_DOCUMENT: Flag = Flag(0x4);
const DOUBLE_QUOTED: Flag = Flag(0x08);
const BACKTICKED: Flag = Flag(0x16);

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
        self.0 & rhs.0 != 0
    }
    fn is_not(&self, rhs: Self) -> bool {
        self.0 & rhs.0 == 0
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

// @TODO change to Cow<str> or &str if possible for later stages
#[derive(Debug, PartialEq)]
enum Lexeme {
    Text(String), // Refers to 'words' as in "2. Shell Command Language"
    Comment(String),
    CommentStart, // Might remove this in favour of just Comment
    Separator,
    EndOfCommand,
    Unprocessed(String),
    NewEnvCommand(String),
    SameEnvCommand(String),

    // List of Operators
    HereDocument(String),
    OpInputRedirect,
    OpOutputRedirect,
    OpErrorRedirect,
    OpPipe,
    Keyword(String),

    Debug(String),
}

struct LexerState<'a> {
    buffer: &'a str,
    token_start: usize,
    flags: Flag,
    emitter: &'a mut dyn FnMut(Lexeme),
    here_doc_closers: Vec<String>,
    here_closer: String,
    here_doc_cursor: usize,
}

// @TODO benchmark skipping whitespace

impl<'a> LexerState<'a> {
    fn new(buffer: &'a str, emitter: &'a mut dyn FnMut(Lexeme)) -> Self {
        Self {
            buffer,
            token_start: 0,
            flags: Flag::new(),
            emitter,
            here_doc_closers: Vec::new(),
            here_closer: "\n".to_string(),
            here_doc_cursor: 0,
        }
    }

    fn finish_here_closer(&mut self) {
        self.flags.unset(HERE_DOCUMENT_OPEN);
        let closer = std::mem::take(&mut self.here_closer);
        self.here_closer.push('\n');
        self.here_doc_closers.push(closer);
    }
    fn push_here_closer(&mut self, index: usize) {
        let hunk = &self.buffer[self.token_start..index];
        self.here_closer.push_str(hunk);
    }

    fn text_till(&self, index: usize) -> String {
        self.buffer[self.token_start..index].to_string()
    }

    fn emit_text_till(&mut self, index: usize) {
        let token = Lexeme::Text(self.text_till(index));
        (self.emitter)(token);
    }

    fn emit(&mut self, token: Lexeme) {
        (self.emitter)(token);
    }

    //#[cfg(debug)]
    fn print_context(&self, mut byte_index: usize) {
        // Get the char at the byte index 'index'
        let ch = self.buffer[byte_index..].chars().next().unwrap();
        byte_index += 1; // because of the padding
        let ch_len = ch.len_utf8();

        let padded = format!("\n{}\n", self.buffer);
        let start = padded[0..byte_index].rfind('\n').unwrap();
        let end = padded[byte_index + ch_len..].find('\n').unwrap();
        println!(
            "{:?}|{:?}|{:?}",
            &padded[start + 1..byte_index],
            &padded[byte_index..byte_index + ch_len],
            &padded[byte_index + ch_len..byte_index + ch_len + end],
        );
    }

    //    fn cursor_over_span(delimiter: char, buffer: &str) ->  {
    //        if let Some(index) = walker.next_till(|c| c == '') {
    //            emit(state.text_till(index));   // This can emit ''
    //            state.token_start = index + 1;  // skip closing quote
    //        }
    //    }
}

//struct Cursor {
//    index: usize,
//}
//impl Cursor {
//    fn range_of_move(&mut self, till: usize) -> std::ops::Range<usize> {
//        let range = self.index .. till;
//        self.index = index;
//        range
//    }
//
//    fn range_of_span_move(&self, delimiter: char, walker: &mut TextGridWalk) {
//        if let Some(index) = walker.next_till(|c| == delimiter);
//    }
//}

//type Walker<'a> = std::iter::Peekable<TextGridWalk<'a>>;

// @TEST << EOF cat
// @TEST << "E"'O'"F" cat
// @TEST << "E'O"F\" cat
// @TEST << "E'O"F\' cat
// @TEST << "E'O"F\ blah cat
// @TEST <<"" cat
// @TEST <<EOF cat \
// @TEST \<EOF>
// @TEST <<EOF1 <<EOF2 cat // See example 2.7.4

// https://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html
// '2.3.0' Is Token Recognition
fn file_lex<F: FnMut(Lexeme)>(body: &str, emit: &mut F) {
    let mut state = LexerState::new(body, emit);
    let mut walker = TextGridWalk::new(body);
    // @TODO (_, _, tuple) for error handling
    while let Some((line, index, ch, _)) = walker.next() {
        //println!("char {:?}", ch);
        //if let QuoteType::None = state.quote_type {
        //    match state.quote_type {
        //        '\'' =>
        //    }
        //}

        if state.flags.is(DOUBLE_QUOTED) {
            match ch {
                // See "2.2.3 Double-Quotes"
                //'\'' if state.flags.is(SINGLE_QUOTED) => {}
                '$' | '`' | '\\' | '"' => {}
                _ => continue,
            }
        } else if state.flags.is(HERE_DOCUMENT_BODY) {
            let closer = state.here_doc_closers[state.here_doc_cursor].as_str();
            if line == closer {
                state.here_doc_cursor += 1;  // unshift 'here_doc_closers'
                state.flags.unset(HERE_DOCUMENT_BODY);
                let line_end = index + line.len();

                walker.next();
                walker.peek_while(|c| c != '\n');
                // Emit up till before newline before current 'line'
                state.emit(Lexeme::HereDocument(state.text_till(index)));
                state.token_start = line_end; // before newline after 'line'
                continue  // start newline processing
            } else {
                match ch {
                    '$' | '`' | '\\' => {}
                    _ => continue,
                }
            }
        }

        // Special case stuff
        match ch {
            // Backslash gets highest priority
            // @TODO: Remove newline (2.2.1 Escape Character)
            '\\' => {
                // If end of file, just return the current backslash
                if state.token_start < index {
                    state.emit_text_till(index);
                }
                // POSIX does not specify what to do with dangling POSIX
                // NOTE: Backslash before EOF is interpreted literally in Dash
                let next = walker.peek().map(|(_, i, _, _)| i).unwrap_or(index);

                // Emit literal character without backslash (unless EOF)
                walker.next();
                state.token_start = next + 1;
                let escaped = state.buffer[next..next + 1].into();
                state.emit(Lexeme::Text(escaped));
                print!("Hello: {:?} ", state.flags);
                state.print_context(next + 1);
            }

            // @TODO multiple here documents
            '<' => {
                if state.flags.is(HERE_DOCUMENT_OPEN) {
                    state.push_here_closer(index);
                    state.finish_here_closer();
                } else if state.token_start < index {
                    state.emit_text_till(index);
                }

                // If the peek is '<'
                // @TODO <<-
                if walker.peek().iter().any(|(_, _, c, _)| *c == '<') {
                    state.flags.set(HERE_DOCUMENT_OPEN);
                    walker.next();
                    let cur = walker.peek_while(is_blank).unwrap_or(index + 2);
                    state.token_start = cur;
                } else {
                    state.emit(Lexeme::OpInputRedirect);
                    state.token_start = index + 1;
                }
            }

            '"' => {
                if state.flags.is(HERE_DOCUMENT_OPEN) {
                    state.push_here_closer(index);
                } else if state.flags.is_not(DOUBLE_QUOTED) {
                    state.flags.set(DOUBLE_QUOTED);
                    if state.token_start < index {
                        state.emit_text_till(index); // Skip ''
                    }
                } else {
                    state.flags.unset(DOUBLE_QUOTED);
                    state.emit_text_till(index); // Allow emit ''
                }
                state.token_start = index + 1; // Skip quote
            }

            '\'' => {
                // Single quote
                if state.token_start < index {
                    // Do not emit ''
                    state.emit_text_till(index);
                }
                state.token_start = index + 1; // skip opening quote
                if let Some(index) = walker.next_till(|c| c == '\'') {
                    state.emit_text_till(index); // This can emit ''
                    state.token_start = index + 1; // skip closing quote
                } else {
                    panic!("Unterminated single quote");
                }
            }

            // @TODO: double quotes
            // @TODO: parens
            // @TODO: curly braces
            // @TODO: here strings
            // @TODO: operators
            // @TODO: $

            // @VOLATILE: make sure this happens before handling blanks
            // In POSIX, only newlines end commands (and ; &) see 2.9.3
            // Carriage-return is a non-space
            '\n' => {
                if state.token_start < index {
                    state.emit_text_till(index);
                }

                if state.here_doc_cursor + 1 >= state.here_doc_closers.len() {
                    // @TODO benchmark this
                    // Skis contiguous whitespace
                    let after_last_semantic_space = walker
                        .peek_while(|c| is_blank(c) || c == '\n')
                        .unwrap_or(index + 1);
                    state.token_start = after_last_semantic_space;
                } else {
                    if state.flags.is(HERE_DOCUMENT_OPEN) {
                        panic!("Here-document delimiter not specified");
                    } else {
                        state.flags.set(HERE_DOCUMENT_BODY);
                        state.token_start = index + 1; // after newline
                    }
                }

                state.emit(Lexeme::EndOfCommand);
            }
            // @VOLATILE: make sure this happens after handling blanks
            c if is_blank(c) => {
                if state.flags.is(HERE_DOCUMENT_OPEN) {
                    state.push_here_closer(index);
                    state.finish_here_closer();
                } else if state.token_start < index {
                    state.emit_text_till(index);
                }
                let cur = walker.peek_while(is_blank).unwrap_or(index + 1);
                state.token_start = cur;
                state.emit(Lexeme::Separator);
            }

            ';' => {
                state.emit(Lexeme::EndOfCommand);
                state.token_start = index + 1;
            }

            // @DESIGN: Allow 'CommentStart, Text("")' to make it down the line
            //          Might change this decision though...
            '#' => {
                // Comments
                if state.flags.is(HERE_DOCUMENT_OPEN) {
                    state.push_here_closer(index);
                    state.finish_here_closer();
                } else if state.token_start < index {
                    state.emit_text_till(index);
                }
                state.token_start = index + 1; // Skip pound
                state.emit(Lexeme::CommentStart);
                // Skip till peek() is '\n'
                let cur = walker.peek_while(|c| c != '\n').unwrap_or(index + 1);
                state.emit_text_till(cur); // Can emit ''
                state.token_start = cur + 1; // Skip newline
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
    const SCRIPT2: &str = r##"
<<     "H"ello cat -
    amazing
Hello
  printf %s\\n hello
  yo
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
        let script_stream = stream::iter(vec![SCRIPT2]);
        task::block_on(async {
            println!("I am doing things\n====");

            job_stream_lex(script_stream, |token| {
                token_list.push(token);
            })
            .await;
        });
        token_list.into_iter().for_each(emit1);
    }

    //#[test]
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
