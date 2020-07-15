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
//const DOUBLE_QUOTED: Flag = Flag(0x08);
const BACKTICKED: Flag = Flag(0x16);


// @VOLATILE: Depends solely on 'LexerState::emit_text_delim()'
const BLANKABLE: bool = true;
const NON_BLANK: bool = false;

#[derive(Debug)]
struct Flag(u8);
use std::ops::BitOr;

impl Flag {
    fn new() -> Self {
        Flag(0)
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


struct NestLevelQueue<T> {
    index: usize,
    list: Vec<T>,
}
impl<T> NestLevelQueue<T> {
    fn first(&self) -> &T {
        &self.list[self.index]
    }
    fn pop_front(&mut self) -> &T {
        let i = self.index;
        self.index += 1;
        &self.list[i]
    }

    fn is_empty(&self) -> bool {
        self.index + 1 > self.list.len()
    }

    fn push(&mut self, value: T) {
        self.list.push(value);
    }
}


// @TODO change to Cow<str> or &str if possible for later stages
#[derive(Debug, PartialEq)]
enum Lexeme {
    Word(String), // Refers to 'words' as in "2. Shell Command Language"
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
    heredoc_delim_list: NestLevelQueue<String>,
    heredoc_delim_buffer: String,
    quote_state: QuoteType,
}

// @TODO benchmark skipping whitespace

impl<'a> LexerState<'a> {
    fn new(buffer: &'a str, emitter: &'a mut dyn FnMut(Lexeme)) -> Self {
        Self {
            buffer,
            token_start: 0,
            flags: Flag::new(),
            emitter,
            heredoc_delim_list: NestLevelQueue {
                index: 0,
                list: Vec::new(),
            },
            heredoc_delim_buffer: String::new(),
            quote_state: UNQUOTED,
        }
    }

    fn finish_here_delim(&mut self) {
        self.flags.unset(HERE_DOCUMENT_OPEN);
        let delim = std::mem::take(&mut self.heredoc_delim_buffer);
        self.heredoc_delim_buffer.push('\n');
        self.heredoc_delim_list.push(delim);
    }
    fn push_here_delim(&mut self, index: usize) {
        let hunk = &self.buffer[self.token_start..index];
        self.heredoc_delim_buffer.push_str(hunk);
    }

    fn text_till(&self, index: usize) -> String {
        self.buffer[self.token_start..index].to_string()
    }

    fn emit(&mut self, token: Lexeme) {
        (self.emitter)(token);
    }

    // Used after every character that is a non-word
    //
    // Here document delimiter recognition functions differently
    // than the rest of the code
    //fn emit_nonblank_word_or_end_heredoc_delim(&mut self, index: usize) {
    fn emit_non_word_delim(&mut self, index: usize) {
        if self.flags.is(HERE_DOCUMENT_OPEN) {
            self.push_here_delim(index);

            let delim = std::mem::take(&mut self.heredoc_delim_buffer);
            self.flags.unset(HERE_DOCUMENT_OPEN);
            self.heredoc_delim_list.push(delim);
        } else if self.token_start < index {
            let token = Lexeme::Word(self.text_till(index));
            (self.emitter)(token);
        }
    }

    // This can build up heredocs
    fn emit_quoted(&mut self, quote_type: QuoteType, index: usize) {
        if self.flags.is(HERE_DOCUMENT_OPEN) {
            self.push_here_delim(index);
        } else {
            let can_blank = self.quote_state == quote_type;
            self.emit_word(index, can_blank);
        }
    }

    fn emit_word(&mut self, index: usize, can_be_empty: bool) {
        if can_be_empty || self.token_start < index {
            let token = self.buffer[self.token_start .. index].to_string();
            (self.emitter)(Lexeme::Word(token));
        }
    }

    #[cfg(debug_assertions)]
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


// @TODO Cannot figure out a way to do this in a higher-level way
// 'struct QuoteType(u8)' fails when doing 'match' in the patterns
// '#[repr(u8)] enum QuoteType' means 'From<u8>' (or 'BitXor') are not free
type QuoteType = u8;
const UNQUOTED: QuoteType = 0;
const HERE_DOC: QuoteType = 1;
const SINGLE_QUOTED: QuoteType = '\'' as u8;
const DOUBLE_QUOTED: QuoteType = '"' as u8;

trait ToggleExt<T: std::ops::BitXor> {
    fn toggle(&mut self, rhs: T);
}

impl ToggleExt<QuoteType> for QuoteType {
    fn toggle(&mut self, rhs: QuoteType) {
        *self ^= rhs;
    }
}

// https://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html
// '2.3.0' Is Token Recognition
fn file_lex<F: FnMut(Lexeme)>(body: &str, emit: &mut F) {
    let mut state = LexerState::new(body, emit);
    let mut walker = TextGridWalk::new(body);
    // @TODO (_, _, tuple) for error handling
    while let Some((unprocessed, index, ch, _)) = walker.next() {
        //println!("char {:?}", ch);
        //if let QuoteType::None = state.quote_type {
        //    match state.quote_type {
        //        '\'' =>
        //    }
        //}

        //println!("{:?}", (UNQUOTED.0, 'a'));
        // Handle skipping due to quoting
        match (state.quote_state, ch) {
            (UNQUOTED, _) => {}

            (SINGLE_QUOTED, '\'') => {}

            (DOUBLE_QUOTED, '$') => {}
            (DOUBLE_QUOTED, '`') => {}
            (DOUBLE_QUOTED, '\\') => {}
            (DOUBLE_QUOTED, '"') => {}

            (HERE_DOC, '`') => {}
            (HERE_DOC, '$') => {}
            (HERE_DOC, '\\') => {}

            _ => continue,
        }

        if state.flags.is(HERE_DOCUMENT_BODY) {

            let delim = state.heredoc_delim_list.first().as_str();
            if unprocessed == delim {
                state.heredoc_delim_list.pop_front();
                state.flags.unset(HERE_DOCUMENT_BODY);
                let line_end = index + unprocessed.len();

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
                state.emit_word(index, BLANKABLE);
                let next = walker.peek().map(|(_, i, _, _)| i);
                // POSIX does not specify what to do with dangling POSIX
                // NOTE: Backslash before EOF is interpreted literally in Dash
                let next = next.unwrap_or(index);

                // Emit literal character without backslash (unless EOF)
                walker.next();
                state.token_start = next + 1;
                let escaped = state.buffer[next..next + 1].into();
                state.emit(Lexeme::Word(escaped));
            }

            // Quotes
            //'\'' => {
            //    // Single quote
            //    state.emit_word(index, NON_BLANK);
            //    state.token_start = index + 1; // skip opening quote
            //    if let Some(index) = walker.next_till(|c| c == '\'') {
            //        state.emit_word(index, NON_BLANK);
            //        state.token_start = index + 1; // skip closing quote
            //    } else {
            //        panic!("Unterminated single quote");
            //    }
            //}

            // @TODO benchmark combine quotes 
            '"' | '\'' => {
                state.emit_quoted(ch as QuoteType, index);
                state.quote_state.toggle(ch as QuoteType);
                state.token_start = index + 1; // Skip quote
            }

            // @TODO multiple here documents
            '<' => {
                state.emit_non_word_delim(index);

                // If the peek is '<'
                // @TODO <<-
                if walker.peek().iter().any(|(_, _, c, _)| *c == '<') {
                    state.flags.set(HERE_DOCUMENT_OPEN);
                    walker.next();
                    let cur = walker.peek_while(is_blank);
                    state.token_start = cur.unwrap_or(index + 2);  // skip '<<'
                } else {
                    state.emit(Lexeme::OpInputRedirect);
                    state.token_start = index + 1;
                }
            }

            // @TODO: parens
            // @TODO: curly braces
            // @TODO: here strings
            // @TODO: operators
            // @TODO: $

            // @VOLATILE: make sure this happens before handling blanks
            // In POSIX, only newlines end commands (and ; &) see 2.9.3
            // Carriage-return is a non-space
            '\n' => {
                state.emit_non_word_delim(index);

                if !state.heredoc_delim_list.is_empty() {
                    if state.flags.is(HERE_DOCUMENT_OPEN) {
                        panic!("Here-document delimiter not specified");
                    } else {
                        state.flags.set(HERE_DOCUMENT_BODY);
                        state.token_start = index + 1; // after newline
                    }
                } else {
                    // @TODO benchmark this
                    // Skis contiguous whitespace
                    let after_last_semantic_space = walker
                        .peek_while(|c| is_blank(c) || c == '\n')
                        .unwrap_or(index + 1);
                    state.token_start = after_last_semantic_space;
                }

                state.emit(Lexeme::EndOfCommand);
            }
            // @VOLATILE: make sure this happens after handling blanks
            _ if is_blank(ch) => {
                state.emit_non_word_delim(index);
                let cur = walker.peek_while(is_blank).unwrap_or(index + 1);
                state.token_start = cur;
                state.emit(Lexeme::Separator);
            }

            ';' => {
                state.emit_non_word_delim(index);
                state.emit(Lexeme::EndOfCommand);
                state.token_start = index + 1;
            }

            // @DESIGN: Allow 'CommentStart, Word("")' to make it down the line
            //          Might change this decision though...
            '#' => {
                // Comments
                state.emit_non_word_delim(index);
                state.token_start = index + 1; // Skip pound
                state.emit(Lexeme::CommentStart);
                // Skip till peek() is '\n'
                let cur = walker.peek_while(|c| c != '\n').unwrap_or(index + 1);
                state.emit_word(cur, BLANKABLE);
                state.token_start = cur + 1; // Skip newline
            }
            _ => {}
        }


        // @TODO if check if quote level is not UNQUOTED
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

    #[test]
    fn transmute_toggle() {
        #[repr(u8)]
        enum Quote {

            Unquoted = 0,
            Heredoc = 1,
            Single = '\'' as u8,
            Double = '"' as u8,
        }

        let input = '\'' as u8;
        let quote_enum = unsafe { std::mem::transmute::<u8, Quote>(input) };
        match quote_enum {
            Quote::Unquoted => println!("Quote"),
            Quote::Heredoc => println!("Heredoc"),
            Quote::Single => println!("Single"),
            Quote::Double => println!("Double"),
        }
    }


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
    ${hello}
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

    #[test]
    fn lexer_test() {
        let mut token_list: Vec<Lexeme> = Vec::with_capacity(100);
        task::block_on(async {
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
                Lexeme::Word("!/bin/ashell".into()),
                Lexeme::EndOfCommand,
                Lexeme::Word("main()".into()),
                Lexeme::Separator,
                Lexeme::Word("{".into()),
                Lexeme::EndOfCommand,
                Lexeme::CommentStart,
                Lexeme::Word(" yo".to_string()),
                Lexeme::EndOfCommand,
                Lexeme::Word("asdf=".into()),
                Lexeme::Word("hello".into()),
                Lexeme::EndOfCommand,
                Lexeme::Word("printf".into()),
                Lexeme::Separator,
                Lexeme::Word("%s".into()),
                Lexeme::Word("\\".into()),
                Lexeme::Word("n".into()),
                Lexeme::Separator,
                Lexeme::Word("${asdf}".into()),
                Lexeme::EndOfCommand,
                Lexeme::Word("echo".into()),
                Lexeme::Separator,
                Lexeme::Word("".into()),
                Lexeme::Separator,
                Lexeme::Word("".into()),
                Lexeme::EndOfCommand,
                Lexeme::Word("}".into()),
                Lexeme::EndOfCommand,
                Lexeme::CommentStart,
                Lexeme::Word("".into()),
                Lexeme::EndOfCommand,
            ]
        )
    }

    //#[test]
    //fn grid() {
    //    TextGridWalk::new(SCRIPT).for_each(|a| println!("{:?}", a));
    //}
}
