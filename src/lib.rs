//run: cargo test parser_tests -- --nocapture
#![allow(dead_code, unused_imports)]

mod helpers;

use async_std::task;
use futures::{stream, stream::Stream, StreamExt};
use helpers::{OwnedToOption, TextGridWalk};

//const COMMENTED: Flag = Flag(0x01);
const NO_FLAGS: Flag = Flag(0x00);
const BUILD_DELIM: Flag = Flag(0x01);
const BUILD_DELIM_TAB: Flag = Flag(0x02);
const HERE_DOCUMENT_STRIP_TABS: Flag = Flag(0x4);
const BACKTICKED: Flag = Flag(0x08);

// @VOLATILE: Depends solely on 'LexerState::emit_text_delim()'
const BLANKABLE: bool = true;
const NON_BLANK: bool = false;

#[derive(Debug, Clone, Copy)]
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
    Word(String), // 'Words' as defined in "2. Shell Command Language"
    Comment(String),
    CommentStart, // Might remove this in favour of just Comment
    Separator,
    EndOfCommand,
    Unprocessed(String),
    NewEnvCommand(String),
    SameEnvCommand(String),


    //
    Variable(String),

    // List of Operators
    HereDoc(String),
    HereDocBegin,
    HereDocEnd,
    OpInputHereDoc,
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
    heredoc_delim_list: NestLevelQueue<(String, Flag)>,
    heredoc_delim_buffer: String,
    quote_state: QuoteType,
}

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

    fn push_heredoc_delim(&mut self, index: usize) {
        let hunk = &self.buffer[self.token_start..index];
        self.heredoc_delim_buffer.push_str(hunk);
    }

    fn text_till(&self, index: usize) -> &str {
        &self.buffer[self.token_start..index]
    }

    ////////////////////////////////////////////////////////////////////////////
    // Emiting functions
    ////////////////////////////////////////////////////////////////////////////
    // There are three different types of emits we can that have side-effects
    // on two different axes:
    // - whether to 1) pushes to the here document delimiter buffer, 2) push
    //   to and delimit the buffer, 3) do not interact with the buffer
    // - whether to emit the default Lexeme::Word(self.token_start .. index)
    //   or to emit something custom


    // Do not interact with buffer, emit custom
    // @HereDocStep 2.2
    fn emit(&mut self, token: Lexeme) {
        (self.emitter)(token);
    }

    // Push to and delimit here-doc, emit default word lexeme
    // Used after every character that is a non-word
    //
    // @HereDocStep 2.3
    fn emit_non_word_delim(&mut self, index: usize) {
        if self.flags.is(BUILD_DELIM | BUILD_DELIM_TAB) {
            self.push_heredoc_delim(index);

            let delim = std::mem::take(&mut self.heredoc_delim_buffer);
            let delay_set_flag = if self.flags.is(BUILD_DELIM_TAB) {
                HERE_DOCUMENT_STRIP_TABS
            } else  {
                NO_FLAGS
            };

            self.flags.unset(BUILD_DELIM | BUILD_DELIM_TAB);
            self.heredoc_delim_list.push((delim, delay_set_flag));
        } else if self.token_start < index {
            let token = Lexeme::Word(self.text_till(index).to_string());
            (self.emitter)(token);
        }
    }

    // Push to here-doc without delimiting, emit default word lexeme
    // @HereDocStep 2.1
    fn emit_quoted(&mut self, quote_type: QuoteType, index: usize) {
        if self.flags.is(BUILD_DELIM | BUILD_DELIM_TAB) {
            self.push_heredoc_delim(index);
        } else {
            let can_blank = self.quote_state == quote_type;
            self.emit_word(index, can_blank);
        }
    }

    // Do not interact with here-doc delim buffer, emit default word lexeme
    fn emit_word(&mut self, index: usize, can_be_empty: bool) {
        if can_be_empty || self.token_start < index {
            let token = self.buffer[self.token_start..index].to_string();
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

//// @TODO Thinking about this if I want to pass in ranges to the emit functions
//
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
const HERE_DOCUMENT: QuoteType = 1;
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

    // 'state.buffer[token_start..index]' defines the token we are currently
    // building up, by default this is considered a word
    // walker.next() is essentianlly incrementing  a token_end
    while let Some((remaining_line, index, ch, _)) = walker.next() {

        // Handle skipping due to quoting
        match (state.quote_state, ch, walker.peek().map(|(_, _, c, _)| c)) {
            (UNQUOTED, _, _) => {}

            (SINGLE_QUOTED, '\'', _) => {}

            (DOUBLE_QUOTED, '$', _) => {}
            (DOUBLE_QUOTED, '`', _) => {}
            (DOUBLE_QUOTED, '\\', _) => {}
            (DOUBLE_QUOTED, '"', _) => {}

            // Finish the here document
            // @HereDocStep 5, final
            (HERE_DOCUMENT, _, _) if remaining_line == state.heredoc_delim_list.first().0.as_str() => {
                state.heredoc_delim_list.pop_front();
                state.flags.unset(HERE_DOCUMENT_STRIP_TABS);
                state.quote_state = UNQUOTED;
                let line_end = index + remaining_line.len();

                walker.next();
                walker.peek_while(|c| c != '\n');
                // @TODO replace with 'emit_word()' when we no longer to debug this
                state.emit(Lexeme::HereDoc(state.text_till(index).into()));
                state.emit(Lexeme::HereDocEnd);
                state.token_start = line_end; // before newline after 'line'
                continue; // start newline processing
            }
            // @HereDocStep 4.1
            (HERE_DOCUMENT, '`', _) => {}
            (HERE_DOCUMENT, '$', _) => {}
            (HERE_DOCUMENT, '\\', _) => {}
            // @HereDocStep 4.2
            // If strip_tabs then strip, else skip over newlines
            (HERE_DOCUMENT, '\n', Some('\t')) if state.flags.is(HERE_DOCUMENT_STRIP_TABS) => {
                // @TODO replace with 'emit_word()' when we no longer to debug this
                state.emit(Lexeme::HereDoc(state.text_till(index + 1).into()));
                state.token_start = index + 2;
                continue
            }

            // No special handling for other characters because quoted
            _ => continue,
        }

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
                // These quotes interact with here document delim uniquely
                state.emit_quoted(ch as QuoteType, index);
                state.quote_state.toggle(ch as QuoteType);
                state.token_start = index + 1; // Skip quote
            }

            // @TODO multiple here documents
            '<' => {
                state.emit_non_word_delim(index);

                match (remaining_line.get(1..2), remaining_line.get(2..3)) {
                    (Some("<"), Some("-")) => {
                        // "<<-"
                        // @TODO <<-
                        state.flags.set(BUILD_DELIM_TAB);
                        walker.next();
                        walker.next();
                        let cur = walker.peek_while(is_blank);
                        state.token_start = cur.unwrap_or(index + 3);
                        state.emit(Lexeme::OpInputHereDoc);
                    }
                    (Some("<"), _) => {
                        // "<<"
                        state.flags.set(BUILD_DELIM);
                        walker.next();
                        let cur = walker.peek_while(is_blank);
                        state.token_start = cur.unwrap_or(index + 2);
                        state.emit(Lexeme::OpInputHereDoc);
                    }
                    _ => {
                        state.token_start = index + 1;
                        state.emit(Lexeme::OpInputRedirect);
                    }
                }
            }

            //'$' => {
            //    state.emit_non_word_delim(index);
            //    match (remaining_line.get(1 .. 2), remaining_line.get(2 .. 3)) {
            //        (Some("("), Some("(")) => {
            //            state.token_start = index + 3;
            //        }
            //        (Some("("), _) => {
            //            state.token_start = index + 2;
            //        }
            //        (Some("{"), _) => {
            //            state.token_start = index + 2;
            //            let cur = walker.peek_while(|c|
            //                c != '}' && c != '#' && c != '%'
            //            );
            //            state.emit(Lexeme::VarLookup(state.text_till(cur).into());
            //            
            //        }

            //        (Some(ch2), _) => {
            //        }

            //        _ => { // Let cursor use as word (plaintext)
            //        //    state.emit(Lexeme::Word("$".into()));
            //        //    state.token_start = index + 1;
            //        }
            //    }
            //}

            // @TODO: parens
            // @TODO: curly braces
            // @TODO: here strings
            // @TODO: operators
            // @TODO: $

            ';' => {
                state.emit_non_word_delim(index);
                state.emit(Lexeme::EndOfCommand);
                state.token_start = index + 1;
            }

            // @HereDocStep 3
            // @VOLATILE: make sure this happens before handling blanks in
            //            case 'is_blank' also returns true for newlines
            //
            // In POSIX, only newlines end commands (and ; &) see 2.9.3
            // Carriage-return is a non-space
            '\n' => {
                state.emit_non_word_delim(index);

                // If a delim was pushed, initialise here document procesing
                if !state.heredoc_delim_list.is_empty() {
                    // If the delim was never finished by newline, error
                    if state.flags.is(BUILD_DELIM | BUILD_DELIM_TAB) {
                        panic!("Here-document delimiter not specified");
                    } else {
                        let delay_set_flag = state.heredoc_delim_list.first().1;
                        state.flags.set(delay_set_flag);
                        state.quote_state = HERE_DOCUMENT;
                        state.token_start = index + 1; // after newline
                        state.emit(Lexeme::EndOfCommand);
                        state.emit(Lexeme::HereDocBegin);
                    }
                } else {
                    // @TODO benchmark skipping whitespace
                    // Skis contiguous whitespace
                    let after_last_semantic_space = walker
                        .peek_while(|c| is_blank(c) || c == '\n')
                        .unwrap_or(index + 1);
                    state.token_start = after_last_semantic_space;
                    state.emit(Lexeme::EndOfCommand);
                }

            }
            // @VOLATILE: make sure this happens after handling newlines in
            //            case 'is_blank' also returns true for newlines
            _ if is_blank(ch) => {
                state.emit_non_word_delim(index);
                // @TODO benchmark skipping whitespace
                let cur = walker.peek_while(is_blank).unwrap_or(index + 1);
                state.token_start = cur;
                state.emit(Lexeme::Separator);
            }

            '#' => {
                // Comments
                state.emit_non_word_delim(index);
                state.token_start = index + 1; // Skip pound
                // Skip till peek() is '\n'
                let cur = walker.peek_while(|c| c != '\n').unwrap_or(index + 1);
                state.emit(Lexeme::Comment(state.text_till(cur).to_string()));
                state.token_start = cur + 1; // Skip newline
            }

            _ => {}  // Allow it to perform 'walker.next()' in peace
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
<<-     "H"ello cat -; <<EOF cat -
    amazing
	${hello}
Hello
  printf %s\\n hello
  yo
EOF
echo hello
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
                Lexeme::Comment("!/bin/ashell".into()),
                Lexeme::EndOfCommand,
                Lexeme::Word("main()".into()),
                Lexeme::Separator,
                Lexeme::Word("{".into()),
                Lexeme::EndOfCommand,
                Lexeme::Comment(" yo".to_string()),
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
                Lexeme::Comment("".into()),
                Lexeme::EndOfCommand,
            ]
        )
    }

    //#[test]
    //fn grid() {
    //    TextGridWalk::new(SCRIPT).for_each(|a| println!("{:?}", a));
    //}
}
