//run: cargo test parser_tests -- --nocapture

// Dash is: git.kernel.org/pub/scm/utils/dash.git
#![allow(dead_code, unused_imports)]

mod helpers;

use async_std::task;
use futures::{stream, stream::Stream, StreamExt};
use helpers::{OwnedToOption, TextGridWalk};
use std::collections::VecDeque;
use std::mem::discriminant;
use std::ops::Range;

const NO_FLAGS: Flag = Flag(0x00);
//const COMMAND_FIRST_TOKEN = Flag(0x01);
const HAS_SINGLE_OR_DOUBLE_QUOTE: Flag = Flag(0x02); // Allows a blank word lexemes
const BUILD_DELIM: Flag = Flag(0x04);
const BUILD_DELIM_TAB: Flag = Flag(0x08);
const STRIP_TABS: Flag = Flag(0x10);
const BACKTICKED: Flag = Flag(0x20);

#[derive(Debug)]
struct Flag(u8);

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

impl std::ops::BitOr for Flag {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        Self(self.0 | rhs.0)
    }
}

#[derive(Debug)]
enum ExpandableQuote {
    Unquoted,
    Double,
    HereDoc,
}
impl PartialEq for ExpandableQuote {
    fn eq(&self, rhs: &Self) -> bool {
        discriminant(self) == discriminant(rhs)
    }
}

#[derive(Clone, Debug)]
#[repr(u8)]
enum Nestable {
    Backtick = 0,
    Arithmetic = 1,
    Parenthesis = 2,
    Curly = 3,
}
const NEST_TOTAL_SIZE: usize = 4; // @VOLATILE: size of 'Nestable'

// @TODO change to Cow<str> or &str if possible for later stages
#[derive(Debug, PartialEq)]
enum Lexeme {
    Word(String), // 'Words' as defined in "2. Shell Command Language"
    Comment(String),
    CommentStart, // Might remove this in favour of just Comment
    Separator,
    EndOfCommand,
    EndOfBackgroundCommand,
    Unprocessed(String),
    NewEnvCommand(String),
    SameEnvCommand(String),

    ArithmeticStart,
    ArithmeticClose,
    SubShellStart,
    SubShellClose,
    ClosureStart,
    ClosureClose,
    //
    Variable(String),

    // List of Operators
    HereDocStart,
    HereDocClose,
    OpInputHereDoc,
    OpInputRedirect,
    OpOutputRedirect,
    OpErrorRedirect,
    OpPipe,
    OpAssign,
    Keyword(String),

    Debug(String),
}

type HereDocDelimList = VecDeque<(String, bool)>;

// Need one per file source processed
// In charge of
struct LexemeBuilder<'a> {
    source: &'a str,
    flags: Flag,
    emitter: &'a mut dyn FnMut(Lexeme),
    buffer: String,
    output_index: usize,
    quote_state: ExpandableQuote,
    nesting_stack: Vec<(ExpandableQuote, Nestable, Info)>,
    nesting_depth: [usize; NEST_TOTAL_SIZE],
    heredoc_delim_list: HereDocDelimList,
}

// For keeping mutability of .'buffer' and use of 'emitter' to local methods
impl<'a> LexemeBuilder<'a> {
    fn new(buffer: &'a str, emitter: &'a mut dyn FnMut(Lexeme)) -> Self {
        Self {
            source: buffer,
            flags: Flag::new(),
            emitter,
            buffer: String::new(),
            output_index: 0,
            quote_state: ExpandableQuote::Unquoted,
            nesting_stack: Vec::new(),
            nesting_depth: [0; NEST_TOTAL_SIZE],
            heredoc_delim_list: HereDocDelimList::new(),
        }
    }

    // Do not interact with buffer, emit custom
    fn emit(&mut self, token: Lexeme) {
        (self.emitter)(token);
    }

    // Append to the work-in-progress buffer
    fn lexeme_append(&mut self, range: Range<usize>) {
        self.buffer.push_str(&self.source[range]);
    }

    // Append to the work-in-progress buffer
    fn lexeme_push_char(&mut self, to_add: char) {
        self.buffer.push(to_add);
    }

    // Plex
    fn lexeme_delimit(&mut self) {
        // Strip leading blanks
        // @HEREDOC Step 2
        let has_quote = self.flags.is(HAS_SINGLE_OR_DOUBLE_QUOTE);
        if self.flags.is(BUILD_DELIM | BUILD_DELIM_TAB) {
            let entry = (self.buffer.clone(), self.flags.is(BUILD_DELIM_TAB));
            self.heredoc_delim_list.push_back(entry);
            self.buffer.clear();
            self.output_index += 1;
            self.flags.unset(BUILD_DELIM | BUILD_DELIM_TAB);
        } else if has_quote || !self.buffer.is_empty() {
            // @POSIX 2.10.2 (Step 1) Shell Grammar Rules
            // @TODO: Reserved words cannot have quotes
            (self.emitter)(Lexeme::Word(self.buffer.clone()));
            self.buffer.clear();
            self.flags.unset(HAS_SINGLE_OR_DOUBLE_QUOTE);
            self.output_index += 1;
        }
    }

    #[cfg(debug_assertions)]
    #[cfg(test)]
    // Print the line associated with 'index' of self.source
    fn print_context(&self, mut index: usize) {
        // Get the char at the byte index 'index'
        let ch = self.source[index..].chars().next().unwrap();
        index += 1; // because of the padding
        let ch_len = ch.len_utf8();

        let padded = format!("\n{}\n", self.source);
        let start = padded[0..index].rfind('\n').unwrap();
        let end = padded[index + ch_len..].find('\n').unwrap();
        println!(
            "{:?}|{:?}|{:?}",
            &padded[start + 1..index],
            &padded[index..index + ch_len],
            &padded[index + ch_len..index + ch_len + end],
        );
    }

    ////////////////////////////////////////////////////////////////////////////
    // Nesting Level
    fn start_a_nesting(&mut self, val: Info, nest_type: Nestable) {
        self.nesting_stack.push((
            std::mem::replace(&mut self.quote_state, ExpandableQuote::Unquoted),
            nest_type.clone(),
            val,
        ));
        self.output_index = 0;
        self.nesting_depth[nest_type as usize] += 1;
    }

    fn current_nesting_type(&self) -> Option<&Nestable> {
        self.nesting_stack.last().map(|(_, t, _)| t)
    }

    // Ensure nestings can not close over eaach other
    // e.g. Cannot have '$( ` ) `
    fn close_a_nesting(&mut self, expected: Nestable) -> Result<(), String> {
        match self.nesting_stack.pop() {
            // @TODO implement error handling
            Some((quote_level, nest_type, _error_info)) => {
                if discriminant(&nest_type) == discriminant(&expected) {
                    self.nesting_depth[nest_type as usize] -= 1;
                    self.quote_state = quote_level;
                    Ok(())
                } else {
                    Err("Mismatched closing delimiter".into())
                }
            }

            // A closer without a opener
            None => Err("Unexpected character".into()),
        }
    }

    fn depth_of(&self, nest_type: Nestable) -> usize {
        self.nesting_depth[nest_type as usize]
    }
}

#[derive(Debug)]
struct Cursor {
    index: usize,
}
impl Cursor {
    fn move_to(&mut self, target: usize) -> Range<usize> {
        debug_assert!(self.index <= target);
        let range = self.index..target;
        self.index = target;
        range
    }
}

//type Walker<'a> = std::iter::Peekable<TextGridWalk<'a>>;

// @TEST << EOF cat
// @TEST << "E"'O'"F" cat
// @TEST << "E'O"F\" cat
// @TEST << "E'O"F\' cat
// @TEST << "E'O"F\ blah cat
// @TEST <<"" cat
// @TEST <<EOF cat \
// @TEST <<"\\$asdf" cat
// @TEST \<EOF>
// @TEST <<EOF1 <<EOF2 cat // See example 2.7.4
// @TEST $( ` ) `     - mismatched nestings
// @TEST: \<EOF>

// Random helper methods
impl<'a> LexemeBuilder<'a> {
    // @VOLATILE: Coordinate with ending heredoc branch of '\n'
    //            Does not seem like heredoc needs ot set '.output_index'
    fn end_command_and_walk_to_next_token(
        &mut self,
        walker: &mut TextGridWalk,
        cursor: &mut Cursor,
        index: usize,
        ch: char,
        token: Lexeme,
    ) {
        self.lexeme_append(cursor.move_to(index));
        self.lexeme_delimit();

        // Skip if current line is a blank line
        if self.output_index > 0 {
            self.emit(token);
        }
        self.output_index = 0;

        // Skip until the next non-blank, non-newline character
        let non_blank = walker.peek_while(|c| is_blank(c) || c == '\n');
        cursor.move_to(non_blank.unwrap_or(index + ch.len_utf8()));
    }

    // @HEREDOC Step 4
    fn heredoc_maybe_end_or_strip_tab(
        &mut self,
        walker: &mut TextGridWalk,
        cursor: &mut Cursor,
        index: usize,
    ) {
        // Start a new here-document
        if let ExpandableQuote::Unquoted = self.quote_state {
            let till_now = cursor.move_to(index);
            self.lexeme_append(till_now);
            self.lexeme_delimit();

            // @VOLATILE: Coordinate with 'end_command_and_walk_to_non_next()'
            self.emit(Lexeme::EndOfCommand);
            self.emit(Lexeme::HereDocStart);
            self.quote_state = ExpandableQuote::HereDoc;
            cursor.move_to(index + '\n'.len_utf8());
        }

        // @NONPOSIX: 2.7.4 Here-Document "until there is a line
        //            containing only the delimiter and a <newline>"
        // I am doing the same as Dash: '<<EOF cat -\nhello\nEOF' is valid

        // Decide to strip the tab or close the here-document
        if let Some((line, after_newline, peek_c, _)) = walker.peek() {
            let (delim, is_strip) = self.heredoc_delim_list.front().unwrap();
            let delim_len = delim.len();
            if line == delim.as_str() {
                self.lexeme_append(cursor.move_to(after_newline));
                self.lexeme_delimit();
                self.emit(Lexeme::HereDocClose);

                let after_closer = walker
                    .peek_while(|c| c != '\n')
                    // @TODO remove need for this (confuses)
                    .unwrap_or(after_newline + delim_len) // no newline
                    ;
                cursor.move_to(after_closer);

                self.quote_state = ExpandableQuote::Unquoted;
                self.heredoc_delim_list.pop_front();
            } else if *is_strip && peek_c == '\t' {
                self.lexeme_append(cursor.move_to(after_newline));
                cursor.move_to(after_newline + '\t'.len_utf8());
            }
        } else {
            panic!("Unterminated here-document")
        }
    }
}

// @TODO: Skip command expansion for heredoc delim
const SKIP_COMMAND_EXPANSION: bool = true;


type Info = (usize, usize);
impl<'a> TextGridWalk<'a> {
    // @TODO: double check if this starter_index is necessary or if we can
    //        just use a value within self
    fn skip_blanks(&mut self, cursor: &mut Cursor, starter_index: usize) {
        let nonblank_index = self.peek_while(is_blank);
        cursor.move_to(nonblank_index.unwrap_or(starter_index));
    }
}

// https://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html
// '2.3.0' Is Token Recognition
fn file_lex<F: FnMut(Lexeme)>(body: &str, emit: &mut F) {
    let mut state = LexemeBuilder::new(body, emit);
    let mut walker = TextGridWalk::new(body);
    let mut cursor = Cursor { index: 0 };

    // 'state.buffer[token_start..index]' defines the token we are currently
    // building up, by default this is considered a word
    // walker.next() is essentianlly incrementing  a token_end
    while let Some((rest_of_line, index, ch, info)) = walker.next() {
        //print!("{:?} ", ch);

        // @TODO: parens
        // @TODO: curly braces
        // @TODO: operators
        // @TODO: $

        match ch {
            '\\' => {
                state.lexeme_append(cursor.move_to(index));
                let mut new_index;
                //println!("init {:?}", state.buffer);

                // Loop mostly to facilitate backtick nesting
                loop {
                    let tick_nest_depth = state.depth_of(Nestable::Backtick);
                    new_index = match (&state.quote_state, walker.peek()) {
                        // Universal for both
                        (_, Some((_, peek_i, '\n', _))) => {
                            walker.next();
                            peek_i + '\n'.len_utf8()
                        }

                        // @BACKTICK Step 3
                        (_, Some((_, peek_i, '`', _))) if tick_nest_depth > 0 => {
                            walker.next();

                            // Count contiguous trailing backslash
                            let mut count = 1;
                            state.buffer.chars().rev().take(tick_nest_depth).all(|c| {
                                if c == '\\' {
                                    count += 1;
                                    true
                                } else {
                                    false
                                }
                            });

                            // Minus one because  '`' and '\`' both have no
                            // backslashes in 'state.buffer'
                            let num_of_backslash = (count - 1) * '\\'.len_utf8();
                            let len = state.buffer.len();

                            if count == tick_nest_depth {
                                // Remove the backslashes
                                state.buffer.truncate(len - num_of_backslash);
                                //println!("start '{}' - {} {}", state.buffer, len, num_of_backslash);
                                state.lexeme_delimit();
                                state.emit(Lexeme::SubShellStart);
                                state.start_a_nesting(info, Nestable::Backtick);
                            } else if count + 1 == tick_nest_depth {
                                // Remove the backslashes
                                state.buffer.truncate(len - num_of_backslash);
                                //println!("close {:?} - {} {}", state.buffer, len, num_of_backslash);
                                state.lexeme_delimit();
                                state.emit(Lexeme::SubShellClose);
                                state.close_a_nesting(Nestable::Backtick).unwrap();
                            } else {
                                panic!("mismatched backticks");
                            }

                            peek_i + '`'.len_utf8()
                        }

                        // Escaping is different in unquoted and quoted
                        // e.g. '\a' -> 'a'
                        //      '\$' -> '$'
                        (ExpandableQuote::Unquoted, Some((_, peek_i, c, _))) => {
                            walker.next();
                            state.lexeme_push_char(c);
                            peek_i + c.len_utf8()
                        }

                        // Double-quotes and here-documents print the backslash
                        // if not escaping a semantic character
                        // e.g. '\a' -> '\a'
                        //      '\$' -> '$'
                        (ExpandableQuote::Double, Some((_, peek_i, c, _))) => {
                            walker.next();
                            if ['$', '`', '\\', '"'].iter().any(|x| *x == c) {
                                state.lexeme_push_char(c);
                            }
                            peek_i + c.len_utf8()
                        }

                        (ExpandableQuote::HereDoc, Some((_, peek_i, c, _))) => {
                            walker.next();
                            if ['$', '`', '\\'].iter().any(|x| *x == c) {
                                state.lexeme_push_char(c);
                            }
                            peek_i + c.len_utf8()
                        }

                        // Universal
                        // POSIX does not specify, but Dash does the following:
                        // If end of file, just return the current backslash
                        (_, None) => {
                            state.lexeme_push_char('\\');
                            state.buffer.len()
                        }
                    };

                    let peek = walker.peek();
                    // @BACKTICK Step 2
                    if peek.map(|(_, _, c, _)| c == '\\').unwrap_or(false) {
                        walker.next();
                    } else {
                        cursor.move_to(new_index);
                        break;
                    }
                }
            }

            // @BACKTICK Step 1
            '`' => {
                state.lexeme_append(cursor.move_to(index));
                state.lexeme_delimit();

                if state.depth_of(Nestable::Backtick) == 0 {
                    state.start_a_nesting(info, Nestable::Backtick);
                    state.emit(Lexeme::SubShellStart);
                } else {
                    state.close_a_nesting(Nestable::Backtick).unwrap();
                    state.emit(Lexeme::SubShellClose);
                }

                cursor.move_to(index + '`'.len_utf8());
            }

            // @POSIX
            '$' => {
                state.lexeme_append(cursor.move_to(index));
                state.lexeme_delimit();

                match (rest_of_line.chars().nth(1), rest_of_line.chars().nth(2)) {
                    (Some('('), Some('(')) => {
                        state.start_a_nesting(info, Nestable::Arithmetic);
                        state.emit(Lexeme::ArithmeticStart);
                        cursor.move_to(index + "$((".len());
                    }
                    (Some('('), _) => {
                        state.start_a_nesting(info, Nestable::Parenthesis);
                        state.emit(Lexeme::SubShellStart);
                        cursor.move_to(index + "$(".len());
                    }
                    //(Some('{'), _)
                    _ => {} // Plaintext
                }
            }

            // @VOLATILE: make sure this happens before handling blanks in
            //            case 'is_blank' also returns true for newlines
            //            Also before here-doc general case
            //
            // In POSIX, only newlines end commands (and ; &) see 2.9.3
            // Carriage-return is a non-space
            '\n' => {
                if state.heredoc_delim_list.is_empty() {
                    state.end_command_and_walk_to_next_token(
                        &mut walker,
                        &mut cursor,
                        index,
                        ch,
                        Lexeme::EndOfCommand,
                    );

                // If the delim was never finished by newline, error
                // @POSIX 2.9
                // @HEREDOC Step 3.1
                } else if state.flags.is(BUILD_DELIM | BUILD_DELIM_TAB) {
                    panic!("Here-document delimiter not specified");

                // set the 'state.quote_state'
                // @HEREDOC Step 3.2
                } else {
                    state.heredoc_maybe_end_or_strip_tab(&mut walker, &mut cursor, index);
                }
            }

            // Do not expand Here-documents past here (i.e. not double-quotes)
            _ if ExpandableQuote::HereDoc == state.quote_state => {}

            // Escaping is different within quotes/here-documents
            '"' => {
                state.flags.set(HAS_SINGLE_OR_DOUBLE_QUOTE);
                state.lexeme_append(cursor.move_to(index));
                state.quote_state = match state.quote_state {
                    ExpandableQuote::Unquoted => ExpandableQuote::Double,
                    ExpandableQuote::Double => ExpandableQuote::Unquoted,
                    ExpandableQuote::HereDoc => panic!("Lexer logic error"),
                };

                cursor.move_to(index + '"'.len_utf8());
            }

            _ if ExpandableQuote::Double == state.quote_state => {}

            ////////////////////////////////////////////////////////////////////
            // Expandable-quotes do not expand past here

            // @VOLATILE: make sure this happens after handling newlines in
            //            case 'is_blank' also returns true for newlines
            _ if is_blank(ch) => {
                state.lexeme_append(cursor.move_to(index));
                state.lexeme_delimit();

                // Skip continguous <blank>
                walker.skip_blanks(&mut cursor, index + ' '.len_utf8());

                match walker.peek().map(|(_, _, c, _)| c) {
                    Some('\n') => {}                // Skip trailing <blank>
                    Some(';') => {}                 // Skip trailing <blank>
                    Some('&') => {}                 // Skip trailing <blank>
                    _ if state.output_index == 0  => {} // Skip leading <blank>
                    // else no leading blank
                    _ => state.emit(Lexeme::Separator),
                }
            }

            // Single quote
            '\'' => {
                state.flags.set(HAS_SINGLE_OR_DOUBLE_QUOTE);
                state.lexeme_append(cursor.move_to(index));
                cursor.move_to(index + 1); // Skip opening quote
                if let Some(closing_quote) = walker.next_till(|c| c == '\'') {
                    state.lexeme_append(cursor.move_to(closing_quote));
                    state.lexeme_delimit();
                    cursor.move_to(closing_quote + 1); // Skip closing quote
                } else {
                    panic!("Unterminated single quote");
                }
                state.flags.unset(HAS_SINGLE_OR_DOUBLE_QUOTE);
            }

            //'(' => {
            //}

            ')' if state.depth_of(Nestable::Parenthesis) > 0 => {
                state.lexeme_append(cursor.move_to(index));
                state.lexeme_delimit();

                match (state.current_nesting_type(), walker.peek()) {
                    (Some(Nestable::Arithmetic), Some((_, _, ')', _))) => {
                        cursor.move_to(index + "))".len());
                        state.emit(Lexeme::ArithmeticClose);
                        state.close_a_nesting(Nestable::Arithmetic).unwrap();
                    }
                    (Some(Nestable::Arithmetic), _) => {
                        panic!("Hello");
                    }
                    (Some(Nestable::Parenthesis), _) => {
                        cursor.move_to(index + ')'.len_utf8());
                        state.emit(Lexeme::SubShellClose);
                        state.close_a_nesting(Nestable::Parenthesis).unwrap();
                    }
                    (Some(_), _) => {
                        panic!("Unmatched parenthesis");
                    }
                    (None, _) => {
                        panic!("Unexpected parenthesis");
                    }
                }
            }

            // @TODO multiple here documents
            '<' => {
                let till_now = cursor.move_to(index);
                state.lexeme_append(till_now);
                state.lexeme_delimit();

                match (rest_of_line.get(1..2), rest_of_line.get(2..3)) {
                    // @HEREDOC Step 1.1
                    (Some("<"), Some("-")) => {
                        state.flags.set(BUILD_DELIM_TAB);
                        walker.next();
                        walker.next();
                        walker.skip_blanks(&mut cursor, index + "<<-".len());
                        state.emit(Lexeme::OpInputHereDoc);
                    }
                    // @HEREDOC Step 1.2
                    (Some("<"), _) => {
                        state.flags.set(BUILD_DELIM);
                        walker.next();
                        walker.skip_blanks(&mut cursor, index + "<<".len());
                        state.emit(Lexeme::OpInputHereDoc);
                    }
                    _ => {
                        cursor.move_to(index + "<".len());
                        state.emit(Lexeme::OpInputRedirect);
                    }
                }
            }

            '&' => state.end_command_and_walk_to_next_token(
                &mut walker,
                &mut cursor,
                index,
                ch,
                Lexeme::EndOfBackgroundCommand,
            ),
            ';' => state.end_command_and_walk_to_next_token(
                &mut walker,
                &mut cursor,
                index,
                ch,
                Lexeme::EndOfCommand,
            ),

            //')' => {
            //    panic!("Unmatched parenthensis");
            //}

            //'{' => {
            //    state.lexeme_append(cursor.move_to(index));
            //    state.lexeme_delimit();
            //    cursor.move_to(index + '{'.len_utf8());
            //    state.emit(Lexeme::ClosureStart);
            //}
            //'}' => {
            //    state.lexeme_append(cursor.move_to(index));
            //    state.lexeme_delimit();
            //    cursor.move_to(index + '}'.len_utf8());
            //    state.emit(Lexeme::ClosureClose);
            //}

            // @POSIX 2.10.2 (Step 7) Shell Grammar Rules
            // @TODO if first word starts with '='
            '=' if state.output_index == 0 => {
                state.lexeme_append(cursor.move_to(index));
                let buffer = &state.buffer;
                if !buffer.is_empty() && is_valid_variable_name(buffer) {
                    state.lexeme_delimit();
                    cursor.move_to(index + '='.len_utf8());
                    state.emit(Lexeme::OpAssign);
                }
            }

            '#' => {
                // Comment
                state.lexeme_append(cursor.move_to(index));

                // @TODO logic errors here
                if state.buffer.is_empty() {
                    state.lexeme_delimit();
                    let after_pound = index + '#'.len_utf8();
                    cursor.move_to(after_pound);

                    let before_newline = walker.peek_while(|c| c != '\n').unwrap_or(after_pound);
                    let till_newline = cursor.move_to(before_newline);
                    state.emit(Lexeme::Comment(body[till_newline].into()));
                }
            }

            _ => {} // Allow it to perform 'walker.next()' in peace
        }

        // @TODO if check if quote level is not UNQUOTED
    }

    state.lexeme_delimit();
}

// @POSIX 2.10.2 (Step 7b) Shell Grammar Rules, 3.230 Name
// @TODO add this
fn is_valid_variable_name(_ident: &str) -> bool {
    true
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
<<-     "H"ello cat -; <<EOF cat -
	strips tab
    amazing
	strips tab
Hello
	does not strip tab
  printf %s\\n hello "${asdf}"\
	does not strip tab
  yo
EOF
echo hello
"##;

    const SCRIPT3: &str = r##"
     curl -LO `<something.txt sed
        \`cat program.sed\\\`echo hello\\\`\`` >hello.txt
     echo $( hello )

     echo `echo \`echo \\\`printf %s\\n hello\\\`\``
"##;

    fn emit1(token: Lexeme) {
        match token {
            Lexeme::EndOfBackgroundCommand => println!("&"),
            Lexeme::EndOfCommand => println!(";"),
            //Lexeme::HereDocClose => println!("HereDocClose"),
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
        let input = SCRIPT2;
        let script_stream = stream::iter(vec![input]);
        task::block_on(async {
            println!("I am doing things\n====");

            job_stream_lex(script_stream, |token| {
                token_list.push(token);
            })
            .await;
        });
        println!("{}\n======", input);
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
                Lexeme::Word("main()".into()),
                Lexeme::Separator,
                Lexeme::Word("{".into()),
                Lexeme::EndOfCommand,
                Lexeme::Comment(" yo".to_string()),
                Lexeme::Word("asdf".into()),
                Lexeme::OpAssign,
                Lexeme::Word("hello".into()),
                Lexeme::EndOfCommand,
                Lexeme::Word("printf".into()),
                Lexeme::Separator,
                Lexeme::Word(r"%s\n".into()),
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
            ]
        )
    }

    //#[test]
    //fn grid() {
    //    TextGridWalk::new(SCRIPT).for_each(|a| println!("{:?}", a));
    //}
}
