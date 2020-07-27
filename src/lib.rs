//run: cargo test parser_tests -- --nocapture

// @TODO echo `printf %s\\\n`  - figure out what should be done in this case

// Dash is: git.kernel.org/pub/scm/utils/dash.git
#![allow(dead_code, unused_imports)]

mod helpers;

use async_std::task;
use futures::{stream, stream::Stream, StreamExt};
use helpers::{OwnedToOption, TextGridWalk};
use std::collections::VecDeque;
use std::mem::{discriminant, Discriminant};
use std::ops::Range;

//const NO_FLAGS: Flag = Flag(0x00);
//
//#[derive(Debug)]
//struct Flag(u8);
//
//impl Flag {
//    fn new() -> Self {
//        Flag(0)
//    }
//    fn is(&self, rhs: Self) -> bool {
//        self.0 & rhs.0 != 0
//    }
//    fn is_not(&self, rhs: Self) -> bool {
//        self.0 & rhs.0 == 0
//    }
//    fn set(&mut self, rhs: Self) {
//        self.0 |= rhs.0;
//    }
//    fn unset(&mut self, rhs: Self) {
//        self.0 &= !rhs.0
//    }
//}
//
//impl std::ops::BitOr for Flag {
//    type Output = Self;
//    fn bitor(self, rhs: Self) -> Self {
//        Self(self.0 | rhs.0)
//    }
//}

#[derive(Clone, Debug)]
#[repr(usize)]
enum LexMode {
    Regular = 0,
    Backtick = 1,
    Arithmetic = 2,
    Parenthesis = 3,
    Curly = 4,
    DoubleQuote = 5,
    HereDocument = 6,
}
const NEST_TOTAL_SIZE: usize = 9; // @VOLATILE: size of 'LexMode'

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
    HereDocStart,
    //HereDocClose,
    EndOfFile,
    Variable(String),

    // List of Operators
    OpInputHereDoc,
    OpInputRedirect,
    OpOutputRedirect,
    OpErrorRedirect,
    OpPipe,
    OpAssign,
    Keyword(String),

    Debug(String),
}

// Token, Strip Tabs?, Quoted Delim?
type HereDocDelimList = VecDeque<(String, bool, bool)>;

// Need one per file source processed
// In charge of
struct LexemeBuilder<'a> {
    source: &'a str,
    //flags: Flag,
    emitter: &'a mut dyn FnMut(Lexeme),
    buffer: String,
    output_index: usize,
    nesting_stack: Vec<(LexMode, Info)>,
    nesting_depth: [usize; NEST_TOTAL_SIZE],
    heredoc_delim_list: HereDocDelimList,
}

// For keeping mutability of .'buffer' and use of 'emitter' to local methods
impl<'a> LexemeBuilder<'a> {
    fn new(buffer: &'a str, emitter: &'a mut dyn FnMut(Lexeme)) -> Self {
        Self {
            source: buffer,
            //flags: Flag::new(),
            emitter,
            buffer: String::new(),
            output_index: 0,
            nesting_stack: Vec::new(),
            nesting_depth: [0; NEST_TOTAL_SIZE],
            heredoc_delim_list: HereDocDelimList::new(),
        }
    }

    // Do not interact with buffer, emit custom
    fn emit(&mut self, lexeme: Lexeme) {
        (self.emitter)(lexeme);
        self.output_index += 1;
    }

    // Append to the work-in-progress buffer
    fn lexeme_append(&mut self, range: Range<usize>) -> &str {
        self.buffer.push_str(&self.source[range]);
        &self.buffer
    }

    // Append to the work-in-progress buffer
    fn lexeme_push_char(&mut self, to_add: char) {
        self.buffer.push(to_add);
    }

    fn lexeme_push_str(&mut self, to_add: &str) {
        self.buffer.push_str(to_add);
    }

    fn lexeme_delimit(&mut self) {
        // Strip leading blanks
        if !self.buffer.is_empty() {
            // @POSIX 2.10.2 (Step 1) Shell Grammar Rules
            // @TODO: Reserved words cannot have quotes
            (self.emitter)(Lexeme::Word(self.buffer.clone()));
            self.buffer.clear();
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
    // Represents a state change for the finite state machine
    fn start_a_nesting(&mut self, val: Info, nest_type: LexMode) {
        self.nesting_depth[nest_type.clone() as usize] += 1;
        self.nesting_stack.push((nest_type, val));
        self.output_index = 0;
    }

    fn current_nesting_type(&self) -> Option<&LexMode> {
        self.nesting_stack.last().map(|(t, _)| t)
    }

    // Ensure nestings can not close over eaach other
    // e.g. Cannot have '$( ` ) `
    // Represents a state change for the finite state machine
    fn close_a_nesting(&mut self, expected: LexMode) -> Result<(), String> {
        match self.nesting_stack.pop() {
            // @TODO implement error handling
            Some((nest_type, _error_info)) => {
                if discriminant(&nest_type) == discriminant(&expected) {
                    self.nesting_depth[nest_type as usize] -= 1;
                    Ok(())
                } else {
                    Err("Mismatched closing delimiter".into())
                }
            }

            // A closer without a opener
            None => Err("Unexpected character".into()),
        }
    }

    fn depth_of(&self, nest_type: LexMode) -> usize {
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

    fn span(&self, target: usize) -> Range<usize> {
        debug_assert!(self.index <= target);
        self.index..target
    }
}

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
        token: Lexeme,
    ) {
        // Skip if current line is a blank line
        if self.output_index > 0 {
            self.emit(token);
            self.output_index = 0;
        }
        // Skip until the next non-blank, non-newline character
        let non_blank = walker.peek_while(|c| is_blank(c) || c == '\n');
        cursor.move_to(non_blank);
    }
}

type Info = (usize, usize);
impl<'a> TextGridWalk<'a> {
    fn walk_to_heredoc_delim_end(&mut self) -> (String, bool) {
        self.peek_while(is_blank);
        let unquoted = 0 as char;

        let mut delim = String::new();
        let mut is_quoted = false;
        let mut quote_type = unquoted;
        debug_assert!(quote_type != '\'' || quote_type != '"');
        // @TODO write up an argument for why this is correct
        while let Some((_, _, ch, _)) = self.peek() {
            match ch {
                // Because I am eagerly command expanding we have to do this
                // manually
                '\\' if quote_type == '"' => {
                    if let Some((_, _, c, _)) = self.peek() {
                        self.next();
                        is_quoted = true;
                        match c {
                            '\n' => {}
                            '$' | '`' | '"' | '\\' => delim.push(c),
                            _ => {
                                delim.push(ch);
                                delim.push(c);
                            }
                        }
                    }
                }

                // Unquoted
                '\\' if quote_type == unquoted => {
                    if let Some((_, _, c, _)) = self.peek() {
                        self.next();
                        is_quoted = true;
                        match c {
                            '\n' => {}
                            _ => delim.push(c),
                        }
                    }
                }

                '"' | '`' => {
                    is_quoted = true;
                    if quote_type == unquoted {
                        quote_type = ch;
                    } else {
                        quote_type = unquoted;
                    }
                }
                _ if quote_type != unquoted => delim.push(ch),
                _ if is_blank(ch) => return (delim, is_quoted),
                '\n' => return (delim, is_quoted),
                _ => delim.push(ch),
            }
            self.next();
        }
        (delim, is_quoted)
    }

    fn walk_regular_backslash(&mut self) -> Range<usize> {
        match self.peek() {
            // Universal for both
            Some((_, i, '\n', _)) => {
                self.next();
                i..i // Only important that it is the range is zero-length
            }

            // Escaping is different in unquoted and quoted
            // e.g. '\a' -> 'a'
            //      '\$' -> '$'
            Some((_, i, c, _)) => {
                self.next();
                i - '\\'.len_utf8()..i + c.len_utf8() // or 'current_end_index'
            }

            // Universal
            // POSIX does not specify, but Dash does the following:
            // If end of file, just return the current backslash
            //
            // The argumenet seems to be:
            // @POSIX 2.3  Step 4 - means backslash not removed at lex step
            // @POSIX 2.3  Step 1 - current token is delimited
            // @POSIX 2.10        - Not recognized as a command
            // @POSIX 2.6         - Word not expanded
            None => {
                let end = self.current_end_index();
                end - '\\'.len_utf8()..end
            }

        }
    }
}

// @POSIX: 2.7.4 Here-Document - Figure out what to do about
// - "until there is a line containing only the delimiter and a <newline>"
//
// I am doing the same as Dash: '<<EOF cat -\nhello\nEOF' is valid
// Expecting to enter after the '\n'
fn lex_here_document(
    state: &mut LexemeBuilder,
    walker: &mut TextGridWalk,
    cursor: &mut Cursor,
) -> bool {
    //@TODO: validate token, error if have newlines inside it
    let (ref token, is_strip_tabs, is_delim_quoted) = state.heredoc_delim_list.pop_front().unwrap();

    // Handle if first character if is tab
    if is_strip_tabs {
        if let Some((_, i, '\t', _)) = walker.next() {
            walker.next();
            cursor.move_to(i + '\t'.len_utf8());
        }
    }
    while let Some(entry) = walker.next() {
        let (_, _, ch, _info) = entry;
        match ch {
            '\n' => {
                if let Some((line, i, c, _)) = walker.peek() {
                    if line == token {
                        // Includes the trailing newline by convention
                        state.lexeme_append(cursor.move_to(i));
                        cursor.move_to(walker.peek_while(|c| c != '\n'));
                        break;
                    } else if is_strip_tabs && c == '\t' {
                        state.lexeme_append(cursor.move_to(i));
                        cursor.move_to(i + '\t'.len_utf8());
                    }
                } // Let post-loop handle the non-case
            }
            _ if is_delim_quoted => {} // Always literal

            // See 'lex_double_quote()' for why these are not special cased
            '\\' | '$' | '`' => lex_regular(entry, state, walker, cursor),

            _ => {}
        }

        if let Some((_, peek_index, peek_char, _)) = walker.peek() {
            if peek_char == '$' || peek_char == '`' {
                state.lexeme_append(cursor.move_to(peek_index));
            }
        }

        //if let Some(x) = walker.next() {
        //    entry = x;
        //} else {
        //    break;
        //}
    }
    // Last character is always '\n' in a here-document
    // Thus the appending will always be done here
    state.lexeme_delimit();
    state.close_a_nesting(LexMode::HereDocument).unwrap();
    state.emit(Lexeme::EndOfFile);
    walker.peek().is_some()
}

fn lex_double_quote(
    state: &mut LexemeBuilder,
    walker: &mut TextGridWalk,
    cursor: &mut Cursor,
) -> bool {
    while let Some(item) = walker.next() {
        let (_, index, ch, _info) = item;
        match ch {
            '"' => {
                state.lexeme_append(cursor.move_to(index));
                state.close_a_nesting(LexMode::DoubleQuote).unwrap();
                cursor.move_to(index + '"'.len_utf8());
                return true;
            }

            // Despite what @POSIX 2.2.3 Double-quotes the same, actually
            // quoted backslash is the same as unquoted backslash
            //
            // Backslash can be regularly lexed because the backslash is
            // is retained within a quote, thus appears in the output:
            //
            // source        => lexer           => quote removal
            // 'hello \ a'   -> <hello> <\ a>   -> 'hello' ' a'
            // 'hello "\ a"' -> <hello> <"\ a"> -> 'hello' '\ a'
            '\\' | '$' | '`' => lex_regular(item, state, walker, cursor),
            _ => {}
        }

        if let Some((_, peek_index, peek_char, _)) = walker.peek() {
            if peek_char == '$' || peek_char == '`' {
                state.lexeme_append(cursor.move_to(peek_index));
            }
        }

    }
    false
}

// @TODO: Make it so that we do not allocate 'buffer' several times,
//        perhaps a second buffer in state?
// @TODO: loop through all continguous escape sequences, optimisation?
fn lex_backtick(state: &mut LexemeBuilder, walker: &mut TextGridWalk, cursor: &mut Cursor) -> bool {
    match walker.next() {
        Some((_, _, '\\', info)) => {
            // Double because two escaped backslash is "\\\\" or r"\\"
            let nest_depth = state.depth_of(LexMode::Backtick) * 2;
            let mut buffer = String::with_capacity(nest_depth);

            // @TODO more comments
            // @HALFPOSIX: This avoid recursively calling lex backtick
            // Build up 'buffer', this is 'lex_regular()' backslash, but with
            // special case for r"\\" and r"\'"
            loop {
                match walker.peek() {
                    // only branch that loops
                    Some((_, _, '\\', _)) => {
                        walker.next(); // move to second backslash
                        walker.next(); // move to next escaping pair
                        buffer.push('\\');
                        buffer.push('\\');
                        continue;
                    }
                    Some((_, _, '`', _)) => {
                        walker.next(); // move to backtick
                        // 'buffer' only has backslashes or is "" here
                        let backslash_count = buffer.len();
                        //println!("tick {:?} - {} {}", buffer, backslash_count, nest_depth);

                        // Extra `+ 2` or one extra nesting from just plain '`'
                        if backslash_count + 2 == nest_depth {
                            state.emit(Lexeme::SubShellStart);
                            state.start_a_nesting(info, LexMode::Backtick);
                            buffer.clear()

                        // Two more nestings (+ 4) (r"\\\`" is third level)
                        } else if backslash_count + 4 >= nest_depth {
                            state.emit(Lexeme::SubShellClose);
                            state.close_a_nesting(LexMode::Backtick).unwrap();
                            buffer.truncate(backslash_count + 4 - nest_depth);
                        } else {
                            panic!("mismatched backticks");
                        }
                    }

                    // Same as lex_regular, just without moving a cursor
                    _ => {
                        let range = walker.walk_regular_backslash();
                        buffer.push_str(&state.source[range]);
                    }
                }
                break;
            }

            // Append buffer
            cursor.move_to(walker.current_end_index());
            state.lexeme_push_str(buffer.as_str());
        }
        Some((_, index, '`', _)) => {
            state.lexeme_append(cursor.move_to(index));
            state.lexeme_delimit();
            state.close_a_nesting(LexMode::Backtick).unwrap();
            state.emit(Lexeme::SubShellClose);
            cursor.move_to(index + '`'.len_utf8());
        }

        Some(entry) => lex_regular(entry, state, walker, cursor),
        None => return false,
    }
    true
}

// Basically implementing a Finite State Machine, where 'start_a_nesting()'
// is how we change states
// https://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html
// '2.3.0' Is Token Recognition
fn file_lex<F: FnMut(Lexeme)>(body: &str, emit: &mut F) {
    let state = &mut LexemeBuilder::new(body, emit);
    let walker = &mut TextGridWalk::new(body);
    let cursor = &mut Cursor { index: 0 };

    // 'state.buffer[token_start..index]' defines the token we are currently
    // building up, by default this is considered a word
    // walker.next() is essentially incrementing  a token_end
    loop {
        let mode = state
            .nesting_stack
            .last()
            .map(|x| &x.0)
            .unwrap_or(&LexMode::Regular);

        // @TODO: parens
        // @TODO: curly braces
        // @TODO: operators
        // @TODO: $

        let is_continue = match mode {
            LexMode::Backtick => lex_backtick(state, walker, cursor),
            LexMode::DoubleQuote => lex_double_quote(state, walker, cursor),
            LexMode::HereDocument => lex_here_document(state, walker, cursor),
            LexMode::Regular | LexMode::Parenthesis | LexMode::Arithmetic => {
                if let Some(item) = walker.next() {
                    lex_regular(item, state, walker, cursor);
                    true
                } else {
                    false
                }
            }
            m => todo!("{:?}", m),
        };
        if !is_continue {
            break;
        }
    }

    //state.lexeme_delimit();

    // @TODO Error on left-over nestings
}

fn lex_regular(
    (rest_of_line, index, ch, info): <TextGridWalk as Iterator>::Item,
    state: &mut LexemeBuilder,
    walker: &mut TextGridWalk,
    cursor: &mut Cursor,
) {
    match ch {
        '\\' => {
            state.lexeme_append(cursor.move_to(index));
            state.lexeme_append(walker.walk_regular_backslash());
            cursor.move_to(walker.current_end_index());
        }
        '\'' => {
            if let None = walker.next_till(|c| c == '\'') {
                panic!("Unterminated single quote");
            }
        }

        // Escaping is different within quotes/here-documents
        '"' => state.start_a_nesting(info, LexMode::DoubleQuote),

        '`' => {
            state.lexeme_append(cursor.move_to(index));
            state.lexeme_delimit();
            state.start_a_nesting(info, LexMode::Backtick);
            state.emit(Lexeme::SubShellStart);
            cursor.move_to(index + '`'.len_utf8());
        }

        // @POSIX
        '$' => {
            state.lexeme_append(cursor.move_to(index));
            state.lexeme_delimit();
            match (rest_of_line.chars().nth(1), rest_of_line.chars().nth(2)) {
                (Some('('), Some('(')) => {
                    state.start_a_nesting(info, LexMode::Arithmetic);
                    state.emit(Lexeme::ArithmeticStart);
                    cursor.move_to(index + "$((".len());
                }
                (Some('('), _) => {
                    state.start_a_nesting(info, LexMode::Parenthesis);
                    state.emit(Lexeme::SubShellStart);
                    cursor.move_to(index + "$(".len());
                }
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
            state.lexeme_append(cursor.move_to(index));
            state.lexeme_delimit();
            if state.heredoc_delim_list.is_empty() {
                state.end_command_and_walk_to_next_token(walker, cursor, Lexeme::EndOfCommand);

            // If the delim was never finished by newline, error
            // @POSIX 2.9
            } else {
                cursor.move_to(index + '\n'.len_utf8());
                state.emit(Lexeme::EndOfCommand);
                state.emit(Lexeme::HereDocStart);
                state.start_a_nesting(info, LexMode::HereDocument);
            }
        }

        // @VOLATILE: make sure this happens after handling newlines in
        //            case 'is_blank' also returns true for newlines
        _ if is_blank(ch) => {
            state.lexeme_append(cursor.move_to(index));
            state.lexeme_delimit();
            // Skip continguous <blank>
            cursor.move_to(walker.peek_while(is_blank));

            match walker.peek().map(|(_, _, c, _)| c) {
                Some('\n') => {}                   // Skip trailing <blank>
                Some(';') => {}                    // Skip trailing <blank>
                Some('&') => {}                    // Skip trailing <blank>
                _ if state.output_index == 0 => {} // Skip leading <blank>
                // else no leading blank
                _ => state.emit(Lexeme::Separator),
            }
        }
        '&' => {
            state.lexeme_append(cursor.move_to(index));
            state.lexeme_delimit();
            state.end_command_and_walk_to_next_token(walker, cursor, Lexeme::EndOfBackgroundCommand);
        }

        ';' => {
            state.lexeme_append(cursor.move_to(index));
            state.lexeme_delimit();
            if let Some((_, _, ';', _)) = walker.peek() {
                state.emit(Lexeme::Debug("Break".into()));
            } else {
                state.end_command_and_walk_to_next_token(walker, cursor, Lexeme::EndOfCommand);
            }
        }

        ')' if state.depth_of(LexMode::Parenthesis) > 0 => {
            state.lexeme_append(cursor.move_to(index));
            state.lexeme_delimit();

            match (state.current_nesting_type(), walker.peek()) {
                (Some(LexMode::Arithmetic), Some((_, _, ')', _))) => {
                    cursor.move_to(index + "))".len());
                    state.emit(Lexeme::ArithmeticClose);
                    state.close_a_nesting(LexMode::Arithmetic).unwrap();
                }
                (Some(LexMode::Arithmetic), _) => {
                    panic!("Hello");
                }
                (Some(LexMode::Parenthesis), _) => {
                    cursor.move_to(index + ')'.len_utf8());
                    state.emit(Lexeme::SubShellClose);
                    state.close_a_nesting(LexMode::Parenthesis).unwrap();
                }
                (Some(_), _) => {
                    panic!("Unmatched parenthesis");
                }
                (None, _) => {
                    panic!("Unexpected parenthesis");
                }
            }
        }

        '<' => {
            state.lexeme_append(cursor.move_to(index));
            state.lexeme_delimit();

            match walker.peek() {
                // "<<"
                Some((_, _, '<', _)) => {
                    walker.next(); // Move to second '<'
                    let is_strip = walker.peek().map(|(_, _, c, _)| c == '-');
                    let is_strip = is_strip.unwrap_or(false);
                    if is_strip {
                        walker.next(); // Is "<<-", move to '-'
                    }
                    let (delim, is_quoted) = walker.walk_to_heredoc_delim_end();

                    state
                        .heredoc_delim_list
                        .push_back((delim, is_strip, is_quoted));
                    cursor.move_to(walker.peek_while(is_blank));
                    state.emit(Lexeme::OpInputHereDoc);
                }
                _ => {
                    cursor.move_to(index + "<".len());
                    state.emit(Lexeme::OpInputRedirect);
                }
            }
        }

        // @TODO Rust RFC 2353 issue #60553 means no more allocation
        '=' if state.output_index == 0 => {
            let varname = &state.source[cursor.span(index)];
            if !varname.is_empty() && is_valid_variable_name(varname) {
                state.emit(Lexeme::Variable(varname.into()));
                cursor.move_to(index + '='.len_utf8());
                state.emit(Lexeme::OpAssign);
            } // else is just plaintext to be lexed as 'Lexeme::Word'
        }

        '#' if state.buffer.is_empty() => {
            state.lexeme_append(cursor.move_to(index));
            state.lexeme_delimit();

            let before_newline = walker.peek_while(|c| c != '\n');
            cursor.move_to(before_newline);
            // Without the '#'
            state.emit(Lexeme::Comment(rest_of_line[1..].into()));
        }

        _ => {} // Allow it to perform 'walker.next()' in peace
    }
}

// @POSIX 2.10.2 (Step 7b) Shell Grammar Rules, 3.230 Name
fn is_valid_variable_name(ident: &str) -> bool {
    let mut chars = ident.chars();
    let test_first_char = chars
        .next()
        .map(|c| c.is_ascii_alphabetic() || c == '_') // Not digit
        .unwrap_or(false);
    test_first_char && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
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
<<-     "Hello$( "EOF cat -; <<EOF cat -; <<-EOF cat -
	strips tab
 backslash newline does not escape\
	this should still strip the tab
Hello$( EOF
	does not strip tab
  printf %s\\n hello "${asdf}"\
	does not strip tab
  yo
EOF
	strips this tab
yo\
	but keeps this tab
EOF
echo "hello " bub
"##;

    const SCRIPT3: &str = r##"
     curl -LO `<something.txt sed
        \`cat program.sed\\\`echo very inner\\\`\`` >hello.txt
     echo $( qwerty )

     echo `echo \`echo \\\`printf %s\\n uiop\\\`\``
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
                Lexeme::EndOfCommand,
                Lexeme::Word("main()".into()),
                Lexeme::Separator,
                Lexeme::Word("{".into()),
                Lexeme::EndOfCommand,
                Lexeme::Comment(" yo".to_string()),
                Lexeme::EndOfCommand,
                Lexeme::Variable("asdf".into()),
                Lexeme::OpAssign,
                Lexeme::Word("'hello'".into()),
                Lexeme::EndOfCommand,
                Lexeme::Word("printf".into()),
                Lexeme::Separator,
                Lexeme::Word(r"%s\\n".into()),
                Lexeme::Separator,
                Lexeme::Word("${asdf}".into()),
                Lexeme::EndOfCommand,
                Lexeme::Word("echo".into()),
                Lexeme::Separator,
                Lexeme::Word("''".into()),
                Lexeme::Separator,
                Lexeme::Word("''".into()),
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
