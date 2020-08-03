//run: cargo test shell_tests -- --nocapture

// @TODO echo `printf %s\\\n`  - figure out what should be done in this case

// Dash is: git.kernel.org/pub/scm/utils/dash.git
use crate::helpers::{OwnedToOption, TextGridWalk};
use async_std::task;
use futures::{stream, stream::Stream, StreamExt};
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
    Parameter = 1,
    Backtick = 2,
    Arithmetic = 3,
    Parenthesis = 4,
    Curly = 5,
    DoubleQuote = 6,
    HereDocument = 7,
}
const NEST_TOTAL_SIZE: usize = 8; // @VOLATILE: 'LexMode' final discriminant + 1

// @TODO change to Cow<str> or &str if possible for later stages
#[derive(Debug, PartialEq)]
pub enum Lexeme {
    Word(String), // 'Words' as defined in "2. Shell Command Language"
    Comment(String),
    CommentStart, // Might remove this in favour of just Comment
    Separator,
    EndOfCommand,
    EndOfBackgroundCommand,
    Unprocessed(String),
    NewEnvCommand(String),
    SameEnvCommand(String),

    ArithmeticStart(usize),
    ArithmeticClose(usize),
    SubShellStart(usize),
    SubShellClose(usize),
    ClosureStart(usize),
    ClosureClose(usize),
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

    Private(usize, usize),
    Debug(String),
}

// Token, Strip Tabs?, Quoted Delim?
type HereDocDelimList = VecDeque<(String, bool, bool)>;
type Info = (usize, usize);

// Need one per file source processed
// In charge of
struct LexemeBuilder<'a> {
    //flags: Flag,
    buffer: LexemeBuffer<'a>,
    nesting: NestingTracker,
    heredoc_delim_list: HereDocDelimList,
}

// For keeping mutability of .'buffer' and use of 'emitter' to local methods
impl<'a> LexemeBuilder<'a> {
    fn new(buffer: &'a str, emitter: &'a mut dyn FnMut(Lexeme)) -> Self {
        Self {
            //flags: Flag::new(),
            buffer: LexemeBuffer {
                source: buffer,
                // @TODO find a good size to init this to
                buffer: String::new(),
                output_index: 0,
                emitter: emitter,
            },
            nesting: NestingTracker {
                stack: Vec::new(),
                depth: [0; NEST_TOTAL_SIZE],
            },
            heredoc_delim_list: HereDocDelimList::new(),
        }
    }
}

struct LexemeBuffer<'a> {
    source: &'a str,
    buffer: String,
    output_index: usize,
    emitter: &'a mut dyn FnMut(Lexeme),
}

impl<'a> LexemeBuffer<'a> {
    fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }
    fn is_first_lexeme(&self) -> bool {
        self.output_index == 0
    }

    // @TODO Not sure how to make this more idiomatic
    //fn substr(&self, range: Range<usize>) -> &str {
    //    &self.source[range]
    //}

    // Do not interact with buffer, emit custom
    fn emit(&mut self, lexeme: Lexeme) {
        (self.emitter)(lexeme);
        self.output_index += 1;
    }

    // Append to the work-in-progress buffer
    fn push_range(&mut self, range: Range<usize>) -> &str {
        self.buffer.push_str(&self.source[range]);
        &self.buffer
    }

    // Append to the work-in-progress buffer
    fn push_char(&mut self, to_add: char) {
        self.buffer.push(to_add);
    }

    fn push_str(&mut self, to_add: &str) {
        self.buffer.push_str(to_add);
    }

    fn delimit(&mut self) {
        // Strip leading blanks
        if !self.buffer.is_empty() {
            // @POSIX 2.10.2 (Step 1) Shell Grammar Rules
            // @TODO: Reserved words cannot have quotes
            // @TODO: benchmark if this is faster than .take()
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
}

// Basically the state changer for the finite state machine
struct NestingTracker {
    stack: Vec<(LexMode, Info)>,
    depth: [usize; NEST_TOTAL_SIZE],
}

impl NestingTracker {
    fn start(&mut self, buffer: &mut LexemeBuffer, val: Info, nest_type: LexMode) {
        self.depth[nest_type.clone() as usize] += 1;
        self.stack.push((nest_type, val));
        buffer.output_index = 0;
    }

    fn current_type(&self) -> Option<&LexMode> {
        self.stack.last().map(|(t, _)| t)
    }

    // Ensure nestings can not close over eaach other
    // e.g. Cannot have '$( ` ) `
    // Represents a state change for the finite state machine
    fn close(&mut self, expected: LexMode) -> Result<(), String> {
        match self.stack.pop() {
            // @TODO implement error handling
            Some((nest_type, _error_info)) => {
                if discriminant(&nest_type) == discriminant(&expected) {
                    self.depth[nest_type as usize] -= 1;
                    Ok(())
                } else {
                    panic!("Mistmatched closing delimiter {:?}", self.stack);
                    //Err("Mismatched closing delimiter".into())
                }
            }

            // A closer without a opener
            None => Err("Unexpected character".into()),
        }
    }
    fn depth_of(&self, nest_type: LexMode) -> usize {
        self.depth[nest_type as usize]
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

impl<'a> TextGridWalk<'a> {
    fn peek_while_varname(&mut self) -> Option<usize> {
        self.peek().and_then(|(rest_of_line, i, _, _)| {
            let varname_end = i + find_valid_variable_name(rest_of_line);
            if varname_end > i {
                while let Some(_) = self.next() {
                    if self.current_end_index() == varname_end {
                        break;
                    }
                }
                Some(self.current_end_index())
            } else {
                None
            }
        })
    }

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
fn lexmode_here_document(
    LexemeBuilder {
        buffer,
        nesting,
        heredoc_delim_list,
        ..
    }: &mut LexemeBuilder,
    walker: &mut TextGridWalk,
    cursor: &mut Cursor,
) -> bool {
    //@TODO: validate token, error if have newlines inside it
    let delim_entry = heredoc_delim_list.front().unwrap();
    let (token, is_strip_tabs, is_delim_quoted) = delim_entry;

    while let Some(entry) = walker.next() {
        let (_, _, ch, info) = entry;
        match ch {
            '\n' => {
                if let Some((line, i, c, _)) = walker.peek() {
                    if line == token {
                        // Includes the trailing newline by convention
                        buffer.push_range(cursor.move_to(i));
                        cursor.move_to(walker.peek_while(|c| c != '\n'));

                        heredoc_delim_list.pop_front();
                        buffer.delimit();
                        nesting.close(LexMode::HereDocument).unwrap();
                        buffer.emit(Lexeme::EndOfFile);

                        break;
                    } else if *is_strip_tabs && c == '\t' {
                        buffer.push_range(cursor.move_to(i));
                        cursor.move_to(i + '\t'.len_utf8());
                    }
                } // Let post-loop handle the non-case
            }
            _ if *is_delim_quoted => {} // Always literal

            // See 'lexmode_double_quote()' for why these are not special cased
            '\\' => lex_backslash(walker, cursor, buffer),
            '`' => {
                lex_backtick(walker, cursor, buffer, nesting, info);
                break; // because this nests
            }
            '$' => {
                lex_dollar(walker, cursor, buffer, nesting, info);
                break; // because this nests
            }
            _ => {}
        }
    }
    // Last character is always '\n' in a here-document
    // Thus the appending will always be done here

    // @TODO figure this out, and deal with case where we reach end of input
    //walker.peek().is_some()
    true
}

fn lexmode_double_quote(
    LexemeBuilder {
        buffer, nesting, ..
    }: &mut LexemeBuilder,
    walker: &mut TextGridWalk,
    cursor: &mut Cursor,
) -> bool {
    while let Some(item) = walker.next() {
        let (_, index, ch, info) = item;
        match ch {
            '"' => {
                buffer.push_range(cursor.move_to(index + '"'.len_utf8()));
                nesting.close(LexMode::DoubleQuote).unwrap();
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
            '\\' => lex_backslash(walker, cursor, buffer),
            '`' => {
                lex_backtick(walker, cursor, buffer, nesting, info);
                return true; // because this nests
            }
            '$' => {
                lex_dollar(walker, cursor, buffer, nesting, info);
                return true; // because this nests
            }
            _ => {}
        }
    }
    false
}

// @TODO: Make it so that we do not allocate 'buffer' several times,
//        perhaps a second buffer in state?
// @TODO: loop through all continguous escape sequences, optimisation?
fn lexmode_backtick(
    state: &mut LexemeBuilder,
    walker: &mut TextGridWalk,
    cursor: &mut Cursor,
) -> bool {
    let LexemeBuilder {
        buffer, nesting, ..
    } = state;

    match walker.next() {
        Some((_, _, '\\', info)) => {
            // Double because two escaped backslash is "\\\\" or r"\\"
            let nest_depth = nesting.depth_of(LexMode::Backtick) * 2;
            let mut temp = String::with_capacity(nest_depth);

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
                        temp.push('\\');
                        temp.push('\\');
                        continue;
                    }
                    Some((_, _, '`', _)) => {
                        walker.next(); // move to backtick
                                       // 'temp' only has backslashes or is "" here
                        let backslash_count = temp.len();
                        //println!("tick {:?} - {} {}", temp, backslash_count, nest_depth);

                        // Extra `+ 2` or one extra nesting from just plain '`'
                        if backslash_count + 2 == nest_depth {
                            // Count depth before starting
                            let id = nesting.depth_of(LexMode::Backtick);
                            buffer.emit(Lexeme::SubShellStart(id));
                            state.nesting.start(buffer, info, LexMode::Backtick);

                            temp.clear()

                        // Two more nestings (+ 4) (r"\\\`" is third level)
                        } else if backslash_count + 4 >= nest_depth {
                            // Count depth after closing
                            nesting.close(LexMode::Backtick).unwrap();
                            let id = nesting.depth_of(LexMode::Backtick);
                            buffer.emit(Lexeme::SubShellClose(id));

                            temp.truncate(backslash_count + 4 - nest_depth);
                        } else {
                            panic!("mismatched backticks");
                        }
                    }

                    // Same as lex_regular, just without moving a cursor
                    _ => {
                        let range = walker.walk_regular_backslash();
                        temp.push_str(&buffer.source[range]);
                    }
                }
                break;
            }

            // Append buffer
            cursor.move_to(walker.current_end_index());
            buffer.push_str(temp.as_str());
        }
        Some((_, index, '`', _)) => {
            buffer.push_range(cursor.move_to(index));
            buffer.delimit();

            // Count depth after closing
            nesting.close(LexMode::Backtick).unwrap();
            let id = nesting.depth_of(LexMode::Backtick);
            buffer.emit(Lexeme::SubShellClose(id));

            cursor.move_to(index + '`'.len_utf8());
        }

        Some(entry) => lex_regular(walker, cursor, state, entry),
        None => return false,
    }
    true
}

fn lexmode_parameter(
    state: &mut LexemeBuilder,
    walker: &mut TextGridWalk,
    cursor: &mut Cursor,
) -> bool {
    if let Some(entry) = walker.next() {
        let (_, index, ch, _) = entry;
        match ch {
            '\\' | '"' | '\'' => lex_regular(walker, cursor, state, entry),
            '}' => {
                let substr = &state.buffer.source[cursor.move_to(index)];
                state.nesting.close(LexMode::Parameter).unwrap();
                cursor.move_to(index + '}'.len_utf8());
                state.buffer.emit(Lexeme::Variable(substr.into()));
            }
            _ => {}
        }
    }
    true
}

// Basically implementing a Finite State Machine, where 'nesting.start()'
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
        let mode = state.nesting.current_type().unwrap_or(&LexMode::Regular);

        // @TODO: parens
        // @TODO: curly braces
        // @TODO: operators
        //println!("Mode {:?} {:?}", mode, walker.peek());

        let is_continue = match mode {
            LexMode::Regular | LexMode::Parenthesis | LexMode::Arithmetic => {
                if let Some(entry) = walker.next() {
                    lex_regular(walker, cursor, state, entry);
                    true
                } else {
                    false
                }
            }
            LexMode::DoubleQuote => lexmode_double_quote(state, walker, cursor),
            LexMode::Parameter => lexmode_parameter(state, walker, cursor),
            LexMode::HereDocument => lexmode_here_document(state, walker, cursor),
            LexMode::Backtick => lexmode_backtick(state, walker, cursor),
            m => todo!("{:?}", m),
        };
        if !is_continue {
            break;
        }
    }

    state.buffer.push_range(cursor.move_to(body.len()));
    state.buffer.delimit();
    state.buffer.emit(Lexeme::EndOfCommand);
    // @TODO Error on left-over nestings?
}

fn lex_backtick(
    walker: &mut TextGridWalk,
    cursor: &mut Cursor,
    buffer: &mut LexemeBuffer,
    nesting: &mut NestingTracker,
    info: Info,
) {
    let after_backtick = walker.current_end_index();
    buffer.push_range(cursor.move_to(after_backtick - '`'.len_utf8()));
    buffer.delimit();

    // Count depth before starting
    let id = nesting.depth_of(LexMode::Backtick);
    buffer.emit(Lexeme::SubShellStart(id));
    nesting.start(buffer, info, LexMode::Backtick);

    cursor.move_to(after_backtick);
}

fn lex_dollar(
    walker: &mut TextGridWalk,
    cursor: &mut Cursor,
    buffer: &mut LexemeBuffer,
    nesting: &mut NestingTracker,
    info: Info,
) {
    if let Some((_, peek_index, peek_ch, _)) = walker.peek() {
        buffer.push_range(cursor.move_to(peek_index - '$'.len_utf8()));
        buffer.delimit();

        match peek_ch {
            '(' => {
                walker.next(); // Skip '$' arriving at '$('
                cursor.move_to(walker.current_end_index());

                if let Some((_, _, '(', _)) = walker.peek() {
                    walker.next(); // Skipped '$(' arriving at '$(('

                    // Count depth before starting
                    let id = nesting.depth_of(LexMode::Arithmetic);
                    buffer.emit(Lexeme::ArithmeticStart(id));
                    nesting.start(buffer, info, LexMode::Arithmetic);
                } else {
                    // Count depth before starting
                    let id = nesting.depth_of(LexMode::Parenthesis);
                    buffer.emit(Lexeme::SubShellStart(id));
                    nesting.start(buffer, info, LexMode::Parenthesis);
                }
            }

            '{' => {
                walker.next(); // Skip '$'
                cursor.move_to(peek_index + '{'.len_utf8());
                nesting.start(buffer, info, LexMode::Parameter);
            }

            '@' | '*' | '#' | '?' | '-' | '$' | '!' => {
                walker.next(); // Skip '$'
                cursor.move_to(peek_index + peek_ch.len_utf8());
                buffer.emit(Lexeme::Variable(peek_ch.to_string()));
            }

            _ => {
                // Do not `walker.next()` since doing a `peek_while...`
                walker.peek_while_varname().map(|closer| {
                    let name = &buffer.source[cursor.move_to(closer)];
                    buffer.emit(Lexeme::Variable(name.into()));
                });
            }
        }
    }
}

fn lex_backslash(walker: &mut TextGridWalk, cursor: &mut Cursor, buffer: &mut LexemeBuffer) {
    let before_first_backslash = walker.current_end_index() - '\\'.len_utf8();
    buffer.push_range(cursor.move_to(before_first_backslash));
    buffer.push_range(walker.walk_regular_backslash());
    cursor.move_to(walker.current_end_index());
}

// @VOLATILE: Coordinate with ending heredoc branch of '\n'
//            Does not seem like heredoc needs ot set '.output_index'
fn lex_command_end_and_eat_whitespace(
    walker: &mut TextGridWalk,
    cursor: &mut Cursor,
    buffer: &mut LexemeBuffer,
    token: Lexeme,
) {
    if !buffer.is_first_lexeme() {
        buffer.emit(token);
        buffer.output_index = 0;
    }
    // Skip until the next non-blank, non-newline character
    let non_blank = walker.peek_while(|c| is_blank(c) || c == '\n');
    cursor.move_to(non_blank);
}

fn lex_regular(
    walker: &mut TextGridWalk,
    cursor: &mut Cursor,
    LexemeBuilder {
        buffer,
        nesting,
        heredoc_delim_list,
        ..
    }: &mut LexemeBuilder,
    (rest_of_line, index, ch, info): <TextGridWalk as Iterator>::Item,
) {
    match ch {
        '\\' => lex_backslash(walker, cursor, buffer),
        '\'' => {
            if let None = walker.next_till(|c| c == '\'') {
                panic!("Unterminated single quote");
            }
        }

        // Escaping is different within quotes/here-documents
        '"' => nesting.start(buffer, info, LexMode::DoubleQuote),
        '`' => lex_backtick(walker, cursor, buffer, nesting, info),

        // @POSIX
        '$' => lex_dollar(walker, cursor, buffer, nesting, info),

        // @VOLATILE: make sure this happens before handling blanks in
        //            case 'is_blank' also returns true for newlines
        //            Also before here-doc general case
        //
        // In POSIX, only newlines end commands (and ; &) see 2.9.3
        // Carriage-return is a non-space
        '\n' => {
            buffer.push_range(cursor.move_to(index));
            buffer.delimit();
            if heredoc_delim_list.is_empty() {
                lex_command_end_and_eat_whitespace(walker, cursor, buffer, Lexeme::EndOfCommand);

            // If the delim was never finished by newline, error
            // @POSIX 2.9
            // 'heredoc_delim_list' is not empty
            } else {
                cursor.move_to(index + '\n'.len_utf8());
                buffer.emit(Lexeme::EndOfCommand);
                buffer.emit(Lexeme::HereDocStart);
                nesting.start(buffer, info, LexMode::HereDocument);

                //@TODO: validate token, error if have newlines inside it
                let entry = heredoc_delim_list.front().unwrap();
                let (_token, is_strip_tabs, _) = entry;

                // Handle if first character if is tab
                if *is_strip_tabs {
                    if let Some((_, i, '\t', _)) = walker.peek() {
                        walker.next();
                        cursor.move_to(i + '\t'.len_utf8());
                    }
                }
            }
        }

        // @VOLATILE: make sure this happens after handling newlines in
        //            case 'is_blank' also returns true for newlines
        _ if is_blank(ch) => {
            buffer.push_range(cursor.move_to(index));
            buffer.delimit();
            // Skip continguous <blank>
            cursor.move_to(walker.peek_while(is_blank));

            match walker.peek().map(|(_, _, c, _)| c) {
                Some('\n') => {}                    // Skip trailing <blank>
                Some(';') => {}                     // Skip trailing <blank>
                Some('&') => {}                     // Skip trailing <blank>
                _ if buffer.is_first_lexeme() => {} // Skip leading <blank>
                // else no leading blank
                _ => buffer.emit(Lexeme::Separator),
            }
        }
        '&' => {
            buffer.push_range(cursor.move_to(index));
            buffer.delimit();
            lex_command_end_and_eat_whitespace(
                walker,
                cursor,
                buffer,
                Lexeme::EndOfBackgroundCommand,
            );
        }

        ';' => {
            buffer.push_range(cursor.move_to(index));
            buffer.delimit();
            if let Some((_, _, ';', _)) = walker.peek() {
                buffer.emit(Lexeme::Debug("Break".into()));
            } else {
                lex_command_end_and_eat_whitespace(walker, cursor, buffer, Lexeme::EndOfCommand);
            }
        }

        ')' if nesting.depth_of(LexMode::Parenthesis) > 0 => {
            buffer.push_range(cursor.move_to(index));
            buffer.delimit();

            match (nesting.current_type(), walker.peek()) {
                (Some(LexMode::Arithmetic), Some((_, _, ')', _))) => {
                    cursor.move_to(index + "))".len());

                    // Count depth after closing
                    nesting.close(LexMode::Arithmetic).unwrap();
                    let id = nesting.depth_of(LexMode::Arithmetic);
                    buffer.emit(Lexeme::ArithmeticClose(id));
                }
                (Some(LexMode::Arithmetic), _) => {
                    panic!("Hello");
                }
                (Some(LexMode::Parenthesis), _) => {
                    cursor.move_to(index + ')'.len_utf8());

                    // Count depth after closing
                    nesting.close(LexMode::Parenthesis).unwrap();
                    let id = nesting.depth_of(LexMode::Parenthesis);
                    buffer.emit(Lexeme::SubShellClose(id));
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
            buffer.push_range(cursor.move_to(index));
            buffer.delimit();

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

                    heredoc_delim_list.push_back((delim, is_strip, is_quoted));
                    cursor.move_to(walker.peek_while(is_blank));
                    buffer.emit(Lexeme::OpInputHereDoc);
                }
                _ => {
                    cursor.move_to(index + "<".len());
                    buffer.emit(Lexeme::OpInputRedirect);
                }
            }
        }

        // @TODO Rust RFC 2353 issue #60553 means no more allocation
        '=' if buffer.is_first_lexeme() => {
            let varname = &buffer.source[cursor.span(index)];
            let is_valid = find_valid_variable_name(varname) == varname.len();
            if !varname.is_empty() && is_valid {
                buffer.emit(Lexeme::Variable(varname.into()));
                cursor.move_to(index + '='.len_utf8());
                buffer.emit(Lexeme::OpAssign);
            } // else is just plaintext to be lexed as 'Lexeme::Word'
        }

        '#' if buffer.is_empty() => {
            buffer.push_range(cursor.move_to(index));
            buffer.delimit();

            let before_newline = walker.peek_while(|c| c != '\n');
            cursor.move_to(before_newline);
            // Without the '#'
            buffer.emit(Lexeme::Comment(rest_of_line[1..].into()));
        }

        _ => {} // Allow it to perform 'walker.next()' in peace
    }
}

// @POSIX 2.10.2 (Step 7b) Shell Grammar Rules
// @POSIX 3.230 Name
//
// Finds the end index that constitute a valid variable name
fn find_valid_variable_name(ident: &str) -> usize {
    let mut chars = ident.chars();
    let default = 0 as char;
    debug_assert!(!default.is_ascii_alphabetic() && default != '_');
    let first_char = chars.next().unwrap_or(default);

    if first_char.is_ascii_alphabetic() || first_char == '_' {
        // Not digit
        let mut sum = first_char.len_utf8();
        while let Some(c) = chars.next() {
            if c.is_ascii_alphanumeric() || c == '_' {
                sum += c.len_utf8();
            } else {
                break;
            }
        }
        sum
    } else {
        0
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

pub async fn job_stream_lex<'a, T, F>(mut job_stream: T, mut emit: F)
where
    T: Stream<Item = &'a str> + Unpin,
    F: FnMut(Lexeme),
{
    while let Some(body) = job_stream.next().await {
        // Maybe parse shebang?
        file_lex(body, &mut emit);
    }
}
