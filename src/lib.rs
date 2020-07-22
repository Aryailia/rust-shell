//run: cargo test parser_tests -- --nocapture

// Dash is: git.kernel.org/pub/scm/utils/dash.git
#![allow(dead_code, unused_imports)]

mod helpers;

use async_std::task;
use futures::{stream, stream::Stream, StreamExt};
use helpers::{OwnedToOption, TextGridWalk};
use std::ops::Range;

//const COMMENTED: Flag = Flag(0x01);
const NO_FLAGS: Flag = Flag(0x00);
const BUILD_DELIM: Flag = Flag(0x01);
const BUILD_DELIM_TAB: Flag = Flag(0x02);
const HERE_DOCUMENT_STRIP_TABS: Flag = Flag(0x4);
const BACKTICKED: Flag = Flag(0x08);

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

type HereDocDelimList = NestLevelQueue<(String, bool)>;

// Need one per file source processed
// In charge of
struct LexemeBuilder<'a> {
    source: &'a str,
    flags: Flag,
    emitter: &'a mut dyn FnMut(Lexeme),
    buffer: String,
    is_first_token: bool,
}

// For keeping mutability of .'buffer' and use of 'emitter' to local methods
impl<'a> LexemeBuilder<'a> {
    fn new(buffer: &'a str, emitter: &'a mut dyn FnMut(Lexeme)) -> Self {
        Self {
            source: buffer,
            flags: Flag::new(),
            emitter,
            buffer: String::new(),
            is_first_token: true,
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
    // bool return value used for triming leading spaces
    fn lexeme_delimit(&mut self, delim_list: &mut HereDocDelimList) -> bool {
        // Strip leading blanks
        if self.flags.is(BUILD_DELIM | BUILD_DELIM_TAB) {
            let delim = self.buffer.clone();
            delim_list.push((delim, self.flags.is(BUILD_DELIM_TAB)));
            self.buffer.clear();
            self.is_first_token = false;
            self.flags.unset(BUILD_DELIM | BUILD_DELIM_TAB);
            false // Does not matter if true or false (probably)
        } else if !self.is_first_token || !self.buffer.is_empty() {
            (self.emitter)(Lexeme::Word(self.buffer.clone()));
            self.buffer.clear();
            self.is_first_token = false;
            true
        } else {
            false
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

// Random helper methods
impl<'a> LexemeBuilder<'a> {
    // @VOLATILE: Coordinate with ending heredoc branch of '\n'
    //            Does not seem like heredoc needs ot set 'is_first_token'
    fn end_command(
        &mut self,
        cursor: &mut Cursor,
        delim_list: &mut HereDocDelimList,
        index: usize,
        ch: char,
        token: Lexeme,
    ) {
        let till = cursor.move_to(index);
        self.lexeme_append(till);
        self.lexeme_delimit(delim_list);
        cursor.move_to(index + ch.len_utf8());

        // Skip blank lines
        if !self.is_first_token {
            self.emit(token);
        }
        self.is_first_token = true;
    }

    // @POSIX 2.2.1 Double-Quotes
    // @POSIX 2.7.4 Here-Document
    // 'end_sentinel()' takes priority over escaping so make sure to check
    // within 'end_sentinel()'
    fn walk_to_quote_end<F>(
        &mut self,
        walker: &mut TextGridWalk,
        start_index: usize,
        to_escape: &[char],
        is_ignore_commands: bool,
        is_strip_tabs: bool,
        end_sentinel: F,
    ) -> Result<usize, ()>
    where
        F: Fn(char, &str, char) -> bool,
    {
        let mut last_char = '\n';
        let mut last_index = 0;
        let mut temp_cursor = Cursor { index: start_index };
        debug_assert!(to_escape.iter().all(|c| *c != '\n'));

        // @TODO: precompute size?
        while let Some((rest_of_line, index, ch, _)) = walker.next() {
            if end_sentinel(last_char, rest_of_line, ch) {
                let range_after_move = temp_cursor.move_to(index);

                self.lexeme_append(range_after_move);
                return Ok(index);
            } else if last_char == '\\' {
                if to_escape.iter().any(|c| *c == ch) {
                    // Skip
                    let till_tick = temp_cursor.move_to(last_index);
                    self.lexeme_append(till_tick);
                    walker.next(); // Skip backslash, next over char
                    temp_cursor.move_to(index + ch.len_utf8());

                    self.lexeme_push_char(ch);
                } else if ch == '\n' {
                    let till_tick = temp_cursor.move_to(last_index);
                    self.lexeme_append(till_tick);
                    walker.next(); // Skip backslash, next over newline
                    temp_cursor.move_to(index + '\n'.len_utf8());
                }
            } else {
                match ch {
                    '$' if !is_ignore_commands => unimplemented!(),
                    '`' if !is_ignore_commands => unimplemented!(),
                    '\t' if is_strip_tabs && last_char == '\n' => {
                        let till_tab = temp_cursor.move_to(index);
                        self.lexeme_append(till_tab);
                        walker.next();
                        temp_cursor.move_to(index + '\t'.len_utf8());
                    }
                    _ => {}
                }
            }
            last_char = ch;
            last_index = index;
        }
        Err(())
    }
}

// @Test: \<EOF>

const STRIP_TABS: bool = true;
const ALLOW_TABS: bool = false;
const SKIP_COMMAND_EXPANSION: bool = true;

// https://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html
// '2.3.0' Is Token Recognition
fn file_lex<F: FnMut(Lexeme)>(body: &str, emit: &mut F) {
    let mut state = LexemeBuilder::new(body, emit);
    let mut walker = TextGridWalk::new(body);
    let mut cursor = Cursor { index: 0 };

    // @TODO consider moving back into 'LexemeBuilder' on quote rewrite
    // Not in state so we can control borrow checker
    let mut heredoc_nesting = HereDocDelimList {
        index: 0,
        list: Vec::new(),
    };
    let mut tick_nesting = Vec::new();
    let mut paren_nesting = Vec::new();

    // @TODO (_, _, tuple) for error handling

    // 'state.buffer[token_start..index]' defines the token we are currently
    // building up, by default this is considered a word
    // walker.next() is essentianlly incrementing  a token_end
    while let Some((rest_of_line, index, ch, info)) = walker.next() {
        //print!("{:?} ", ch);
        match ch {
            // Backslash gets highest priority
            // Escaping is different within quotes/here-documents
            //
            '\\' => {
                let till_backslash = cursor.move_to(index);
                state.lexeme_append(till_backslash);
                // @TODO make better when RFC-2497 if-let-chains is resolved

                // @BACKTICK Step 2.2
                // Allows us to not have to use '.chars().rev()' later on
                match walker.peek() {
                    Some((_, _, '`', _)) if tick_nesting.is_empty() => {
                        // @TODO Check if we can skip empty lexemes
                        if !state.buffer.is_empty() {
                            state.lexeme_delimit(&mut heredoc_nesting);
                        }
                    }
                    _ => {}
                }
                let mut index = index;

                loop {
                    let tick_nest_level = tick_nesting.len();
                    match walker.peek() {
                        // Escaped newlines are skipped and cannot delimit tokens
                        // @POSIX 2.2.1: Escape Character
                        Some((_, i, '\n', _)) => {
                            walker.next();
                            cursor.move_to(i + '\n'.len_utf8());
                        }
                        // @BACKTICK Step 2.1
                        Some((_, i, '\\', _)) if tick_nest_level > 0 => {
                            walker.next();
                            cursor.move_to(i + '\\'.len_utf8());
                            state.lexeme_push_char('\\');

                            match walker.peek() {
                                Some((_, i, '\\', _)) => {
                                    walker.next();
                                    index = i;
                                    continue;
                                }
                                _ => {}
                            }
                        }
                        // @BACKTICK Step 2.3
                        Some((_, i, '`', info)) if tick_nest_level > 0 => {
                            walker.next();
                            cursor.move_to(i + '`'.len_utf8());

                            let mut count = 1;
                            state.buffer.chars().take(tick_nest_level).all(|c| {
                                if c == '\\' {
                                    count += 1;
                                    true
                                } else {
                                    false
                                }
                            });

                            if count == tick_nest_level {
                                state.emit(Lexeme::SubShellStart);
                                tick_nesting.push(info);
                            } else if count + 1 == tick_nest_level {
                                state.lexeme_delimit(&mut heredoc_nesting);
                                state.emit(Lexeme::SubShellClose);
                                tick_nesting.pop();
                            } else {
                                panic!("mismatched backticks");
                            }
                        }

                        Some((_, i, c, _)) => {
                            walker.next();
                            cursor.move_to(i + c.len_utf8());
                            state.lexeme_push_char(c);
                        }

                        // POSIX does not specify behaviour, but following Dash:
                        // If end of file, just return the current backslash
                        None => {
                            cursor.move_to(index + '\\'.len_utf8());
                            state.lexeme_push_char('\\');
                        }
                    }
                    break
                }

                // Backslash if peek is EOF, else the peek character
            }

            // Quotes
            '\'' => {
                // Single quote
                state.lexeme_append(cursor.move_to(index));
                cursor.move_to(index + 1); // Skip opening quote
                if let Some(index) = walker.next_till(|c| c == '\'') {
                    state.lexeme_append(cursor.move_to(index));
                    cursor.move_to(index + 1); // Skip closing quote
                } else {
                    panic!("Unterminated single quote");
                }
            }

            // Escaping is different within quotes/here-documents
            '"' => {
                state.lexeme_append(cursor.move_to(index));

                let index_of_closing_quote = state
                    .walk_to_quote_end(
                        &mut walker,
                        index + '"'.len_utf8(), // After '"'
                        &['$', '`', '"', '\\'], // newline implicit
                        SKIP_COMMAND_EXPANSION, // @TODO
                        ALLOW_TABS,             // For here-document, ignore
                        |last_ch, _, cur_ch| last_ch != '\\' && cur_ch == '"',
                    )
                    .expect("Unterminated quote");

                cursor.move_to(index_of_closing_quote + '"'.len_utf8());
            }

            // @TODO multiple here documents
            '<' => {
                let till_now = cursor.move_to(index);
                state.lexeme_append(till_now);
                state.lexeme_delimit(&mut heredoc_nesting);

                match (rest_of_line.get(1..2), rest_of_line.get(2..3)) {
                    // @HEREDOC Step 1.1
                    (Some("<"), Some("-")) => {
                        state.flags.set(BUILD_DELIM_TAB);
                        walker.next();
                        walker.next();
                        let non_blank_index = walker.peek_while(is_blank);
                        cursor.move_to(non_blank_index.unwrap_or(index + "<<-".len()));

                        state.emit(Lexeme::OpInputHereDoc);
                    }
                    // @HEREDOC Step 1.2
                    (Some("<"), _) => {
                        state.flags.set(BUILD_DELIM);
                        walker.next();
                        let nonblank = walker.peek_while(is_blank);
                        cursor.move_to(nonblank.unwrap_or(index + "<<".len()));
                        state.emit(Lexeme::OpInputHereDoc);
                    }
                    _ => {
                        cursor.move_to(index + "<".len());
                        state.emit(Lexeme::OpInputRedirect);
                    }
                }
            }

            // @TODO: parens
            // @TODO: curly braces
            // @TODO: here strings
            // @TODO: operators
            // @TODO: $

            // @VOLATILE: make sure this happens before handling blanks in
            //            case 'is_blank' also returns true for newlines
            //
            // In POSIX, only newlines end commands (and ; &) see 2.9.3
            // Carriage-return is a non-space
            '\n' => {
                // @WIP
                // @HEREDOC Step 2
                if !heredoc_nesting.is_empty() {
                    // If the delim was never finished by newline, error
                    // @POSIX 2.9
                    if state.flags.is(BUILD_DELIM | BUILD_DELIM_TAB) {
                        panic!("Here-document delimiter not specified");
                    } else {
                        let till_now = cursor.move_to(index);
                        state.lexeme_append(till_now);
                        state.lexeme_delimit(&mut heredoc_nesting);

                        // @VOLATILE: Coordinate with 'end_command()'
                        let (delim, flag) = heredoc_nesting.pop_front();
                        state.emit(Lexeme::EndOfCommand);
                        state.emit(Lexeme::HereDocStart);

                        // Will always add a newline to end of here-document
                        let closing_heredoc_index = state
                            .walk_to_quote_end(
                                &mut walker,
                                index + '\n'.len_utf8(), // After '\n'
                                &['$', '`', '\\'],       // newline implicit
                                SKIP_COMMAND_EXPANSION,
                                *flag,
                                |last, line, _| last == '\n' && line == delim,
                            )
                            .expect("Unterminated here-document");

                        // Leave buffer, just emitting the quoted text
                        state.lexeme_delimit(&mut heredoc_nesting);
                        state.emit(Lexeme::HereDocClose);

                        cursor.move_to(closing_heredoc_index);
                        // @TODO skip to end of line
                    }
                } else {
                    state.end_command(
                        &mut cursor,
                        &mut heredoc_nesting,
                        index,
                        ch,
                        Lexeme::EndOfCommand,
                    );
                }
                //state.end_command(
                //    &mut cursor,
                //    &mut heredoc_nesting,
                //    index,
                //    ch,
                //    Lexeme::EndOfCommand,
                //);
            }
            // @VOLATILE: make sure this happens after handling newlines in
            //            case 'is_blank' also returns true for newlines
            _ if is_blank(ch) => {
                let till_before_space = cursor.move_to(index);
                state.lexeme_append(till_before_space);
                let is_not_left = state.lexeme_delimit(&mut heredoc_nesting);
                let before_non_blank = walker
                    .peek_while(is_blank)
                    .unwrap_or(index + ' '.len_utf8())  // Skip at least ' '
                    ;
                cursor.move_to(before_non_blank);

                // If not in leading or trailing blanks
                // @TODO: benchmark not having all these ifs since
                //        separators only affect field splitting
                match walker.peek() {
                    Some((_, _, '\n', _)) => {}
                    Some((_, _, ';', _)) => {}
                    Some((_, _, '&', _)) => {}
                    _ if is_not_left => state.emit(Lexeme::Separator),
                    _ => {}
                }
            }

            '&' => state.end_command(
                &mut cursor,
                &mut heredoc_nesting,
                index,
                ch,
                Lexeme::EndOfBackgroundCommand,
            ),
            ';' => state.end_command(
                &mut cursor,
                &mut heredoc_nesting,
                index,
                ch,
                Lexeme::EndOfCommand,
            ),

            // @BACKTICK Step 1
            '`' => {
                state.lexeme_append(cursor.move_to(index));
                // @UNSTABLE: Should be okay to not print emtpy buffer as
                // backticks produce output
                if !state.buffer.is_empty() {
                    state.lexeme_delimit(&mut heredoc_nesting);
                }

                if tick_nesting.is_empty() {
                    state.is_first_token = true;
                    tick_nesting.push(info);
                    state.emit(Lexeme::SubShellStart);
                } else {
                    tick_nesting.pop();
                    state.emit(Lexeme::SubShellClose);
                }

                cursor.move_to(index + '`'.len_utf8());
            }

            '$' => {
                state.lexeme_append(cursor.move_to(index));
                if !state.buffer.is_empty() {
                    state.lexeme_delimit(&mut heredoc_nesting);
                }
                match (rest_of_line.chars().nth(1), rest_of_line.chars().nth(2)) {

                    (Some('('), Some('('))  => {
                        paren_nesting.push((info, Paren::Arithmetic));
                        state.emit(Lexeme::ArithmeticStart);
                        state.is_first_token = true;
                        cursor.move_to(index + "$((".len());
                    }
                    (Some('('), _)  => {
                        paren_nesting.push((info, Paren::Command));
                        state.emit(Lexeme::SubShellStart);
                        state.is_first_token = true;
                        cursor.move_to(index + "$(".len());
                    }
                    _ => {}
                }


            }

            ')' if !paren_nesting.is_empty() => {
                state.lexeme_append(cursor.move_to(index));
                if !state.buffer.is_empty() {
                    state.lexeme_delimit(&mut heredoc_nesting);
                }

                match (paren_nesting.pop(), walker.peek()) {
                    (Some((_, Paren::Arithmetic)), Some((_, _, ')', _))) =>{
                        cursor.move_to(index + "))".len());
                        state.emit(Lexeme::ArithmeticClose);
                    }
                    (Some((_, Paren::Arithmetic)), _) => {
                        panic!("Expected \"))\" to close arithmetic");
                    }
                    (Some((_, Paren::Command)), _) => {
                        cursor.move_to(index + ')'.len_utf8());
                        state.emit(Lexeme::SubShellClose);
                    }
                    (None, _) => {
                        panic!("Unmatched parenthesis");
                    }
                }
                cursor.move_to(index + ')'.len_utf8());
                state.emit(Lexeme::SubShellClose);
            }
            //')' => {
            //    panic!("Unmatched parenthensis");
            //}

            //'{' => {
            //    state.lexeme_append(cursor.move_to(index));
            //    if !state.buffer.is_empty() {
            //        state.lexeme_delimit(&mut heredoc_nesting);
            //    }
            //    cursor.move_to(index + '{'.len_utf8());
            //    state.emit(Lexeme::ClosureStart);
            //}
            //'}' => {
            //    state.lexeme_append(cursor.move_to(index));
            //    if !state.buffer.is_empty() {
            //        state.lexeme_delimit(&mut heredoc_nesting);
            //    }
            //    cursor.move_to(index + '}'.len_utf8());
            //    state.emit(Lexeme::ClosureClose);
            //}


            // @POSIX 2.10.2 (Step 7) Shell Grammar Rules
            // @TODO if first word starts with '='
            '=' if state.is_first_token => {
                state.lexeme_append(cursor.move_to(index));
                state.lexeme_delimit(&mut heredoc_nesting);
                cursor.move_to(index + '='.len_utf8());
                state.emit(Lexeme::OpAssign);
            }

            '#' => { // Comment
                // @TODO logic errors here
                if state.buffer.is_empty() {
                    let till_pound = cursor.move_to(index);
                    state.lexeme_append(till_pound);
                    state.lexeme_delimit(&mut heredoc_nesting);
                    let after_pound = index + '#'.len_utf8();
                    cursor.move_to(after_pound);

                    // Skip till peek() is '\n'
                    let before_newline = walker.peek_while(|c| c != '\n').unwrap_or(after_pound);

                    let range = cursor.move_to(before_newline);
                    state.emit(Lexeme::Comment(state.source[range].into()));
                }
            }
            _ => {} // Allow it to perform 'walker.next()' in peace
        }

        // @TODO if check if quote level is not UNQUOTED
    }
}

enum Paren {
    Arithmetic,
    Command,
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
#!/bin/ashell

     curl -LO `<something.txt sed \`cat program.sed\`` >hello.txt
     echo $( hello )

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
        let script_stream = stream::iter(vec![SCRIPT3]);
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
