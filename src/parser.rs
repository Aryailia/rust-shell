//run: cargo test shell_tests -- --nocapture

use crate::lexer::Lexeme;

use futures::channel::mpsc::{UnboundedReceiver, UnboundedSender};
use futures::stream::{Peekable, Stream};
use futures::{future, stream, StreamExt};
use std::io;
use std::mem::{discriminant, replace, Discriminant};
use std::pin::Pin;
use std::process::{Command, Stdio};

type ParserInput = UnboundedReceiver<Lexeme>;

// The output type
#[derive(Debug)]
pub enum Statement {
    Assign,
    Label(usize),
    External(Executable),
    Debug(Vec<Lexeme>),
}

//#[derive(Debug)]
pub struct Executable {
    args: Vec<Lexeme>,
    handles: IoHandles,
}

impl std::fmt::Debug for Executable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Executable({:?}    <{} >{} 2>{})",
            self.args,
            match &self.handles.stdin {
                FileId::Descriptor(0) => "&0".to_string(),
                FileId::Descriptor(1) => "&1".to_string(),
                FileId::Descriptor(2) => "&2".to_string(),
                FileId::Path(s) => format!("{:?}", s),
                _ => format!("{:?}", self.handles.stdin),
            },
            match &self.handles.stdout {
                FileId::Descriptor(0) => "&0".to_string(),
                FileId::Descriptor(1) => "&1".to_string(),
                FileId::Descriptor(2) => "&2".to_string(),
                FileId::Path(s) => format!("{:?}", s),
                _ => format!("{:?}", self.handles.stdout),
            },
            match &self.handles.stderr {
                FileId::Descriptor(0) => "&0".to_string(),
                FileId::Descriptor(1) => "&1".to_string(),
                FileId::Descriptor(2) => "&2".to_string(),
                FileId::Path(s) => format!("{:?}", s),
                _ => format!("{:?}", self.handles.stderr),
            },
        )
    }
}

impl Executable {
    // @TODO Change to From<_>?
    //fn run(&self) {
    //    let mut args = self.args.iter();
    //    let mut cmd = Command::new(args.next().unwrap());
    //    cmd.args(args);
    //    //match &self.stdin {
    //    //    FileId::Descriptor(0) => cmd.stdin(io::stdin()),
    //    //    FileId::Descriptor(1) => cmd.stdin(io::stdout()),
    //    //    FileId::Descriptor(2) => cmd.stdin(io::stderr()),
    //    //    FileId::Descriptor(_) => &mut cmd,
    //    //    FileId::Path(_) => cmd.stdin(Stdio::piped()),
    //    //    FileId::Piped => cmd.stdin(Stdio::piped()),
    //    //};
    //}
}

type Word = Vec<Lexeme>; // word as in @POSIX 2

// Representation of file descriptors
#[derive(Clone, Debug)]
enum FileId {
    Descriptor(usize),
    Path(Word),
    PublicVariable(String),
    PrivateVariable(usize, usize),
    Piped,
}

// @TODO @POSIX 2.7 Redirection says at least [0,9] shall be supported
// Maybe an associated array or an array of (descripto
#[derive(Clone, Debug)]
struct IoHandles {
    stdin: FileId,
    stdout: FileId,
    stderr: FileId,
}

impl IoHandles {
    fn new() -> Self {
        Self {
            stdin: FileId::Descriptor(0),
            stdout: FileId::Descriptor(1),
            stderr: FileId::Descriptor(2),
        }
    }
}

#[derive(Debug)]
struct ParserNesting {
    level: usize,

    // Number of temporary values needed (0-indexed), handles for the nesting
    stack: Vec<(usize, IoHandles, ParseMode)>,
}
impl ParserNesting {
    fn new() -> Self {
        Self {
            level: 0, // The lengths of the below should = `level + 1`
            // Keeping track of the IO handles for each nesting
            stack: vec![(0, IoHandles::new(), ParseMode::Regular)],
        }
    }
    fn start(&mut self, mode: ParseMode, redirected_handles: Option<IoHandles>) {
        let level = self.level;

        self.stack.push((
            0,
            redirected_handles.unwrap_or(IoHandles {
                stdin: self.stack[level].1.stdin.clone(),
                stdout: FileId::PrivateVariable(level, 0),
                stderr: self.stack[level].1.stderr.clone(),
            }),
            mode,
        ));
        self.stack[level].0 += 1; // Post placement means 0-indexed
        self.level += 1;
    }

    fn close(&mut self) -> (usize, usize) {
        self.level -= 1;
        let level = self.level;
        self.stack.pop().expect("Unreachable");
        (level, self.stack[level].0)
    }

    fn reset(&mut self) {
        self.stack[self.level].0 = 0;
    }

    fn copy_current(&self) -> IoHandles {
        self.stack[self.level].1.clone()
    }

    fn current_mode(&self) -> &ParseMode {
        &self.stack[self.level].2
    }
}

// The state for the FSM (Finite State Machine)
#[derive(Debug)]
struct StatementBuilder {
    output: UnboundedSender<Statement>,
    mode: ParseMode,

    buffer: Vec<Lexeme>,
    nesting: ParserNesting,
    command_handles: IoHandles,
    label: usize, // For loops and function calls
}

impl StatementBuilder {
    fn emit(&self, stmt: Statement) {
        self.output.unbounded_send(stmt).unwrap();
    }

    fn emit_statement(&mut self) {
        let Self {
            buffer,
            nesting,
            command_handles,
            output,
            ..
        } = self;
        let len = buffer.len();
        let last_unnested_index = buffer
            .iter()
            .rev()
            .position(|lexeme| match lexeme {
                Lexeme::ArithmeticStart(_) | Lexeme::SubShellStart(_) | Lexeme::ClosureStart(_) => {
                    true
                }
                _ => false,
            })
            .map(|reverse_index| len - reverse_index)
            .unwrap_or(0);

        let args = buffer.split_off(last_unnested_index);

        //println!("buffer {:?} {:?}", find_nest_start_index, self.buffer);
        //match args.first() {
        //    Some(Word
        //    None => None
        //}
        if !args.is_empty() {
            let handles = replace(command_handles, nesting.copy_current());

            output
                .unbounded_send(Statement::External(Executable { args, handles }))
                .unwrap()
        }
    }
}

//fn word_expand(word: String) -> String {
//    QuoteWalker::with_parts_capacity(word.as_str(), 1)
//}

// The primary mode
fn parse_regular(builder: &mut StatementBuilder, lexeme: Lexeme) {
    //println!("Lexeme {:?}", lexeme);

    match &lexeme {
        Lexeme::Comment(_) => {}

        Lexeme::SubShellStart(_) => {
            builder.nesting.start(ParseMode::Scoped, None);
            builder.buffer.push(lexeme); // Important for 'emit_statement()'
        }
        Lexeme::SubShellClose(_) | Lexeme::ClosureClose(_) => {
            builder.emit_statement();
            if let None = builder.buffer.pop() {
                // remove the starter
                unreachable!("The lexer should catch unmatched parens");
            }

            let handle = builder.nesting.close();
            builder.buffer.push(Lexeme::Private(handle.0, handle.1));
        }

        Lexeme::OpAssign => {
            if let Some(Lexeme::Variable(name)) = builder.buffer.pop() {
                builder.command_handles.stdout = FileId::PublicVariable(name);
            } else {
                // This should be ensured by the lexer
                unreachable!("'=' did not find a variable");
            }
        }

        Lexeme::OpInput | Lexeme::OpOutput | Lexeme::OpInputHereDoc => {
            let len = builder.buffer.len();
            let switch_to = ParseMode::NextWord(lexeme, len);
            builder.nesting.start(switch_to, None);
        }

        Lexeme::Separator => {}

        Lexeme::EndOfCommand => {
            builder.nesting.reset();
            builder.emit_statement()
        }
        Lexeme::Text(_) => {
            builder.buffer.push(lexeme);
        }
        _ => builder.buffer.push(lexeme),
    }
}

// The possible modes for the FSM
#[derive(Debug)]
enum ParseMode {
    Regular,
    Scoped,
    NextWord(Lexeme, usize),
}

// The main parser command
pub async fn job_stream_parse(input: ParserInput, output: UnboundedSender<Statement>) {
    let input = &mut input.peekable();
    let mut input = Pin::new(input);

    let nesting = ParserNesting::new();
    let command_handles = nesting.copy_current();

    let builder = &mut StatementBuilder {
        output,
        mode: ParseMode::Regular,
        nesting,
        buffer: Vec::new(),
        label: 0,
        command_handles,
    };

    loop {
        //println!("{:?} {:?}", builder.nesting.level, input.as_mut().peek().await);
        //println!("{:?}", builder.nesting.current_mode());

        // Doing a FSM-type approach avoids recursion
        // 'parsemode_next_word()' calls parse_regular
        // async recursion requires 'Box<..>'
        match builder.nesting.current_mode() {
            ParseMode::Regular | ParseMode::Scoped => {
                if let Some(lexeme) = input.as_mut().next().await {
                    parse_regular(builder, lexeme);
                } else {
                    break;
                }
            }
            ParseMode::NextWord(lexeme_ref, len) => {
                // Rustc complains if not on separate line
                let len_at_start = *len;
                let type_at_start = discriminant(lexeme_ref);

                parsemode_next_word(builder, input.as_mut(), len_at_start, type_at_start).await;
            } //mode => unreachable!("Parser FSM state: {:?}", mode),
        }
    }
    builder.emit_statement();
}

async fn parsemode_next_word(
    builder: &mut StatementBuilder,
    mut stream: Pin<&mut Peekable<ParserInput>>,
    start_len: usize,
    lexeme_type: Discriminant<Lexeme>,
) {
    while let Some(lexeme_ref) = stream.as_mut().peek().await {
        match lexeme_ref {
            Lexeme::Text(_) | Lexeme::Variable(_) => {
                let lexeme = stream.as_mut().next().await.expect("never none");
                builder.buffer.push(lexeme);
            }
            _ if lexeme_type == discriminant(&Lexeme::OpInputHereDoc) => {
                todo!();
            }
            Lexeme::ArithmeticStart(_) | Lexeme::SubShellStart(_) | Lexeme::ClosureStart(_) => {
                let lexeme = stream.as_mut().next().await.expect("never none");
                parse_regular(builder, lexeme);
                break; // resume the current nesting
            }

            // In particular, 'Lexeme::Separator' will break here
            _ => {
                builder.nesting.close();
                let file = FileId::Path(builder.buffer.split_off(start_len));

                if lexeme_type == discriminant(&Lexeme::OpInput) {
                    builder.command_handles.stdout = file;
                } else if lexeme_type == discriminant(&Lexeme::OpOutput) {
                    builder.command_handles.stdout = file;
                //} else if lexeme_type == discriminant(&Lexeme::OpError) {
                //    builder.command_handles.stdout = file;
                } else {
                    unreachable!("Should never reach this: {:?}", lexeme_type);
                }

                break;
            }
        }
    }
}

const NON_QUOTE: char = 0 as char;
const UNQUOTED: bool = false;

struct QuoteState(char, bool);
struct QuoteWalk<'a> {
    state: QuoteState,
    iter_list: Vec<std::str::Chars<'a>>,
    iter_index: usize,
}

impl<'a> QuoteWalk<'a> {
    fn with_parts_capacity(source: &'a str, parts_capacity: usize) -> Self {
        let mut iter_list = Vec::with_capacity(parts_capacity);
        iter_list.push(source.chars());
        Self {
            state: QuoteState(NON_QUOTE, UNQUOTED),
            iter_list,
            iter_index: 0,
        }
    }

    // @VOLATILE: dealing with 'is_last' bool in '.next()' might cause problems
    // if you 'walk_chain()' after .next() reaches end
    // @TODO maybe we want to solve this by making this a fixed buffer, only
    // able to process 'capacity' number of items
    // Also can solve this with 'peekable()' maybe
    fn walk_chain(mut self, source: &'a str) -> Self {
        self.iter_list.push(source.chars());
        self
    }

    // Returns whether this character is to be included in the output or not
    fn update_quote_state(state: &mut QuoteState, ch: char, is_last_ch: bool) -> bool {
        let QuoteState(quote_style, backslashed) = state;
        match ch {
            _ if *backslashed /* && *quote_style != '\'' */ => {
                *backslashed = false;
                true
            }
            // One of these patterns is handle above, but that is intended
            '\\' if *quote_style != '\'' && !is_last_ch => {
                *backslashed = true;
                false
            }
            '\'' | '"' => {
                if *quote_style == ch {
                    *quote_style = NON_QUOTE;
                    false
                } else if *quote_style == NON_QUOTE {
                    *quote_style = ch;
                    false
                } else {
                    true
                }
            }
            _ => true,
        }
    }
}

impl<'a> Iterator for QuoteWalk<'a> {
    type Item = (char, bool, bool);
    fn next(&mut self) -> Option<Self::Item> {
        let index = &mut self.iter_index;

        let (ch, rest) = match self.iter_list[*index].next() {
            Some(c) => (c, self.iter_list[*index].as_str()),
            None => {
                *index += 1;
                let iter = self.iter_list.get_mut(*index)?;
                // @Invariant: all Chars in iter_list are non_empty
                let c = iter.next().unwrap();
                (c, iter.as_str())
            }
        };

        let is_last = (*index + 1 >= self.iter_list.len()) && rest.is_empty();
        let is_quoted = self.state.0 != NON_QUOTE || self.state.1;
        let state = &mut self.state;
        let is_included = Self::update_quote_state(state, ch, is_last);
        Some((ch, is_quoted, is_included))
    }
}

// Same thing as `QuoteWalk.filter(|..| is_included)`, just with 'retain()'
fn quote_removal(mut word: String) -> String {
    let mut state = QuoteState(NON_QUOTE, UNQUOTED);
    let len = word.len();
    let mut end = 0;
    word.retain(|ch| {
        end += ch.len_utf8();
        QuoteWalk::update_quote_state(&mut state, ch, end == len)
    });
    word
}

#[cfg(test)]
mod parser_helper_tests {
    use super::*;

    #[test]
    fn extendable_quote_walk() {
        let a = String::from(r#"'h\as'b\\\'f""#);
        let b = String::from(r#"end\"q" here\"#);
        let dequoted = QuoteWalk::with_parts_capacity(a.as_str(), 2)
            .walk_chain(b.as_str())
            .filter(|(_, _, is_included)| *is_included)
            .map(|(s, _, _)| s)
            .collect::<String>();
        let answer = r#"h\asb\'fend"q here\"#;
        assert_eq!(quote_removal(format!("{}{}", a, b)), answer.to_string());
        assert_eq!(dequoted, answer.to_string());

        let walk = QuoteWalk::with_parts_capacity(a.as_str(), 2)
            .walk_chain(b.as_str())
            .collect::<Vec<_>>();
        assert_eq!(
            walk,
            vec![
                ('\'', false, false), // start has false for middle (is_quoted)
                ('h', true, true),
                ('\\', true, true),
                ('a', true, true),
                ('s', true, true),
                ('\'', true, false),
                ('b', false, true),
                ('\\', false, false), // start has false for middle (is_quoted)
                ('\\', true, true),
                ('\\', false, false), // start has false for middle (is_quoted)
                ('\'', true, true),
                ('f', false, true),
                ('"', false, false), // start has false for middle (is_quoted)
                ('e', true, true),
                ('n', true, true),
                ('d', true, true),
                ('\\', true, false), // true for middle because inside double quote
                ('"', true, true),
                ('q', true, true),
                ('"', true, false),
                (' ', false, true),
                ('h', false, true),
                ('e', false, true),
                ('r', false, true),
                ('e', false, true),
                ('\\', false, true),
            ]
        );
    }
}
