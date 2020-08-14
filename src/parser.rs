//run: cargo test shell_tests -- --nocapture

use crate::model::{Executable, FileId, IoHandles, Lexeme, Parseme};

use futures::channel::mpsc::{UnboundedReceiver, UnboundedSender};
use futures::stream::{Peekable, Stream};
use futures::{future, stream, StreamExt};
use std::io;
use std::mem::{discriminant, replace, Discriminant};
use std::pin::Pin;
use std::process::{Command, Stdio};
use std::collections::VecDeque;

type ParserInput = UnboundedReceiver<Lexeme>;

// The output type

#[derive(Debug)]
struct Nesting {
    temp_var_count: usize,
    handles: IoHandles,
    mode: ParseMode,
}

#[derive(Debug)]
struct ParserNesting {
    level: usize,
    label_index: usize,

    // Number of temporary values needed (0-indexed), handles for the nesting
    stack: Vec<(usize, IoHandles, ParseMode)>,
}
impl ParserNesting {
    fn new() -> Self {
        Self {
            level: 0, // The lengths of the below should = `level + 1`
            // Keeping track of the IO handles for each nesting
            stack: vec![(0, IoHandles::new(), ParseMode::Regular)],
            label_index: 0,
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

    fn close(&mut self) -> (usize, usize, ParseMode) {
        self.level -= 1;
        let level = self.level;
        let popped = self.stack.pop().unwrap();
        (level, self.stack[level].0, popped.2)
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
struct ParsemeBuilder {
    output: UnboundedSender<Parseme>,
    mode: ParseMode,

    buffer: Vec<Lexeme>,
    nesting: ParserNesting,
    command_handles: IoHandles,
    label: usize, // For loops and function calls
    queue: Queue,
    output_index: usize,
}

impl ParsemeBuilder {
    fn emit(&mut self, stmt: Parseme) {
        self.output.unbounded_send(stmt).unwrap();
        self.output_index += 1;
    }

    fn emit_parseme(&mut self) {
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
                Lexeme::ArithmeticStart
                | Lexeme::SubShellStart
                | Lexeme::ClosureStart
                | Lexeme::If
                | Lexeme::While
                | Lexeme::Until => true,
                _ => false,
            })
            .map(|reverse_index| len - reverse_index)
            .unwrap_or(0);

        let args = buffer.split_off(last_unnested_index);
        if !args.is_empty() {
            let handles = replace(command_handles, nesting.copy_current());

            self.output_index += 1;
            output
                .unbounded_send(Parseme::External(Executable { args, handles }))
                .unwrap()
        }
    }
}

//fn word_expand(word: String) -> String {
//    QuoteWalker::with_parts_capacity(word.as_str(), 1)
//}

// This is intentionally not async, parsing one lexeme at a time
// For relational lexemes (requires more than on lexeme to have meaning), we
// change states for our parser FSM ('parsemode_*')
// True if opens a nesting
fn parse_regular(builder: &mut ParsemeBuilder) {
    // @TODO This should be exhaustive to catch all errors
    while let Some(lexeme) = builder.queue.pop_front() {
        match &lexeme {
            Lexeme::Comment(_) => {}

            Lexeme::SubShellStart => {
                builder.nesting.start(ParseMode::Scoped, None);
                builder.buffer.push(lexeme); // Important for 'emit_parseme()'
            }
            Lexeme::SubShellClose | Lexeme::ClosureClose => {
                builder.emit_parseme();
                if let None = builder.buffer.pop() {
                    // remove the starter
                    unreachable!("The lexer should catch unmatched parens");
                }

                let handle = builder.nesting.close();
                builder.buffer.push(Lexeme::Private((handle.0, handle.1)));
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

            Lexeme::While => {
                let start_index = builder.output_index;
                builder.nesting.start(ParseMode::While(start_index), None);
            }

            Lexeme::Separator => {}

            Lexeme::EndOfCommand => {
                builder.nesting.reset();
                builder.emit_parseme();
            }
            Lexeme::Text(_) => {
                builder.buffer.push(lexeme);
            }

            Lexeme::Do => {
                // @TODO: fix lexer so we do not need this
                if let Some(Lexeme::EndOfCommand) = builder.queue.get(0) {
                    builder.queue.pop_front();
                }


                builder.queue.push_front(Lexeme::Do); // for 'count_parsemes()'
                println!("queue {:?}", builder.queue);
                let end_index = builder.output_index + count_parsemes(&builder.queue, 0, discriminant(&Lexeme::Done));
                //println!("index {} {}", builder.output_index, body);

                // + 1 to arrive after the final jump of Lexeme::Done
                match builder.nesting.current_mode() {
                    ParseMode::While(_) => {
                        builder.emit(Parseme::JumpIfFalse(end_index));
                    }
                    ParseMode::Until(_) => {
                        builder.emit(Parseme::JumpIfTrue(end_index));
                    }
                    _ => unreachable!("Should be count by queueing function"),
                }

                builder.queue.pop_front(); // Remove the added one
            }

            Lexeme::Done => {
                match builder.nesting.close().2 {
                    ParseMode::While(index) | ParseMode::Until(index) =>
                        builder.emit(Parseme::Jump(index)),
                    _ => panic!("'done' with no associated 'do'"),
                }
            }

            _ => {
                builder.buffer.push(lexeme);
            }
        }
    }
}

fn pop_front_while_next_is<T, F: Fn(&T) -> bool>(queue: &mut VecDeque<T>, sentinel: F) {
    while let Some(x) = queue.get(0) {
        if sentinel(x) {
            queue.pop_front();
        } else {
            break;
        }
    }
}

//
// The possible modes for the FSM
#[derive(Debug)]
enum ParseMode {
    Regular,
    Scoped,
    NextWord(Lexeme, usize),
    ControlFlow(Lexeme),
    ControlBody(Lexeme),
    While(usize),
    Until(usize),
}

pub async fn job_stream_parse(input: ParserInput, output: UnboundedSender<Parseme>) {
    let input = &mut input.peekable();
    let mut input = Pin::new(input);

    let nesting = ParserNesting::new();
    let command_handles = nesting.copy_current();

    let builder = &mut ParsemeBuilder {
        output,
        mode: ParseMode::Regular,
        nesting,
        buffer: Vec::new(),
        label: 0,
        command_handles,
        queue: VecDeque::new(),
        output_index: 0,
    };

    while let Some(peek) = input.as_mut().peek().await {
        match peek {
            Lexeme::While | Lexeme::Until => {
                buffer_loop(builder, input.as_mut()).await;
            }
            _ => {
                let command_end = discriminant(&Lexeme::EndOfCommand);
                let queue = &mut builder.queue;
                buffer_parsemes_till(queue, input.as_mut(), command_end).await;
            }
        }
        parse_regular(builder);
    }
    //println!("{:?}", builder.queue);
}


type Queue = VecDeque<Lexeme>;


async fn buffer_loop(builder: &mut ParsemeBuilder, mut stream: Pin<&mut Peekable<ParserInput>>) {
    let ParsemeBuilder { queue, ..} = builder;
    let command_end = discriminant(&Lexeme::EndOfCommand);
    buffer_parsemes_till(queue, stream.as_mut(), command_end).await;
    //builder.nesting.reset();

    let _count = if let Some(Lexeme::Do) = stream.as_mut().peek().await {
        let while_end = discriminant(&Lexeme::Done);
        buffer_parsemes_till(queue, stream.as_mut(), while_end).await
    } else {
        panic!("Did not find do associated with while loop");
    };
    buffer_parsemes_till(queue, stream.as_mut(), command_end).await;

    // nesting.start()

    //println!("{} {:?}", parseme_count, builder.buffer);
}

// @TODO convert these into array lookups
async fn buffer_parsemes_till(
    queue: &mut Queue,
    mut stream: Pin<&mut Peekable<ParserInput>>,
    close_lexeme: Discriminant<Lexeme>,
) {
    let mut level = 0usize;
    while let Some(lexeme) = stream.as_mut().next().await {
        match &lexeme {
            Lexeme::If | Lexeme::Do => { level += 1; }
            Lexeme::EndIf | Lexeme::Done => { level -= 1; }
            _ => {}
        }

        if level == 0 && discriminant(&lexeme) == close_lexeme {
            queue.push_back(lexeme);
            break;
        } else {
            queue.push_back(lexeme);
        }
        //println!("{:?} {} {}", lexeme, level, count);
    }
}

fn count_parsemes(queue: &Queue, start_level: usize, closer: Discriminant<Lexeme>) -> usize {
    let mut level = start_level; // @TODO Maybe just use a signed number?
    let mut iter = queue.iter();
    let mut sum = 0;
    while let Some(lexeme_ref) = iter.next() {
        match lexeme_ref {
            Lexeme::If | Lexeme::Do => level += 1,
            Lexeme::EndIf | Lexeme::Done => level -= 1,
            _ => {}
        }
        //println!("{:?}", lexeme_ref);
        sum += match lexeme_ref {
            Lexeme::EndOfCommand | Lexeme::SubShellClose => 1,
            Lexeme::Then | Lexeme::Do | Lexeme::Done => 1, // Jumps
            _ => 0,
        };
        if level == 0 && discriminant(lexeme_ref) == closer {
            break;
        }
    }
    sum
}

////////////////////////////////////////////////////////////////////////////////
// Probably axing this stuff or moving to run step

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
