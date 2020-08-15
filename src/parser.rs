//run: cargo test shell_tests -- --nocapture

// This implements a double buffer because we need to buffer receiver
// until the end of a structure (like a while-do-done), so we know how
// long it is. We need to calculate the jump offset at some point.
// Could also do this later down the pipeline

// Input 'Lexeme's, output 'Parseme's

use crate::model::{Executable, FileId, IoHandles, Parseme as P};
use crate::model::{Lexeme as L, LexemeRepr, LEXEME_LEVEL, LEXEME_PARSEME_COUNT};

use futures::channel::mpsc::{UnboundedReceiver, UnboundedSender};
use futures::{stream::Peekable, stream::Stream, StreamExt};
use std::collections::VecDeque;
use std::mem::replace;
use std::pin::Pin;

type ParserInput<'a> = Pin<&'a mut Peekable<UnboundedReceiver<L>>>;

//#[derive(Debug)]
//struct Nesting {
//    temp_var_count: usize,
//    handles: IoHandles,
//    mode: ParseMode,
//}


//enum Parseme {
//    Lexeme(L),
//    Temp(usize, usize),
//}

#[derive(Debug)]
struct ParserNesting {
    level: usize,
    label_index: usize,

    // Number of temporary values needed (0-indexed), handles for the nesting
    stack: Vec<(usize, IoHandles, ParseMode)>,
    command_handles: IoHandles,
}
impl ParserNesting {
    fn new() -> Self {
        Self {
            level: 0, // The lengths of the below should = `level + 1`
            // Keeping track of the IO handles for each nesting
            stack: vec![(0, IoHandles::new(), ParseMode::Regular)],
            label_index: 0,
            command_handles: IoHandles::new(),
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
        self.command_handles = self.copy_current();
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
    output: UnboundedSender<P>,

    buffer: Vec<L>,
    nesting: ParserNesting,
    label: usize, // For loops and function calls
    queue: Queue,
    output_index: usize,
}

fn emit(output: &mut UnboundedSender<P>, output_index: &mut usize, stmt: P) {
    output.unbounded_send(stmt).unwrap();
    *output_index += 1;
}


trait ParsemeBuffer {
    fn pop_parseme(&mut self, nesting: &mut ParserNesting) -> Option<P>;
}

impl ParsemeBuffer for Vec<L> {
    fn pop_parseme(&mut self, nesting: &mut ParserNesting) -> Option<P> {
        let len = self.len();
        let last_unnested_index = self
            .iter()
            .rev()
            .position(|lexeme| match lexeme {
                L::ArithmeticStart
                | L::SubShellStart
                | L::ClosureStart
                | L::If
                | L::While
                | L::Until => true,
                _ => false,
            })
            .map(|reverse_index| len - reverse_index)
            .unwrap_or(0);

        let args = self.split_off(last_unnested_index);
        if !args.is_empty() {
            let default_handles = nesting.copy_current();
            let handles = replace(&mut nesting.command_handles, default_handles);
            Some(P::Statement(Executable { args, handles }))
        } else {
            None
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
fn parse_regular(ParsemeBuilder { buffer, nesting, queue, output, output_index, ..  }: &mut ParsemeBuilder) {
    // @TODO This should be exhaustive to catch all errors
    while let Some(lexeme) = queue.pop_front() {
        match &lexeme {
            L::Comment(_) => {}

            L::SubShellStart => {
                nesting.start(ParseMode::Scoped, None);
                buffer.push(lexeme); // Important for 'pop_parseme()'
            }
            L::SubShellClose | L::ClosureClose => {
                buffer.push(lexeme); // Important for 'emit_parseme()'
                if let Some(stmt) = buffer.pop_parseme(nesting) {
                    emit(output, output_index, stmt);
                } else {
                    panic!("No statements inside of me");
                }
                if let None = buffer.pop() {
                    // remove the starter
                    unreachable!("The lexer should catch unmatched parens");
                }

                let handle = nesting.close();
                buffer.push(L::Private((handle.0, handle.1)));
            }

            L::OpAssign => {
                if let Some(L::Variable(name)) = buffer.pop() {
                    nesting.command_handles.stdout = FileId::PublicVariable(name);
                } else {
                    // This should be ensured by the lexer
                    unreachable!("'=' did not find a variable");
                }
            }

            L::OpInput | L::OpOutput | L::OpInputHereDoc => {
                let len = buffer.len();
                let switch_to = ParseMode::NextWord(lexeme, len);
                nesting.start(switch_to, None);
            }

            L::While => {
                let start_index = *output_index;
                nesting.start(
                    ParseMode::While(start_index),
                    Some(nesting.copy_current()),
                );
            }

            L::Separator => {}

            L::EndOfCommand => {
                nesting.reset();
                if let Some(stmt) = buffer.pop_parseme(nesting) {
                    emit(output, output_index, stmt);
                }
                // Fine to be an empty command?
            }
            L::Text(_) => {
                buffer.push(lexeme);
            }

            L::Do => {
                // @TODO: fix lexer so we do not need this
                if let Some(L::EndOfCommand) = queue.get(0) {
                    queue.pop_front();
                }

                queue.push_front(L::Do); // for 'count_parsemes()'
                let body_count = count_parsemes(queue, 0, L::Done.id());
                let end_index = *output_index + body_count;
                //println!("index {} {}", output_index, body);

                // + 1 to arrive after the final jump of L::Done
                match nesting.current_mode() {
                    ParseMode::While(_) => {
                        emit(output, output_index, P::JumpIfFalse(end_index));
                    }
                    ParseMode::Until(_) => {
                        emit(output, output_index, P::JumpIfTrue(end_index));
                    }
                    _ => unreachable!("Should be count by queueing function"),
                }

                queue.pop_front(); // Remove the added one
            }

            L::Done => match nesting.close().2 {
                ParseMode::While(index) | ParseMode::Until(index) => emit(output, output_index, P::Jump(index)),
                _ => panic!("'done' with no associated 'do'"),
            },

            _ => {
                buffer.push(lexeme);
            }
        }
    }
}

//fn pop_front_while_next_is<T, F: Fn(&T) -> bool>(queue: &mut VecDeque<T>, sentinel: F) {
//    while let Some(x) = queue.get(0) {
//        if sentinel(x) {
//            queue.pop_front();
//        } else {
//            break;
//        }
//    }
//}

//
// The possible modes for the FSM
#[derive(Debug)]
enum ParseMode {
    Regular,
    Scoped,
    NextWord(L, usize),
    ControlFlow(L),
    ControlBody(L),
    While(usize),
    Until(usize),
}

pub async fn job_stream_parse(input: UnboundedReceiver<L>, output: UnboundedSender<P>) {
    let input = &mut input.peekable();
    let mut input = Pin::new(input);

    let nesting = ParserNesting::new();

    let builder = &mut ParsemeBuilder {
        output,
        nesting,
        buffer: Vec::new(),
        label: 0,
        queue: VecDeque::new(),
        output_index: 0,
    };

    while let Some(peek) = input.as_mut().peek().await {
        let queue = &mut builder.queue;
        match peek {
            L::While | L::Until => {
                buffer_loop(queue, input.as_mut()).await;
            }
            _ => {
                let eoc = L::EndOfCommand.id();
                buffer_parsemes_to(queue, input.as_mut(), 0, eoc).await;
            }
        }
        verify_nesting(queue).unwrap();
        parse_regular(builder);
    }
    //println!("{:?}", builder.queue);
}

////////////////////////////////////////////////////////////////////////////////
// Buffering commands
type Queue = VecDeque<L>;

async fn buffer_loop(queue: &mut Queue, mut stream: ParserInput<'_>) {
    if let Some(L::While) = stream.as_mut().next().await {
        queue.push_back(L::While);
    } else {
        unreachable!("Not called on a while loop");
    }

    let eoc = L::EndOfCommand.id();
    buffer_parsemes_to(queue, stream.as_mut(), 0, eoc).await;

    if let Some(L::Do) = stream.as_mut().peek().await {
        let while_end = L::Done.id();
        let level = LEXEME_LEVEL[L::While.id() as usize] as isize;
        buffer_parsemes_to(queue, stream.as_mut(), level, while_end).await
    } else {
        panic!("Did not find do associated with while loop");
    };
    buffer_parsemes_to(queue, stream.as_mut(), 0, eoc).await;
}

// @TODO convert these into array lookups
async fn buffer_parsemes_to(
    queue: &mut Queue,
    mut stream: ParserInput<'_>,
    start_level: isize,
    close_lexeme: LexemeRepr,
) {
    //println!("{:?}", queue);
    let mut level = start_level;
    while let Some(lexeme) = stream.as_mut().next().await {
        level += LEXEME_LEVEL[lexeme.id() as usize] as isize;
        //println!("  {} {:?}", level, lexeme);

        if level < 0 {
            panic!("Logic error with start_level or LEXEME_LEVEL");
        } else if level == 0 && lexeme.id() == close_lexeme {
            queue.push_back(lexeme); // Must push after borrow
            break;
        } else {
            queue.push_back(lexeme); // Must push after borrow
        }
        //println!("{:?} {} {}", lexeme, level, count);
    }
}

fn verify_nesting(queue: &Queue) -> Result<(), String> {
    enum O<'a> {
        Fine,
        Unclosed(L),
        NoAssociated(&'a str, &'a str),
    }

    let mut stack = Vec::new();
    let mut queue_iter = queue.iter();
    while let Some(lexeme_ref) = queue_iter.next() {
        match lexeme_ref {
            L::While => stack.push(L::While),
            L::Until => stack.push(L::Until),
            _ => {}
        }

        let result = match lexeme_ref {
            L::Do => match stack.pop() {
                Some(L::For) | Some(L::While) | Some(L::Until) => {
                    stack.push(L::Do);
                    O::Fine
                }
                Some(l)
                    if stack
                        .iter()
                        .any(|x| x.id() == L::While.id() || x.id() == L::Until.id()) =>
                {
                    O::Unclosed(l)
                }
                _ => O::NoAssociated("do", "'while', 'until', or 'for'"),
            },
            L::Done => match stack.pop() {
                Some(L::Do) => O::Fine,
                Some(l) if stack.iter().any(|x| x.id() == L::Do.id()) => O::Unclosed(l),
                _ => O::NoAssociated("done", "'do'"),
            },
            _ => O::Fine,
        };

        match result {
            O::Fine => {}
            O::Unclosed(l) => return Err(format!("Close the '{:?}' first", l)),
            O::NoAssociated(cur_lexeme, err) => {
                return Err(format!(
                    "Unexpected '{}' at <some location>, No associated {}",
                    cur_lexeme, err,
                ))
            }
        }
    }
    Ok(())
}

fn count_parsemes(queue: &Queue, start_level: isize, closer: LexemeRepr) -> usize {
    let mut iter = queue.iter();
    let mut level = start_level; // @TODO Maybe just use a signed number?
    let mut count = 0;
    //println!("queue {:?}", queue);
    while let Some(lexeme_ref) = iter.next() {
        level += LEXEME_LEVEL[lexeme_ref.id() as usize] as isize;
        count += LEXEME_PARSEME_COUNT[lexeme_ref.id() as usize] as usize;
        //println!("{:?} {}", lexeme_ref, count);
        if level == 0 && lexeme_ref.id() == closer {
            break;
        }
    }
    count
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
