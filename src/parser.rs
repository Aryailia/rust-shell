//run: cargo test shell_tests -- --nocapture

use crate::lexer::Lexeme;
use futures::{future, stream, stream::Stream, StreamExt};
use std::mem::{discriminant, take};
use std::process::Command;

fn dummy_is_valid(lexeme: &Lexeme) -> bool {
    match lexeme {
        Lexeme::Word(word) => word.chars().all(|c| c.is_ascii_alphanumeric()),
        _ => false
    }
}

fn parse_statement(statement: Vec<Lexeme>) -> Option<()> {
    debug_assert_eq!(statement.is_empty(), false);
    let mut arg_list = statement.iter();
    let first_lexeme = arg_list.next().unwrap();
    if dummy_is_valid(first_lexeme) {
        let cmd = match first_lexeme {
            Lexeme::Word(word) => word,
            x => unreachable!("Should be a Lexeme::Word: {:?}", x),
        };

        let arg_list2 = arg_list.filter_map(|lexeme| {
            match lexeme {
                Lexeme::Word(word) => Some(word),
                _ => None
            }
        }).collect::<Vec<_>>();
        let output = Command::new(cmd)
            .args(arg_list2)
            .output()
            .expect(format!("failed to execute cmd {}", cmd).as_str())
            ;
        println!("Output {:?}", String::from_utf8(output.stdout));
        //println!("{:?}({:?})", cmd, arg_list2);

        Some(())
    } else {
        None
    }
}


pub async fn job_stream_parse<'a, T>(job_stream: T)
where
    T: Stream<Item = Lexeme> + Unpin,
{
    let mut statement_buffer = Vec::new();
    job_stream
        // Group by statements
        .map(|lexeme| {
            //println!("Lexeme {:?}", lexeme);
            lexeme
        })
        .filter_map(|lexeme: Lexeme| {
            let output = match lexeme {
                Lexeme::Comment(_) => None,
                Lexeme::EndOfCommand => {
                    if statement_buffer.is_empty() {
                        None
                    } else {
                        Some(take(&mut statement_buffer))
                    }
                }
                Lexeme::Word(word) => {
                    statement_buffer.push(Lexeme::Word(quote_removal(word)));
                    None
                }
                _ => {
                    statement_buffer.push(lexeme);
                    None
                }
            };
            future::ready(output)
        })
        .map(|statement| {
            println!("Statement {:?}", statement);
            statement
        })
        .map(|statement| {
            parse_statement(statement)
        })
        .for_each(|lexeme| future::ready(println!("{:?}", lexeme)))
        .await;

    //while let Some(a) = stream.next().await {
    //    println!("{:?}", a);
    //}
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
        assert_eq!(walk, vec![
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
        ]);
    }
}
