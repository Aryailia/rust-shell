//run: cargo test parser_tests -- --nocapture

use crate::lexer::Lexeme;
use futures::{future, stream, stream::Stream, StreamExt};
use std::mem::{discriminant, take};
use std::process::Command;


fn quote_removal(lexeme: Lexeme) -> Lexeme {
    if let Lexeme::Word(mut word) = lexeme {
        //let quoted = false;
        word.retain(|ch| {
            true
        });
        Lexeme::Word(word)
    } else {
        panic!("DEV ERROR: Only works for Lexeme::Word(..)");
    }
}

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
        //let output = Command::new(cmd)
        //    .args(arg_list2)
        //    .output()
        //    .expect(format!("failed to execute cmd {}", cmd).as_str())
        //    ;
        //println!("Output {:?}", String::from_utf8(output.stdout));
        println!("{:?}", arg_list2);

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
        .filter_map(|lexeme| {
            let output = match lexeme {
                Lexeme::Comment(_) => None,
                Lexeme::EndOfCommand => {
                    if statement_buffer.is_empty() {
                        None
                    } else {
                        Some(take(&mut statement_buffer))
                    }
                }
                x => {
                    statement_buffer.push(x);
                    None
                }
            };
            future::ready(output)
        })
        .map(|statement| {
            //println!("Statement {:?}", statement);
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
