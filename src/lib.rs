//run: cargo test parser_tests -- --nocapture

// Dash is: git.kernel.org/pub/scm/utils/dash.git
#![allow(dead_code, unused_imports)]

mod helpers;
mod lexer;
mod parser;

use async_std::task;
use futures::{stream, stream::Stream, StreamExt};
use lexer::{job_stream_lex, Lexeme};
use parser::job_stream_parse;

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
        let input = r"sed -n 's/ *//' ~/.enivironment/bookmarks.yml";
        let script_stream = stream::iter(vec![input]);
        let (lex_tx, lex_rx) = futures::channel::mpsc::unbounded::<Lexeme>();

        let lexer = task::spawn(job_stream_lex(script_stream, move |token| {
            // Currently no clue as to what are the costs/benefits for using
            // bounded vs unbounded mpsc, also 'unbounded_send()' vs 'send()'
            lex_tx.unbounded_send(token).unwrap();
        }));
        let parser = task::spawn(job_stream_parse(lex_rx));
        task::block_on(async {
            futures::join!(lexer, parser);
        });
    }

    //#[test]
    fn lexer_development() {
        //let (producer, consumer) = futures::channel::mpsc::unbounded::<Lexeme>();

        let mut token_list: Vec<Lexeme> = Vec::with_capacity(100);
        let input = SCRIPT3;
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
                Lexeme::Variable("asdf".into()),
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
                Lexeme::EndOfCommand,
            ]
        )
    }

    //#[test]
    //fn grid() {
    //    TextGridWalk::new(SCRIPT).for_each(|a| println!("{:?}", a));
    //}
}
