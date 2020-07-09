use std::io::Write;
use futures::{future, Stream, StreamExt};
use std::process::Command;
//use futures_timer::Delay;
use crossterm::{
    event::{DisableMouseCapture, EnableMouseCapture, Event, EventStream, KeyCode, KeyModifiers},
    terminal,
    cursor,
    //QueueableCommand,
    style::Print,
    ExecutableCommand,
};

fn main() {
    async_std::task::block_on(async {
        repl().await.unwrap();
    });
}

fn parse_user_input(input_stream: EventStream) -> impl Stream<Item = String> {
    let mut line_buffer = String::with_capacity(1024);
    let mut stdout = std::io::stdout();
    input_stream.filter_map(move |event| {
        let output = match event {
            Ok(Event::Key(e)) => match (e.code, e.modifiers) {
                (KeyCode::Esc, _) => Some("exit".to_string()),
                (KeyCode::Char('c'), KeyModifiers::CONTROL) => {
                    line_buffer.clear();
                    stdout.write(b"^C\r\n").unwrap();
                    Some(String::from(""))
                }
                (KeyCode::Char('d'), KeyModifiers::CONTROL) => {
                    if line_buffer.is_empty() {
                        Some("exit".to_string())
                    } else {
                        None
                    }
                }

                (KeyCode::Enter, _) => {
                    stdout.write(b"\r\n").unwrap();
                    Some(line_buffer.split_off(0))
                }

                (KeyCode::Backspace, _) => {
                    if !line_buffer.is_empty() {
                        stdout.execute(cursor::MoveLeft(1)).unwrap()
                            .execute(Print(" ")).unwrap()
                            .execute(cursor::MoveLeft(1)).unwrap();
                        line_buffer.pop();
                    }
                    //println!()
                    None
                }

                (KeyCode::Char(ch), _) => {
                    stdout.execute(crossterm::style::Print(ch)).unwrap();
                    line_buffer.push(ch);
                    None
                }
                _ => None,
            },
            Ok(Event::Mouse(e)) => {
                println!("Mouse {:?}", e);
                None
            }
            Ok(Event::Resize(_, _)) => {
                //println!("Resize {:?} {:?}", a, b);
                None
            }
            Err(_) => None,
        };
        future::ready(output)
    })
}

fn prompt() -> &'static str {
    "> "
}

async fn repl() -> crossterm::Result<()> {
    let mut stdout = std::io::stdout();
    let mut stderr = std::io::stderr();

    terminal::enable_raw_mode()?;
    stdout.execute(EnableMouseCapture)?;

    let event_stream = EventStream::new();
    stdout.execute(Print(prompt()))?;
    let mut line_stream = parse_user_input(event_stream);
    while let Some(line) = line_stream.next().await {
        terminal::disable_raw_mode()?;

        let mut args = line.split_whitespace();
        match args.next() {
            Some("exit") => break,
            Some(cmd_str) => {
                let result = Command::new(cmd_str)
                    .args(args.collect::<Vec<_>>())
                    .spawn()
                    .and_then(|mut child| child.wait());
                match result {
                    Ok(_) => {}
                    Err(_) => {
                        stderr.execute(Print("Invalid command\n")).unwrap();
                    }
                }
            }
            _ => {}
        }
        terminal::enable_raw_mode()?;
        stdout.execute(Print(prompt())).unwrap();
    }

    terminal::disable_raw_mode()?;
    stdout.execute(DisableMouseCapture)?;
    Ok(())
}
