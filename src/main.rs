use futures::{future, Stream, StreamExt};
//use futures_timer::Delay;
use crossterm::{
    cursor::{MoveUp, MoveLeft, MoveDown, MoveRight},
    event::{DisableMouseCapture, EnableMouseCapture, Event, EventStream, KeyCode, KeyModifiers},
    //QueueableCommand,
    style::Print,
    terminal,
    ExecutableCommand,
};
use unicode_width::UnicodeWidthStr;

fn main() {
    async_std::task::block_on(async {
        repl().await.unwrap();
    });
}


//trait IgnoreError<T: Result>: Sized {
//    fn ignore() {
//        result
//
//    }
//}

fn parse_user_input(input_stream: EventStream) -> impl Stream<Item = bool> {
    //let mut line_buffer = String::with_capacity(1024);
    let mut stdout = std::io::stdout();
    let emoji_list = [
        "\u{2764}",         // heart
        "\u{2764}\u{fe0e}", // heart
        "\u{2764}\u{fe0f}", // heart
        "\u{26a1}",         // lightning
        "\u{26a1}\u{fe0e}", // lightning
        "\u{26a1}\u{fe0f}", // lightning
        "你好",
        "你",
        "これは",
        "ａｂｃ",
        "\u{1f469}", // woman
        "\u{1f52c}", // microscope
        "\u{1f469}\u{200d}\u{1f52c}", // female scientist
    ];
    input_stream.filter_map(move |event| {
        let output = match event {
            Ok(Event::Key(e)) => match (e.code, e.modifiers) {
                (KeyCode::Esc, _) => Some(true),
                (KeyCode::Char('h'), _) => {
                    stdout.execute(MoveLeft(1)).unwrap();
                    None
                }
                (KeyCode::Char('j'), _) => {
                    stdout.execute(MoveDown(1)).unwrap();
                    None
                }
                (KeyCode::Char('k'), _) => {
                    stdout.execute(MoveUp(1)).unwrap();
                    None
                }
                (KeyCode::Char('l'), _) => {
                    stdout.execute(MoveRight(1)).unwrap();
                    None
                }
                (KeyCode::Char('o'), _) => {
                    stdout.execute(Print(emoji_list.join("\r\n"))).unwrap();
                    None
                }
                (KeyCode::Char('p'), _) => {
                    stdout.execute(Print(emoji_list.join(""))).unwrap();
                    None
                }
                (KeyCode::Char('s'), _) => {
                    for emoji in &emoji_list {
                        stdout.execute(Print(UnicodeWidthStr::width(*emoji))).unwrap();
                        stdout.execute(Print(emoji)).unwrap();
                        stdout.execute(Print(UnicodeWidthStr::width_cjk(*emoji))).unwrap();
                        stdout.execute(Print(" | ")).unwrap();
                    }
                    None
                }
                (KeyCode::Char(' '), _) => {
                    stdout.execute(Print(" ")).unwrap();
                    None
                }
                (KeyCode::Char('1'), _) => {
                    stdout.execute(Print("你")).unwrap();
                    None
                }

                (KeyCode::Enter, _) => {
                    stdout.execute(Print("\r\n")).unwrap();
                    None
                }
                _ => {
                    stdout.execute(Print("i")).unwrap();
                    Some(false)
                },
            },
            Ok(Event::Mouse(_)) => None,
            Ok(Event::Resize(_, _)) => None,
            Err(_) => None,
        };
        future::ready(output)
    })
}

async fn repl() -> crossterm::Result<()> {
    let mut stdout = std::io::stdout();

    terminal::enable_raw_mode()?;
    stdout.execute(EnableMouseCapture)?;

    let event_stream = EventStream::new();
    let mut line_stream = parse_user_input(event_stream);
    while let Some(terminate) = line_stream.next().await {
        if terminate {
            break;
        }
    }

    terminal::disable_raw_mode()?;
    stdout.execute(DisableMouseCapture)?;
    Ok(())
}
