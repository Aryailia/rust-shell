//run: cargo test shell_tests -- --nocapture

use crate::model::Parseme;

use futures::channel::mpsc::{UnboundedReceiver, UnboundedSender};
use futures::stream::{Stream, StreamExt};


pub async fn job_stream_run(mut input: UnboundedReceiver<Parseme>) {
    let mut program = Vec::new();
    while let Some(stmt) = input.next().await {

        match stmt {
            Parseme::Statement(cmd) => {
                //Command::new()
                program.push(Parseme::Statement(cmd));
            }
            p => program.push(p),
            //_ => println!("{} | {:?}", row, stmt),
        }
    }
    program.iter()
        .enumerate()
        .for_each(|(row, stmt)| println!("{} | {:?}", row, stmt));
    //println!("{:?}", program);
}
