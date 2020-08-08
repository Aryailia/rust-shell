//run: cargo test shell_tests -- --nocapture

use crate::model::Parseme;

use futures::channel::mpsc::{UnboundedReceiver, UnboundedSender};
use futures::stream::{Stream, StreamExt};


pub async fn job_stream_run(mut input: UnboundedReceiver<Parseme>) {
    let mut row = 0;
    while let Some(stmt) = input.next().await {
        row += 1;
        println!("{} | {:?}", row, stmt);
    }
}
