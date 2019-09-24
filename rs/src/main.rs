use std::str::FromStr;
use clap::{Arg, App, SubCommand};
use fern;
use log;
use chrono;

use ac7;

fn main() {
    let matches = App::new("AC-7")
        .author("Incomplete <incomplete@aixon.co>")
        .subcommand(SubCommand::with_name("neat")
            .about("The NEAT algorithm")
            .arg(Arg::with_name("generation")
                .short("g")
                .long("generation")
                .required(true)
                .takes_value(true)
                .help("number of generates to simulate")))
        .get_matches();
    setup_logger().unwrap();
    if let Some(matches) = matches.subcommand_matches("neat") {
        let generation = matches.value_of("generation").unwrap();
        let num_generations = u32::from_str(generation).unwrap();
        ac7::neat::simulate(num_generations);
    } else {
        println!("Bad command");
    }
}


fn setup_logger() -> Result<(), fern::InitError> {
    fern::Dispatch::new()
        .format(|out, message, record| {
            out.finish(format_args!(
                    "{} [{}] [{}] {}",
                    chrono::Local::now().format("%Y-%m-%d %H:%M:%S"),
                    record.level(),
                    record.target(),
                    message
            ))
        })
    .level(log::LevelFilter::Debug)
        .chain(std::io::stdout())
        .chain(fern::log_file("ac7.log")?)
        .apply()?;
    Ok(())
}
