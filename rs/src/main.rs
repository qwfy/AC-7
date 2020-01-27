use chrono;
use clap::{value_t_or_exit, App, Arg, SubCommand};
use fern;
use log;
use std::str::FromStr;

use ac7;

fn main() {
    let matches = App::new("AC-7")
        .author("Incomplete <incomplete@aixon.co>")
        .subcommand(
            SubCommand::with_name("neat")
                .about("The NEAT algorithm")
                .arg(
                    Arg::with_name("num-generations")
                        .short("g")
                        .long("num-generations")
                        .required(true)
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("compatability-threshold")
                        .short("c")
                        .long("compatability-threshold")
                        .required(true)
                        .takes_value(true),
                ),
        )
        .get_matches();
    setup_logger().unwrap();
    if let Some(matches) = matches.subcommand_matches("neat") {
        let num_generations = value_t_or_exit!(matches, "num-generations", u32);
        let compatability_threshold = value_t_or_exit!(matches, "compatability-threshold", f32);
        ac7::neat::simulate(&ac7::neat::Param {
            num_generations,
            compatability_threshold,
        });
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
