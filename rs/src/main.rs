use std::str::FromStr;

use chrono;
use clap::{App, Arg, SubCommand, value_t_or_exit};
use fern;
use log;

use ac7;

fn main() {
    let matches = App::new("AC-7")
        .author("Incomplete <incomplete@aixon.co>")
        .subcommand(
            SubCommand::with_name("neat")
                .about("The NEAT algorithm")
        )
        .get_matches();
    setup_logger().unwrap();
    if let Some(matches) = matches.subcommand_matches("neat") {
        ac7::neat::simulate(&ac7::data::Param {
            num_generations: 2,
            compatibility_threshold: 3.0,
            c_disjoint: 2.0,
            c_excess: 3.0,
            c_common: 4.0,
            initial_population_size: 10,
            new_edge_enable_probability: 0.9,
            num_inputs: 2,
            num_outputs: 1,
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
