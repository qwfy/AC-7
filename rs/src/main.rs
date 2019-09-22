use std::str::FromStr;
use clap::{Arg, App, SubCommand};
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
    if let Some(matches) = matches.subcommand_matches("neat") {
        let generation = matches.value_of("generation").unwrap();
        let num_generations = i32::from_str(generation).unwrap();
        ac7::neat::simulate(num_generations);
    } else {
        println!("Bad command");
    }
}
