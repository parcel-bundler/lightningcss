use std::{fs::{self, File}, io::{Read, Write}};

use clap::{Parser, Args};
use parcel_css::stylesheet::{StyleSheet, MinifyOptions, PrinterOptions, ParserOptions};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
enum Cli {
    ///Minify a given CSS file
    Minify(MinifyArgs)
}

#[derive(Args, Debug)]
struct MinifyArgs {
    ///The CSS file to minify
    input_file: String,
    ///The name of the output file
    #[clap(short, long)]
    output_file: Option<String>
}

pub fn main() -> Result<(), Box<dyn std::error::Error>> {
    match Cli::parse() {
        Cli::Minify(cli_args) => {
            minify_css(cli_args)?;
        }
    }

    Ok(())
}

fn minify_css(cli_args: MinifyArgs) -> Result<(), Box<dyn std::error::Error>> {
    let source = read_file(&cli_args.input_file)?;
    let mut stylesheet = StyleSheet::parse(cli_args.input_file.to_string(), &source, ParserOptions::default()).unwrap();
    stylesheet.minify(MinifyOptions::default());
    let res = stylesheet.to_css(PrinterOptions { minify: true, ..PrinterOptions::default() }).unwrap();
    Ok(if let Some(output_file) = cli_args.output_file {
        write_file(&output_file, &res.code)?;
    }
    else {
        println!("{}", res.code);
    })
}

fn write_file(output_filename: &str, file_content: &str) -> Result<(), std::io::Error> {
    let mut file = File::create(output_filename)?;
    file.write_all(file_content.as_bytes())?;
    Ok(())
}

fn read_file(filename: &str) -> Result<String, std::io::Error> {
    let mut file = fs::File::open(filename)?;

    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;
    Ok(buffer)
}