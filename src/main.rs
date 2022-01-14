use std::{fs::{self, File}, io::{Read, Write}, path::Path};

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
            let source = read_file(&cli_args.input_file)?;
            let mut stylesheet = StyleSheet::parse(cli_args.input_file.to_string(), &source, ParserOptions::default()).unwrap();
            stylesheet.minify(MinifyOptions::default());
            let res = stylesheet.to_css(PrinterOptions { minify: true, ..PrinterOptions::default() }).unwrap();

            let output_filename = if let Some(output_file) = cli_args.output_file {
                output_file
            }
            else {
                generate_output_filename(&cli_args.input_file)
            };

            write_file(&output_filename, &res.code)?;
        }
    }

    Ok(())
}

fn write_file(output_filename: &str, file_content: &str) -> Result<(), std::io::Error> {
    let mut file = File::create(output_filename)?;
    file.write_all(file_content.as_bytes())?;
    Ok(())
}

fn generate_output_filename(input_file: &str) ->String {
    let path = Path::new(input_file);

    let extension = path.extension().unwrap();
    let base_name = path.file_stem().unwrap();

    format!("{}.minified.{}", base_name.to_str().unwrap(), extension.to_str().unwrap())
}

fn read_file(filename: &str) -> Result<String, std::io::Error> {
    let mut file = fs::File::open(filename)?;

    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;
    Ok(buffer)
}