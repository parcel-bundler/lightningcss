use std::fs;
use clap::Parser;
use parcel_css::stylesheet::{MinifyOptions, ParserOptions, PrinterOptions, StyleSheet};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct CliArgs {
  /// The CSS file to minify
  input_file: String,
  /// Minify the output 
  #[clap(short, long)]
  minify: bool,
    /// The destination file for the output
  #[clap(short, long)]
  output_file: Option<String>,
}

pub fn main() -> Result<(), std::io::Error> {
  let cli_args = CliArgs::parse();
  let source = fs::read_to_string(&cli_args.input_file)?;
  let mut stylesheet = StyleSheet::parse(cli_args.input_file.to_string(), &source, ParserOptions::default()).unwrap();

  if cli_args.minify {
    stylesheet.minify(MinifyOptions::default()).unwrap();
  }

  let res = stylesheet.to_css(PrinterOptions { 
    minify: cli_args.minify, 
    ..PrinterOptions::default()
  }).unwrap();

  if let Some(output_file) = cli_args.output_file {
    fs::write(output_file, res.code.as_bytes())?;
  } else {
    println!("{}", res.code);
  }

  Ok(())
}
