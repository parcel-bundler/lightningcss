use std::fs;
use clap::{Args, Parser};
use parcel_css::stylesheet::{MinifyOptions, ParserOptions, PrinterOptions, StyleSheet};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
enum Cli {
  /// Minify a given CSS file
  Minify(MinifyArgs),
}

#[derive(Args, Debug)]
struct MinifyArgs {
  /// The CSS file to minify
  input_file: String,
  /// The name of the output file
  #[clap(short, long)]
  output_file: Option<String>,
}

pub fn main() -> Result<(), std::io::Error> {
  match Cli::parse() {
    Cli::Minify(cli_args) => {
      minify_css(cli_args)
    }
  }
}

fn minify_css(cli_args: MinifyArgs) -> Result<(), std::io::Error> {
  let source = fs::read_to_string(&cli_args.input_file)?;
  let mut stylesheet = StyleSheet::parse(cli_args.input_file.to_string(), &source, ParserOptions::default()).unwrap();
  stylesheet.minify(MinifyOptions::default()).unwrap();
  let res = stylesheet.to_css(PrinterOptions { minify: true, ..PrinterOptions::default()}).unwrap();

  if let Some(output_file) = cli_args.output_file {
    fs::write(output_file, res.code.as_bytes())?;
  } else {
    println!("{}", res.code);
  }

  Ok(())
}
