use std::{fs, path, io, ffi};
use clap::Parser;
use parcel_css::stylesheet::{MinifyOptions, ParserOptions, PrinterOptions, StyleSheet};

#[derive(Parser, Debug)]
#[clap(author, about, long_about = None)]
struct CliArgs {
  /// Target CSS file
  input_file: String,
  /// Destination file for the output
  #[clap(short, long)]
  output_file: Option<String>,
  /// Minify the output 
  #[clap(short, long)]
  minify: bool,
  /// Enable parsing CSS nesting
  #[clap(short, long)]
  nesting: bool,
  /// Enable CSS modules in output
  #[clap(short, long, group = "css_modules")]
  css_modules: bool,
  /// Default: <output_file>.json
  #[clap(long, requires = "css_modules")]
  css_modules_output_file: Option<String>,
}

pub fn main() -> Result<(), std::io::Error> {
  let cli_args = CliArgs::parse();
  let source = fs::read_to_string(&cli_args.input_file)?;

  let mut stylesheet = StyleSheet::parse(cli_args.input_file.to_string(), &source, ParserOptions {
    nesting: cli_args.nesting,
    css_modules: cli_args.css_modules,
    ..ParserOptions::default()
  }).unwrap();

  if cli_args.minify {
    stylesheet.minify(MinifyOptions::default()).unwrap();
  }

  let res = stylesheet.to_css(PrinterOptions { 
    minify: cli_args.minify, 
    ..PrinterOptions::default()
  }).unwrap();

  if let Some(output_file) = &cli_args.output_file {
    fs::write(output_file, res.code.as_bytes())?;

    if cli_args.css_modules {
      let css_modules_filename = cli_args.css_modules_output_file.unwrap_or(infer_css_modules_filename(cli_args.output_file)?);
      if let Some(exports) = res.exports {
        let css_modules_json = serde_json::to_string(&exports)?;
        fs::write(css_modules_filename, css_modules_json)?;
      }
    }
  } else {
    println!("{}", res.code);
    if cli_args.css_modules {
      let css_modules_json = serde_json::to_string(&res.exports)?;
      println!("{}", css_modules_json);
    }
  }

  Ok(())
}

fn infer_css_modules_filename(output_file: Option<String>) -> Result<String, std::io::Error> {
  if let Some(file) = output_file {
    let path = path::Path::new(&file);
    if path.extension() == Some(ffi::OsStr::new("json")) {
      Err(io::Error::new(io::ErrorKind::Other, "Cannot infer a css modules json filename, since the output file extension is '.json'"))
    } else {
      // unwrap: the filename option is a String from clap, so is valid utf-8
      Ok(path.with_extension("json").to_str().unwrap().into())
    }
  } else {
    Ok("styles.json".into())
  }
}
