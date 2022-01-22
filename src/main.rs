use clap::Parser;
use parcel_css::stylesheet::{MinifyOptions, ParserOptions, PrinterOptions, StyleSheet};
use serde::Serialize;
use std::{ffi, fs, io, path};

#[derive(Parser, Debug)]
#[clap(author, about, long_about = None)]
struct CliArgs {
  /// Target CSS file
  input_file: String,
  /// Destination file for the output
  #[clap(short, long, group = "output_file")]
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
  /// Enable sourcemap, at <output_file>.map
  #[clap(short, long, requires = "output_file")]
  sourcemap: bool,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct SourceMapJson<'a> {
  version: u8,
  mappings: String,
  sources: &'a Vec<String>,
  sources_content: &'a Vec<String>,
  names: &'a Vec<String>,
}

pub fn main() -> Result<(), std::io::Error> {
  let cli_args = CliArgs::parse();
  let source = fs::read_to_string(&cli_args.input_file)?;

  let filename = path::Path::new(&cli_args.input_file)
    .file_name()
    .unwrap()
    .to_str()
    .unwrap()
    .into();
  let mut stylesheet = StyleSheet::parse(
    filename,
    &source,
    ParserOptions {
      nesting: cli_args.nesting,
      css_modules: cli_args.css_modules,
      ..ParserOptions::default()
    },
  )
  .unwrap();

  if cli_args.minify {
    stylesheet.minify(MinifyOptions::default()).unwrap();
  }

  let mut res = stylesheet
    .to_css(PrinterOptions {
      minify: cli_args.minify,
      source_map: cli_args.sourcemap,
      ..PrinterOptions::default()
    })
    .unwrap();

  let map = if let Some(ref mut source_map) = res.source_map {
    source_map
      .set_source_content(0, &res.code)
      .map_err(|_| io::Error::new(io::ErrorKind::Other, "Error setting sourcemap"))?;
    let mut vlq_output: Vec<u8> = Vec::new();
    source_map
      .write_vlq(&mut vlq_output)
      .map_err(|_| io::Error::new(io::ErrorKind::Other, "Error writing sourcemap vlq"))?;

    let sm = SourceMapJson {
      version: 3,
      mappings: unsafe { String::from_utf8_unchecked(vlq_output) },
      sources: source_map.get_sources(),
      sources_content: source_map.get_sources_content(),
      names: source_map.get_names(),
    };

    serde_json::to_vec(&sm).ok()
  } else {
    None
  };
  if let Some(output_file) = &cli_args.output_file {
    let mut code = res.code;
    if cli_args.sourcemap {
      if let Some(map_buf) = map {
        let map_filename: String = output_file.to_owned() + ".map";
        code += &format!("\n/*# sourceMappingURL={} */\n", map_filename);
        fs::write(map_filename, map_buf)?;
      }
    }

    fs::write(output_file, code.as_bytes())?;

    if cli_args.css_modules {
      let css_modules_filename = cli_args
        .css_modules_output_file
        .unwrap_or(infer_css_modules_filename(cli_args.output_file)?);
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
      Err(io::Error::new(
        io::ErrorKind::Other,
        "Cannot infer a css modules json filename, since the output file extension is '.json'",
      ))
    } else {
      // unwrap: the filename option is a String from clap, so is valid utf-8
      Ok(path.with_extension("json").to_str().unwrap().into())
    }
  } else {
    Ok("styles.json".into())
  }
}
