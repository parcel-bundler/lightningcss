use clap::Parser;
use parcel_css::stylesheet::{MinifyOptions, ParserOptions, PrinterOptions, StyleSheet};
use parcel_css::bundler::{FileProvider, Bundler};
use serde::Serialize;
use std::{ffi, fs, io, path, path::Path};
use parcel_sourcemap::SourceMap;

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
  #[clap(long)]
  nesting: bool,
  /// Enable parsing custom media queries
  #[clap(long)]
  custom_media: bool,
  /// Enable CSS modules in output.
  /// If no filename is provided, <output_file>.json will be used.
  #[clap(long, group = "css_modules", requires = "output_file")]
  css_modules: Option<Option<String>>,
  /// Enable sourcemap, at <output_file>.map
  #[clap(long, requires = "output_file")]
  sourcemap: bool,
  #[clap(long)]
  bundle: bool
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

  let absolute_path = fs::canonicalize(&cli_args.input_file)?;
  let filename = pathdiff::diff_paths(absolute_path, std::env::current_dir()?).unwrap();
  let filename = filename.to_str().unwrap();
  let options = ParserOptions {
    nesting: cli_args.nesting,
    css_modules: cli_args.css_modules.is_some(),
    custom_media: cli_args.custom_media,
    ..ParserOptions::default()
  };

  let fs = FileProvider::new();
  let mut source_map = if cli_args.sourcemap {
    Some(SourceMap::new("/"))
  } else {
    None
  };

  let mut stylesheet = if cli_args.bundle {
    let mut bundler = Bundler::new(&fs, source_map.as_mut(), options);
    bundler.bundle(Path::new(&cli_args.input_file)).unwrap()
  } else {
    if let Some(sm) = &mut source_map {
      sm.add_source(&filename);
      sm.set_source_content(0, &source);  
    }
    StyleSheet::parse(
      filename.into(),
      &source,
      options,
    )
    .unwrap()
  };

  if cli_args.minify {
    stylesheet.minify(MinifyOptions::default()).unwrap();
  }

  let mut res = stylesheet
    .to_css(PrinterOptions {
      minify: cli_args.minify,
      source_map: source_map.as_mut(),
      ..PrinterOptions::default()
    })
    .unwrap();

  let map = if let Some(ref mut source_map) = source_map {
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

    if let Some(css_modules) = cli_args.css_modules {
      let css_modules_filename = if let Some(name) = css_modules {
        name
      } else {
        infer_css_modules_filename(&output_file)?
      };
      if let Some(exports) = res.exports {
        let css_modules_json = serde_json::to_string(&exports)?;
        fs::write(css_modules_filename, css_modules_json)?;
      }
    }
  } else {
    println!("{}", res.code);
  }

  Ok(())
}

fn infer_css_modules_filename(output_file: &str) -> Result<String, std::io::Error> {
  let path = path::Path::new(output_file);
  if path.extension() == Some(ffi::OsStr::new("json")) {
    Err(io::Error::new(
      io::ErrorKind::Other,
      "Cannot infer a css modules json filename, since the output file extension is '.json'",
    ))
  } else {
    // unwrap: the filename option is a String from clap, so is valid utf-8
    Ok(path.with_extension("json").to_str().unwrap().into())
  }
}
