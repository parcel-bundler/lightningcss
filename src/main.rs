use clap::Parser;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
enum Cli {
    Minify {input_file: String }
}

pub fn main() {
    let args = Cli::parse();
}