extern crate quicli;
extern crate structopt;
extern crate fuzzy_pickles;

use std::{
    fs::File,
    io::prelude::*,
};

use fuzzy_pickles::{
    ast,
    visit::{Visit, Visitor},
};

use quicli::prelude::*;
use structopt::StructOpt;

/// Get statistics about Rust files
#[derive(Debug, StructOpt)]
struct Cli {
    /// Don't output any statistics
    #[structopt(long = "check-only")]
    check_only: bool,
    /// Report parsing failures, but do not exit on them
    #[structopt(long = "keep-going")]
    keep_going: bool,
    #[structopt(flatten)]
    verbosity: Verbosity,
    /// The files to read
    files: Vec<String>,
}

#[derive(Debug, Default)]
struct Stats {
    statements: usize,
}

impl<'ast> Visitor<'ast> for Stats {
    fn exit_statement(&mut self, _: &ast::Statement) {
        self.statements +=1;
    }
}

fn main() -> CliResult {
    let args = Cli::from_args();
    args.verbosity.setup_env_logger(&env!("CARGO_PKG_NAME"))?;

    for fname in &args.files {
        info!("Processing file {}", fname);

        let mut f = File::open(fname)
            .with_context(|e| format!("Can't open {}: {}", fname, e))?;
        let mut s = String::new();
        f.read_to_string(&mut s)
            .with_context(|e| format!("Can't read {}: {}", fname, e))?;


        let file = match fuzzy_pickles::parse_rust_file(&s) {
            Ok(file) => file,
            Err(detail) => {
                let message = format!("Unable to parse {}\n{}", fname, detail.with_text(&s));
                if args.keep_going {
                    eprintln!("{}", message);
                    continue;
                } else {
                    panic!("{}", message);
                }
            }
        };

        let mut stats = Stats::default();

        file.visit(&mut stats);

        if !args.check_only {
            println!("{}: {} statements", fname, stats.statements);
        }
    }

    Ok(())
}
