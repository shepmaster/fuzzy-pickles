extern crate fuzzy_pickles;

use std::fs::File;
use std::env;
use std::io::prelude::*;

use fuzzy_pickles::{Visit, Visitor};

#[derive(Debug, Default)]
struct Stats {
    statements: usize,
}

impl Visitor for Stats {
    fn visit_statement(&mut self, _: &fuzzy_pickles::Statement) {
        self.statements +=1;
    }
}

fn main() {
    for fname in env::args().skip(1) {
        let mut f = File::open(&fname)
            .unwrap_or_else(|e| panic!("Can't open {}: {}", fname, e));
        let mut s = String::new();
        f.read_to_string(&mut s)
            .unwrap_or_else(|e| panic!("Can't read {}: {}", fname, e));

        let file = match fuzzy_pickles::parse_rust_file(&s) {
            Ok(file) => file,
            Err(detail) => {
                panic!("Unable to parse {}\n{}", fname, detail.with_text(&s));
            }
        };

        let mut stats = Stats::default();

        file.visit(&mut stats);

//        println!("{}: {} statements", fname, stats.statements)
    }
}
