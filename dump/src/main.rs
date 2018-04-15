extern crate fuzzy_pickles;

use std::env;
use std::fs::File;
use std::io::prelude::*;

#[derive(Debug, Default)]
struct Stats {
    statements: usize,
}

fn main() {
    for fname in env::args().skip(1) {
        let mut f = File::open(&fname).unwrap_or_else(|e| panic!("Can't open {}: {}", fname, e));
        let mut s = String::new();
        f.read_to_string(&mut s)
            .unwrap_or_else(|e| panic!("Can't read {}: {}", fname, e));

        let file = match fuzzy_pickles::parse_rust_file(&s) {
            Ok(file) => file,
            Err(detail) => {
                panic!("Unable to parse {}\n{}", fname, detail.with_text(&s));
            }
        };

        println!("{:#?}", file);
    }
}
