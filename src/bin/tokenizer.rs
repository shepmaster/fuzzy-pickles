extern crate fuzzy_pickles;

use std::fs::File;
use std::io::Read;

use fuzzy_pickles::tokenizer::Tokens;

fn main() {
    let mut s = String::new();

    let fname = std::env::args().nth(1)
        .expect("A file name is required");

    let mut f = File::open(fname).expect("Unable to open file");
    f.read_to_string(&mut s).expect("unable to read file");

    let tokens = Tokens::new(&s);
    let toks: Vec<_> = tokens.collect();

    let n = 50;
    println!("Parsed {} tokens. The last {} were", toks.len(), n);
    println!("{:?}", &toks[toks.len().saturating_sub(n)..]);
}
