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
    let toks: Result<Vec<_>, _> = tokens.collect();

    match toks {
        Ok(toks) => {
            for (i, t) in toks.into_iter().enumerate() {
                let extent = t.extent();
                let tok_str = &s[extent.0..extent.1];
                let x = format!("{:?}", t);

                if tok_str.contains("\n") {
                    println!("{} {: >30} | {:?}", i, x, tok_str);
                } else {
                    println!("{} {: >30} | {}", i, x, tok_str);
                }
            }
        }
        Err(e) => {
            println!("failed to tokenize: {:?}", e)
        }
    }
}
