//! # Fuzzy Pickles
//!
//! This is a library for parsing Rust code, paying specific attention
//! to locations of items in the source code — their [`Extent`]s*.
//!
//! # Examples
//!
//! ## Navigating the AST
//!
//! Parsing a Rust file returns an AST of the file. You can delve into
//! individual parts of a given AST node; every field is public. Enums
//! contain `is_X`, `as_X`, and `into_X` methods to quickly narrow
//! down to specific variant. If you'd like to see the raw text of the
//! node, you can index the original source code text with the AST
//! node.
//!
//! ```
//! extern crate fuzzy_pickles;
//!
//! use fuzzy_pickles::parse_rust_file;
//!
//! fn main() {
//!     let example_source = r#"
//!     fn main() { let the_variable_name = 1 + 1; }
//!     "#;
//!
//!     let file = parse_rust_file(example_source)
//!         .expect("Unable to parse source");
//!
//!     let main_fn = file.items[0].value.as_function()
//!         .expect("Not a function");
//!
//!     let first_expr = main_fn.body.statements[0].as_expression()
//!         .expect("Not an expression");
//!
//!     let let_expr = first_expr.value.as_let()
//!         .expect("Not a let expression");
//!
//!     let name_pat = let_expr.pattern.kind.as_ident()
//!         .expect("Not an ident pattern");
//!     assert_eq!("the_variable_name", &example_source[name_pat]);
//!
//!     let value = let_expr.value.as_ref()
//!         .expect("No value for let");
//!
//!     let addition_expr = value.value.as_binary()
//!         .expect("Not a binary expression");
//!     assert_eq!("1 + 1", &example_source[addition_expr]);
//! }
//! ```
//!
//! ## Using a visitor
//!
//! Doing this amount of digging can be tedious and error prone,
//! however. The crate also comes with visitor traits allowing you to
//! quickly find relevant nodes.
//!
//! ```
//! extern crate fuzzy_pickles;
//!
//! use fuzzy_pickles::{parse_rust_file, ast, visit::{Visit, Visitor}};
//!
//! #[derive(Debug, Default)]
//! struct AddVisitor<'ast>(Vec<&'ast ast::Binary>);
//!
//! impl<'ast> Visitor<'ast> for AddVisitor<'ast> {
//!     fn exit_binary(&mut self, binary: &'ast ast::Binary) {
//!         self.0.push(binary)
//!     }
//! }
//!
//! fn main() {
//!     let example_source = r#"
//!     fn main() { let the_variable_name = 1 + 1; }
//!     "#;
//!
//!     let file = parse_rust_file(example_source)
//!         .expect("Unable to parse source");
//!
//!     let mut v = AddVisitor::default();
//!     file.visit(&mut v);
//!
//!     let binary = v.0.pop().expect("Didn't find the binary operator");
//!     assert!(v.0.is_empty(), "Found additional binary operators");
//!     assert_eq!("1 + 1", &example_source[binary])
//! }
//! ```
//!
//! ## Reporting errors
//!
//! The parser attempts to have a reasonable level of detail when the
//! input source code is malformed.
//!
//! ```should_panic
//! extern crate fuzzy_pickles;
//!
//! use fuzzy_pickles::parse_rust_file;
//!
//! fn main() {
//!     // Oops, we forgot to close our parenthesis!
//!     let example_source = r#"
//!     fn main( { let the_variable_name = 1 + 1; }
//!     "#;
//!
//!     let error = parse_rust_file(example_source)
//!         .unwrap_err();
//!
//!     let pretty_error = error.with_text(example_source);
//!     panic!("{}", pretty_error);
//! }
//! ```
//!
//! This produces an error that shows the offending location and what
//! possible symbols were expected. We've truncated this output:
//!
//! ```text
//! Unable to parse text (line 2, column 14)
//!
//!     fn main( { let the_variable_name = 1 + 1; }
//!              ^
//! Expected:
//!   ExpectedAmpersand
//!   ExpectedBox
//!   ...
//! ```
//!

#[macro_use]
extern crate fuzzy_pickles_derive;

#[macro_use]
extern crate peresil;

extern crate unicode_xid;

#[cfg(test)]
#[macro_use]
mod test_utils;

mod combinators;
mod whitespace_apportioner;

pub mod ast;
pub mod tokenizer;
pub mod visit;
pub mod parser;

use std::fmt;
use crate::whitespace_apportioner::WhitespaceApportioner;

/// A pair of `(start, end)` points corresponding to something
/// interesting in the source text.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Extent(pub usize, pub usize);

impl PartialEq<(usize, usize)> for Extent {
    fn eq(&self, other: &(usize, usize)) -> bool {
        (self.0, self.1) == *other
    }
}

impl PartialEq<Extent> for (usize, usize) {
    fn eq(&self, other: &Extent) -> bool {
        (other.0, other.1) == *self
    }
}

impl From<(usize, usize)> for Extent {
    fn from(other: (usize, usize)) -> Extent {
        Extent(other.0, other.1)
    }
}

impl From<Extent> for (usize, usize) {
    fn from(other: Extent) -> (usize, usize) {
        (other.0, other.1)
    }
}

impl std::ops::Index<Extent> for str {
    type Output = str;

    fn index(&self, Extent(s, e): Extent) -> &Self::Output {
        &self[s..e]
    }
}

impl<'a> std::ops::Index<&'a Extent> for str {
    type Output = str;

    fn index(&self, &Extent(s, e): &'a Extent) -> &Self::Output {
        &self[s..e]
    }
}

/// A type that has an extent
pub trait HasExtent {
    fn extent(&self) -> Extent;
}

impl<T: HasExtent> HasExtent for Box<T>{
    fn extent(&self) -> Extent { (**self).extent() }
}

impl<T: HasExtent> HasExtent for &T {
    fn extent(&self) -> Extent { (**self).extent() }
}

impl HasExtent for Extent {
    fn extent(&self) -> Extent { *self }
}

/// Information about a tokenization or parsing error
#[derive(Debug, PartialEq)]
pub enum ErrorDetail {
    Tokenizer(tokenizer::ErrorDetail),
    Parser(parser::ErrorDetail),
}

impl ErrorDetail {
    /// Enhance the error with the source code
    pub fn with_text<'a>(&'a self, text: &'a str) -> ErrorDetailText<'a> {
        ErrorDetailText { detail: self, text }
    }
}

impl From<tokenizer::ErrorDetail> for ErrorDetail {
    fn from(other: tokenizer::ErrorDetail) -> Self {
        ErrorDetail::Tokenizer(other)
    }
}

impl From<parser::ErrorDetail> for ErrorDetail {
    fn from(other: parser::ErrorDetail) -> Self {
        ErrorDetail::Parser(other)
    }
}

/// Information about a tokenization or parsing error including original source code
#[derive(Debug)]
pub struct ErrorDetailText<'a> {
    detail: &'a ErrorDetail,
    text: &'a str,
}

impl<'a> fmt::Display for ErrorDetailText<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.detail {
            ErrorDetail::Tokenizer(ref t) => t.with_text(self.text).fmt(f),
            ErrorDetail::Parser(ref p) => p.with_text(self.text).fmt(f),
        }
    }
}

struct HumanTextError<'a> {
    head_of_line: &'a str,
    tail_of_line: &'a str,
    line: usize,
    column: usize,
}

impl<'a> HumanTextError<'a> {
    fn new(text: &'a str, location: usize) -> HumanTextError<'a> {
        let (head, tail) = text.split_at(location);
        let start_of_line = head.rfind('\n').unwrap_or(0);
        let end_of_line = tail.find('\n').unwrap_or(tail.len());

        let head_of_line = &head[start_of_line..];
        let tail_of_line = &tail[..end_of_line];

        let line = head.matches('\n').count() + 1; // Normally the first line is #1, so add one
        let column = head_of_line.len();

        HumanTextError { head_of_line, tail_of_line, line, column }
    }
}

fn extract_whitespace(file: &str) -> Result<(WhitespaceApportioner, Vec<tokenizer::Token>), tokenizer::ErrorDetail> {
    use crate::tokenizer::{Token, Tokens};

    let mut ws = WhitespaceApportioner::default();
    let mut tokens = Vec::new();

    for token in Tokens::new(file) {
        let token = token?;

        match token {
            Token::Whitespace(w) => {
                ws.push(ast::Whitespace::Whitespace(w))
            }
            Token::CommentLine(c) => {
                let c = ast::Comment::Line(c);
                ws.push(ast::Whitespace::Comment(c));
            }
            Token::CommentBlock(c) => {
                let c = ast::Comment::Block(c);
                ws.push(ast::Whitespace::Comment(c));
            }
            o => tokens.push(o),
        }
    }

    Ok((ws, tokens))
}

/// The entrypoint to parsing Rust code.
pub fn parse_rust_file(file: &str) -> Result<ast::File, ErrorDetail> {
    use crate::{
        parser::{attributed, item, shebang, Point, Master, State},
        tokenizer::Token,
        visit::Visit,
    };

    let (mut ws, tokens) = extract_whitespace(file)?;

    let mut pt = Point::new(&tokens);
    let mut pm = Master::with_state(State::new());
    let mut items = Vec::new();

    let shebang = match shebang(&mut pm, pt) {
        peresil::Progress { status: peresil::Status::Success(s), point } => {
            pt = point;
            Some(s)
        }
        _ => None,
    };

    loop {
        if pt.s.first().map(Token::is_end_of_file).unwrap_or(true) { break }

        let item = attributed(item)(&mut pm, pt);
        let item = pm.finish(item);

        let next_pt = match item.status {
            peresil::Status::Success(s) => {
                items.push(s);
                item.point
            },
            peresil::Status::Failure(e) => {
                return Err(ErrorDetail::Parser(parser::ErrorDetail {
                    location: tokens[item.point.offset].extent().0,
                    errors: e.into_iter().collect(),
                }))
            },
        };

        assert!(next_pt.offset > pt.offset, "Unable to make progress");
        pt = next_pt;
    }

    let mut file = ast::File { shebang, items, whitespace: Vec::new() };

    file.visit_mut(&mut ws);
    assert!(ws.is_empty(), "Did not assign all whitespace");

    Ok(file)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn can_parse_an_empty_rust_file() {
        let r = parse_rust_file("");
        assert!(r.is_ok());
    }

    #[test]
    fn parse_shebang() {
        let r = parse_rust_file("#!/bin/false --version");
        assert!(r.is_ok());
    }

    #[test]
    fn error_on_last_token_does_not_panic() {
        let r = parse_rust_file("an_ident");
        assert!(r.is_err());
    }

    #[test]
    fn error_on_unclosed_macro_args_does_not_panic() {
        let r = parse_rust_file("c!(");
        assert!(r.is_err());
    }
}
