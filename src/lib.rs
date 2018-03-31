#[macro_use]
extern crate fuzzy_pickles_derive;

#[macro_use]
extern crate peresil;

extern crate unicode_xid;

#[cfg(test)]
#[macro_use]
mod test_utils;

mod combinators;

pub mod ast;
pub mod tokenizer;
pub mod visit;
pub mod parser;

use std::fmt;

pub type Extent = (usize, usize);

pub trait HasExtent {
    fn extent(&self) -> Extent;
}

impl<T: HasExtent> HasExtent for Box<T>{
    fn extent(&self) -> Extent { (**self).extent() }
}

impl<'a, T: HasExtent> HasExtent for &'a T {
    fn extent(&self) -> Extent { (**self).extent() }
}

impl HasExtent for Extent {
    fn extent(&self) -> Extent { *self }
}

#[derive(Debug, PartialEq)]
pub enum ErrorDetail {
    Tokenizer(tokenizer::ErrorDetail),
    Parser(parser::ErrorDetail),
}

impl ErrorDetail {
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
        let start_of_line = head.rfind("\n").unwrap_or(0);
        let end_of_line = tail.find("\n").unwrap_or_else(|| tail.len());

        let head_of_line = &head[start_of_line..];
        let tail_of_line = &tail[..end_of_line];

        let line = head.matches("\n").count() + 1; // Normally the first line is #1, so add one
        let column = head_of_line.len();

        HumanTextError { head_of_line, tail_of_line, line, column }
    }
}

// Construct a point, initialize  the master. This is what stores errors
// todo: rename?

pub fn parse_rust_file(file: &str) -> Result<ast::File, ErrorDetail> {
    use parser::{attributed, item, Point, Master, State};
    use tokenizer::{Token, Tokens};

    let tokens: Vec<_> = Tokens::new(file).collect::<Result<_, _>>()?;
    let (_ws, tokens): (Vec<_>, Vec<_>) = tokens.into_iter().partition(|t| {
        t.is_whitespace() || t.is_comment() || t.is_doc_comment() || t.is_comment_block() || t.is_doc_comment_block()
    });

    let mut pt = Point::new(&tokens);
    let mut pm = Master::with_state(State::new());
    let mut items = Vec::new();

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

        if next_pt.offset <= pt.offset {
            panic!("Unable to make progress");
        }
        pt = next_pt;
    }

    Ok(ast::File { items: items })
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
