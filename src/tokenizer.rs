use unicode_xid::UnicodeXID;
use peresil;
use peresil::combinators::*;

use super::{Extent, ex, split_point_at_non_zero_offset};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Decompose)]
pub enum Token {
    LeftSquare(Extent),
    RightSquare(Extent),
    LeftParen(Extent),
    RightParen(Extent),
    LeftAngle(Extent),
    RightAngle(Extent),
    LeftCurly(Extent),
    RightCurly(Extent),

    // TODO: Decide how to name the foo-equals tokens.
    // Should they match (e.g. caret and caret-equals)?
    // Should they infer any meaning (e.g. xor)
    Ampersand(Extent),
    Asterisk(Extent),
    At(Extent),
    Backslash(Extent),
    Bang(Extent),
    CaretEquals(Extent),
    Colon(Extent),
    Comma(Extent),
    DivideEquals(Extent),
    Dollar(Extent),
    DoubleColon(Extent),
    DoubleEquals(Extent),
    DoublePeriod(Extent),
    DoublePipe(Extent),
    Equals(Extent),
    Hash(Extent),
    Minus(Extent),
    MinusEquals(Extent),
    Percent(Extent),
    PercentEquals(Extent),
    Period(Extent),
    Pipe(Extent),
    Plus(Extent),
    PlusEquals(Extent),
    QuestionMark(Extent),
    Semicolon(Extent),
    Slash(Extent),
    Tilde(Extent),
    ThickArrow(Extent),
    ThinArrow(Extent),
    TimesEquals(Extent),
    TriplePeriod(Extent),
    Caret(Extent),

    Character(Extent),
    String(Extent),
    StringRaw(Extent),

    Byte(Extent),
    ByteString(Extent),
    ByteStringRaw(Extent),

    Ident(Extent),
    Digits(Extent),
    Whitespace(Extent),
    DocComment(Extent),
    Comment(Extent),
    Lifetime(Extent),
}

impl Token {
    pub fn extent(&self) -> Extent {
        use self::Token::*;

        match *self {
            Ampersand(s)     |
            Asterisk(s)      |
            At(s)            |
            Backslash(s)     |
            Bang(s)          |
            Byte(s)          |
            ByteString(s)    |
            ByteStringRaw(s) |
            Caret(s)         |
            CaretEquals(s)   |
            Character(s)     |
            Colon(s)         |
            Comma(s)         |
            Comment(s)       |
            Digits(s)        |
            DivideEquals(s)  |
            DocComment(s)    |
            Dollar(s)        |
            DoubleColon(s)   |
            DoubleEquals(s)  |
            DoublePeriod(s)  |
            DoublePipe(s)    |
            Equals(s)        |
            Hash(s)          |
            Ident(s)         |
            LeftAngle(s)     |
            LeftCurly(s)     |
            LeftParen(s)     |
            LeftSquare(s)    |
            Lifetime(s)      |
            Minus(s)         |
            MinusEquals(s)   |
            Percent(s)       |
            PercentEquals(s) |
            Period(s)        |
            Pipe(s)          |
            Plus(s)          |
            PlusEquals(s)    |
            QuestionMark(s)  |
            RightAngle(s)    |
            RightCurly(s)    |
            RightParen(s)    |
            RightSquare(s)   |
            Semicolon(s)     |
            Slash(s)         |
            String(s)        |
            StringRaw(s)     |
            Tilde(s)         |
            ThickArrow(s)    |
            ThinArrow(s)     |
            TimesEquals(s)   |
            TriplePeriod(s)  |
            Whitespace(s)    => s
        }
    }
}

enum Error {
    Literal(&'static str),
    ExpectedIdent,
    ExpectedDigits,
    ExpectedHex,
    ExpectedWhitespace,
    ExpectedComment,
    ExpectedCharacter,
    UnterminatedRawString,
}

impl peresil::Recoverable for Error {
    fn recoverable(&self) -> bool { true }
}

type Point<'s> = peresil::StringPoint<'s>;
type Master<'s> = peresil::ParseMaster<Point<'s>, Error>;
type Progress<'s, T> = peresil::Progress<Point<'s>, T, Error>;

pub struct Tokens<'s> {
    pm: Master<'s>,
    pt: Point<'s>,
}

impl<'s> Tokens<'s> {
    pub fn new(code: &'s str) -> Self {
        Tokens {
            pm: Master::new(),
            pt: Point::new(code),
        }
    }
}

impl<'s> Iterator for Tokens<'s> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match single_token(&mut self.pm, self.pt) {
            Progress { status: peresil::Status::Success(value), point } => {
                self.pt = point;
                Some(value)
            }
            Progress { status: peresil::Status::Failure(_), .. } => {
                // TODO: improved error reporting

                // if !point.s.is_empty() {
                //     let h = &point.s[..min(10, point.s.len())];
                //     println!("Ending at offset {}: {:?}", self.pt.offset, h);
                // }
                None
            }
        }
    }
}

fn single_token<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Token> {
    pm.alternate(pt)
        .one(comment_or_doc_comment)
        .one(map(character, Token::Character))
        .one(map(string, Token::String))
        .one(map(string_raw, Token::StringRaw))
        .one(map(byte, Token::Byte))
        .one(map(byte_string, Token::ByteString))
        .one(map(byte_string_raw, Token::ByteStringRaw))
        .one(map(lifetime, Token::Lifetime))
        .one(map(literal("->"), Token::ThinArrow))
        .one(map(literal("=>"), Token::ThickArrow))
        .one(map(literal("+="), Token::PlusEquals))
        .one(map(literal("-="), Token::MinusEquals))
        .one(map(literal("*="), Token::TimesEquals))
        .one(map(literal("/="), Token::DivideEquals))
        .one(map(literal("%="), Token::PercentEquals))
        .one(map(literal("^="), Token::CaretEquals))
        .one(map(literal("=="), Token::DoubleEquals))
        .one(map(literal("="), Token::Equals))
        .one(map(literal("+"), Token::Plus))
        .one(map(literal("-"), Token::Minus))
        .one(map(literal("@"), Token::At))
        .one(map(literal("%"), Token::Percent))
        .one(map(literal("#"), Token::Hash))
        .one(map(literal("^"), Token::Caret))
        .one(map(literal("!"), Token::Bang))
        .one(map(literal("~"), Token::Tilde))
        .one(map(literal("["), Token::LeftSquare))
        .one(map(literal("]"), Token::RightSquare))
        .one(map(literal("("), Token::LeftParen))
        .one(map(literal(")"), Token::RightParen))
        .one(map(literal("<"), Token::LeftAngle))
        .one(map(literal(">"), Token::RightAngle))
        .one(map(literal("{"), Token::LeftCurly))
        .one(map(literal("}"), Token::RightCurly))
        .one(map(literal(";"), Token::Semicolon))
        .one(map(literal("::"), Token::DoubleColon))
        .one(map(literal(":"), Token::Colon))
        .one(map(literal("||"), Token::DoublePipe))
        .one(map(literal("|"), Token::Pipe))
        .one(map(literal("*"), Token::Asterisk))
        .one(map(literal("&"), Token::Ampersand))
        .one(map(literal(","), Token::Comma))
        .one(map(literal("..."), Token::TriplePeriod))
        .one(map(literal(".."), Token::DoublePeriod))
        .one(map(literal("."), Token::Period))
        .one(map(literal("?"), Token::QuestionMark))
        .one(map(literal("$"), Token::Dollar))
        .one(map(literal("/"), Token::Slash))
        .one(map(literal(r#"\"#), Token::Backslash))
        .one(map(ident, Token::Ident))
        .one(map(digits, Token::Digits))
        .one(map(whitespace, Token::Whitespace))
        .finish()
}

fn ident<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let mut ci = pt.s.chars();
    let mut idx = 0;

    if let Some(c) = ci.next() {
        if UnicodeXID::is_xid_start(c) || c == '_' {
            idx += c.len_utf8();

            idx += ci.take_while(|&c| UnicodeXID::is_xid_continue(c)).map(|c| c.len_utf8()).sum();
        }
    }

    split_point_at_non_zero_offset(pt, idx, Error::ExpectedIdent).map(|(_, e)| e)
}

fn digits<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let ci = pt.s.chars();
    let idx = ci.take_while(|c| c.is_digit(10)).map(|c| c.len_utf8()).sum();

    split_point_at_non_zero_offset(pt, idx, Error::ExpectedDigits).map(|(_, e)| e)
}

fn whitespace<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let ci = pt.s.chars();
    let idx = ci.take_while(|&c| {
        c == ' ' || c == '\t' || c == '\r' || c == '\n'
    }).map(|c| c.len_utf8()).sum();

    split_point_at_non_zero_offset(pt, idx, Error::ExpectedWhitespace).map(|(_, e)| e)
}

fn comment_or_doc_comment<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Token> {
    let spt = pt;
    if pt.s.starts_with("///") && !pt.s[3..].starts_with("/") {
        let eol = pt.s.find("\n").unwrap_or(pt.s.len());
        let (pt, _) = try_parse!(spt.consume_to(Some(eol)).map_err(|_| Error::ExpectedComment));
        Progress::success(pt, Token::DocComment(ex(spt, pt)))
    } else if pt.s.starts_with("//") {
        let eol = pt.s.find("\n").unwrap_or(pt.s.len());
        let (pt, _) = try_parse!(spt.consume_to(Some(eol)).map_err(|_| Error::ExpectedComment));
        Progress::success(pt, Token::Comment(ex(spt, pt)))
    } else {
        Progress::failure(pt, Error::ExpectedComment)
    }
}

fn character<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        spt = point;
        _   = literal("'");
        _   = character_char;
        _   = literal("'");
    }, |_, pt| ex(spt, pt))
}

fn character_char<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, &'s str> {
    pm.alternate(pt)
        .one(escaped_char)
        .one(single_char)
        .finish()
}

fn escaped_char<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, &'s str> {
    sequence!(pm, pt, {
        spt = point;
        _   = literal("\\");
        _   = escaped_char_code;
    }, |_, pt| spt.to(pt))
}

fn escaped_char_code<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    pm.alternate(pt)
        .one(literal("n"))
        .one(literal("r"))
        .one(literal("t"))
        .one(literal("\\"))
        .one(literal("'"))
        .one(literal("\""))
        .one(literal("0"))
        .one(escaped_char_hex)
        .one(escaped_char_unicode)
        .finish()
}

fn escaped_char_hex<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        spt = point;
        _   = literal("x");
        _   = hex_string;
    }, |_, pt| ex(spt, pt))
}

fn escaped_char_unicode<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        spt = point;
        _   = literal("u{");
        _   = hex_string;
        _   = literal("}");
    }, |_, pt| ex(spt, pt))
}

fn hex_string<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, &'s str> {
    let ci = pt.s.chars();
    let idx = ci.take_while(|c| c.is_digit(16)).map(|c| c.len_utf8()).sum();

    let idx = if idx == 0 { None } else { Some(idx) };
    pt.consume_to(idx).map_err(|_| Error::ExpectedHex)
}

fn single_char<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, &'s str> {
    match pt.s.char_indices().next() {
        Some((_, c)) => {
            let i = c.len_utf8();
            let (head, tail) = pt.s.split_at(i);
            let pt = Point { s: tail, offset: pt.offset + i };
            Progress::success(pt, head)
        }
        None => {
            Progress::failure(pt, Error::ExpectedCharacter)
        }
    }
}

fn string<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        spt = point;
        _   = literal("\"");
        _   = string_char;
        _   = literal("\"");
    }, |_, pt| ex(spt, pt))
}

fn string_char<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, &'s str> {
    let res = |i| {
        let (head, tail) = pt.s.split_at(i);
        let pt = Point { s: tail, offset: pt.offset + i };
        Progress::success(pt, head)
    };

    let mut escaped = false;
    for (i, c) in pt.s.char_indices() {
        match (escaped, c) {
            (true, _) => escaped = false,
            (false, '\\') => escaped = true,
            (false, '"') => return res(i),
            (false, _) => { /* Next char */ },
        }
    }

    res(pt.s.len())
}

fn string_raw<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        spt = point;
        _   = literal("r");
        h   = zero_or_more(literal("#"));
        _   = literal(r#"""#);
        _   = raw_string_tail(h.len());
    }, |_, pt| ex(spt, pt))
}

fn raw_string_tail<'s>(hashes: usize) -> impl Fn(&mut Master<'s>, Point<'s>) ->
    Progress<'s, &'s str>
{
    let mut s = r#"""#.to_string();
    for _ in 0..hashes { s.push('#') };

    move |_, pt| {
        match pt.s.find(&s) {
            Some(end) => {
                let (str_content, quote_tail) = pt.s.split_at(end);
                let (_quotes, tail) = quote_tail.split_at(s.len());
                let pt = Point { s: tail, offset: pt.offset + end + s.len() };
                Progress::success(pt, str_content)
            }
            None => {
                Progress::failure(pt, Error::UnterminatedRawString)
            }
        }
    }
}

fn byte<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        spt = point;
        _   = literal("b");
        _   = character;
    }, |_, pt| ex(spt, pt))
}

fn byte_string<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        spt = point;
        _   = literal("b");
        _   = string;
    }, |_, pt| ex(spt, pt))
}

fn byte_string_raw<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        spt = point;
        _   = literal("b");
        _   = string_raw;
    }, |_, pt| ex(spt, pt))
}

fn lifetime<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        spt = point;
        _   = literal("'");
        _   = ident;
    }, |_, pt| ex(spt, pt))
}

fn literal<'s>(expected: &'static str) ->
    impl Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, Extent>
{
    move |_, spt| {
        let (pt, _) = try_parse!(spt.consume_literal(expected).map_err(|_| Error::Literal(expected)));
        Progress::success(pt, ex(spt, pt))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn tok(s: &str) -> Vec<Token> {
        Tokens::new(s).collect()
    }

    #[test]
    fn character() {
        let toks = tok("'a'");
        match toks[0] {
            Token::Character(s) => assert_eq!(s, (0, 3)),
            _ => panic!("Not a character: {:?}", toks[0]),
        }
    }

    #[test]
    fn character_escaped() {
        let toks = tok(r#"'\\'"#);
        match toks[0] {
            Token::Character(s) => assert_eq!(s, (0, 4)),
            _ => panic!("Not a character: {:?}", toks[0]),
        }
    }

    #[test]
    fn character_escaped_hex() {
        let toks = tok(r#"'\x41'"#);
        match toks[0] {
            Token::Character(s) => assert_eq!(s, (0, 6)),
            _ => panic!("Not a character: {:?}", toks[0]),
        }
    }

    #[test]
    fn character_escaped_unicode() {
        let toks = tok(r#"'\u{1F63B}'"#);
        match toks[0] {
            Token::Character(s) => assert_eq!(s, (0, 11)),
            _ => panic!("Not a character: {:?}", toks[0]),
        }
    }

    #[test]
    fn character_limited_to_single() {
        let toks = tok("impl<'a> Foo<'a> for Bar<'a> { }");
        match toks[2] {
            Token::Lifetime(s) => assert_eq!(s, (5, 7)),
            _ => panic!("Not a lifetime"),
        }
        match toks[7] {
            Token::Lifetime(s) => assert_eq!(s, (13, 15)),
            _ => panic!("Not a lifetime"),
        }
        match toks[14] {
            Token::Lifetime(s) => assert_eq!(s, (25, 27)),
            _ => panic!("Not a lifetime"),
        }
    }

    #[test]
    fn string_raw() {
        let toks = tok(r###"r#"inner"#"###);
        match toks[0] {
            Token::StringRaw(s) => assert_eq!(s, (0, 10)),
            _ => panic!("Not a raw string"),
        }
    }

    #[test]
    fn byte() {
        let toks = tok(r#"b'a'"#);
        match toks[0] {
            Token::Byte(s) => assert_eq!(s, (0, 4)),
            _ => panic!("Not a byte: {:?}", toks[0]),
        }
    }

    #[test]
    fn byte_string() {
        let toks = tok(r#"b"abc""#);
        match toks[0] {
            Token::ByteString(s) => assert_eq!(s, (0, 6)),
            _ => panic!("Not a byte string"),
        }
    }

    #[test]
    fn byte_string_raw() {
        let toks = tok(r#"br"abc""#);
        match toks[0] {
            Token::ByteStringRaw(s) => assert_eq!(s, (0, 7)),
            _ => panic!("Not a raw byte string: {:?}", toks[0]),
        }
    }

    #[test]
    fn tilde_is_a_token_even_though_unused() {
        let toks = tok("~");
        match toks[0] {
            Token::Tilde(s) => assert_eq!(s, (0, 1)),
            _ => panic!("Not a tilde: {:?}", toks[0]),
        }
    }
}
