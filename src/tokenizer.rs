use unicode_xid::UnicodeXID;
use peresil;
use peresil::combinators::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Token<'s> {
    LeftSquare(&'s str),
    RightSquare(&'s str),
    LeftParen(&'s str),
    RightParen(&'s str),
    LeftAngle(&'s str),
    RightAngle(&'s str),
    LeftCurly(&'s str),
    RightCurly(&'s str),

    // TODO: Decide how to name the foo-equals tokens.
    // Should they match (e.g. caret and caret-equals)?
    // Should they infer any meaning (e.g. xor)
    Ampersand(&'s str),
    Asterisk(&'s str),
    At(&'s str),
    Backslash(&'s str),
    Bang(&'s str),
    CaretEquals(&'s str),
    Colon(&'s str),
    Comma(&'s str),
    DivideEquals(&'s str),
    Dollar(&'s str),
    DoubleColon(&'s str),
    DoubleEquals(&'s str),
    DoublePeriod(&'s str),
    DoublePipe(&'s str),
    Equals(&'s str),
    Hash(&'s str),
    Minus(&'s str),
    MinusEquals(&'s str),
    Percent(&'s str),
    PercentEquals(&'s str),
    Period(&'s str),
    Pipe(&'s str),
    Plus(&'s str),
    PlusEquals(&'s str),
    QuestionMark(&'s str),
    Semicolon(&'s str),
    Slash(&'s str),
    ThickArrow(&'s str),
    ThinArrow(&'s str),
    TimesEquals(&'s str),
    TriplePeriod(&'s str),
    Caret(&'s str),

    Ident(&'s str),
    Digits(&'s str),
    Whitespace(&'s str),
    DocComment(&'s str),
    Comment(&'s str),
    String(&'s str),
    RawString(&'s str),
    Character(&'s str),
    Lifetime(&'s str),
}

impl<'s> Token<'s> {
    pub fn data(&self) -> &'s str {
        use self::Token::*;

        match *self {
            Ampersand(s)     |
            Asterisk(s)      |
            At(s)            |
            Backslash(s)     |
            Bang(s)          |
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
            RawString(s)     |
            RightAngle(s)    |
            RightCurly(s)    |
            RightParen(s)    |
            RightSquare(s)   |
            Semicolon(s)     |
            Slash(s)         |
            String(s)        |
            ThickArrow(s)    |
            ThinArrow(s)     |
            TimesEquals(s)   |
            TriplePeriod(s)  |
            Whitespace(s)    => s
        }
    }

    pub fn is_left_paren(&self) -> bool {
        match *self {
            Token::LeftParen(_) => true,
            _ => false,
        }
    }

    pub fn is_right_paren(&self) -> bool {
        match *self {
            Token::RightParen(_) => true,
            _ => false,
        }
    }

    pub fn is_left_square(&self) -> bool {
        match *self {
            Token::LeftSquare(_) => true,
            _ => false,
        }
    }

    pub fn is_right_square(&self) -> bool {
        match *self {
            Token::RightSquare(_) => true,
            _ => false,
        }
    }

    pub fn is_left_curly(&self) -> bool {
        match *self {
            Token::LeftCurly(_) => true,
            _ => false,
        }
    }

    pub fn is_right_curly(&self) -> bool {
        match *self {
            Token::RightCurly(_) => true,
            _ => false,
        }
    }
}

enum Error {
    Literal(&'static str),
    ExpectedIdent,
    ExpectedDigits,
    ExpectedWhitespace,
    ExpectedComment,
    MultiCodepointCharacter,
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
    type Item = Token<'s>;

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

fn single_token<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Token<'s>> {
    pm.alternate(pt)
        .one(comment_or_doc_comment)
        .one(char_or_lifetime)
        .one(map(string, Token::String))
        .one(map(raw_string, Token::RawString))
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

fn ident<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, &'s str> {
    let mut ci = pt.s.chars();
    let mut idx = 0;

    if let Some(c) = ci.next() {
        if UnicodeXID::is_xid_start(c) || c == '_' {
            idx += c.len_utf8();

            idx += ci.take_while(|&c| UnicodeXID::is_xid_continue(c)).map(|c| c.len_utf8()).sum();
        }
    }

    let idx = if idx == 0 { None } else { Some(idx) };
    pt.consume_to(idx).map_err(|_| Error::ExpectedIdent)
}

fn digits<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, &'s str> {
    let ci = pt.s.chars();
    let idx = ci.take_while(|c| c.is_digit(10)).map(|c| c.len_utf8()).sum();

    let idx = if idx == 0 { None } else { Some(idx) };
    pt.consume_to(idx).map_err(|_| Error::ExpectedDigits)
}

fn whitespace<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, &'s str> {
    let ci = pt.s.chars();
    let idx = ci.take_while(|&c| {
        c == ' ' || c == '\t' || c == '\r' || c == '\n'
    }).map(|c| c.len_utf8()).sum();

    let idx = if idx == 0 { None } else { Some(idx) };
    pt.consume_to(idx).map_err(|_| Error::ExpectedWhitespace)
}

fn comment_or_doc_comment<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Token<'s>> {
    if pt.s.starts_with("///") && !pt.s[3..].starts_with("/") {
        let eol = pt.s.find("\n").unwrap_or(pt.s.len());
        pt.consume_to(Some(eol)).map(Token::DocComment).map_err(|_| Error::ExpectedComment)
    } else if pt.s.starts_with("//") {
        let eol = pt.s.find("\n").unwrap_or(pt.s.len());
        pt.consume_to(Some(eol)).map(Token::Comment).map_err(|_| Error::ExpectedComment)
    } else {
        Progress::failure(pt, Error::ExpectedComment)
    }
}

fn string<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, &'s str> {
    sequence!(pm, pt, {
        spt = point;
        _   = literal("\"");
        _   = str_char;
        _   = literal("\"");
    }, |_, pt| spt.to(pt))
}

fn str_char<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, &'s str> {
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

fn raw_string<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, &'s str> {
    sequence!(pm, pt, {
        spt = point;
        _   = literal("r");
        h   = zero_or_more(literal("#"));
        _   = literal(r#"""#);
        _   = raw_string_tail(h.len());
    }, |_, pt| spt.to(pt))
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

fn char_or_lifetime<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Token<'s>> {
    if pt.s.starts_with("'") {
        let pt2 = Point {
            s: &pt.s[1..],
            offset: pt.offset + 1,
        };

        let mut ci = pt2.s.chars();
        if let Some(c) = ci.next() {
            if Some('\'') == ci.next() {
                return pt.consume_to(Some(2 + c.len_utf8()))
                    .map(Token::Character)
                    .map_err(|_| Error::MultiCodepointCharacter)
            }
        }

        let (pt2, _name) = try_parse!(ident(pm, pt2).map_err(|_| Error::MultiCodepointCharacter));

        if pt2.s.starts_with(r#"'"#) {
            Progress::failure(pt, Error::MultiCodepointCharacter)
        } else {
            Progress::success(pt2, Token::Lifetime(pt.to(pt2)))
        }
    } else {
        Progress::failure(pt, Error::Literal("'"))
    }
}

fn literal<'s>(expected: &'static str) -> impl Fn(&mut Master<'s>, Point<'s>) ->
    Progress<'s, &'s str>
{
    move |_, pt| pt.consume_literal(expected).map_err(|_| Error::Literal(expected))
}

#[cfg(test)]
mod test {
    use super::*;

    fn tok<'s>(s: &'s str) -> Vec<Token<'s>> {
        Tokens::new(s).collect()
    }

    #[test]
    fn can_tokenize_raw_strings() {
        let toks = tok(r###"r#"inner"#"###);
        match toks[0] {
            Token::RawString(s) => assert_eq!(s, r###"r#"inner"#"###),
            _ => panic!("Not a raw string"),
        }
    }
}
