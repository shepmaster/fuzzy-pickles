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

    // Keywords
    As(Extent),
    Box(Extent),
    Break(Extent),
    Const(Extent),
    Continue(Extent),
    Crate(Extent),
    Else(Extent),
    Enum(Extent),
    Extern(Extent),
    Fn(Extent),
    For(Extent),
    If(Extent),
    Impl(Extent),
    In(Extent),
    Let(Extent),
    Loop(Extent),
    Match(Extent),
    Mod(Extent),
    Move(Extent),
    Mut(Extent),
    Pub(Extent),
    Ref(Extent),
    Return(Extent),
    Static(Extent),
    Struct(Extent),
    Trait(Extent),
    Type(Extent),
    Use(Extent),
    Unsafe(Extent),
    Where(Extent),
    While(Extent),

    //
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
            As(s)            |
            Asterisk(s)      |
            At(s)            |
            Backslash(s)     |
            Bang(s)          |
            Box(s)           |
            Break(s)         |
            Byte(s)          |
            ByteString(s)    |
            ByteStringRaw(s) |
            Caret(s)         |
            CaretEquals(s)   |
            Character(s)     |
            Colon(s)         |
            Comma(s)         |
            Comment(s)       |
            Const(s)         |
            Continue(s)      |
            Crate(s)         |
            Digits(s)        |
            DivideEquals(s)  |
            DocComment(s)    |
            Dollar(s)        |
            DoubleColon(s)   |
            DoubleEquals(s)  |
            DoublePeriod(s)  |
            DoublePipe(s)    |
            Else(s)          |
            Enum(s)          |
            Equals(s)        |
            Extern(s)        |
            Fn(s)            |
            For(s)           |
            Hash(s)          |
            Ident(s)         |
            If(s)            |
            Impl(s)          |
            In(s)            |
            LeftAngle(s)     |
            LeftCurly(s)     |
            LeftParen(s)     |
            LeftSquare(s)    |
            Let(s)           |
            Lifetime(s)      |
            Loop(s)          |
            Match(s)         |
            Minus(s)         |
            MinusEquals(s)   |
            Mod(s)           |
            Move(s)          |
            Mut(s)           |
            Percent(s)       |
            PercentEquals(s) |
            Period(s)        |
            Pipe(s)          |
            Plus(s)          |
            PlusEquals(s)    |
            Pub(s)           |
            QuestionMark(s)  |
            Ref(s)           |
            Return(s)        |
            RightAngle(s)    |
            RightCurly(s)    |
            RightParen(s)    |
            RightSquare(s)   |
            Semicolon(s)     |
            Slash(s)         |
            Static(s)        |
            String(s)        |
            StringRaw(s)     |
            Struct(s)        |
            ThickArrow(s)    |
            ThinArrow(s)     |
            Tilde(s)         |
            TimesEquals(s)   |
            Trait(s)         |
            TriplePeriod(s)  |
            Type(s)          |
            Unsafe(s)        |
            Use(s)           |
            Where(s)         |
            While(s)         |
            Whitespace(s)    => s
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Error {
    Literal(&'static str),
    ExpectedIdent,
    ExpectedDigits,
    ExpectedHex,
    ExpectedWhitespace,
    ExpectedComment,
    ExpectedCharacter,
    UnterminatedRawString,
    UnableToTokenize(usize),
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
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pt.s.is_empty() {
            return None
        }

        match single_token(&mut self.pm, self.pt) {
            Progress { status: peresil::Status::Success(value), point } => {
                self.pt = point;
                Some(Ok(value))
            }
            Progress { status: peresil::Status::Failure(_), point } => {
                // TODO: include the inner failure, split out master error type?
                Some(Err(Error::UnableToTokenize(point.offset)))
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
        .one(map(literal("as"), Token::As))
        .one(map(literal("box"), Token::Box))
        .one(map(literal("break"), Token::Break))
        .one(map(literal("const"), Token::Const))
        .one(map(literal("continue"), Token::Continue))
        .one(map(literal("crate"), Token::Crate))
        .one(map(literal("else"), Token::Else))
        .one(map(literal("enum"), Token::Enum))
        .one(map(literal("extern"), Token::Extern))
        .one(map(literal("fn"), Token::Fn))
        .one(map(literal("for"), Token::For))
        .one(map(literal("if"), Token::If))
        .one(map(literal("impl"), Token::Impl))
        .one(map(literal("in"), Token::In))
        .one(map(literal("let"), Token::Let))
        .one(map(literal("loop"), Token::Loop))
        .one(map(literal("match"), Token::Match))
        .one(map(literal("mod"), Token::Mod))
        .one(map(literal("move"), Token::Move))
        .one(map(literal("mut"), Token::Mut))
        .one(map(literal("pub"), Token::Pub))
        .one(map(literal("ref"), Token::Ref))
        .one(map(literal("return"), Token::Return))
        .one(map(literal("static"), Token::Static))
        .one(map(literal("struct"), Token::Struct))
        .one(map(literal("trait"), Token::Trait))
        .one(map(literal("type"), Token::Type))
        .one(map(literal("use"), Token::Use))
        .one(map(literal("unsafe"), Token::Unsafe))
        .one(map(literal("where"), Token::Where))
        .one(map(literal("while"), Token::While))
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
        Tokens::new(s).collect::<Result<_, _>>().expect("Tokenization failed")
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
