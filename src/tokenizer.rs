use std::collections::BTreeSet;
use std::fmt;

use unicode_xid::UnicodeXID;
use peresil;
use peresil::combinators::*;

use ::{Extent, HumanTextError};
use ::combinators::{not, peek};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Decompose)]
pub enum Token {
    // Paired delimiters
    LeftAngle(Extent),
    LeftCurly(Extent),
    LeftParen(Extent),
    LeftSquare(Extent),
    RightAngle(Extent),
    RightCurly(Extent),
    RightParen(Extent),
    RightSquare(Extent),

    // Symbols
    //
    // TODO: Decide how to name the foo-equals tokens.
    // Should they match (e.g. caret and caret-equals)?
    // Should they infer any meaning (e.g. xor)
    Ampersand(Extent),
    AmpersandEquals(Extent),
    Asterisk(Extent),
    At(Extent),
    Backslash(Extent),
    Bang(Extent),
    Caret(Extent),
    CaretEquals(Extent),
    Colon(Extent),
    Comma(Extent),
    DivideEquals(Extent),
    Dollar(Extent),
    DoubleAmpersand(Extent),
    DoubleColon(Extent),
    DoubleEquals(Extent),
    DoubleLeftAngle(Extent),
    DoublePeriod(Extent),
    DoublePeriodEquals(Extent),
    DoublePipe(Extent),
    DoubleRightAngle(Extent),
    Equals(Extent),
    GreaterThanOrEquals(Extent),
    Hash(Extent),
    LessThanOrEquals(Extent),
    Minus(Extent),
    MinusEquals(Extent),
    NotEqual(Extent),
    Percent(Extent),
    PercentEquals(Extent),
    Period(Extent),
    Pipe(Extent),
    PipeEquals(Extent),
    Plus(Extent),
    PlusEquals(Extent),
    QuestionMark(Extent),
    Semicolon(Extent),
    ShiftLeftEquals(Extent),
    ShiftRightEquals(Extent),
    Slash(Extent),
    ThickArrow(Extent),
    ThinArrow(Extent),
    Tilde(Extent),
    TimesEquals(Extent),
    TriplePeriod(Extent),

    // Keywords
    As(Extent),
    Auto(Extent),
    Box(Extent),
    Break(Extent),
    Const(Extent),
    Continue(Extent),
    Crate(Extent),
    Default(Extent),
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
    SelfIdent(Extent),
    Static(Extent),
    Struct(Extent),
    Trait(Extent),
    Type(Extent),
    Union(Extent),
    Unsafe(Extent),
    Use(Extent),
    Where(Extent),
    While(Extent),

    // String-like
    Character(Extent),
    String(Extent),
    StringRaw(Extent),
    Byte(Extent),
    ByteString(Extent),
    ByteStringRaw(Extent),

    // Other
    Ident(Extent),
    Number(Number),
    Whitespace(Extent),
    CommentLine(Extent),
    CommentBlock(Extent),
    DocCommentOuterLine(Extent),
    DocCommentInnerLine(Extent),
    DocCommentOuterBlock(Extent),
    DocCommentInnerBlock(Extent),
    Lifetime(Extent),
    EndOfFile(Extent),
}

impl Token {
    pub fn extent(&self) -> Extent {
        use self::Token::*;

        match *self {
            Ampersand(s)           |
            AmpersandEquals(s)     |
            As(s)                  |
            Asterisk(s)            |
            At(s)                  |
            Auto(s)                |
            Backslash(s)           |
            Bang(s)                |
            Box(s)                 |
            Break(s)               |
            Byte(s)                |
            ByteString(s)          |
            ByteStringRaw(s)       |
            Caret(s)               |
            CaretEquals(s)         |
            Character(s)           |
            Colon(s)               |
            Comma(s)               |
            CommentLine(s)         |
            CommentBlock(s)        |
            Const(s)               |
            Continue(s)            |
            Crate(s)               |
            Default(s)             |
            DivideEquals(s)        |
            DocCommentInnerLine(s) |
            DocCommentInnerBlock(s)|
            DocCommentOuterLine(s) |
            DocCommentOuterBlock(s)|
            Dollar(s)              |
            DoubleAmpersand(s)     |
            DoubleColon(s)         |
            DoubleEquals(s)        |
            DoubleLeftAngle(s)     |
            DoublePeriod(s)        |
            DoublePeriodEquals(s)  |
            DoublePipe(s)          |
            DoubleRightAngle(s)    |
            Else(s)                |
            EndOfFile(s)           |
            Enum(s)                |
            Equals(s)              |
            Extern(s)              |
            Fn(s)                  |
            For(s)                 |
            GreaterThanOrEquals(s) |
            Hash(s)                |
            Ident(s)               |
            If(s)                  |
            Impl(s)                |
            In(s)                  |
            LeftAngle(s)           |
            LeftCurly(s)           |
            LeftParen(s)           |
            LeftSquare(s)          |
            LessThanOrEquals(s)    |
            Let(s)                 |
            Lifetime(s)            |
            Loop(s)                |
            Match(s)               |
            Minus(s)               |
            MinusEquals(s)         |
            Mod(s)                 |
            Move(s)                |
            Mut(s)                 |
            NotEqual(s)            |
            Percent(s)             |
            PercentEquals(s)       |
            Period(s)              |
            Pipe(s)                |
            PipeEquals(s)          |
            Plus(s)                |
            PlusEquals(s)          |
            Pub(s)                 |
            QuestionMark(s)        |
            Ref(s)                 |
            Return(s)              |
            RightAngle(s)          |
            RightCurly(s)          |
            RightParen(s)          |
            RightSquare(s)         |
            SelfIdent(s)           |
            Semicolon(s)           |
            ShiftLeftEquals(s)     |
            ShiftRightEquals(s)    |
            Slash(s)               |
            Static(s)              |
            String(s)              |
            StringRaw(s)           |
            Struct(s)              |
            ThickArrow(s)          |
            ThinArrow(s)           |
            Tilde(s)               |
            TimesEquals(s)         |
            Trait(s)               |
            TriplePeriod(s)        |
            Type(s)                |
            Union(s)               |
            Unsafe(s)              |
            Use(s)                 |
            Where(s)               |
            While(s)               |
            Whitespace(s)          => s,

            Number(s) => s.extent(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Decompose)]
pub enum Number {
    Binary(NumberBinary),
    Decimal(NumberDecimal),
    Hexadecimal(NumberHexadecimal),
    Octal(NumberOctal),
}

impl Number {
    fn extent(&self) -> Extent {
        use self::Number::*;

        match *self {
            Binary(n) => n.extent(),
            Decimal(n) => n.extent(),
            Hexadecimal(n) => n.extent(),
            Octal(n) => n.extent(),
        }
    }

    pub fn into_simple(self) -> Option<Extent> {
        match self {
            Number::Decimal(d) => {
                if d.fractional.is_none() &&
                    d.exponent.is_none() &&
                    d.type_suffix.is_none() &&
                    d.underscores == 0
                {
                    Some(d.extent)
                } else {
                    None
                }
            }
            _ => None
        }
    }
}

macro_rules! number {
    ($name:ident) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        pub struct $name {
            pub extent: Extent,
            pub integral: Extent,
            pub fractional: Option<Extent>,
            pub exponent: Option<Extent>,
            pub type_suffix: Option<Extent>,
            underscores: usize,
        }

        impl $name {
            fn finish(details: NumberDetailsPartial,
                      extent: Extent,
                      exponent: Option<Extent>,
                      type_suffix: Option<Extent>) -> $name
            {
                let NumberDetailsPartial { integral, fractional, underscores } = details;
                $name { extent, integral, fractional, exponent, type_suffix, underscores }
            }

            pub fn extent(&self) -> Extent {
                self.extent
            }
        }
    }
}

number!(NumberBinary);
number!(NumberDecimal);
number!(NumberHexadecimal);
number!(NumberOctal);

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Error {
    Literal(&'static str),
    ExpectedIdent,
    ExpectedNumber,
    ExpectedHex,
    ExpectedWhitespace,
    ExpectedComment,
    ExpectedCharacter,
    UnterminatedRawString,

    // Internal parsing errors, should be recovered
    InvalidFollowForFractionalNumber,
}

impl peresil::Recoverable for Error {
    fn recoverable(&self) -> bool { true }
}

/// Information about a tokenization error
#[derive(Debug, PartialEq, Eq)]
pub struct ErrorDetail {
    location: usize,
    errors: BTreeSet<Error>,
}

impl ErrorDetail {
    /// Enhance the error with the source code
    pub fn with_text<'a>(&'a self, text: &'a str) -> ErrorDetailText<'a> {
        ErrorDetailText { detail: self, text }
    }
}

/// Information about a tokenization error including original source code
#[derive(Debug)]
pub struct ErrorDetailText<'a> {
    detail: &'a ErrorDetail,
    text: &'a str,
}

impl<'a> fmt::Display for ErrorDetailText<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let human = HumanTextError::new(self.text, self.detail.location);

        writeln!(f, "Unable to tokenize text (line {}, column {})", human.line, human.column)?;
        writeln!(f, "{}{}", human.head_of_line, human.tail_of_line)?;
        writeln!(f, "{:>width$}", "^", width = human.column)?;
        writeln!(f, "Expected:")?;
        for e in &self.detail.errors {
            writeln!(f, "  {:?}", e)?; // TODO: should be Display
        }
        Ok(())
    }
}

type Point<'s> = peresil::StringPoint<'s>;
type Master<'s> = peresil::ParseMaster<Point<'s>, Error>;
type Progress<'s, T> = peresil::Progress<Point<'s>, T, Error>;

pub struct Tokens<'s> {
    pm: Master<'s>,
    pt: Point<'s>,
    is_exhausted: bool,
}

impl<'s> Tokens<'s> {
    pub fn new(code: &'s str) -> Self {
        Tokens {
            pm: Master::new(),
            pt: Point::new(code),
            is_exhausted: false,
        }
    }
}

impl<'s> Iterator for Tokens<'s> {
    type Item = Result<Token, ErrorDetail>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_exhausted {
            return None
        }

        if self.pt.s.is_empty() {
            self.is_exhausted = true;
            return Some(Ok(Token::EndOfFile(Extent(self.pt.offset, self.pt.offset))));
        }

        let tok = single_token(&mut self.pm, self.pt);
        let tok = self.pm.finish(tok);

        match tok {
            peresil::Progress { status: peresil::Status::Success(value), point } => {
                assert_ne!(self.pt.offset, point.offset, "Tokenizer did not make progress");
                self.pt = point;
                Some(Ok(value))
            }
            peresil::Progress { status: peresil::Status::Failure(errors), point } => {
                Some(Err(ErrorDetail {
                    location: point.offset,
                    errors: errors.into_iter().collect(),
                }))
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

        // Symbols; longest first
        .one(map(literal(">>="), Token::ShiftRightEquals))
        .one(map(literal("<<="), Token::ShiftLeftEquals))
        .one(map(literal("..."), Token::TriplePeriod))
        .one(map(literal("..="), Token::DoublePeriodEquals))

        // Symbols - 2 character
        .one(map(literal("!="), Token::NotEqual))
        .one(map(literal("%="), Token::PercentEquals))
        .one(map(literal("&&"), Token::DoubleAmpersand))
        .one(map(literal("&="), Token::AmpersandEquals))
        .one(map(literal("*="), Token::TimesEquals))
        .one(map(literal("+="), Token::PlusEquals))
        .one(map(literal("-="), Token::MinusEquals))
        .one(map(literal("->"), Token::ThinArrow))
        .one(map(literal("/="), Token::DivideEquals))
        .one(map(literal("<<"), Token::DoubleLeftAngle))
        .one(map(literal("<="), Token::LessThanOrEquals))
        .one(map(literal("=="), Token::DoubleEquals))
        .one(map(literal("=>"), Token::ThickArrow))
        .one(map(literal(">="), Token::GreaterThanOrEquals))
        .one(map(literal(">>"), Token::DoubleRightAngle))
        .one(map(literal("^="), Token::CaretEquals))
        .one(map(literal("|="), Token::PipeEquals))
        .one(map(literal(".."), Token::DoublePeriod))
        .one(map(literal("::"), Token::DoubleColon))
        .one(map(literal("||"), Token::DoublePipe))

        // Symbols - 1 character
        .one(map(literal("!"), Token::Bang))
        .one(map(literal("#"), Token::Hash))
        .one(map(literal("$"), Token::Dollar))
        .one(map(literal("%"), Token::Percent))
        .one(map(literal("&"), Token::Ampersand))
        .one(map(literal("*"), Token::Asterisk))
        .one(map(literal("+"), Token::Plus))
        .one(map(literal(","), Token::Comma))
        .one(map(literal("-"), Token::Minus))
        .one(map(literal("."), Token::Period))
        .one(map(literal("/"), Token::Slash))
        .one(map(literal(":"), Token::Colon))
        .one(map(literal(";"), Token::Semicolon))
        .one(map(literal("="), Token::Equals))
        .one(map(literal("?"), Token::QuestionMark))
        .one(map(literal("@"), Token::At))
        .one(map(literal("^"), Token::Caret))
        .one(map(literal("|"), Token::Pipe))
        .one(map(literal("~"), Token::Tilde))
        .one(map(literal(r#"\"#), Token::Backslash))

        // Paired delimiters
        .one(map(literal("("), Token::LeftParen))
        .one(map(literal(")"), Token::RightParen))
        .one(map(literal("<"), Token::LeftAngle))
        .one(map(literal(">"), Token::RightAngle))
        .one(map(literal("["), Token::LeftSquare))
        .one(map(literal("]"), Token::RightSquare))
        .one(map(literal("{"), Token::LeftCurly))
        .one(map(literal("}"), Token::RightCurly))

        // Specialty items
        .one(keyword_or_ident)
        .one(map(number, Token::Number))
        .one(map(whitespace, Token::Whitespace))
        .finish()
}

fn keyword_or_ident<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Token> {
    ident_raw(pm, pt).map(|(s, extent)| {
        match s {
            "as" => Token::As(extent),
            "auto" => Token::Auto(extent),
            "box" => Token::Box(extent),
            "break" => Token::Break(extent),
            "const" => Token::Const(extent),
            "continue" => Token::Continue(extent),
            "crate" => Token::Crate(extent),
            "default" => Token::Default(extent),
            "else" => Token::Else(extent),
            "enum" => Token::Enum(extent),
            "extern" => Token::Extern(extent),
            "fn" => Token::Fn(extent),
            "for" => Token::For(extent),
            "if" => Token::If(extent),
            "impl" => Token::Impl(extent),
            "in" => Token::In(extent),
            "let" => Token::Let(extent),
            "loop" => Token::Loop(extent),
            "match" => Token::Match(extent),
            "mod" => Token::Mod(extent),
            "move" => Token::Move(extent),
            "mut" => Token::Mut(extent),
            "pub" => Token::Pub(extent),
            "ref" => Token::Ref(extent),
            "return" => Token::Return(extent),
            "self" => Token::SelfIdent(extent),
            "static" => Token::Static(extent),
            "struct" => Token::Struct(extent),
            "trait" => Token::Trait(extent),
            "type" => Token::Type(extent),
            "use" => Token::Use(extent),
            "union" => Token::Union(extent),
            "unsafe" => Token::Unsafe(extent),
            "where" => Token::Where(extent),
            "while" => Token::While(extent),
            _ => Token::Ident(extent)
        }
    })
}

fn ident<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    ident_raw(pm, pt).map(|(_, e)| e)
}

fn ident_raw<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (&'s str, Extent)> {
    let mut ci = pt.s.chars();
    let mut idx = 0;

    if let Some(c) = ci.next() {
        if UnicodeXID::is_xid_start(c) || c == '_' {
            idx += c.len_utf8();

            idx += ci.take_while(|&c| UnicodeXID::is_xid_continue(c)).map(|c| c.len_utf8()).sum::<usize>();
        }
    }

    split_point_at_non_zero_offset(pt, idx, Error::ExpectedIdent)
}

enum NumberPartial {
    Binary(NumberDetailsPartial),
    Decimal(NumberDetailsPartial),
    Hexadecimal(NumberDetailsPartial),
    Octal(NumberDetailsPartial),
}

impl NumberPartial {
    fn finish(self, extent: Extent, exponent: Option<Extent>, type_suffix: Option<Extent>) ->
        Number
    {
        match self {
            NumberPartial::Binary(v) => {
                Number::Binary(NumberBinary::finish(v, extent, exponent, type_suffix))
            },
            NumberPartial::Decimal(v) => {
                Number::Decimal(NumberDecimal::finish(v, extent, exponent, type_suffix))
            },
            NumberPartial::Hexadecimal(v) => {
                Number::Hexadecimal(NumberHexadecimal::finish(v, extent, exponent, type_suffix))
            },
            NumberPartial::Octal(v) => {
                Number::Octal(NumberOctal::finish(v, extent, exponent, type_suffix))
            },
        }
    }
}

struct NumberDetailsPartial {
    integral: Extent,
    fractional: Option<Extent>,
    underscores: usize,
}

fn number<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Number> {
    sequence!(pm, pt, {
        spt         = point;
        value       = number_value;
        exponent    = optional(number_exponent);
        type_suffix = optional(ident);
    }, |_, pt| value.finish(ex(spt, pt), exponent, type_suffix))
}

fn number_value<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, NumberPartial> {
    pm.alternate(pt)
        .one(map(number_base("0b", 2), NumberPartial::Binary))
        .one(map(number_base("0x", 16), NumberPartial::Hexadecimal))
        .one(map(number_base("0o", 8), NumberPartial::Octal))
        .one(map(number_base("", 10), NumberPartial::Decimal))
        .finish()
}

fn number_base<'s>(prefix: &'static str, radix: u32) ->
    impl Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, NumberDetailsPartial>
{
    move |pm, pt| {
        sequence!(pm, pt, {
            _                       = literal(prefix);
            (integral, underscores) = number_digits(radix);
            fractional              = optional(number_fractional(radix));
        }, |_, _| NumberDetailsPartial { integral, fractional, underscores })
    }
}

fn number_fractional<'s>(radix: u32) ->
    impl Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, Extent>
{
    move |pm, pt| {
        sequence!(pm, pt, {
            spt = point;
            _   = literal(".");
            _   = not(peek(literal(".")), Error::InvalidFollowForFractionalNumber);
            _   = not(peek(ident), Error::InvalidFollowForFractionalNumber);
            _   = optional(number_digits(radix));
        }, |_, pt| ex(spt, pt))
    }
}

fn number_digits<'s>(radix: u32) ->
    impl Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, (Extent, usize)>
{
    move |_, pt| {
        let mut underscores = 0;
        let ci = pt.s.chars();
        let idx = ci
            .take_while(|&c| c.is_digit(radix) || c == '_')
            .inspect(|&c| if c == '_' { underscores += 1 })
            .map(|c| c.len_utf8())
            .sum();

        split_point_at_non_zero_offset(pt, idx, Error::ExpectedNumber).map(|(_, e)| (e, underscores))
    }
}

// TODO: add a case-insensitive matcher?
fn number_exponent<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    pm.alternate(pt)
        .one(number_exponent_lowercase)
        .one(number_exponent_uppercase)
        .finish()
}

fn number_exponent_lowercase<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        _          = literal("e");
        (value, _) = number_digits(10);
    }, |_, _| value)
}

fn number_exponent_uppercase<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        _          = literal("E");
        (value, _) = number_digits(10);
    }, |_, _| value)
}

fn whitespace<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    let ci = pt.s.chars();
    let idx = ci.take_while(|&c| {
        c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\u{200e}' || c == '\u{200f}'
    }).map(|c| c.len_utf8()).sum();

    split_point_at_non_zero_offset(pt, idx, Error::ExpectedWhitespace).map(|(_, e)| e)
}

fn comment_or_doc_comment<'s>(_pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Token> {
    let spt = pt;
    if pt.s.starts_with("///") && !pt.s.starts_with("////") {
        let eol = pt.s.find("\n").unwrap_or(pt.s.len());
        let (pt, _) = try_parse!(spt.consume_to(Some(eol)).map_err(|_| Error::ExpectedComment));
        Progress::success(pt, Token::DocCommentOuterLine(ex(spt, pt)))
    } else if pt.s.starts_with("//!") {
        let eol = pt.s.find("\n").unwrap_or(pt.s.len());
        let (pt, _) = try_parse!(spt.consume_to(Some(eol)).map_err(|_| Error::ExpectedComment));
        Progress::success(pt, Token::DocCommentInnerLine(ex(spt, pt)))
    } else if pt.s.starts_with("//") {
        let eol = pt.s.find("\n").unwrap_or(pt.s.len());
        let (pt, _) = try_parse!(spt.consume_to(Some(eol)).map_err(|_| Error::ExpectedComment));
        Progress::success(pt, Token::CommentLine(ex(spt, pt)))
    } else if pt.s.starts_with("/**") && !pt.s.starts_with("/***") && !pt.s.starts_with("/**/") {
        let eol = pt.s[3..].find("*/").map(|x| 3 + x + 2).unwrap_or(pt.s.len());
        let (pt, _) = try_parse!(spt.consume_to(Some(eol)).map_err(|_| Error::ExpectedComment));
        Progress::success(pt, Token::DocCommentOuterBlock(ex(spt, pt)))
    } else if pt.s.starts_with("/*!") {
        let eol = pt.s[3..].find("*/").map(|x| 3 + x + 2).unwrap_or(pt.s.len());
        let (pt, _) = try_parse!(spt.consume_to(Some(eol)).map_err(|_| Error::ExpectedComment));
        Progress::success(pt, Token::DocCommentInnerBlock(ex(spt, pt)))
    } else if pt.s.starts_with("/*") {
        let eol = pt.s[2..].find("*/").map(|x| 2 + x + 2).unwrap_or(pt.s.len());
        let (pt, _) = try_parse!(spt.consume_to(Some(eol)).map_err(|_| Error::ExpectedComment));
        Progress::success(pt, Token::CommentBlock(ex(spt, pt)))
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

fn ex(start: Point, end: Point) -> Extent {
    let ex = Extent(start.offset, end.offset);
    assert!(ex.1 >= ex.0, "{} does not come before {}", ex.1, ex.0);
    ex
}

fn split_point_at_non_zero_offset<'s>(pt: Point<'s>, idx: usize, e: Error) ->
    Progress<'s, (&'s str, Extent)>
{
    if idx == 0 {
        peresil::Progress::failure(pt, e)
    } else {
        let (matched, tail) = pt.s.split_at(idx);
        let end = pt.offset + idx;
        let end_pt = Point { s: tail, offset: end };

        peresil::Progress::success(end_pt, (matched, Extent(pt.offset, end)))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! tokenize_as {
        ($input:expr, $p:path) => ({
            let toks = tok($input);
            unwrap_as!(toks[0], $p)
        })
    }

    fn tok(s: &str) -> Vec<Token> {
        Tokens::new(s).collect::<Result<_, _>>().expect("Tokenization failed")
    }

    #[test]
    fn keyword_is_not_an_ident() {
        let s = tokenize_as!("for", Token::For);
        assert_eq!(s, (0, 3))
    }

    #[test]
    fn ident_can_have_keyword_substring() {
        let s = tokenize_as!("form", Token::Ident);
        assert_eq!(s, (0, 4))
    }

    #[test]
    fn character() {
        let s = tokenize_as!("'a'", Token::Character);
        assert_eq!(s, (0, 3));
    }

    #[test]
    fn character_escaped() {
        let s = tokenize_as!(r#"'\\'"#, Token::Character);
        assert_eq!(s, (0, 4));
    }

    #[test]
    fn character_escaped_hex() {
        let s = tokenize_as!(r#"'\x41'"#, Token::Character);
        assert_eq!(s, (0, 6));
    }

    #[test]
    fn character_escaped_unicode() {
        let s = tokenize_as!(r#"'\u{1F63B}'"#, Token::Character);
        assert_eq!(s, (0, 11));
    }

    #[test]
    fn character_limited_to_single() {
        let toks = tok("impl<'a> Foo<'a> for Bar<'a> { }");

        let s = unwrap_as!(toks[2], Token::Lifetime);
        assert_eq!(s, (5, 7));

        let s = unwrap_as!(toks[7], Token::Lifetime);
        assert_eq!(s, (13, 15));

        let s = unwrap_as!(toks[14], Token::Lifetime);
        assert_eq!(s, (25, 27));
    }

    #[test]
    fn string_raw() {
        let s = tokenize_as!(r###"r#"inner"#"###, Token::StringRaw);
        assert_eq!(s, (0, 10));
    }

    #[test]
    fn byte() {
        let s = tokenize_as!(r#"b'a'"#, Token::Byte);
        assert_eq!(s, (0, 4));
    }

    #[test]
    fn byte_string() {
        let s = tokenize_as!(r#"b"abc""#, Token::ByteString);
        assert_eq!(s, (0, 6));
    }

    #[test]
    fn byte_string_raw() {
        let s = tokenize_as!(r#"br"abc""#, Token::ByteStringRaw);
        assert_eq!(s, (0, 7));
    }

    #[test]
    fn tilde_is_a_token_even_though_unused() {
        let s = tokenize_as!("~", Token::Tilde);
        assert_eq!(s, (0, 1));
    }

    #[test]
    fn number_binary() {
        let s = tokenize_as!("0b0101", Token::Number);
        assert_eq!(s.extent(), (0, 6));
        let n = unwrap_as!(s, Number::Binary);
        assert_eq!(n.integral, (2, 6));
    }

    #[test]
    fn number_decimal() {
        let s = tokenize_as!("123456", Token::Number);
        assert_eq!(s.extent(), (0, 6));
        let n = unwrap_as!(s, Number::Decimal);
        assert_eq!(n.integral, (0, 6));
        let n = s.into_simple();
        assert_eq!(n, Some(Extent(0, 6)));
    }

    #[test]
    fn number_hexadecimal() {
        let s = tokenize_as!("0xBeeF", Token::Number);
        assert_eq!(s.extent(), (0, 6));
        let n = unwrap_as!(s, Number::Hexadecimal);
        assert_eq!(n.integral, (2, 6));
    }

    #[test]
    fn number_octal() {
        let s = tokenize_as!("0o0777", Token::Number);
        assert_eq!(s.extent(), (0, 6));
        let n = unwrap_as!(s, Number::Octal);
        assert_eq!(n.integral, (2, 6));
    }

    #[test]
    fn number_decimal_with_decimal() {
        let s = tokenize_as!("0.", Token::Number);
        assert_eq!(s.extent(), (0, 2));
        let n = unwrap_as!(s, Number::Decimal);
        assert_eq!(n.integral, (0, 1));
        assert_eq!(n.fractional, Some(Extent(1, 2)));
    }

    #[test]
    fn number_with_decimal() {
        let s = tokenize_as!("0xA.", Token::Number);
        assert_eq!(s.extent(), (0, 4));
        let n = unwrap_as!(s, Number::Hexadecimal);
        assert_eq!(n.integral, (2, 3));
        assert_eq!(n.fractional, Some(Extent(3, 4)));
    }

    #[test]
    fn number_with_fractional_part() {
        let s = tokenize_as!("0b01.10", Token::Number);
        assert_eq!(s.extent(), (0, 7));
        let n = unwrap_as!(s, Number::Binary);
        assert_eq!(n.integral, (2, 4));
        assert_eq!(n.fractional, Some(Extent(4, 7)));
    }

    #[test]
    fn number_with_exponent() {
        let s = tokenize_as!("0b1000E7", Token::Number);
        assert_eq!(s.extent(), (0, 8));
        let n = unwrap_as!(s, Number::Binary);
        assert_eq!(n.integral, (2, 6));
        assert_eq!(n.exponent, Some(Extent(7, 8)));
    }

    #[test]
    fn number_with_type_suffix() {
        let s = tokenize_as!("0o1234_usize", Token::Number);
        assert_eq!(s.extent(), (0, 12));
        let n = unwrap_as!(s, Number::Octal);
        assert_eq!(n.integral, (2, 7));
        assert_eq!(n.type_suffix, Some(Extent(7, 12)));
    }

    #[test]
    fn number_with_spacers() {
        let s = tokenize_as!("0x0A_1b_2C_3d", Token::Number);
        assert_eq!(s.extent(), (0, 13));
        let n = unwrap_as!(s, Number::Hexadecimal);
        assert_eq!(n.integral, (2, 13));
    }

    #[test]
    fn number_decimal_with_spacers() {
        let s = tokenize_as!("01_23", Token::Number);
        assert_eq!(s.extent(), (0, 5));
        let n = unwrap_as!(s, Number::Decimal);
        assert_eq!(n.integral, (0, 5));
    }

    #[test]
    fn number_with_everything() {
        let s = tokenize_as!("0o__12__56__.43__e__32__my_type", Token::Number);
        assert_eq!(s.extent(), (0, 31));
        let n = unwrap_as!(s, Number::Octal);
        assert_eq!(n.integral, (2, 12));
        assert_eq!(n.fractional, Some(Extent(12, 17)));
        assert_eq!(n.exponent, Some(Extent(18, 24)));
        assert_eq!(n.type_suffix, Some(Extent(24, 31)));
    }

    #[test]
    fn number_decimal_with_leading_spacer_is_an_ident() {
        let s = tokenize_as!("_42", Token::Ident);
        assert_eq!(s, (0, 3));
    }

    #[test]
    fn number_followed_by_range_is_not_fractional() {
        let toks = tok("1..2");

        let s = unwrap_as!(toks[0], Token::Number);
        assert_eq!(s.extent(), (0, 1));

        let s = unwrap_as!(toks[1], Token::DoublePeriod);
        assert_eq!(s, (1, 3));

        let s = unwrap_as!(toks[2], Token::Number);
        assert_eq!(s.extent(), (3, 4));
    }

    #[test]
    fn number_followed_by_ident_is_not_fractional() {
        let toks = tok("1.foo");

        let s = unwrap_as!(toks[0], Token::Number);
        assert_eq!(s.extent(), (0, 1));

        let s = unwrap_as!(toks[1], Token::Period);
        assert_eq!(s, (1, 2));

        let s = unwrap_as!(toks[2], Token::Ident);
        assert_eq!(s, (2, 5));
    }

    #[test]
    fn whitespace_unicode_direction_markers() {
        let s = tokenize_as!("\u{200e}\u{200f}", Token::Whitespace);
        assert_eq!(s, (0, 6))
    }

    #[test]
    fn comment_block() {
        let s = tokenize_as!("/* hi */", Token::CommentBlock);
        assert_eq!(s, (0, 8))
    }

    #[test]
    fn comment_block_not_immediately_closed() {
        let s = tokenize_as!("/*/ */", Token::CommentBlock);
        assert_eq!(s, (0, 6))
    }

    #[test]
    fn comment_block_immediately_closed() {
        let s = tokenize_as!("/**/", Token::CommentBlock);
        assert_eq!(s, (0, 4))
    }

    #[test]
    fn doc_comment_outer_block() {
        let s = tokenize_as!("/** hi */", Token::DocCommentOuterBlock);
        assert_eq!(s, (0, 9))
    }

    #[test]
    fn doc_comment_inner_block() {
        let s = tokenize_as!("/*! hi */", Token::DocCommentInnerBlock);
        assert_eq!(s, (0, 9))
    }

    #[test]
    fn doc_comment_outer_line() {
        let s = tokenize_as!("/// hi", Token::DocCommentOuterLine);
        assert_eq!(s, (0, 6))
    }

    #[test]
    fn doc_comment_inner_line() {
        let s = tokenize_as!("//! hi", Token::DocCommentInnerLine);
        assert_eq!(s, (0, 6))
    }

    #[test]
    fn end_of_file() {
        let s = tokenize_as!("", Token::EndOfFile);
        assert_eq!(s, (0, 0))
    }
}
