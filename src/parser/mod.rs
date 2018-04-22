mod combinators;
mod expression;

#[cfg(test)]
#[macro_use]
mod test_utils;

use {Extent, HumanTextError};
use ast::*;
use combinators::*;
use self::{
    combinators::*,
    expression::{
        expr_byte,
        expr_byte_string,
        expr_macro_call,
        expression,
        statement_expression,
    },
};
use tokenizer::{self, Token};
use peresil;
use peresil::combinators::*;
use std::{self, fmt};
use std::collections::BTreeSet;

pub(crate) type Point<'s> = TokenPoint<'s, Token>;
pub(crate) type Master<'s> = peresil::ParseMaster<Point<'s>, Error, State>;
pub(crate) type Progress<'s, T> = peresil::Progress<Point<'s>, T, Error>;

// ------

/// A Point that allows splitting the tokens based on parser whims.
///
/// The tokenizer greedily constructs tokens such that `>>=` will be
/// one token. Unfortunately, this can occur in a context where we
/// want separate tokens:
///
/// ```rust,ignore
/// let foo: Vec<Vec<u8>>= vec![];
/// ```
///
/// To handle this, if the requested token fails, we attempt to split
/// the current token. If the head of the split matches, we accept it
/// and track that we are in the middle of a split through
/// `sub_offset`.
///
/// This has the nice benefit of getting our automatic rewind
/// capability from the point and the grammar logic can stay clean.
pub(crate) struct TokenPoint<'s, T: 's> {
    pub offset: usize,
    pub sub_offset: Option<u8>,
    pub s: &'s [T],
}

impl<'s, T: 's> fmt::Debug for TokenPoint<'s, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.sub_offset {
            Some(s) => write!(f, "TokenPoint {{ {}.{} }}", self.offset, s),
            None => write!(f, "TokenPoint {{ {} }}", self.offset),
        }
    }
}

impl<'s, T: 's> TokenPoint<'s, T> {
    pub(crate) fn new(slice: &'s [T]) -> Self {
        TokenPoint {
            offset: 0,
            sub_offset: None,
            s: slice,
        }
    }

    // You'd better know what you are doing, as this doesn't care about split tokens!
    fn advance_by(&self, offset: usize) -> Self {
        TokenPoint {
            offset: self.offset + offset,
            sub_offset: None,
            s: &self.s[offset..],
        }
    }

    fn location(&self) -> (usize, Option<u8>) {
        (self.offset, self.sub_offset)
    }
}

impl<'s, T> peresil::Point for TokenPoint<'s, T> {
    fn zero() -> Self {
        Self::new(&[])
    }
}

impl<'s, T> Copy for TokenPoint<'s, T> {}
impl<'s, T> Clone for TokenPoint<'s, T> {
    fn clone(&self) -> Self { *self }
}

impl<'s, T> PartialOrd for TokenPoint<'s, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'s, T> Ord for TokenPoint<'s, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.location().cmp(&other.location())
    }
}

impl<'s, T> PartialEq for TokenPoint<'s, T> {
    fn eq(&self, other: &Self) -> bool {
        self.location().eq(&other.location())
    }
}

impl<'s, T> Eq for TokenPoint<'s, T> {}

// -----

#[derive(Debug, Default)]
pub(crate) struct State {
    expression_ambiguity: expression::ExpressionAmbiguity,
}

impl State {
    pub(crate) fn new() -> Self {
        State::default()
    }

    fn ex(&self, start: Point, end: Point) -> Extent {
        use std::cmp::Ordering;

        // When calculating the extent of an item, we need to look
        // back one token from the end. Since that's already gone, we
        // use the initial point.
        let relative_tokens = start.s;

        let start_offset = |pt: Point| -> usize {
            let Extent(a, _) = relative_tokens[0].extent();
            let a_x = pt.sub_offset.map_or(0, |x| x + 1) as usize;
            a + a_x
        };

        let end_offset = |pt: Point| -> usize {
            let offset = pt.offset - start.offset - 1;
            let Extent(_, b) = relative_tokens[offset].extent();
            let b_x = pt.sub_offset.map_or(0, |x| x + 1) as usize;
            b + b_x
        };

        match start.offset.cmp(&end.offset) {
            Ordering::Less => {
                let a = start_offset(start);
                let b = end_offset(end);
                Extent(a, b)
            }
            Ordering::Equal => {
                match start.sub_offset.cmp(&end.sub_offset) {
                    Ordering::Less => {
                        let a = start_offset(start);
                        let b = start_offset(end);
                        Extent(a, b)
                    }
                    Ordering::Equal => {
                        let a = start_offset(start);
                        Extent(a, a)
                    }
                    Ordering::Greater => panic!("points are backwards ({:?}, {:?})", start, end),
                }
            }
            Ordering::Greater => panic!("points are backwards ({:?}, {:?})", start, end),
        }
    }
}

// define an error type - emphasis on errors. Need to implement Recoverable (more to discuss.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Error {
    ExpectedAmpersand,
    ExpectedAmpersandEquals,
    ExpectedAs,
    ExpectedAsterisk,
    ExpectedAt,
    ExpectedAuto,
    #[allow(unused)]
    ExpectedBackslash,
    ExpectedBang,
    ExpectedBox,
    ExpectedBreak,
    ExpectedByte,
    ExpectedByteString,
    ExpectedByteStringRaw,
    ExpectedCaret,
    ExpectedCaretEquals,
    ExpectedCharacter,
    ExpectedColon,
    ExpectedComma,
    ExpectedConst,
    ExpectedContinue,
    ExpectedCrate,
    ExpectedDefault,
    ExpectedDivideEquals,
    ExpectedDocCommentInnerBlock,
    ExpectedDocCommentInnerLine,
    ExpectedDocCommentOuterBlock,
    ExpectedDocCommentOuterLine,
    #[allow(unused)]
    ExpectedDollar,
    ExpectedDoubleAmpersand,
    ExpectedDoubleColon,
    ExpectedDoubleEquals,
    ExpectedDoubleLeftAngle,
    ExpectedDoublePeriod,
    ExpectedDoublePeriodEquals,
    ExpectedDoublePipe,
    ExpectedDoubleRightAngle,
    ExpectedElse,
    ExpectedEnum,
    ExpectedEquals,
    ExpectedExtern,
    ExpectedFn,
    ExpectedFor,
    ExpectedGreaterThanOrEquals,
    ExpectedHash,
    ExpectedIdent,
    ExpectedIf,
    ExpectedImpl,
    ExpectedIn,
    ExpectedLeftAngle,
    ExpectedLeftCurly,
    ExpectedLeftParen,
    ExpectedLeftSquare,
    ExpectedLessThanOrEquals,
    ExpectedLet,
    ExpectedLifetime,
    ExpectedLoop,
    ExpectedMatch,
    ExpectedMinus,
    ExpectedMinusEquals,
    ExpectedMod,
    ExpectedMove,
    ExpectedMut,
    ExpectedNotEqual,
    ExpectedNumber,
    ExpectedPercent,
    ExpectedPercentEquals,
    ExpectedPeriod,
    ExpectedPipe,
    ExpectedPipeEquals,
    ExpectedPlus,
    ExpectedPlusEquals,
    ExpectedPub,
    ExpectedQuestionMark,
    ExpectedRef,
    ExpectedReturn,
    ExpectedRightAngle,
    ExpectedRightCurly,
    ExpectedRightParen,
    ExpectedRightSquare,
    ExpectedSelfIdent,
    ExpectedSemicolon,
    ExpectedShiftLeftEquals,
    ExpectedShiftRightEquals,
    ExpectedSlash,
    ExpectedStatic,
    ExpectedString,
    ExpectedStringRaw,
    ExpectedStruct,
    ExpectedThickArrow,
    ExpectedThinArrow,
    #[allow(unused)]
    ExpectedTilde,
    ExpectedTimesEquals,
    ExpectedTrait,
    ExpectedTriplePeriod,
    ExpectedType,
    ExpectedUnion,
    ExpectedUnsafe,
    ExpectedUse,
    ExpectedWhere,
    ExpectedWhile,

    ExpectedExpression,

    BlockNotAllowedHere,
}

impl peresil::Recoverable for Error {
    fn recoverable(&self) -> bool { true }
}

/// Information about a parsing error
#[derive(Debug, PartialEq)]
pub struct ErrorDetail {
    pub(crate) location: usize,
    pub(crate) errors: BTreeSet<Error>,
}

impl ErrorDetail {
    /// Enhance the error with the source code
    pub fn with_text<'a>(&'a self, text: &'a str) -> ErrorDetailText<'a> {
        ErrorDetailText { detail: self, text }
    }
}

/// Information about a parsing error including original source code
#[derive(Debug)]
pub struct ErrorDetailText<'a> {
    detail: &'a ErrorDetail,
    text: &'a str,
}

impl<'a> fmt::Display for ErrorDetailText<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let human = HumanTextError::new(self.text, self.detail.location);

        writeln!(f, "Unable to parse text (line {}, column {})", human.line, human.column)?;
        writeln!(f, "{}{}", human.head_of_line, human.tail_of_line)?;
        writeln!(f, "{:>width$}", "^", width = human.column)?;
        writeln!(f, "Expected:")?;
        for e in &self.detail.errors {
            writeln!(f, "  {:?}", e)?; // TODO: should be Display
        }
        Ok(())
    }
}

pub(crate) fn item<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Item> {
    pm.alternate(pt)
        .one(map(attribute_containing, Item::AttributeContaining))
        .one(map(p_const, Item::Const))
        .one(map(extern_crate, Item::ExternCrate))
        .one(map(extern_block, Item::ExternBlock))
        .one(map(function, Item::Function))
        .one(map(item_macro_call, Item::MacroCall))
        .one(map(module, Item::Module))
        .one(map(p_enum, Item::Enum))
        .one(map(p_impl, Item::Impl))
        .one(map(p_static, Item::Static))
        .one(map(p_struct, Item::Struct))
        .one(map(p_trait, Item::Trait))
        .one(map(p_union, Item::Union))
        .one(map(p_use, Item::Use))
        .one(map(type_alias, Item::TypeAlias))
        .finish()
}

macro_rules! shim {
    ($name:ident, $matcher:expr, $error:expr) => {
        shim!($name, $matcher, $error, Extent);
    };
    ($name:ident, $matcher:expr, $error:expr, $t:ty) => {
        fn $name<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, $t> {
            token($matcher, $error)(pm, pt)
        }
    };
}

macro_rules! shims {
    [$( ($( $arg:tt )*), )*] => {
        $( shim!($( $arg )*); )*
    };
}

shims! [
    // Match up these names better
    (ident_normal, Token::into_ident, Error::ExpectedIdent),
    (lifetime_normal, Token::into_lifetime, Error::ExpectedLifetime),
    (number_normal, Token::into_number, Error::ExpectedNumber, tokenizer::Number),

    (character, Token::into_character, Error::ExpectedCharacter),
    (string, Token::into_string, Error::ExpectedString),
    (string_raw, Token::into_string_raw, Error::ExpectedStringRaw),
    (byte, Token::into_byte, Error::ExpectedByte),
    (byte_string, Token::into_byte_string, Error::ExpectedByteString),
    (byte_string_raw, Token::into_byte_string_raw, Error::ExpectedByteStringRaw),

    // Keywords
    (kw_as, Token::into_as, Error::ExpectedAs),
    (kw_auto, Token::into_auto, Error::ExpectedAuto),
    (kw_box, Token::into_box, Error::ExpectedBox),
    (kw_break, Token::into_break, Error::ExpectedBreak),
    (kw_const, Token::into_const, Error::ExpectedConst),
    (kw_continue, Token::into_continue, Error::ExpectedContinue),
    (kw_crate, Token::into_crate, Error::ExpectedCrate),
    (kw_default, Token::into_default, Error::ExpectedDefault),
    (kw_else, Token::into_else, Error::ExpectedElse),
    (kw_enum, Token::into_enum, Error::ExpectedEnum),
    (kw_extern, Token::into_extern, Error::ExpectedExtern),
    (kw_fn, Token::into_fn, Error::ExpectedFn),
    (kw_for, Token::into_for, Error::ExpectedFor),
    (kw_if, Token::into_if, Error::ExpectedIf),
    (kw_impl, Token::into_impl, Error::ExpectedImpl),
    (kw_in, Token::into_in, Error::ExpectedIn),
    (kw_let, Token::into_let, Error::ExpectedLet),
    (kw_loop, Token::into_loop, Error::ExpectedLoop),
    (kw_match, Token::into_match, Error::ExpectedMatch),
    (kw_mod, Token::into_mod, Error::ExpectedMod),
    (kw_move, Token::into_move, Error::ExpectedMove),
    (kw_mut, Token::into_mut, Error::ExpectedMut),
    (kw_pub, Token::into_pub, Error::ExpectedPub),
    (kw_ref, Token::into_ref, Error::ExpectedRef),
    (kw_return, Token::into_return, Error::ExpectedReturn),
    (kw_self_ident, Token::into_self_ident, Error::ExpectedSelfIdent),
    (kw_static, Token::into_static, Error::ExpectedStatic),
    (kw_struct, Token::into_struct, Error::ExpectedStruct),
    (kw_trait, Token::into_trait, Error::ExpectedTrait),
    (kw_type, Token::into_type, Error::ExpectedType),
    (kw_union, Token::into_union, Error::ExpectedUnion),
    (kw_unsafe, Token::into_unsafe, Error::ExpectedUnsafe),
    (kw_use, Token::into_use, Error::ExpectedUse),
    (kw_where, Token::into_where, Error::ExpectedWhere),
    (kw_while, Token::into_while, Error::ExpectedWhile),

    // Paired delimiters
    (left_angle, Token::into_left_angle, Error::ExpectedLeftAngle),
    (left_curly, Token::into_left_curly, Error::ExpectedLeftCurly),
    (left_paren, Token::into_left_paren, Error::ExpectedLeftParen),
    (left_square, Token::into_left_square, Error::ExpectedLeftSquare),
    (right_angle, Token::into_right_angle, Error::ExpectedRightAngle),
    (right_curly, Token::into_right_curly, Error::ExpectedRightCurly),
    (right_paren, Token::into_right_paren, Error::ExpectedRightParen),
    (right_square, Token::into_right_square, Error::ExpectedRightSquare),

    // Symbols
    (ampersand, Token::into_ampersand, Error::ExpectedAmpersand),
    (ampersand_equals, Token::into_ampersand_equals, Error::ExpectedAmpersandEquals),
    (asterisk, Token::into_asterisk, Error::ExpectedAsterisk),
    (at, Token::into_at, Error::ExpectedAt),
    (bang, Token::into_bang, Error::ExpectedBang),
    (caret, Token::into_caret, Error::ExpectedCaret),
    (caret_equals, Token::into_caret_equals, Error::ExpectedCaretEquals),
    (colon, Token::into_colon, Error::ExpectedColon),
    (comma, Token::into_comma, Error::ExpectedComma),
    (divide_equals, Token::into_divide_equals, Error::ExpectedDivideEquals),
    (double_ampersand, Token::into_double_ampersand, Error::ExpectedDoubleAmpersand),
    (double_colon, Token::into_double_colon, Error::ExpectedDoubleColon),
    (double_equals, Token::into_double_equals, Error::ExpectedDoubleEquals),
    (double_left_angle, Token::into_double_left_angle, Error::ExpectedDoubleLeftAngle),
    (double_period, Token::into_double_period, Error::ExpectedDoublePeriod),
    (double_period_equals, Token::into_double_period_equals, Error::ExpectedDoublePeriodEquals),
    (double_pipe, Token::into_double_pipe, Error::ExpectedDoublePipe),
    (double_right_angle, Token::into_double_right_angle, Error::ExpectedDoubleRightAngle),
    (equals, Token::into_equals, Error::ExpectedEquals),
    (greater_than_or_equals, Token::into_greater_than_or_equals, Error::ExpectedGreaterThanOrEquals),
    (hash, Token::into_hash, Error::ExpectedHash),
    (less_than_or_equals, Token::into_less_than_or_equals, Error::ExpectedLessThanOrEquals),
    (minus, Token::into_minus, Error::ExpectedMinus),
    (minus_equals, Token::into_minus_equals, Error::ExpectedMinusEquals),
    (not_equal, Token::into_not_equal, Error::ExpectedNotEqual),
    (percent, Token::into_percent, Error::ExpectedPercent),
    (percent_equals, Token::into_percent_equals, Error::ExpectedPercentEquals),
    (period, Token::into_period, Error::ExpectedPeriod),
    (pipe, Token::into_pipe, Error::ExpectedPipe),
    (pipe_equals, Token::into_pipe_equals, Error::ExpectedPipeEquals),
    (plus, Token::into_plus, Error::ExpectedPlus),
    (plus_equals, Token::into_plus_equals, Error::ExpectedPlusEquals),
    (question_mark, Token::into_question_mark, Error::ExpectedQuestionMark),
    (semicolon, Token::into_semicolon, Error::ExpectedSemicolon),
    (shift_left_equals, Token::into_shift_left_equals, Error::ExpectedShiftLeftEquals),
    (shift_right_equals, Token::into_shift_right_equals, Error::ExpectedShiftRightEquals),
    (slash, Token::into_slash, Error::ExpectedSlash),
    (thick_arrow, Token::into_thick_arrow, Error::ExpectedThickArrow),
    (thin_arrow, Token::into_thin_arrow, Error::ExpectedThinArrow),
    (times_equals, Token::into_times_equals, Error::ExpectedTimesEquals),
    (triple_period, Token::into_triple_period, Error::ExpectedTriplePeriod),

    (doc_comment_inner_block, Token::into_doc_comment_inner_block, Error::ExpectedDocCommentInnerBlock),
    (doc_comment_inner_line, Token::into_doc_comment_inner_line, Error::ExpectedDocCommentInnerLine),
    (doc_comment_outer_block, Token::into_doc_comment_outer_block, Error::ExpectedDocCommentOuterBlock),
    (doc_comment_outer_line, Token::into_doc_comment_outer_line, Error::ExpectedDocCommentOuterLine),
];

fn token<'s, F, T>(token_convert: F, error: Error) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
    where F: Fn(Token) -> Option<T>
{
    move |_, pt| {
        let original_token = match pt.s.first() {
            Some(&token) => token,
            None => return Progress::failure(pt, error),
        };

        let token = match pt.sub_offset {
            Some(sub_offset) => {
                split(original_token, sub_offset).expect("Cannot resume a split token").1
            },
            None => original_token,
        };

        match token_convert(token) {
            Some(v) => {
                // We exactly matched the requested token
                Progress::success(pt.advance_by(1), v)
            }
            None => {
                // Maybe we can split the token
                let sub_offset = pt.sub_offset.map(|x| x + 1).unwrap_or(0);
                match split(original_token, sub_offset) {
                    Some((token, _)) => {
                        match token_convert(token) {
                            // The split did match
                            Some(v) => {
                                let pt = Point {
                                    sub_offset: Some(sub_offset),
                                    ..pt
                                };
                                Progress::success(pt, v)
                            }
                            None => {
                                // The split did not match
                                Progress::failure(pt, error)
                            }
                        }
                    }
                    None => {
                        // Cannot split
                        Progress::failure(pt, error)
                    }
                }
            }
        }
    }
}

fn split(token: Token, n: u8) -> Option<(Token, Token)> {
    match (token, n) {
        (Token::DoubleLeftAngle(extent), 0) => {
            let Extent(s, e) = extent;
            let a = Token::LeftAngle(Extent(s, s+1));
            let b = Token::LeftAngle(Extent(s+1, e));
            Some((a, b))
        }
        (Token::DoubleRightAngle(extent), 0) => {
            let Extent(s, e) = extent;
            let a = Token::RightAngle(Extent(s, s+1));
            let b = Token::RightAngle(Extent(s+1, e));
            Some((a, b))
        }
        (Token::ShiftRightEquals(extent), 0) => {
            let Extent(s, e) = extent;
            let a = Token::RightAngle(Extent(s, s+1));
            let b = Token::GreaterThanOrEquals(Extent(s+1, e));
            Some((a, b))
        }
        (Token::ShiftRightEquals(extent), 1) => {
            let Extent(s, e) = extent;
            let a = Token::RightAngle(Extent(s+1, s+2));
            let b = Token::Equals(Extent(s+2, e));
            Some((a, b))
        }
        (Token::GreaterThanOrEquals(extent), 0) => {
            let Extent(s, e) = extent;
            let a = Token::RightAngle(Extent(s, s+1));
            let b = Token::Equals(Extent(s+1, e));
            Some((a, b))
        }
        (Token::DoublePipe(extent), 0) => {
            let Extent(s, e) = extent;
            let a = Token::Pipe(Extent(s, s+1));
            let b = Token::Pipe(Extent(s+1, e));
            Some((a, b))
        }
        (Token::DoubleAmpersand(extent), 0) => {
            let Extent(s, e) = extent;
            let a = Token::Ampersand(Extent(s, s+1));
            let b = Token::Ampersand(Extent(s+1, e));
            Some((a, b))
        }
        _ => None
    }
}

fn function<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Function> {
    sequence!(pm, pt, {
        spt    = point;
        header = function_header;
        body   = block;
    }, |pm: &mut Master, pt| Function {
        extent: pm.state.ex(spt, pt),
        header,
        body,
        whitespace: Vec::new()
    })
}

fn function_header<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, FunctionHeader> {
    sequence!(pm, pt, {
        spt         = point;
        visibility  = optional(visibility);
        qualifiers  = function_qualifiers;
        _           = kw_fn;
        name        = ident;
        generics    = optional(generic_declarations);
        arguments   = function_arglist;
        return_type = optional(function_return_type);
        wheres      = optional(where_clause);
    }, |pm: &mut Master, pt| {
        FunctionHeader {
            extent: pm.state.ex(spt, pt),
            visibility,
            qualifiers,
            name,
            generics,
            arguments,
            return_type,
            wheres: wheres.unwrap_or_else(Vec::new),
            whitespace: Vec::new(),
        }})
}

// TODO: This is overly loose; we can't really have a `default extern` function.
// TODO: Not all places that call this really can allow all of these qualifiers
fn function_qualifiers<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, FunctionQualifiers>
{
    sequence!(pm, pt, {
        spt        = point;
        is_default = optional(ext(kw_default));
        is_const   = optional(ext(kw_const));
        is_unsafe  = optional(ext(kw_unsafe));
        is_extern  = optional(function_qualifier_extern);
    }, |pm: &mut Master, pt| {
        let is_extern = is_extern;
        let (is_extern, abi) = match is_extern {
            Some((ex, abi)) => (Some(ex), abi),
            None => (None, None),
        };
        FunctionQualifiers {
            extent: pm.state.ex(spt, pt),
            is_default,
            is_const,
            is_unsafe,
            is_extern,
            abi,
        }
    })
}

fn function_qualifier_extern<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, (Extent, Option<String>)>
{
    sequence!(pm, pt, {
        is_extern = ext(kw_extern);
        abi       = optional(string_literal);
    }, |_, _| (is_extern, abi))
}

fn ident<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Ident> {
    pm.alternate(pt)
        .one(kw_default)
        .one(kw_self_ident)
        .one(kw_union)
        .one(ident_normal)
        .finish()
        .map(|extent| Ident { extent })
        .map_err(|_| Error::ExpectedIdent)
}

fn generic_declarations<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, GenericDeclarations> {
    sequence!(pm, pt, {
        spt       = point;
        _         = left_angle;
        lifetimes = zero_or_more_tailed_values(comma, attributed(generic_declaration_lifetime));
        types     = zero_or_more_tailed_values(comma, attributed(generic_declaration_type));
        _         = right_angle;
    }, |pm: &mut Master, pt| GenericDeclarations { extent: pm.state.ex(spt, pt), lifetimes, types })
}

fn generic_declaration_lifetime<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, GenericDeclarationLifetime> {
    sequence!(pm, pt, {
        spt        = point;
        name       = lifetime;
        bounds     = optional(generic_declaration_lifetime_bounds);
    }, |pm: &mut Master, pt| GenericDeclarationLifetime {
        extent: pm.state.ex(spt, pt),
        name,
        bounds: bounds.unwrap_or_else(Vec::new),
    })
}

fn generic_declaration_lifetime_bounds<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<Lifetime>> {
    sequence!(pm, pt, {
        _      = colon;
        bounds = zero_or_more_tailed_values(plus, lifetime);
    }, |_, _| bounds)
}

fn generic_declaration_type<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, GenericDeclarationType> {
    sequence!(pm, pt, {
        spt        = point;
        name       = ident;
        // Over-permissive; allows interleaving trait bounds and default types
        bounds     = optional(generic_declaration_type_bounds);
        default    = optional(generic_declaration_type_default);
    }, |pm: &mut Master, pt| GenericDeclarationType { extent: pm.state.ex(spt, pt), name, bounds, default })
}

fn generic_declaration_type_bounds<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TraitBounds> {
    sequence!(pm, pt, {
        _      = colon;
        bounds = trait_bounds;
    }, |_, _| bounds)
}

fn generic_declaration_type_default<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Type> {
    sequence!(pm, pt, {
        _   = equals;
        typ = typ;
    }, |_, _| typ)
}

fn function_arglist<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<Argument>> {
    sequence!(pm, pt, {
        _        = left_paren;
        self_arg = optional(map(self_argument, Argument::SelfArgument));
        args     = zero_or_more_tailed_values_append(self_arg, comma, function_argument);
        _        = right_paren;
    }, move |_, _| args)
}

fn self_argument<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, SelfArgument> {
    pm.alternate(pt)
        .one(map(self_argument_longhand, SelfArgument::Longhand))
        .one(map(self_argument_shorthand, SelfArgument::Shorthand))
        .finish()
}

fn self_argument_longhand<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, SelfArgumentLonghand> {
    sequence!(pm, pt, {
        spt    = point;
        is_mut = optional(kw_mut);
        name   = kw_self_ident;
        _      = colon;
        typ    = typ;
        _      = optional(comma);
    }, |pm: &mut Master, pt| SelfArgumentLonghand {
        extent: pm.state.ex(spt, pt),
        is_mut,
        name: Ident { extent: name },
        typ,
        whitespace: Vec::new(),
    })
}

fn self_argument_shorthand<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, SelfArgumentShorthand> {
    sequence!(pm, pt, {
        spt       = point;
        qualifier = optional(self_argument_qualifier);
        name      = kw_self_ident;
        _         = optional(comma);
    }, |pm: &mut Master, pt| SelfArgumentShorthand {
        extent: pm.state.ex(spt, pt),
        qualifier,
        name: Ident { extent: name },
        whitespace: Vec::new(),
    })
}

fn self_argument_qualifier<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, SelfArgumentShorthandQualifier>
{
    pm.alternate(pt)
        .one(map(typ_reference_kind, SelfArgumentShorthandQualifier::Reference))
        .one(map(ext(kw_mut), SelfArgumentShorthandQualifier::Mut))
        .finish()
}

fn function_argument<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Argument> {
    sequence!(pm, pt, {
        name = pattern;
        _    = colon;
        typ  = typ;
    }, |_, _| Argument::Named(NamedArgument { name, typ, whitespace: Vec::new() }))
}

fn function_return_type<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Type> {
    sequence!(pm, pt, {
        _   = thin_arrow;
        typ = typ;
    }, |_, _| typ)
}

fn where_clause<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<Where>> {
    sequence!(pm, pt, {
        _ = kw_where;
        w = one_or_more_tailed_values(comma, where_clause_item);
    }, |_, _| w)
}

fn where_clause_item<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Where> {
    sequence!(pm, pt, {
        spt   = point;
        hrtbs = optional(higher_ranked_trait_bounds);
        kind  = where_clause_kind;
    }, |pm: &mut Master, pt|  Where {
        extent: pm.state.ex(spt, pt),
        higher_ranked_trait_bounds: hrtbs.unwrap_or_else(Vec::new),
        kind,
    })
}

fn where_clause_kind<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, WhereKind> {
    pm.alternate(pt)
        .one(map(where_lifetime, WhereKind::Lifetime))
        .one(map(where_type, WhereKind::Type))
        .finish()
}

fn where_lifetime<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, WhereLifetime> {
    sequence!(pm, pt, {
        spt    = point;
        name   = lifetime;
        bounds = generic_declaration_lifetime_bounds;
    }, |pm: &mut Master, pt| WhereLifetime { extent: pm.state.ex(spt, pt), name, bounds  })
}

fn where_type<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, WhereType> {
    sequence!(pm, pt, {
        spt    = point;
        name   = typ;
        bounds = generic_declaration_type_bounds;
    }, |pm: &mut Master, pt| WhereType { extent: pm.state.ex(spt, pt), name, bounds  })
}

fn trait_bounds<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TraitBounds> {
    sequence!(pm, pt, {
        spt   = point;
        types = zero_or_more_tailed_values(plus, trait_bound);
    }, |pm: &mut Master, pt| TraitBounds { extent: pm.state.ex(spt, pt), types })
}

fn trait_bound<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TraitBound> {
    pm.alternate(pt)
        .one(map(trait_bound_lifetime, TraitBound::Lifetime))
        .one(map(trait_bound_normal, TraitBound::Normal))
        .one(map(trait_bound_relaxed, TraitBound::Relaxed))
        .finish()
}

fn trait_bound_lifetime<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TraitBoundLifetime> {
    sequence!(pm, pt, {
        spt      = point;
        lifetime = lifetime;
    }, |pm: &mut Master, pt| TraitBoundLifetime { extent: pm.state.ex(spt, pt), lifetime })
}

fn trait_bound_normal<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TraitBoundNormal> {
    sequence!(pm, pt, {
        spt = point;
        typ = trait_bound_normal_child;
    }, |pm: &mut Master, pt| TraitBoundNormal { extent: pm.state.ex(spt, pt), typ })
}

fn trait_bound_normal_child<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, TraitBoundType>
{
    pm.alternate(pt)
        .one(map(typ_named, TraitBoundType::Named))
        .one(map(typ_higher_ranked_trait_bounds, TraitBoundType::HigherRankedTraitBounds))
        .finish()
}

fn trait_bound_relaxed<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TraitBoundRelaxed> {
    sequence!(pm, pt, {
        spt = point;
        _   = question_mark;
        typ = trait_bound_normal_child;
    }, |pm: &mut Master, pt| TraitBoundRelaxed { extent: pm.state.ex(spt, pt), typ, whitespace: Vec::new() })
}

fn block<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Block> {
    sequence!(pm, pt, {
        spt               = point;
        _                 = left_curly;
        (mut stmts, term) = zero_or_more_implicitly_tailed_values_terminated(semicolon, statement);
        _                 = right_curly;
    }, |pm: &mut Master, pt| {
        let expr = if !term && stmts.last().map_or(false, Statement::is_expression) {
            stmts.pop().and_then(Statement::into_expression)
        } else {
            None
        };

        Block {
            extent: pm.state.ex(spt, pt),
            statements: stmts,
            expression: expr,
            whitespace: Vec::new(),
        }
    })
}

fn statement<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Statement> {
    pm.alternate(pt)
        .one(map(attributed(item), Statement::Item))
        .one(map(statement_expression, Statement::Expression))
        .one(map(statement_empty, Statement::Empty))
        .finish()
}

fn statement_empty<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        spt = point;
        _   = peek(semicolon);
    }, |pm: &mut Master, pt| pm.state.ex(spt, pt))
}

impl ImplicitSeparator for Statement {
    fn is_implicit_separator(&self) -> bool {
        match *self {
            Statement::Expression(ref e) => e.may_terminate_statement(),
            Statement::Item(_)           => true,
            Statement::Empty(_)          => false,
        }
    }
}

// TODO: There's a good amount of duplication here; revisit and DRY up
// Mostly in the required ; for paren and square...
fn item_macro_call<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, MacroCall> {
    sequence!(pm, pt, {
        spt  = point;
        name = ident;
        _    = bang;
        arg  = optional(ident);
        args = item_macro_call_args;
    }, |pm: &mut Master, pt| MacroCall { extent: pm.state.ex(spt, pt), name, arg, args })
}

fn item_macro_call_args<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, MacroCallArgs> {
    pm.alternate(pt)
        .one(map(item_macro_call_paren, MacroCallArgs::Paren))
        .one(map(item_macro_call_square, MacroCallArgs::Square))
        .one(map(item_macro_call_curly, MacroCallArgs::Curly))
        .finish()
}

fn item_macro_call_paren<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        _    = left_paren;
        args = parse_nested_until(Token::is_left_paren, Token::is_right_paren);
        _    = right_paren;
        _    = semicolon;
    }, |_, _| args)
}

fn item_macro_call_square<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        _    = left_square;
        args = parse_nested_until(Token::is_left_square, Token::is_right_square);
        _    = right_square;
        _    = semicolon;
    }, |_, _| args)
}

fn item_macro_call_curly<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        _    = left_curly;
        args = parse_nested_until(Token::is_left_curly, Token::is_right_curly);
        _    = right_curly;
    }, |_, _| args)
}

fn character_literal<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Character> {
    character(pm, pt)
        .map(|extent| Character { extent, value: extent }) // FIXME: value
}

fn string_literal<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, String> {
    // FIXME: value
    pm.alternate(pt)
        .one(map(string, |extent| String { extent, value: extent }))
        .one(map(string_raw, |extent| String { extent, value: extent }))
        .finish()
}

fn number_literal<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Number> {
    pm.alternate(pt)
        .one(map(number_normal, convert_number))
        .finish()
}

fn convert_number(n: tokenizer::Number) -> Number {
    match n {
        tokenizer::Number::Binary(tokenizer::NumberBinary { extent, integral, fractional, exponent, type_suffix, .. }) => {
            let value = NumberValue::Binary(NumberBinary { extent, decimal: integral, fraction: fractional, exponent, suffix: type_suffix });
            Number { extent, is_negative: None, value, whitespace: Vec::new() }
        }
        tokenizer::Number::Octal(tokenizer::NumberOctal { extent, integral, fractional, exponent, type_suffix, .. }) => {
            let value = NumberValue::Octal(NumberOctal { extent, decimal: integral, fraction: fractional, exponent, suffix: type_suffix });
            Number { extent, is_negative: None, value, whitespace: Vec::new() }
        }
        tokenizer::Number::Hexadecimal(tokenizer::NumberHexadecimal { extent, integral, fractional, exponent, type_suffix, .. }) => {
            let value = NumberValue::Hexadecimal(NumberHexadecimal { extent, decimal: integral, fraction: fractional, exponent, suffix: type_suffix });
            Number { extent, is_negative: None, value, whitespace: Vec::new() }
        }
        tokenizer::Number::Decimal(tokenizer::NumberDecimal { extent, integral, fractional, exponent, type_suffix, .. }) => {
            let value = NumberValue::Decimal(NumberDecimal { extent, decimal: integral, fraction: fractional, exponent, suffix: type_suffix });
            Number { extent, is_negative: None, value, whitespace: Vec::new() }
        }
    }
}


fn path<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Path> {
    sequence!(pm, pt, {
        spt        = point;
        _          = optional(double_colon);
        components = one_or_more_tailed_values(double_colon, ident);
    }, |pm: &mut Master, pt| Path { extent: pm.state.ex(spt, pt), components })
}

fn pathed_ident<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PathedIdent> {
    sequence!(pm, pt, {
        spt        = point;
        _          = optional(double_colon);
        components = one_or_more_tailed_values(double_colon, path_component);
    }, |pm: &mut Master, pt| PathedIdent { extent: pm.state.ex(spt, pt), components })
}

fn path_component<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PathComponent> {
    sequence!(pm, pt, {
        spt       = point;
        ident     = ident;
        turbofish = optional(turbofish);
    }, |pm: &mut Master, pt| PathComponent { extent: pm.state.ex(spt, pt), ident, turbofish })
}

fn turbofish<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Turbofish> {
    sequence!(pm, pt, {
        spt       = point;
        _         = double_colon;
        _         = left_angle;
        lifetimes = zero_or_more_tailed_values(comma, lifetime);
        types     = zero_or_more_tailed_values(comma, typ);
        _     = right_angle;
    }, |pm: &mut Master, pt| Turbofish { extent: pm.state.ex(spt, pt), lifetimes, types })
}

fn pattern<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Pattern> {
    sequence!(pm, pt, {
        spt  = point;
        name = optional(pattern_name);
        kind = pattern_kind;
    }, |pm: &mut Master, pt| Pattern { extent: pm.state.ex(spt, pt), name, kind })
}

fn pattern_name<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternName> {
    sequence!(pm, pt, {
        spt    = point;
        is_ref = optional(ext(kw_ref));
        is_mut = optional(ext(kw_mut));
        name   = ident;
        _      = at;
    }, |pm: &mut Master, _| PatternName { extent: pm.state.ex(spt, pt), is_ref, is_mut, name, whitespace: Vec::new() })
}

fn pattern_kind<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternKind> {
    pm.alternate(pt)
        // Must precede character and number as it contains them
        .one(map(pattern_range_exclusive, PatternKind::RangeExclusive))
        .one(map(pattern_range_inclusive, PatternKind::RangeInclusive))
        .one(map(pattern_char, PatternKind::Character))
        .one(map(pattern_byte, PatternKind::Byte))
        .one(map(pattern_number, PatternKind::Number))
        .one(map(pattern_reference, PatternKind::Reference))
        .one(map(pattern_byte_string, PatternKind::ByteString))
        .one(map(pattern_string, PatternKind::String))
        .one(map(pattern_struct, PatternKind::Struct))
        .one(map(pattern_tuple, PatternKind::Tuple))
        .one(map(pattern_slice, PatternKind::Slice))
        .one(map(pattern_macro_call, PatternKind::MacroCall))
        .one(map(pattern_box, PatternKind::Box))
        // Must be last, otherwise it collides with struct names
        .one(map(pattern_ident, PatternKind::Ident))
        .finish()
}

fn pattern_ident<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternIdent> {
    sequence!(pm, pt, {
        spt    = point;
        is_ref = optional(ext(kw_ref));
        is_mut = optional(ext(kw_mut));
        ident  = pathed_ident;
        tuple  = optional(pattern_tuple);
    }, |pm: &mut Master, pt| PatternIdent { extent: pm.state.ex(spt, pt), is_ref, is_mut, ident, tuple })
}

fn pattern_tuple<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternTuple> {
    sequence!(pm, pt, {
        spt     = point;
        _       = left_paren;
        members = zero_or_more_tailed_values(comma, pattern_tuple_member);
        _       = right_paren;
    }, |pm: &mut Master, pt| PatternTuple { extent: pm.state.ex(spt, pt), members })
}

fn pattern_tuple_member<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, PatternTupleMember>
{
    pm.alternate(pt)
        .one(map(pattern, PatternTupleMember::Pattern))
        .one(map(ext(double_period), PatternTupleMember::Wildcard))
        .finish()
}

fn pattern_slice<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternSlice> {
    sequence!(pm, pt, {
        spt     = point;
        _       = left_square;
        members = zero_or_more_tailed_values(comma, pattern_slice_member);
        _       = right_square;
    }, |pm: &mut Master, pt| PatternSlice { extent: pm.state.ex(spt, pt), members })
}

fn pattern_slice_member<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, PatternSliceMember>
{
    pm.alternate(pt)
        .one(map(pattern_slice_subslice, PatternSliceMember::Subslice))
        .one(map(pattern, PatternSliceMember::Pattern))
        .one(map(ext(double_period), PatternSliceMember::Wildcard))
        .finish()
}

fn pattern_slice_subslice<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternSliceSubslice> {
    sequence!(pm, pt, {
        spt    = point;
        is_ref = optional(kw_ref);
        is_mut = optional(kw_mut);
        name   = ident;
        _      = double_period;
    }, |pm: &mut Master, pt| PatternSliceSubslice {
        extent: pm.state.ex(spt, pt),
        is_ref,
        is_mut,
        name,
    })
}

fn pattern_struct<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternStruct> {
    sequence!(pm, pt, {
        spt      = point;
        name     = pathed_ident;
        _        = left_curly;
        fields   = zero_or_more_tailed_values(comma, pattern_struct_field);
        wildcard = optional(double_period);
        _        = right_curly;
    }, |pm: &mut Master, pt| PatternStruct {
        extent: pm.state.ex(spt, pt),
        name,
        fields,
        wildcard: wildcard.is_some(),
        whitespace: Vec::new(),
    })
}

fn pattern_struct_field<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternStructField> {
    pm.alternate(pt)
        .one(map(pattern_struct_field_long, PatternStructField::Long))
        .one(map(map(pattern_ident, |ident| {
            PatternStructFieldShort { ident }
        }), PatternStructField::Short))
        .finish()
}

fn pattern_struct_field_long<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, PatternStructFieldLong>
{
    sequence!(pm, pt, {
        spt     = point;
        name    = ident;
        _       = colon;
        pattern = pattern;
    }, |pm: &mut Master, pt| PatternStructFieldLong { extent: pm.state.ex(spt, pt), name, pattern, whitespace: Vec::new() })
}

fn pattern_byte<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternByte> {
    expr_byte(pm, pt).map(|value| PatternByte { extent: value.extent, value })
}

fn pattern_char<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternCharacter> {
    character_literal(pm, pt).map(|value| PatternCharacter { extent: value.extent, value })
}

fn pattern_byte_string<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternByteString> {
    expr_byte_string(pm, pt).map(|value| PatternByteString { extent: value.extent, value })
}

fn pattern_string<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternString> {
    string_literal(pm, pt).map(|value| PatternString { extent: value.extent, value })
}

fn pattern_number<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternNumber> {
    sequence!(pm, pt, {
        spt         = point;
        is_negative = optional(minus);
        value       = number_literal;
    }, |pm: &mut Master, pt| PatternNumber { extent: pm.state.ex(spt, pt), is_negative, value })
}

fn pattern_reference<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternReference> {
    sequence!(pm, pt, {
        spt     = point;
        _       = ampersand;
        is_mut  = optional(ext(kw_mut));
        pattern = pattern;
    }, |pm: &mut Master, pt| PatternReference {
        extent: pm.state.ex(spt, pt),
        is_mut,
        pattern: Box::new(pattern),
        whitespace: Vec::new()
    })
}

fn pattern_box<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternBox> {
    sequence!(pm, pt, {
        spt     = point;
        _       = kw_box;
        pattern = pattern;
    }, |pm: &mut Master, pt| PatternBox {
        extent: pm.state.ex(spt, pt),
        pattern: Box::new(pattern),
        whitespace: Vec::new(),
    })
}

fn pattern_range_exclusive<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, PatternRangeExclusive>
{
    sequence!(pm, pt, {
        spt   = point;
        start = pattern_range_component;
        _     = double_period;
        end   = pattern_range_component;
    }, |pm: &mut Master, pt| PatternRangeExclusive {
        extent: pm.state.ex(spt, pt),
        start,
        end,
        whitespace: Vec::new()
    })
}

fn range_inclusive_operator<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, RangeInclusiveOperator>
{
    pm.alternate(pt)
        .one(map(triple_period, RangeInclusiveOperator::Legacy))
        .one(map(double_period_equals, RangeInclusiveOperator::Recommended))
        .finish()
}

fn pattern_range_inclusive<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, PatternRangeInclusive>
{
    sequence!(pm, pt, {
        spt      = point;
        start    = pattern_range_component;
        operator = range_inclusive_operator;
        end      = pattern_range_component;
    }, |pm: &mut Master, pt| PatternRangeInclusive {
        extent: pm.state.ex(spt, pt),
        start,
        operator,
        end,
        whitespace: Vec::new()
    })
}

fn pattern_range_component<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternRangeComponent> {
    pm.alternate(pt)
        .one(map(pathed_ident, PatternRangeComponent::Ident))
        .one(map(character_literal, PatternRangeComponent::Character))
        .one(map(expr_byte, PatternRangeComponent::Byte))
        .one(map(pattern_number, PatternRangeComponent::Number))
        .finish()
}

fn pattern_macro_call<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternMacroCall> {
    expr_macro_call(pm, pt).map(|value| PatternMacroCall { extent: value.extent, value })
}

fn p_struct<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Struct> {
    sequence!(pm, pt, {
        spt            = point;
        visibility     = optional(visibility);
        _              = kw_struct;
        name           = ident;
        generics       = optional(generic_declarations);
        (body, wheres) = struct_defn_body;
    }, |pm: &mut Master, pt| Struct {
        extent: pm.state.ex(spt, pt),
        visibility,
        name,
        generics,
        wheres: wheres.unwrap_or_else(Vec::new),
        body,
        whitespace: Vec::new(),
    })
}

fn struct_defn_body<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, (StructDefinitionBody, Option<Vec<Where>>)>
{
    pm.alternate(pt)
        .one(map(struct_defn_body_brace, |(b, w)| (StructDefinitionBody::Brace(b), w)))
        .one(map(struct_defn_body_tuple, |(b, w)| (StructDefinitionBody::Tuple(b), w)))
        .one(map(ext(semicolon), |b| (StructDefinitionBody::Empty(b), None)))
        .finish()
}

fn struct_defn_body_brace<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, (StructDefinitionBodyBrace, Option<Vec<Where>>)>
{
    sequence!(pm, pt, {
        wheres = optional(where_clause);
        body   = struct_defn_body_brace_only;
    }, |_, _| (body, wheres))
}

fn struct_defn_body_brace_only<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, StructDefinitionBodyBrace>
{
    sequence!(pm, pt, {
        spt    = point;
        _      = left_curly;
        fields = zero_or_more_tailed_values(comma, attributed(struct_defn_field));
        _      = right_curly;
    }, |pm: &mut Master, pt| StructDefinitionBodyBrace { extent: pm.state.ex(spt, pt), fields, whitespace: Vec::new() })
}

fn struct_defn_body_tuple<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, (StructDefinitionBodyTuple, Option<Vec<Where>>)>
{
    sequence!(pm, pt, {
        spt    = point;
        fields = struct_defn_body_tuple_only;
        wheres = optional(where_clause);
        _      = semicolon;
    }, |pm: &mut Master, pt| (StructDefinitionBodyTuple { extent: pm.state.ex(spt, pt), fields, whitespace: Vec::new() }, wheres))
}

fn struct_defn_body_tuple_only<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<Attributed<StructDefinitionFieldUnnamed>>> {
    sequence!(pm, pt, {
        _     = left_paren;
        types = zero_or_more_tailed_values(comma, attributed(tuple_defn_field));
        _     = right_paren;
    }, |_, _| types)
}

fn tuple_defn_field<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, StructDefinitionFieldUnnamed> {
    sequence!(pm, pt, {
        spt        = point;
        visibility = optional(visibility);
        typ        = typ;
    }, |pm: &mut Master, pt| StructDefinitionFieldUnnamed {
        extent: pm.state.ex(spt, pt),
        visibility,
        typ,
    })
}

fn struct_defn_field<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, StructDefinitionFieldNamed> {
    sequence!(pm, pt, {
        spt        = point;
        visibility = optional(visibility);
        name       = ident;
        _          = colon;
        typ        = typ;
    }, |pm: &mut Master, pt| StructDefinitionFieldNamed {
        extent: pm.state.ex(spt, pt),
        visibility,
        name,
        typ,
        whitespace: Vec::new(),
    })
}

fn p_union<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Union> {
    sequence!(pm, pt, {
        spt        = point;
        visibility = optional(visibility);
        _          = kw_union;
        name       = ident;
        generics   = optional(generic_declarations);
        wheres     = optional(where_clause);
        _          = left_curly;
        fields     = zero_or_more_tailed_values(comma, attributed(struct_defn_field));
        _          = right_curly;
    }, |pm: &mut Master, pt| Union {
        extent: pm.state.ex(spt, pt),
        visibility,
        name,
        generics,
        wheres: wheres.unwrap_or_else(Vec::new),
        fields,
        whitespace: Vec::new(),
    })
}

fn p_enum<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Enum> {
    sequence!(pm, pt, {
        spt        = point;
        visibility = optional(visibility);
        _          = kw_enum;
        name       = ident;
        generics   = optional(generic_declarations);
        wheres     = optional(where_clause);
        _          = left_curly;
        variants   = zero_or_more_tailed_values(comma, attributed(enum_variant));
        _          = right_curly;
    }, |pm: &mut Master, pt| Enum {
        extent: pm.state.ex(spt, pt),
        visibility,
        name,
        generics,
        wheres: wheres.unwrap_or_else(Vec::new),
        variants,
        whitespace: Vec::new(),
    })
}

fn enum_variant<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, EnumVariant> {
    sequence!(pm, pt, {
        spt  = point;
        name = ident;
        body = enum_variant_body;
    }, |pm: &mut Master, pt| EnumVariant {
        extent: pm.state.ex(spt, pt),
        name,
        body,
        whitespace: Vec::new(),
    })
}

fn enum_variant_body<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, EnumVariantBody> {
    pm.alternate(pt)
        .one(map(struct_defn_body_tuple_only, EnumVariantBody::Tuple))
        .one(map(struct_defn_body_brace_only, EnumVariantBody::Struct))
        .one(map(optional(enum_discriminant), EnumVariantBody::Unit))
        .finish()
}

fn enum_discriminant<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Attributed<Expression>> {
    sequence!(pm, pt, {
        _     = equals;
        value = expression;
    }, |_, _| value)
}

fn p_trait<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Trait> {
    sequence!(pm, pt, {
        spt        = point;
        visibility = optional(visibility);
        is_unsafe  = optional(kw_unsafe);
        is_auto    = optional(kw_auto);
        _          = kw_trait;
        name       = ident;
        generics   = optional(generic_declarations);
        bounds     = optional(generic_declaration_type_bounds);
        wheres     = optional(where_clause);
        _          = left_curly;
        members    = zero_or_more(attributed(trait_impl_member));
        _          = right_curly;
    }, |pm: &mut Master, pt| Trait {
        extent: pm.state.ex(spt, pt),
        visibility,
        is_unsafe,
        is_auto,
        name,
        generics,
        bounds,
        wheres: wheres.unwrap_or_else(Vec::new),
        members,
        whitespace: Vec::new(),
    })
}

// TOOD: this is a terrrrrrible name. It is not an impl!
fn trait_impl_member<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TraitMember> {
    pm.alternate(pt)
        .one(map(trait_member_function, TraitMember::Function))
        .one(map(trait_member_type, TraitMember::Type))
        .one(map(trait_member_const, TraitMember::Const))
        .one(map(item_macro_call, TraitMember::MacroCall))
        .finish()
}

fn trait_member_function<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TraitMemberFunction> {
    sequence!(pm, pt, {
        spt    = point;
        header = trait_impl_function_header;
        body   = trait_impl_function_body;
    }, |pm: &mut Master, pt| TraitMemberFunction { extent: pm.state.ex(spt, pt), header, body })
}

fn trait_member_type<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TraitMemberType> {
    sequence!(pm, pt, {
        spt     = point;
        _       = kw_type;
        name    = ident;
        bounds  = optional(generic_declaration_type_bounds);
        default = optional(generic_declaration_type_default);
        _       = semicolon;
    }, |pm: &mut Master, pt| TraitMemberType {
        extent: pm.state.ex(spt, pt),
        name,
        bounds,
        default,
        whitespace: Vec::new(),
    })
}

fn trait_member_const<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TraitMemberConst> {
    sequence!(pm, pt, {
        spt   = point;
        _     = kw_const;
        name  = ident;
        _     = colon;
        typ   = typ;
        value = optional(trait_member_const_value);
        _     = semicolon;
    }, |pm: &mut Master, pt| TraitMemberConst { extent: pm.state.ex(spt, pt), name, typ, value, whitespace: Vec::new() })
}

fn trait_member_const_value<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Attributed<Expression>> {
    sequence!(pm, pt, {
        _     = equals;
        value = expression;
    }, |_, _| value)
}

fn visibility<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Visibility> {
    sequence!(pm, pt, {
        spt       = point;
        _         = kw_pub;
        qualifier = optional(visibility_qualifier);
    }, |pm: &mut Master, pt| Visibility { extent: pm.state.ex(spt, pt), qualifier, whitespace: Vec::new() })
}

fn visibility_qualifier<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, VisibilityQualifier>
{
    sequence!(pm, pt, {
        _         = left_paren;
        qualifier = visibility_qualifier_kind;
        _         = right_paren;
    }, |_, _| qualifier)
}

fn visibility_qualifier_kind<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, VisibilityQualifier>
{
    pm.alternate(pt)
        .one(map(kw_self_ident, |_| VisibilityQualifier::SelfIdent))
        .one(map(kw_crate, |_| VisibilityQualifier::Crate))
        .one(map(path, VisibilityQualifier::Path))
        .finish()
}

// TODO: Massively duplicated!!!
fn trait_impl_function_header<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TraitImplFunctionHeader> {
    sequence!(pm, pt, {
        spt         = point;
        visibility  = optional(visibility);
        qualifiers  = function_qualifiers; // TODO: shouldn't allow const / default
        _           = kw_fn;
        name        = ident;
        generics    = optional(generic_declarations);
        arguments   = trait_impl_function_arglist;
        return_type = optional(function_return_type);
        wheres      = optional(where_clause);
    }, |pm: &mut Master, pt| {
        TraitImplFunctionHeader {
            extent: pm.state.ex(spt, pt),
            visibility,
            qualifiers,
            name,
            generics,
            arguments,
            return_type,
            wheres: wheres.unwrap_or_else(Vec::new),
            whitespace: Vec::new(),
        }})
}

fn trait_impl_function_arglist<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<TraitImplArgument>> {
    sequence!(pm, pt, {
        _        = left_paren;
        self_arg = optional(map(self_argument, TraitImplArgument::SelfArgument));
        args     = zero_or_more_tailed_values_append(self_arg, comma, trait_impl_function_argument);
        _        = right_paren;
    }, move |_, _| args)
}

fn trait_impl_function_argument<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TraitImplArgument> {
    sequence!(pm, pt, {
        name = optional(trait_impl_function_argument_name);
        typ  = typ;
    }, |_, _| TraitImplArgument::Named(TraitImplArgumentNamed { name, typ, whitespace: Vec::new() }))
}

fn trait_impl_function_argument_name<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, Pattern>
{
    sequence!(pm, pt, {
        name = pattern;
        _    = colon;
    }, |_, _| name)
}

fn trait_impl_function_body<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Option<Block>> {
    pm.alternate(pt)
        .one(map(block, Some))
        .one(map(semicolon, |_| None))
        .finish()
}

fn p_impl<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Impl> {
    sequence!(pm, pt, {
        spt       = point;
        is_unsafe = optional(ext(kw_unsafe));
        _         = kw_impl;
        generics  = optional(generic_declarations);
        kind      = p_impl_kind;
        wheres    = optional(where_clause);
        _         = left_curly;
        body      = zero_or_more(attributed(impl_member));
        _         = right_curly;
    }, |pm: &mut Master, pt| Impl {
        extent: pm.state.ex(spt, pt),
        is_unsafe,
        generics,
        kind,
        wheres: wheres.unwrap_or_else(Vec::new),
        body,
        whitespace: Vec::new(),
    })
}

fn p_impl_kind<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ImplKind> {
    pm.alternate(pt)
        .one(map(p_impl_of_trait, ImplKind::Trait))
        .one(map(p_impl_of_inherent, ImplKind::Inherent))
        .finish()
}

fn p_impl_of_inherent<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ImplOfInherent> {
    sequence!(pm, pt, {
        spt       = point;
        type_name = typ;
    }, |pm: &mut Master, pt| ImplOfInherent {
        extent: pm.state.ex(spt, pt),
        type_name,
        whitespace: Vec::new(),
    })
}

fn p_impl_of_trait<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ImplOfTrait> {
    sequence!(pm, pt, {
        spt         = point;
        is_negative = optional(ext(bang));
        trait_name  = typ;
        _           = kw_for;
        type_name   = type_or_wildcard;
    }, |pm: &mut Master, pt| ImplOfTrait {
        extent: pm.state.ex(spt, pt),
        is_negative,
        trait_name,
        type_name,
        whitespace: Vec::new(),
    })
}

fn type_or_wildcard<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ImplOfTraitType> {
    pm.alternate(pt)
        .one(map(typ, ImplOfTraitType::Type))
        .one(map(ext(double_period), ImplOfTraitType::Wildcard))
        .finish()
}

fn impl_member<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ImplMember> {
    pm.alternate(pt)
        .one(map(impl_const, ImplMember::Const))
        .one(map(impl_function, ImplMember::Function))
        .one(map(impl_type, ImplMember::Type))
        .one(map(item_macro_call, ImplMember::MacroCall))
        .finish()
}

fn impl_function<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ImplFunction> {
    sequence!(pm, pt, {
        spt    = point;
        header = function_header;
        body   = block;
    }, |pm: &mut Master, pt| ImplFunction { extent: pm.state.ex(spt, pt), header, body })
}

fn impl_type<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ImplType> {
    sequence!(pm, pt, {
        spt  = point;
        _    = kw_type;
        name = ident;
        _    = equals;
        typ  = typ;
        _    = semicolon;
    }, |pm: &mut Master, pt| ImplType { extent: pm.state.ex(spt, pt), name, typ, whitespace: Vec::new() })
}

fn impl_const<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ImplConst> {
    sequence!(pm, pt, {
        spt        = point;
        visibility = optional(visibility);
        _          = kw_const;
        name       = ident;
        _          = colon;
        typ        = typ;
        _          = equals;
        value      = expression;
        _          = semicolon;
    }, |pm: &mut Master, pt| ImplConst {
        extent: pm.state.ex(spt, pt),
        visibility, name,
        typ,
        value,
        whitespace: Vec::new(),
    })
}

// TODO: optional could take E that is `into`, or just a different one

fn p_const<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Const> {
    sequence!(pm, pt, {
        spt        = point;
        visibility = optional(visibility);
        _          = kw_const;
        name       = ident;
        _          = colon;
        typ        = typ;
        _          = equals;
        value      = expression;
        _          = semicolon;
    }, |pm: &mut Master, pt| Const { extent: pm.state.ex(spt, pt), visibility, name, typ, value, whitespace: Vec::new() })
}

fn p_static<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Static> {
    sequence!(pm, pt, {
        spt        = point;
        visibility = optional(visibility);
        _          = kw_static;
        is_mut     = optional(ext(kw_mut));
        name       = ident;
        _          = colon;
        typ        = typ;
        _          = equals;
        value      = expression;
        _          = semicolon;
    }, |pm: &mut Master, pt| Static { extent: pm.state.ex(spt, pt), visibility, is_mut, name, typ, value, whitespace: Vec::new() })
}

fn extern_crate<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Crate> {
    sequence!(pm, pt, {
        spt        = point;
        visibility = optional(visibility);
        _          = kw_extern;
        _          = kw_crate;
        name       = ident;
        rename     = optional(extern_crate_rename);
        _          = semicolon;
    }, |pm: &mut Master, pt| Crate { extent: pm.state.ex(spt, pt), visibility, name, rename, whitespace: Vec::new() })
}

fn extern_crate_rename<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Ident> {
    sequence!(pm, pt, {
        _    = kw_as;
        name = ident;
    }, |_, _| name)
}

fn extern_block<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExternBlock> {
    sequence!(pm, pt, {
        spt     = point;
        _       = kw_extern;
        abi     = optional(string_literal);
        _       = left_curly;
        members = zero_or_more(attributed(extern_block_member));
        _       = right_curly;
    }, |pm: &mut Master, pt| ExternBlock { extent: pm.state.ex(spt, pt), abi, members, whitespace: Vec::new() })
}

fn extern_block_member<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExternBlockMember> {
    pm.alternate(pt)
        .one(map(extern_block_static, ExternBlockMember::Static))
        .one(map(extern_block_type, ExternBlockMember::Type))
        .one(map(extern_block_member_function, ExternBlockMember::Function))
        .finish()
}

// TODO: very similar to regular statics; DRY
fn extern_block_static<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExternBlockMemberStatic> {
    sequence!(pm, pt, {
        spt        = point;
        visibility = optional(visibility);
        _          = kw_static;
        is_mut     = optional(ext(kw_mut));
        name       = ident;
        _          = colon;
        typ        = typ;
        _          = semicolon;
    }, |pm: &mut Master, pt| ExternBlockMemberStatic { extent: pm.state.ex(spt, pt), visibility, is_mut, name, typ, whitespace: Vec::new() })
}

fn extern_block_type<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExternBlockMemberType> {
    sequence!(pm, pt, {
        spt        = point;
        visibility = optional(visibility);
        _          = kw_type;
        name       = ident;
        _          = semicolon;
    }, |pm: &mut Master, pt| ExternBlockMemberType {
        extent: pm.state.ex(spt, pt),
        visibility,
        name,
    })
}

// TODO: Massively duplicated!!!
fn extern_block_member_function<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, ExternBlockMemberFunction>
{
    sequence!(pm, pt, {
        spt         = point;
        visibility  = optional(visibility);
        _           = kw_fn;
        name        = ident;
        generics    = optional(generic_declarations);
        arguments   = extern_block_function_arglist;
        return_type = optional(function_return_type);
        wheres      = optional(where_clause);
        _           = semicolon;
    }, |pm: &mut Master, pt| {
        ExternBlockMemberFunction {
            extent: pm.state.ex(spt, pt),
            visibility,
            name,
            generics,
            arguments,
            return_type,
            wheres: wheres.unwrap_or_else(Vec::new),
            whitespace: Vec::new(),
        }
    })
}

fn extern_block_function_arglist<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, Vec<ExternBlockMemberFunctionArgument>>
{
    sequence!(pm, pt, {
        _    = left_paren;
        args = zero_or_more_tailed_values(comma, extern_block_function_argument);
        _    = right_paren;
    }, move |_, _| args)
}

fn extern_block_function_argument<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, ExternBlockMemberFunctionArgument>
{
    pm.alternate(pt)
        .one(map(extern_block_function_argument_named, ExternBlockMemberFunctionArgument::Named))
        .one(map(extern_block_function_argument_variadic, ExternBlockMemberFunctionArgument::Variadic))
        .finish()
}

fn extern_block_function_argument_named<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, ExternBlockMemberFunctionArgumentNamed>
{
    sequence!(pm, pt, {
        spt  = point;
        name = pattern;
        _    = colon;
        typ  = typ;
    }, |pm: &mut Master, pt| ExternBlockMemberFunctionArgumentNamed {
        extent: pm.state.ex(spt, pt),
        name,
        typ,
        whitespace: Vec::new(),
    })
}

fn extern_block_function_argument_variadic<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, ExternBlockMemberFunctionArgumentVariadic>
{
    sequence!(pm, pt, {
        spt = point;
        _   = triple_period;
    }, |pm: &mut Master, pt| ExternBlockMemberFunctionArgumentVariadic { extent: pm.state.ex(spt, pt) })
}

fn p_use<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Use> {
    sequence!(pm, pt, {
        spt        = point;
        visibility = optional(visibility);
        _          = kw_use;
        _          = optional(double_colon);
        path       = use_path;
        _          = semicolon;
    }, move |pm: &mut Master, pt| {
        Use { extent: pm.state.ex(spt, pt), visibility, path, whitespace: Vec::new() }
    })
}

fn use_path<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, UsePath> {
    sequence!(pm, pt, {
        spt  = point;
        path = zero_or_more(use_path_component);
        tail = use_tail;
    }, move |pm: &mut Master, pt| {
        UsePath { extent: pm.state.ex(spt, pt), path, tail }
    })
}

fn use_path_component<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Ident> {
    sequence!(pm, pt, {
        name = ident;
        _    = double_colon;
    }, |_, _| name)
}

fn use_tail<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, UseTail> {
    pm.alternate(pt)
        .one(map(use_tail_ident, UseTail::Ident))
        .one(map(use_tail_glob, UseTail::Glob))
        .one(map(use_tail_multi, UseTail::Multi))
        .finish()
}

fn use_tail_ident<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, UseTailIdent> {
    sequence!(pm, pt, {
        spt    = point;
        name   = ident;
        rename = optional(use_tail_ident_rename);
    }, |pm: &mut Master, pt| UseTailIdent { extent: pm.state.ex(spt, pt), name, rename })
}

fn use_tail_ident_rename<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Ident> {
    sequence!(pm, pt, {
        _    = kw_as;
        name = ident;
    }, |_, _| name)
}

fn use_tail_glob<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, UseTailGlob> {
    sequence!(pm, pt, {
        spt = point;
        _   = asterisk;
    }, |pm: &mut Master, pt| UseTailGlob { extent: pm.state.ex(spt, pt) })
}

fn use_tail_multi<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, UseTailMulti> {
    sequence!(pm, pt, {
        spt   = point;
        _     = left_curly;
        paths = zero_or_more_tailed_values(comma, use_path);
        _     = right_curly;
    }, |pm: &mut Master, pt| UseTailMulti { extent: pm.state.ex(spt, pt), paths })
}

fn type_alias<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeAlias> {
    sequence!(pm, pt, {
        spt        = point;
        visibility = optional(visibility);
        _          = kw_type;
        name       = ident;
        generics   = optional(generic_declarations);
        wheres     = optional(where_clause);
        _          = equals;
        defn       = typ;
        _          = semicolon;
    }, |pm: &mut Master, pt| TypeAlias {
        extent: pm.state.ex(spt, pt),
        visibility,
        name,
        generics,
        wheres: wheres.unwrap_or_else(Vec::new),
        defn,
        whitespace: Vec::new(),
    })
}

fn module<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Module> {
    sequence!(pm, pt, {
        spt        = point;
        visibility = optional(visibility);
        _          = kw_mod;
        name       = ident;
        body       = module_body_or_not;
    }, |pm: &mut Master, pt| Module { extent: pm.state.ex(spt, pt), visibility, name, body, whitespace: Vec::new() })
}

fn module_body_or_not<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Option<Vec<Attributed<Item>>>> {
    pm.alternate(pt)
        .one(map(module_body, Some))
        .one(map(semicolon, |_| None))
        .finish()
}

fn module_body<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<Attributed<Item>>> {
    sequence!(pm, pt, {
        _    = left_curly;
        body = zero_or_more(attributed(item));
        _    = right_curly;
    }, |_, _| body)
}

fn typ<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Type> {
    sequence!(pm, pt, {
        spt        = point;
        kind       = typ_kind;
        additional = zero_or_more_tailed_values_resume(plus, typ_additional);
    }, |pm: &mut Master, pt| Type { extent: pm.state.ex(spt, pt), kind, additional })
}

fn typ_single<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Type> {
    sequence!(pm, pt, {
        spt  = point;
        kind = typ_kind;
    }, |pm: &mut Master, pt| Type { extent: pm.state.ex(spt, pt), kind, additional: vec![] })
}

fn typ_kind<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeKind> {
    pm.alternate(pt)
        .one(map(typ_array, TypeKind::Array))
        .one(map(typ_disambiguation, TypeKind::Disambiguation))
        .one(map(typ_function, TypeKind::Function))
        .one(map(typ_higher_ranked_trait_bounds, TypeKind::HigherRankedTraitBounds))
        .one(map(typ_impl_trait, TypeKind::ImplTrait))
        .one(map(typ_named, TypeKind::Named))
        .one(map(typ_pointer, TypeKind::Pointer))
        .one(map(typ_reference, TypeKind::Reference))
        .one(map(typ_slice, TypeKind::Slice))
        .one(map(typ_tuple, TypeKind::Tuple))
        .one(map(ext(bang), TypeKind::Uninhabited))
        .finish()
}

fn typ_reference<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeReference> {
    sequence!(pm, pt, {
        spt  = point;
        kind = typ_reference_kind;
        typ  = typ;
    }, |pm: &mut Master, pt| TypeReference { extent: pm.state.ex(spt, pt), kind, typ: Box::new(typ) })
}

fn typ_reference_kind<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeReferenceKind> {
    sequence!(pm, pt, {
        spt      = point;
        _        = ampersand;
        lifetime = optional(lifetime);
        mutable  = optional(ext(kw_mut));
    }, |pm: &mut Master, pt| TypeReferenceKind { extent: pm.state.ex(spt, pt), lifetime, mutable, whitespace: Vec::new() })
}

fn typ_pointer<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypePointer> {
    sequence!(pm, pt, {
        spt  = point;
        _    = asterisk;
        kind = typ_pointer_kind;
        typ  = typ;
    }, |pm: &mut Master, pt| TypePointer { extent: pm.state.ex(spt, pt), kind, typ: Box::new(typ), whitespace: Vec::new() })
}

fn typ_pointer_kind<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypePointerKind> {
    pm.alternate(pt)
        .one(map(kw_const, |_| TypePointerKind::Const))
        .one(map(kw_mut, |_| TypePointerKind::Mutable))
        .finish()
}

fn typ_tuple<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeTuple> {
    sequence!(pm, pt, {
        spt   = point;
        _     = left_paren;
        types = zero_or_more_tailed_values(comma, typ);
        _     = right_paren;
    }, |pm: &mut Master, pt| TypeTuple { extent: pm.state.ex(spt, pt), types })
}

fn typ_higher_ranked_trait_bounds<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, TypeHigherRankedTraitBounds>
{
    sequence!(pm, pt, {
        spt       = point;
        lifetimes = higher_ranked_trait_bounds;
        child     = typ_higher_ranked_trait_bounds_child;
    }, |pm: &mut Master, pt| TypeHigherRankedTraitBounds { extent: pm.state.ex(spt, pt), lifetimes, child, whitespace: Vec::new() })
}

fn higher_ranked_trait_bounds<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, Vec<Lifetime>>
{
    sequence!(pm, pt, {
        _         = kw_for;
        _         = left_angle;
        lifetimes = zero_or_more_tailed_values(comma, lifetime);
        _         = right_angle;
    }, |_, _| lifetimes)
}

fn typ_higher_ranked_trait_bounds_child<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, TypeHigherRankedTraitBoundsChild>
{
    pm.alternate(pt)
        .one(map(typ_named, TypeHigherRankedTraitBoundsChild::Named))
        .one(map(typ_function, TypeHigherRankedTraitBoundsChild::Function))
        .one(map(typ_reference, TypeHigherRankedTraitBoundsChild::Reference))
        .finish()
}

fn typ_impl_trait<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeImplTrait> {
    sequence!(pm, pt, {
        spt  = point;
        _    = kw_impl;
        name = typ_named;
    }, |pm: &mut Master, pt| TypeImplTrait { extent: pm.state.ex(spt, pt), name, whitespace: Vec::new() })
}

fn typ_additional<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, TypeAdditional>
{
    pm.alternate(pt)
        .one(map(typ_named, TypeAdditional::Named))
        .one(map(lifetime, TypeAdditional::Lifetime))
        .finish()
}

fn typ_named<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeNamed> {
    sequence!(pm, pt, {
        spt  = point;
        _    = optional(double_colon);
        path = one_or_more_tailed_values(double_colon, typ_named_component);
    }, |pm: &mut Master, pt| TypeNamed { extent: pm.state.ex(spt, pt), path })
}

fn typ_named_component<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeNamedComponent> {
    sequence!(pm, pt, {
        spt      = point;
        ident    = ident;
        generics = optional(typ_generics);
    }, |pm: &mut Master, pt| TypeNamedComponent { extent: pm.state.ex(spt, pt), ident, generics })
}

fn typ_disambiguation<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeDisambiguation> {
    sequence!(pm, pt, {
        spt  = point;
        core = disambiguation_core;
        path = zero_or_more_tailed_values_resume(double_colon, typ_named_component);
    }, move |pm: &mut Master, pt| TypeDisambiguation {
        extent: pm.state.ex(spt, pt),
        from_type: Box::new(core.from_type),
        to_type: core.to_type.map(Box::new),
        path,
        whitespace: core.whitespace,
    })
}

struct DisambiguationCore {
    from_type: Type,
    to_type: Option<TypeNamed>,
    whitespace: Vec<Whitespace>,
}

fn disambiguation_core<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, DisambiguationCore> {
    sequence!(pm, pt, {
        _         = left_angle;
        from_type = typ;
        to_type   = optional(disambiguation_core_to_type);
        _         = right_angle;
    }, |_, _| DisambiguationCore { from_type, to_type, whitespace: Vec::new() })
}

fn disambiguation_core_to_type<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, TypeNamed>
{
    sequence!(pm, pt, {
        _       = kw_as;
        to_type = typ_named;
    }, |_, _| to_type)
}

fn typ_array<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeArray> {
    sequence!(pm, pt, {
        spt   = point;
        _     = left_square;
        typ   = typ;
        _     = semicolon;
        count = expression;
        _     = right_square;
    }, |pm: &mut Master, pt| TypeArray {
        extent: pm.state.ex(spt, pt),
        typ: Box::new(typ),
        count: Box::new(count),
        whitespace: Vec::new(),
    })
}

fn typ_slice<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeSlice> {
    sequence!(pm, pt, {
        spt = point;
        _   = left_square;
        typ = typ;
        _   = right_square;
    }, |pm: &mut Master, pt| TypeSlice { extent: pm.state.ex(spt, pt), typ: Box::new(typ), whitespace: Vec::new() })
}

fn typ_generics<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeGenerics> {
    pm.alternate(pt)
        .one(map(typ_generics_fn, TypeGenerics::Function))
        .one(map(typ_generics_angle, TypeGenerics::Angle))
        .finish()
}

fn typ_generics_fn<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeGenericsFunction> {
    sequence!(pm, pt, {
        spt         = point;
        _           = left_paren;
        types       = zero_or_more_tailed_values(comma, typ);
        _           = right_paren;
        return_type = optional(function_return_type);
    }, |pm: &mut Master, pt| TypeGenericsFunction {
        extent: pm.state.ex(spt, pt),
        types,
        return_type: return_type.map(Box::new),
        whitespace: Vec::new(),
    })
}

fn typ_generics_angle<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeGenericsAngle> {
    sequence!(pm, pt, {
        spt     = point;
        _       = left_angle;
        members = zero_or_more_tailed_values(comma, typ_generics_angle_member);
        _       = right_angle;
    }, |pm: &mut Master, pt| TypeGenericsAngle { extent: pm.state.ex(spt, pt), members, whitespace: Vec::new() })
}

// Parsing all of these equally is a bit inconsistent with the
// compler. The compiler *parses* lifetimes after types, but later
// errors about it. It does *not* parse associated types before types
// though.
fn typ_generics_angle_member<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, TypeGenericsAngleMember>
{
    pm.alternate(pt)
        .one(map(associated_type, TypeGenericsAngleMember::AssociatedType))
        .one(map(lifetime, TypeGenericsAngleMember::Lifetime))
        .one(map(typ, TypeGenericsAngleMember::Type))
        .finish()
}

fn associated_type<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, AssociatedType> {
    sequence!(pm, pt, {
        spt   = point;
        name  = ident;
        _     = equals;
        value = typ;
    }, |pm: &mut Master, pt| AssociatedType { extent: pm.state.ex(spt, pt), name, value, whitespace: Vec::new() })
}

fn typ_function<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeFunction> {
    sequence!(pm, pt, {
        spt         = point;
        qualifiers  = function_qualifiers; // TODO: shouldn't allow const / default
        _           = kw_fn;
        _           = left_paren;
        arguments   = zero_or_more_tailed_values(comma, typ_function_argument);
        arguments   = zero_or_more_tailed_values_append(arguments, comma, typ_function_argument_variadic);
        _           = right_paren;
        return_type = optional(function_return_type);
    }, |pm: &mut Master, pt| TypeFunction {
        extent: pm.state.ex(spt, pt),
        qualifiers,
        arguments,
        return_type: return_type.map(Box::new),
        whitespace: Vec::new(),
    })
}

fn typ_function_argument<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, TypeFunctionArgument>
{
    sequence!(pm, pt, {
        spt  = point;
        name = optional(typ_function_argument_name);
        typ  = typ;
    }, |pm: &mut Master, pt| TypeFunctionArgument::Named(TypeFunctionArgumentNamed {
        extent: pm.state.ex(spt, pt),
        name,
        typ,
    }))
}

fn typ_function_argument_name<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, Ident>
{
    sequence!(pm, pt, {
        name = ident;
        _    = colon;
    }, |_, _| name)
}

fn typ_function_argument_variadic<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, TypeFunctionArgument>
{
    map(triple_period, TypeFunctionArgument::Variadic)(pm, pt)
}

fn lifetime<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Lifetime> {
    lifetime_normal(pm, pt)
        .map(|extent| Lifetime { extent: extent, name: Ident { extent } })
    // FIXME: value; can we actually have whitespace here?
}

pub(crate) fn attributed<'s, F, T>(f: F) ->
    impl Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, Attributed<T>>
where
    F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    move |pm, pt| {
        sequence!(pm, pt, {
            spt        = point;
            attributes = zero_or_more(attribute);
            value      = f;
        }, |pm: &mut Master<'s>, pt| Attributed {
            extent: pm.state.ex(spt, pt),
            attributes,
            value,
        })
    }
}

fn attribute<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Attribute> {
    pm.alternate(pt)
        .one(map(doc_comment_outer_line, Attribute::DocCommentLine))
        .one(map(doc_comment_outer_block, Attribute::DocCommentBlock))
        .one(map(attribute_literal, Attribute::Literal))
        .finish()
}

fn attribute_literal<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, AttributeLiteral> {
    sequence!(pm, pt, {
        spt  = point;
        _    = hash;
        _    = left_square;
        text = parse_nested_until(Token::is_left_square, Token::is_right_square);
        _    = right_square;
    }, |pm: &mut Master, pt| AttributeLiteral { extent: pm.state.ex(spt, pt), text })
}

fn attribute_containing<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, AttributeContaining> {
    pm.alternate(pt)
        .one(map(doc_comment_inner_line, AttributeContaining::DocCommentLine))
        .one(map(doc_comment_inner_block, AttributeContaining::DocCommentBlock))
        .one(map(attribute_containing_literal, AttributeContaining::Literal))
        .finish()
}

fn attribute_containing_literal<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, AttributeContainingLiteral> {
    sequence!(pm, pt, {
        spt  = point;
        _    = hash;
        _    = bang;
        _    = left_square;
        text = parse_nested_until(Token::is_left_square, Token::is_right_square);
        _    = right_square;
    }, |pm: &mut Master, pt| AttributeContainingLiteral { extent: pm.state.ex(spt, pt), text })
}

#[cfg(test)]
mod test {
    use super::*;
    use super::test_utils::*;

    #[test]
    fn parse_use() {
        let p = qp(p_use, "use foo::Bar;");
        assert_extent!(p, (0, 13))
    }

    #[test]
    fn parse_use_public() {
        let p = qp(p_use, "pub use foo::Bar;");
        assert_extent!(p, (0, 17))
    }

    #[test]
    fn parse_use_glob() {
        let p = qp(p_use, "use foo::*;");
        assert_extent!(p, (0, 11))
    }

    #[test]
    fn parse_use_with_multi() {
        let p = qp(p_use, "use foo::{Bar, Baz};");
        assert_extent!(p, (0, 20))
    }

    #[test]
    fn parse_use_no_path() {
        let p = qp(p_use, "use {Bar, Baz};");
        assert_extent!(p, (0, 15))
    }

    #[test]
    fn parse_use_absolute_path() {
        let p = qp(p_use, "use ::{Bar, Baz};");
        assert_extent!(p, (0, 17))
    }

    #[test]
    fn parse_use_rename() {
        let p = qp(p_use, "use foo as bar;");
        assert_extent!(p, (0, 15))
    }

    #[test]
    fn parse_use_with_multi_rename() {
        let p = qp(p_use, "use foo::{bar as a, baz as b};");
        assert_extent!(p, (0, 30))
    }

    #[test]
    fn parse_use_with_nested() {
        let p = qp(p_use, "use foo::{self, inner::{self, Type}};");
        assert_extent!(p, (0, 37))
    }

    #[test]
    fn parse_use_all_space() {
        let p = qp(p_use, "use foo :: { bar as a , baz as b } ;");
        assert_extent!(p, (0, 36))
    }

    #[test]
    fn item_mod_multiple() {
        let p = qp(item, "mod foo { use super::*; }");
        assert_extent!(p, (0, 25))
    }

    #[test]
    fn item_macro_call_with_parens() {
        let p = qp(item, "foo!();");
        assert_extent!(p, (0, 7))
    }

    #[test]
    fn item_macro_call_with_square_brackets() {
        let p = qp(item, "foo![];");
        assert_extent!(p, (0, 7))
    }

    #[test]
    fn item_macro_call_with_curly_braces() {
        let p = qp(item, "foo! { }");
        assert_extent!(p, (0, 8))
    }

    #[test]
    fn item_macro_call_with_ident() {
        let p = qp(item, "macro_rules! name { }");
        assert_extent!(p, (0, 21))
    }

    #[test]
    fn item_macro_call_all_space() {
        let p = qp(item, "foo ! bar [ ] ;");
        assert_extent!(p, (0, 15))
    }

    #[test]
    fn item_mod() {
        let p = qp(module, "mod foo { }");
        assert_extent!(p, (0, 11))
    }

    #[test]
    fn item_mod_public() {
        let p = qp(module, "pub mod foo;");
        assert_extent!(p, (0, 12))
    }

    #[test]
    fn item_mod_another_file() {
        let p = qp(module, "mod foo;");
        assert_extent!(p, (0, 8))
    }

    #[test]
    fn item_trait() {
        let p = qp(item, "trait Foo {}");
        assert_extent!(p, (0, 12))
    }

    #[test]
    fn item_auto_trait() {
        let p = qp(item, "auto trait Foo {}");
        assert_extent!(p, (0, 17))
    }

    #[test]
    fn item_trait_public() {
        let p = qp(item, "pub trait Foo {}");
        assert_extent!(p, (0, 16))
    }

    #[test]
    fn item_trait_unsafe() {
        let p = qp(item, "unsafe trait Foo {}");
        assert_extent!(p, (0, 19))
    }

    #[test]
    fn item_trait_with_generics() {
        let p = qp(item, "trait Foo<T> {}");
        assert_extent!(p, (0, 15))
    }

    #[test]
    fn item_trait_with_members() {
        let p = qp(item, "trait Foo { fn bar(&self) -> u8; }");
        assert_extent!(p, (0, 34))
    }

    #[test]
    fn item_trait_with_members_with_patterns() {
        let p = qp(item, "trait Foo { fn bar(&self, &a: &u8) -> u8; }");
        assert_extent!(p, (0, 43))
    }

    #[test]
    fn item_trait_with_members_with_body() {
        let p = qp(item, "trait Foo { fn bar(&self) -> u8 { 42 } }");
        assert_extent!(p, (0, 40))
    }

    #[test]
    fn item_trait_with_unnamed_parameters() {
        let p = qp(item, "trait Foo { fn bar(&self, u8); }");
        assert_extent!(p, (0, 32))
    }

    #[test]
    fn item_trait_with_qualified_function() {
        let p = qp(item, r#"trait Foo { const unsafe extern "C" fn bar(); }"#);
        assert_extent!(p, (0, 47))
    }

    #[test]
    fn item_trait_with_associated_type() {
        let p = qp(item, "trait Foo { type Bar; }");
        assert_extent!(p, (0, 23))
    }

    #[test]
    fn item_trait_with_associated_type_with_bounds() {
        let p = qp(item, "trait Foo { type Bar: Baz; }");
        assert_extent!(p, (0, 28))
    }

    #[test]
    fn item_trait_with_associated_type_with_default() {
        let p = qp(item, "trait Foo { type Bar = (); }");
        assert_extent!(p, (0, 28))
    }

    #[test]
    fn item_trait_with_associated_type_with_bounds_and_default() {
        let p = qp(item, "trait Foo { type Bar: Baz = (); }");
        assert_extent!(p, (0, 33))
    }

    #[test]
    fn item_trait_with_associated_const() {
        let p = qp(item, "trait Foo { const Bar: u8; }");
        assert_extent!(p, (0, 28))
    }

    #[test]
    fn item_trait_with_associated_const_with_default() {
        let p = qp(item, "trait Foo { const Bar: u8 = 42; }");
        assert_extent!(p, (0, 33))
    }

    #[test]
    fn item_trait_with_supertraits() {
        let p = qp(item, "trait Foo: Bar + Baz {}");
        assert_extent!(p, (0, 23))
    }

    #[test]
    fn item_trait_with_where_clause() {
        let p = qp(item, "trait Foo where A: B {}");
        assert_extent!(p, (0, 23))
    }

    #[test]
    fn item_trait_with_macro() {
        let p = qp(item, "trait Foo { bar!{} }");
        assert_extent!(p, (0, 20))
    }

    #[test]
    fn item_trait_all_space() {
        let p = qp(item, "trait Foo : Bar { type A : B ; fn a ( a : u8) -> u8 { a } }");
        assert_extent!(p, (0, 59))
    }

    #[test]
    fn item_type_alias() {
        let p = qp(item, "type Foo<T> = Bar<T, u8>;");
        assert_extent!(p, (0, 25))
    }

    #[test]
    fn item_type_alias_public() {
        let p = qp(item, "pub type Foo<T> = Bar<T, u8>;");
        assert_extent!(p, (0, 29))
    }

    #[test]
    fn item_type_alias_with_trait_bounds() {
        let p = qp(item, "type X<T: Foo> where T: Bar = Option<T>;");
        assert_extent!(p, (0, 40))
    }

    #[test]
    fn item_const() {
        let p = qp(item, r#"const FOO: &'static str = "hi";"#);
        assert_extent!(p, (0, 31))
    }

    #[test]
    fn item_const_public() {
        let p = qp(item, "pub const FOO: u8 = 42;");
        assert_extent!(p, (0, 23))
    }

    #[test]
    fn item_static() {
        let p = qp(item, r#"static FOO: &'static str = "hi";"#);
        assert_extent!(p, (0, 32))
    }

    #[test]
    fn item_static_mut() {
        let p = qp(item, r#"static mut FOO: &'static str = "hi";"#);
        assert_extent!(p, (0, 36))
    }

    #[test]
    fn item_static_public() {
        let p = qp(item, "pub static FOO: u8 = 42;");
        assert_extent!(p, (0, 24))
    }

    #[test]
    fn item_extern_crate() {
        let p = qp(item, "extern crate foo;");
        assert_extent!(p, (0, 17))
    }

    #[test]
    fn item_extern_crate_public() {
        let p = qp(item, "pub extern crate foo;");
        assert_extent!(p, (0, 21))
    }

    #[test]
    fn item_extern_crate_rename() {
        let p = qp(item, "extern crate foo as bar;");
        assert_extent!(p, (0, 24))
    }

    #[test]
    fn item_extern_block() {
        let p = qp(item, r#"extern {}"#);
        assert_extent!(p, (0, 9))
    }

    #[test]
    fn item_extern_block_with_abi() {
        let p = qp(item, r#"extern "C" {}"#);
        assert_extent!(p, (0, 13))
    }

    #[test]
    fn item_extern_block_with_fn() {
        let p = qp(item, r#"extern { fn foo(bar: u8) -> bool; }"#);
        assert_extent!(p, (0, 35))
    }

    #[test]
    fn item_extern_block_with_variadic_fn() {
        let p = qp(item, r#"extern { fn foo(bar: u8, ...) -> bool; }"#);
        assert_extent!(p, (0, 40))
    }

    #[test]
    fn item_extern_block_with_fn_and_generics() {
        let p = qp(item, r#"extern { fn foo<A, B>(bar: A) -> B; }"#);
        assert_extent!(p, (0, 37))
    }

    #[test]
    fn item_extern_block_with_attribute() {
        let p = qp(item, r#"extern { #[wow] static A: u8; }"#);
        assert_extent!(p, (0, 31))
    }

    #[test]
    fn item_extern_block_with_static() {
        let p = qp(item, r#"extern { static FOO: u32; }"#);
        assert_extent!(p, (0, 27))
    }

    #[test]
    fn item_extern_block_with_static_and_qualifiers() {
        let p = qp(item, r#"extern { pub static mut FOO: u32; }"#);
        assert_extent!(p, (0, 35))
    }

    #[test]
    fn item_extern_block_with_type() {
        let p = qp(item, r#"extern { type opaque; }"#);
        assert_extent!(p, (0, 23))
    }

    #[test]
    fn item_extern_block_with_type_and_qualifiers() {
        let p = qp(item, r#"extern { pub type opaque; }"#);
        assert_extent!(p, (0, 27))
    }

    #[test]
    fn item_attribute_containing() {
        let p = qp(item, r#"#![feature(sweet)]"#);
        assert_extent!(p, (0, 18))
    }

    #[test]
    fn item_attribute_containing_doc_comment_line() {
        let p = qp(item, "//! Hello");
        assert_extent!(p, (0, 9))
    }

    #[test]
    fn item_attribute_containing_doc_comment_block() {
        let p = qp(item, "/*! Hello */");
        assert_extent!(p, (0, 12))
    }

    #[test]
    fn inherent_impl() {
        let p = qp(p_impl, "impl Bar {}");
        assert_extent!(p, (0, 11))
    }

    #[test]
    fn inherent_impl_with_function() {
        let p = qp(p_impl, "impl Bar { fn foo() {} }");
        assert_extent!(p, (0, 24))
    }

    #[test]
    fn inherent_impl_with_const_function() {
        let p = qp(p_impl, "impl Bar { const fn foo() {} }");
        assert_extent!(p, (0, 30))
    }

    #[test]
    fn inherent_impl_with_default_function() {
        let p = qp(p_impl, "impl Bar { default fn foo() {} }");
        assert_extent!(p, (0, 32))
    }

    #[test]
    fn inherent_impl_with_unsafe_function() {
        let p = qp(p_impl, "impl Bar { unsafe fn foo() {} }");
        assert_extent!(p, (0, 31))
    }

    #[test]
    fn inherent_impl_with_extern_function() {
        let p = qp(p_impl, "impl Bar { extern fn foo() {} }");
        assert_extent!(p, (0, 31))
    }

    #[test]
    fn inherent_impl_with_default_const_unsafe_function() {
        let p = qp(p_impl, "impl Bar { default const unsafe fn foo() {} }");
        assert_extent!(p, (0, 45))
    }

    #[test]
    fn inherent_impl_with_default_unsafe_extern_function() {
        let p = qp(p_impl, "impl Bar { default unsafe extern fn foo() {} }");
        assert_extent!(p, (0, 46))
    }

    #[test]
    fn impl_with_trait() {
        let p = qp(p_impl, "impl Foo for Bar {}");
        assert_extent!(p, (0, 19))
    }

    #[test]
    fn impl_with_negative_trait() {
        let p = qp(p_impl, "impl !Foo for Bar {}");
        assert_extent!(p, (0, 20))
    }

    #[test]
    fn impl_trait_with_wildcard_type() {
        let p = qp(p_impl, "impl Foo for .. {}");
        assert_extent!(p, (0, 18))
    }

    #[test]
    fn impl_with_generics() {
        let p = qp(p_impl, "impl<'a, T> Foo<'a, T> for Bar<'a, T> {}");
        assert_extent!(p, (0, 40))
    }

    #[test]
    fn impl_with_generics_no_space() {
        let p = qp(p_impl, "impl<'a,T>Foo<'a,T>for Bar<'a,T>{}");
        assert_extent!(p, (0, 34))
    }

    #[test]
    fn impl_with_trait_bounds() {
        let p = qp(p_impl, "impl<T> Foo for Bar<T> where T: Quux {}");
        assert_extent!(p, (0, 39))
    }

    #[test]
    fn impl_with_attribute() {
        let p = qp(p_impl, "impl Foo { #[attribute] fn bar() {} }");
        assert_extent!(p, (0, 37))
    }

    #[test]
    fn impl_with_attributes() {
        let p = qp(p_impl, "impl Foo { #[a] #[b] fn bar() {} }");
        assert_extent!(p, (0, 34))
    }

    #[test]
    fn impl_with_associated_type() {
        let p = qp(p_impl, "impl Foo { type A = B; }");
        assert_extent!(p, (0, 24))
    }

    #[test]
    fn impl_with_associated_const() {
        let p = qp(p_impl, "impl Foo { const A: i32 = 42; }");
        assert_extent!(p, (0, 31))
    }

    #[test]
    fn impl_with_public_associated_const() {
        let p = qp(p_impl, "impl Foo { pub(crate) const A: i32 = 42; }");
        assert_extent!(p, (0, 42))
    }

    #[test]
    fn impl_with_unsafe() {
        let p = qp(p_impl, "unsafe impl Foo {}");
        assert_extent!(p, (0, 18))
    }

    #[test]
    fn impl_with_macro_call() {
        let p = qp(p_impl, "impl Foo { bar!(); }");
        assert_extent!(p, (0, 20))
    }

    #[test]
    fn enum_with_trailing_stuff() {
        let p = qp(p_enum, "enum A {} impl Foo for Bar {}");
        assert_extent!(p, (0, 9))
    }

    #[test]
    fn enum_with_generic_types() {
        let p = qp(p_enum, "enum A { Foo(Vec<u8>) }");
        assert_extent!(p, (0, 23))
    }

    #[test]
    fn enum_with_generic_declarations() {
        let p = qp(p_enum, "enum A<T> { Foo(Vec<T>) }");
        assert_extent!(p, (0, 25))
    }

    #[test]
    fn enum_with_struct_variant() {
        let p = qp(p_enum, "enum A { Foo { a: u8 } }");
        assert_extent!(p, (0, 24))
    }

    #[test]
    fn enum_with_attribute() {
        let p = qp(p_enum, "enum Foo { #[attr] A(u8)}");
        assert_extent!(p, (0, 25))
    }

    #[test]
    fn enum_with_attribute_on_value() {
        let p = qp(p_enum, "enum Foo { A(#[attr] u8) }");
        assert_extent!(p, (0, 26))
    }

    #[test]
    fn enum_with_discriminant() {
        let p = qp(p_enum, "enum Foo { A = 1, B = 2 }");
        assert_extent!(p, (0, 25))
    }

    #[test]
    fn enum_with_where_clause() {
        let p = qp(p_enum, "enum Foo<A> where A: Bar { Z }");
        assert_extent!(p, (0, 30))
    }

    #[test]
    fn enum_public() {
        let p = qp(p_enum, "pub enum A {}");
        assert_extent!(p, (0, 13))
    }

    #[test]
    fn fn_with_public_modifier() {
        let p = qp(function_header, "pub fn foo()");
        assert_extent!(p, (0, 12))
    }

    #[test]
    fn fn_with_const_modifier() {
        let p = qp(function_header, "const fn foo()");
        assert_extent!(p, (0, 14))
    }

    #[test]
    fn fn_with_extern_modifier() {
        let p = qp(function_header, "extern fn foo()");
        assert_extent!(p, (0, 15))
    }

    #[test]
    fn fn_with_extern_modifier_and_abi() {
        let p = qp(function_header, r#"extern "C" fn foo()"#);
        assert_extent!(p, (0, 19))
    }

    #[test]
    fn fn_with_self_type_reference() {
        let p = qp(function_header, "fn foo(&self)");
        assert_extent!(p, (0, 13))
    }

    #[test]
    fn fn_with_self_type_value() {
        let p = qp(function_header, "fn foo(self)");
        assert_extent!(p, (0, 12))
    }

    #[test]
    fn fn_with_self_type_value_mut() {
        let p = qp(function_header, "fn foo(mut self)");
        assert_extent!(p, (0, 16))
    }

    #[test]
    fn fn_with_self_type_reference_mut() {
        let p = qp(function_header, "fn foo(&mut self)");
        assert_extent!(p, (0, 17))
    }

    #[test]
    fn fn_with_self_type_with_lifetime() {
        let p = qp(function_header, "fn foo<'a>(&'a self)");
        assert_extent!(p, (0, 20))
    }

    #[test]
    fn fn_with_self_type_and_regular() {
        let p = qp(function_header, "fn foo(&self, a: u8)");
        assert_extent!(p, (0, 20))
    }

    #[test]
    fn fn_with_self_type_explicit_type() {
        let p = qp(function_header, "fn foo(self: &mut Self)");
        assert_extent!(p, (0, 23))
    }

    #[test]
    fn fn_with_self_type_explicit_type_mutable() {
        let p = qp(function_header, "fn foo(mut self: &mut Self)");
        assert_extent!(p, (0, 27))
    }

    #[test]
    fn fn_with_argument() {
        let p = qp(function_header, "fn foo(a: u8)");
        assert_extent!(p, (0, 13))
    }

    #[test]
    fn fn_with_arguments_all_space() {
        let p = qp(function_header, "fn foo ( a : u8 )");
        assert_extent!(p, (0, 17))
    }

    #[test]
    fn fn_with_argument_with_generic() {
        let p = qp(function_header, "fn foo(a: Vec<u8>)");
        assert_extent!(p, (0, 18))
    }

    #[test]
    fn fn_with_arguments() {
        let p = qp(function_header, "fn foo(a: u8, b: u8)");
        assert_extent!(p, (0, 20))
    }

    #[test]
    fn fn_with_arguments_with_patterns() {
        let p = qp(function_header, "fn foo(&a: &u8)");
        assert_extent!(p, (0, 15))
    }

    #[test]
    fn fn_with_return_type() {
        let p = qp(function_header, "fn foo() -> bool");
        assert_extent!(p, (0, 16))
    }

    #[test]
    fn fn_with_generics() {
        let p = qp(function_header, "fn foo<A, B>()");
        assert_extent!(p, (0, 14))
    }

    #[test]
    fn fn_with_lifetimes() {
        let p = qp(function_header, "fn foo<'a, 'b>()");
        assert_extent!(p, (0, 16))
    }

    #[test]
    fn fn_with_lifetimes_and_generics() {
        let p = qp(function_header, "fn foo<'a, T>()");
        assert_extent!(p, (0, 15))
    }

    #[test]
    fn fn_with_whitespace_before_arguments() {
        let p = qp(function_header, "fn foo () -> ()");
        assert_extent!(p, (0, 15))
    }

    #[test]
    fn fn_with_whitespace_before_generics() {
        let p = qp(function_header, "fn foo <'a, T>() -> ()");
        assert_extent!(p, (0, 22))
    }

    #[test]
    fn fn_with_unsafe_qualifier() {
        let p = qp(function_header, "unsafe fn foo()");
        assert_extent!(p, (0, 15))
    }

    #[test]
    fn block_with_multiple_implicit_statement_macro_calls() {
        let p = qp(block, "{ a! {} b! {} }");
        assert_extent!(p, (0, 15));
    }

    #[test]
    fn block_promotes_implicit_statement_to_expression() {
        let p = qp(block, "{ if a {} }");
        assert!(p.statements.is_empty());
        assert_extent!(p.expression.unwrap(), (2, 9));
    }

    #[test]
    fn block_with_multiple_empty_statements() {
        let p = qp(block, "{ ; ; ; }");
        assert_extent!(p, (0, 9));
    }

    #[test]
    fn statement_match_no_semicolon() {
        let p = qp(statement, "match a { _ => () }");
        assert_extent!(p.into_expression().unwrap(), (0, 19))
    }

    #[test]
    fn statement_use() {
        let p = qp(statement, "use foo::Bar;");
        assert_extent!(p, (0, 13))
    }

    #[test]
    fn statement_any_item() {
        let p = qp(statement, "struct Foo {}");
        assert_extent!(p, (0, 13))
    }

    #[test]
    fn statement_union() {
        // Since `union` is a weird contextual keyword, it's worth testing specially
        let p = qp(statement, "union union { union: union }");
        assert_extent!(p, (0, 28))
    }

    #[test]
    fn statement_braced_expression_followed_by_method() {
        let p = qp(statement, "match 1 { _ => 1u8 }.count_ones()");
        assert_extent!(p, (0, 33))
    }

    #[test]
    fn pathed_ident_with_leading_separator() {
        let p = qp(pathed_ident, "::foo");
        assert_extent!(p, (0, 5))
    }

    #[test]
    fn pathed_ident_with_turbofish() {
        let p = qp(pathed_ident, "foo::<Vec<u8>>");
        assert_extent!(p, (0, 14))
    }

    #[test]
    fn pathed_ident_with_turbofish_with_lifetime() {
        let p = qp(pathed_ident, "StructWithLifetime::<'a, u8>");
        assert_extent!(p, (0, 28))
    }

    #[test]
    fn pathed_ident_all_space() {
        let p = qp(pathed_ident, "foo :: < Vec < u8 > , Option < bool > >");
        assert_extent!(p, (0, 39))
    }

    #[test]
    fn number_decimal_cannot_start_with_underscore() {
        let p = parse_full(number_literal, "_123");
        let (err_loc, errs) = unwrap_progress_err(p);
        assert_eq!(err_loc, 0);
        assert!(errs.contains(&Error::ExpectedNumber));
    }

    #[test]
    fn number_with_exponent() {
        let p = qp(number_literal, "1e2");
        assert_extent!(p, (0, 3))
    }

    #[test]
    fn number_with_prefix_and_exponent() {
        let p = qp(number_literal, "0x1e2");
        assert_extent!(p, (0, 5))
    }

    #[test]
    fn number_with_fractional() {
        let p = qp(number_literal, "1.2");
        assert_extent!(p, (0, 3))
    }

    #[test]
    fn number_with_fractional_with_suffix() {
        let p = qp(number_literal, "1.2f32");
        assert_extent!(p, (0, 6))
    }

    #[test]
    fn number_with_prefix_and_fractional() {
        let p = qp(number_literal, "0x1.2");
        assert_extent!(p, (0, 5))
    }

    #[test]
    fn number_with_prefix_exponent_and_fractional() {
        let p = qp(number_literal, "0o7.3e9");
        assert_extent!(p, (0, 7))
    }

    #[test]
    fn number_with_prefix_can_have_underscore_after_prefix() {
        let p = qp(number_literal, "0x_123");
        assert_extent!(p, (0, 6))
    }

    #[test]
    fn number_binary_can_have_suffix() {
        let p = qp(number_literal, "0b111u8");
        assert_extent!(p, (0, 7))
    }

    #[test]
    fn number_decimal_can_have_suffix() {
        let p = qp(number_literal, "123i16");
        assert_extent!(p, (0, 6))
    }

    #[test]
    fn number_hexadecimal_can_have_suffix() {
        let p = qp(number_literal, "0xBEEF__u32");
        assert_extent!(p, (0, 11))
    }

    #[test]
    fn number_octal_can_have_suffix() {
        let p = qp(number_literal, "0o777_isize");
        assert_extent!(p, (0, 11))
    }

    #[test]
    fn pattern_with_path() {
        let p = qp(pattern, "foo::Bar::Baz");
        assert_extent!(p, (0, 13))
    }

    #[test]
    fn pattern_with_ref() {
        let p = qp(pattern, "ref a");
        assert_extent!(p, (0, 5))
    }

    #[test]
    fn pattern_with_tuple() {
        let p = qp(pattern, "(a, b)");
        assert_extent!(p, (0, 6))
    }

    #[test]
    fn pattern_with_enum_tuple() {
        let p = qp(pattern, "Baz(a)");
        assert_extent!(p, (0, 6))
    }

    #[test]
    fn pattern_with_tuple_wildcard() {
        let p = qp(pattern, "(..)");
        assert_extent!(p, (0, 4))
    }

    #[test]
    fn pattern_with_tuple_wildcard_anywhere() {
        let p = qp(pattern, "(a, .., b)");
        assert_extent!(p, (0, 10))
    }

    #[test]
    fn pattern_with_tuple_all_space() {
        let p = qp(pattern, "( a , .. , b )");
        assert_extent!(p, (0, 14))
    }

    #[test]
    fn pattern_with_enum_struct() {
        let p = qp(pattern, "Baz { a: a }");
        assert_extent!(p, (0, 12))
    }

    #[test]
    fn pattern_with_enum_struct_shorthand() {
        let p = qp(pattern, "Baz { a }");
        assert_extent!(p, (0, 9))
    }

    #[test]
    fn pattern_with_enum_struct_shorthand_with_ref() {
        let p = qp(pattern, "Baz { ref a }");
        assert_extent!(p, (0, 13))
    }

    #[test]
    fn pattern_with_enum_struct_wildcard() {
        let p = qp(pattern, "Baz { .. }");
        assert_extent!(p, (0, 10))
    }

    #[test]
    fn pattern_with_byte_literal() {
        let p = qp(pattern, "b'a'");
        assert_extent!(p, (0, 4))
    }

    #[test]
    fn pattern_with_char_literal() {
        let p = qp(pattern, "'a'");
        assert_extent!(p, (0, 3))
    }

    #[test]
    fn pattern_with_byte_string_literal() {
        let p = qp(pattern, r#"b"hello""#);
        assert_extent!(p, (0, 8))
    }

    #[test]
    fn pattern_with_string_literal() {
        let p = qp(pattern, r#""hello""#);
        assert_extent!(p, (0, 7))
    }

    #[test]
    fn pattern_with_numeric_literal() {
        let p = qp(pattern, "42");
        assert_extent!(p, (0, 2))
    }

    #[test]
    fn pattern_with_numeric_literal_negative() {
        let p = qp(pattern, "- 42");
        assert_extent!(p, (0, 4))
    }

    #[test]
    fn pattern_with_slice() {
        let p = qp(pattern, "[a, b]");
        assert_extent!(p, (0, 6))
    }

    #[test]
    fn pattern_with_slice_and_subslices() {
        let p = qp(pattern, "[a, b..]");
        assert_extent!(p, (0, 8))
    }

    #[test]
    fn pattern_with_slice_and_modified_subslices() {
        let p = qp(pattern, "[a, ref mut b..]");
        assert_extent!(p, (0, 16))
    }

    #[test]
    fn pattern_with_reference() {
        let p = qp(pattern, "&a");
        assert_extent!(p, (0, 2))
    }

    #[test]
    fn pattern_with_reference_mutable() {
        let p = qp(pattern, "&mut ()");
        assert!(p.kind.is_reference());
        assert_extent!(p, (0, 7));
    }

    #[test]
    fn pattern_with_named_subpattern() {
        let p = qp(pattern, "a @ 1");
        assert_extent!(p, (0, 5));
    }

    #[test]
    fn pattern_with_named_subpattern_qualifiers() {
        let p = qp(pattern, "ref mut a @ 1");
        assert_extent!(p, (0, 13));
    }

    #[test]
    fn pattern_with_numeric_inclusive_range() {
        let p = qp(pattern, "1 ..= 10");
        assert_extent!(p, (0, 8))
    }

    #[test]
    fn pattern_with_numeric_inclusive_range_negative() {
        let p = qp(pattern, "-10 ..= -1");
        assert_extent!(p, (0, 10))
    }

    #[test]
    fn pattern_with_character_inclusive_range() {
        let p = qp(pattern, "'a'..='z'");
        assert_extent!(p, (0, 9))
    }

    #[test]
    fn pattern_with_byte_inclusive_range() {
        let p = qp(pattern, "b'a'..=b'z'");
        assert_extent!(p, (0, 11))
    }

    #[test]
    fn pattern_with_pathed_ident_inclusive_range() {
        let p = qp(pattern, "foo::a..=z");
        assert_extent!(p, (0, 10))
    }

    #[test]
    fn pattern_with_legacy_numeric_inclusive_range() {
        let p = qp(pattern, "1 ... 10");
        assert_extent!(p, (0, 8))
    }

    #[test]
    fn pattern_with_legacy_numeric_inclusive_range_negative() {
        let p = qp(pattern, "-10 ... -1");
        assert_extent!(p, (0, 10))
    }

    #[test]
    fn pattern_with_legacy_character_inclusive_range() {
        let p = qp(pattern, "'a'...'z'");
        assert_extent!(p, (0, 9))
    }

    #[test]
    fn pattern_with_legacy_byte_inclusive_range() {
        let p = qp(pattern, "b'a'...b'z'");
        assert_extent!(p, (0, 11))
    }

    #[test]
    fn pattern_with_legacy_pathed_ident_inclusive_range() {
        let p = qp(pattern, "foo::a...z");
        assert_extent!(p, (0, 10))
    }

    #[test]
    fn pattern_with_numeric_exclusive_range() {
        let p = qp(pattern, "1 .. 10");
        assert_extent!(p, (0, 7))
    }

    #[test]
    fn pattern_with_numeric_exclusive_range_negative() {
        let p = qp(pattern, "-10 .. -1");
        assert_extent!(p, (0, 9))
    }

    #[test]
    fn pattern_with_character_exclusive_range() {
        let p = qp(pattern, "'a'..'z'");
        assert_extent!(p, (0, 8))
    }

    #[test]
    fn pattern_with_byte_exclusive_range() {
        let p = qp(pattern, "b'a'..b'z'");
        assert_extent!(p, (0, 10))
    }

    #[test]
    fn pattern_with_pathed_ident_exclusive_range() {
        let p = qp(pattern, "foo::a..z");
        assert_extent!(p, (0, 9))
    }

    #[test]
    fn pattern_with_macro_call() {
        let p = qp(pattern, "foo![]");
        assert_extent!(p, (0, 6))
    }

    #[test]
    fn pattern_with_box() {
        let p = qp(pattern, "box a");
        assert_extent!(p, (0, 5))
    }

    #[test]
    fn type_tuple() {
        let p = qp(typ, "(u8, u8)");
        assert_extent!(p, (0, 8))
    }

    #[test]
    fn type_tuple_all_space() {
        let p = qp(typ, "( u8 , u8 )");
        assert_extent!(p, (0, 11))
    }

    #[test]
    fn type_with_generics() {
        let p = qp(typ, "A<T>");
        assert_extent!(p, (0, 4))
    }

    #[test]
    fn type_with_generics_all_space() {
        let p = qp(typ, "A < T >");
        assert_extent!(p, (0, 7))
    }

    #[test]
    fn type_impl_trait() {
        let p = qp(typ, "impl Foo");
        assert_extent!(p, (0, 8))
    }

    #[test]
    fn type_fn_trait() {
        let p = qp(typ, "Fn(u8) -> u8");
        assert_extent!(p, (0, 12))
    }

    #[test]
    fn type_ref() {
        let p = qp(typ, "&mut Foo");
        assert_extent!(p, (0, 8))
    }

    #[test]
    fn type_mut_ref() {
        let p = qp(typ, "&mut Foo");
        assert_extent!(p, (0, 8))
    }

    #[test]
    fn type_mut_ref_with_lifetime() {
        let p = qp(typ, "&'a mut Foo");
        assert_extent!(p, (0, 11))
    }

    #[test]
    fn type_const_pointer() {
        let p = qp(typ, "*const Foo");
        assert_extent!(p, (0, 10))
    }

    #[test]
    fn type_mut_pointer() {
        let p = qp(typ, "*mut Foo");
        assert_extent!(p, (0, 8))
    }

    #[test]
    fn type_uninhabited() {
        let p = qp(typ, "!");
        assert_extent!(p, (0, 1))
    }

    #[test]
    fn type_slice() {
        let p = qp(typ, "[u8]");
        assert_extent!(p, (0, 4))
    }

    #[test]
    fn type_array() {
        let p = qp(typ, "[u8; 42]");
        assert_extent!(p, (0, 8))
    }

    #[test]
    fn type_array_allows_expressions() {
        let p = qp(typ, "[u8; 1 + 1]");
        assert_extent!(p, (0, 11))
    }

    #[test]
    fn type_fn() {
        let p = qp(typ, "fn(u8) -> u8");
        assert_extent!(p, (0, 12))
    }

    #[test]
    fn type_fn_with_names() {
        let p = qp(typ, "fn(a: u8) -> u8");
        assert_extent!(p, (0, 15))
    }

    #[test]
    fn type_fn_with_const() {
        let p = qp(typ, "const fn()");
        assert_extent!(p, (0, 10))
    }

    #[test]
    fn type_fn_with_unsafe() {
        let p = qp(typ, "unsafe fn()");
        assert_extent!(p, (0, 11))
    }

    #[test]
    fn type_fn_with_extern() {
        let p = qp(typ, "extern fn()");
        assert_extent!(p, (0, 11))
    }

    #[test]
    fn type_fn_with_extern_and_abi() {
        let p = qp(typ, r#"extern "C" fn()"#);
        assert_extent!(p, (0, 15))
    }

    #[test]
    fn type_fn_with_variadic() {
        let p = qp(typ, r#"fn(...)"#);
        assert_extent!(p, (0, 7))
    }

    #[test]
    fn type_higher_ranked_trait_bounds() {
        let p = qp(typ, "for <'a> Foo<'a>");
        assert_extent!(p, (0, 16))
    }

    #[test]
    fn type_higher_ranked_trait_bounds_on_functions() {
        let p = qp(typ, "for <'a> fn(&'a u8)");
        assert_extent!(p, (0, 19))
    }

    #[test]
    fn type_higher_ranked_trait_bounds_on_references() {
        let p = qp(typ, "for <'a> &'a u8");
        assert_extent!(p, (0, 15))
    }

    #[test]
    fn type_with_additional_named_type() {
        let p = qp(typ, "Foo + Bar");
        assert_extent!(p, (0, 9))
    }

    #[test]
    fn type_with_additional_lifetimes() {
        let p = qp(typ, "Foo + 'a");
        assert_extent!(p, (0, 8))
    }

    #[test]
    fn type_disambiguation() {
        let p = qp(typ, "<Foo as Bar>");
        assert_extent!(p, (0, 12))
    }

    #[test]
    fn type_disambiguation_with_associated_type() {
        let p = qp(typ, "<Foo as Bar>::Quux");
        assert_extent!(p, (0, 18))
    }

    #[test]
    fn type_disambiguation_without_disambiguation() {
        let p = qp(typ, "<Foo>");
        assert_extent!(p, (0, 5))
    }

    #[test]
    fn type_disambiguation_with_double_angle_brackets() {
        let p = qp(typ, "<<A as B> as Option<T>>");
        assert_extent!(p, (0, 23))
    }

    #[test]
    fn struct_basic() {
        let p = qp(p_struct, "struct S { field: TheType, other: OtherType }");
        assert_extent!(p, (0, 45))
    }

    #[test]
    fn struct_with_generic_fields() {
        let p = qp(p_struct, "struct S { field: Option<u8> }");
        assert_extent!(p, (0, 30))
    }

    #[test]
    fn struct_with_fields_with_no_space() {
        let p = qp(p_struct, "struct S{a:u8}");
        assert_extent!(p, (0, 14))
    }

    #[test]
    fn struct_with_fields_with_all_space() {
        let p = qp(p_struct, "struct S { a : u8 }");
        assert_extent!(p, (0, 19))
    }

    #[test]
    fn struct_with_generic_declarations() {
        let p = qp(p_struct, "struct S<T> { field: Option<T> }");
        assert_extent!(p, (0, 32))
    }

    #[test]
    fn struct_public() {
        let p = qp(p_struct, "pub struct S {}");
        assert_extent!(p, (0, 15))
    }

    #[test]
    fn struct_public_field() {
        let p = qp(p_struct, "struct S { pub age: u8 }");
        assert_extent!(p, (0, 24))
    }

    #[test]
    fn struct_with_attributed_field() {
        let p = qp(p_struct, "struct S { #[foo(bar)] #[baz(quux)] field: u8 }");
        assert_extent!(p, (0, 47))
    }

    #[test]
    fn struct_with_block_documented_field() {
        let p = qp(p_struct, "struct S { /** use me */ field: u8 }");
        assert_extent!(p, (0, 36))
    }

    #[test]
    fn struct_with_line_documented_field() {
        let p = qp(p_struct, "struct S { /// use me\n field: u8 }");
        assert_extent!(p, (0, 34))
    }

    #[test]
    fn struct_with_tuple() {
        let p = qp(p_struct, "struct S(u8);");
        assert_extent!(p, (0, 13))
    }

    #[test]
    fn struct_with_tuple_and_annotation() {
        let p = qp(p_struct, "struct S(#[foo] u8);");
        assert_extent!(p, (0, 20))
    }

    #[test]
    fn struct_with_tuple_and_visibility() {
        let p = qp(p_struct, "struct S(pub u8);");
        assert_extent!(p, (0, 17))
    }

    #[test]
    fn struct_empty() {
        let p = qp(p_struct, "struct S;");
        assert_extent!(p, (0, 9))
    }

    #[test]
    fn struct_with_where_clause() {
        let p = qp(p_struct, "struct S<A> where A: Foo { a: A }");
        assert_extent!(p, (0, 33))
    }

    #[test]
    fn struct_with_tuple_and_where_clause() {
        let p = qp(p_struct, "struct S<A>(A) where A: Foo;");
        assert_extent!(p, (0, 28))
    }

    #[test]
    fn union_basic() {
        let p = qp(p_union, "union U { field: TheType, other: OtherType }");
        assert_extent!(p, (0, 44))
    }

    #[test]
    fn union_is_still_an_ident() {
        let p = qp(p_union, "union union { union: union }");
        assert_extent!(p, (0, 28))
    }

    #[test]
    fn union_with_generic_declarations() {
        let p = qp(p_union, "union S<T> { field: Option<T> }");
        assert_extent!(p, (0, 31))
    }

    #[test]
    fn union_with_where_clause() {
        let p = qp(p_union, "union S<A> where A: Foo { a: A }");
        assert_extent!(p, (0, 32))
    }

    #[test]
    fn union_public() {
        let p = qp(p_union, "pub union S {}");
        assert_extent!(p, (0, 14))
    }

    #[test]
    fn union_public_field() {
        let p = qp(p_union, "union S { pub age: u8 }");
        assert_extent!(p, (0, 23))
    }

    #[test]
    fn union_with_attributed_field() {
        let p = qp(p_union, "union S { #[foo(bar)] #[baz(quux)] field: u8 }");
        assert_extent!(p, (0, 46))
    }

    #[test]
    fn where_clause_with_path() {
        let p = qp(where_clause_item, "P: foo::bar::baz::Quux<'a>");
        assert_extent!(p, (0, 26))
    }

    #[test]
    fn where_clause_with_multiple_bounds() {
        let p = qp(where_clause_item, "P: A + B");
        assert_extent!(p, (0, 8))
    }

    #[test]
    fn where_clause_with_multiple_types() {
        let p = qp(where_clause, "where P: A, Q: B");
        assert_extent!(p[1], (12, 16))
    }

    #[test]
    fn where_clause_with_lifetimes() {
        let p = qp(where_clause_item, "'a: 'b + 'c");
        assert_extent!(p, (0, 11))
    }

    #[test]
    fn where_clause_with_higher_ranked_trait_bounds() {
        let p = qp(where_clause_item, "for<'a> [u8; 4]: Send");
        assert_extent!(p, (0, 21))
    }

    #[test]
    fn ident_with_leading_underscore() {
        let p = qp(ident, "_foo");
        assert_extent!(p, (0, 4))
    }

    #[test]
    fn ident_can_have_keyword_substring() {
        let p = qp(ident, "form");
        assert_extent!(p, (0, 4))
    }

    #[test]
    fn lifetime_ident() {
        let p = qp(lifetime, "'a");
        assert_extent!(p, (0, 2))
    }

    #[test]
    fn lifetime_static() {
        let p = qp(lifetime, "'static");
        assert_extent!(p, (0, 7))
    }

    #[test]
    fn generic_declarations_() {
        let p = qp(generic_declarations, "<A>");
        assert_extent!(p, (0, 3))
    }

    #[test]
    fn generic_declarations_allow_type_bounds() {
        let p = qp(generic_declarations, "<A: Foo>");
        assert_extent!(p, (0, 8))
    }

    #[test]
    fn generic_declarations_with_default_types() {
        let p = qp(generic_declarations, "<A = Bar>");
        assert_extent!(p, (0, 9))
    }

    #[test]
    fn generic_declarations_with_type_bounds_and_default_types() {
        let p = qp(generic_declarations, "<A: Foo = Bar>");
        assert_extent!(p, (0, 14))
    }

    #[test]
    fn generic_declarations_allow_lifetime_bounds() {
        let p = qp(generic_declarations, "<'a: 'b>");
        assert_extent!(p, (0, 8))
    }

    #[test]
    fn generic_declarations_with_attributes() {
        let p = qp(generic_declarations, "<#[foo] 'a, #[bar] B>");
        assert_extent!(p, (0, 21))
    }

    #[test]
    fn generic_declarations_all_space() {
        let p = qp(generic_declarations, "< 'a : 'b , A : Foo >");
        assert_extent!(p, (0, 21))
    }

    #[test]
    fn trait_bounds_with_lifetime() {
        let p = qp(trait_bounds, "'a + 'b");
        assert_extent!(p, (0, 7))
    }

    #[test]
    fn trait_bounds_with_relaxed() {
        let p = qp(trait_bounds, "?A + ?B");
        assert_extent!(p, (0, 7))
    }

    #[test]
    fn trait_bounds_with_associated_types() {
        let p = qp(trait_bounds, "A<B, C = D>");
        assert_extent!(p, (0, 11))
    }

    #[test]
    fn visibility_self() {
        let p = qp(visibility, "pub(self)");
        assert_extent!(p, (0, 9))
    }

    #[test]
    fn visibility_super() {
        let p = qp(visibility, "pub(super)");
        assert_extent!(p, (0, 10))
    }

    #[test]
    fn visibility_crate() {
        let p = qp(visibility, "pub(crate)");
        assert_extent!(p, (0, 10))
    }

    #[test]
    fn visibility_path() {
        let p = qp(visibility, "pub(::foo::bar)");
        assert_extent!(p, (0, 15))
    }
}
