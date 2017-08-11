#![feature(conservative_impl_trait)]

#[macro_use]
extern crate fuzzy_pickles_derive;

#[macro_use]
extern crate peresil;

extern crate unicode_xid;

pub mod tokenizer;

use std::collections::BTreeSet;
use std::cmp::max;
use std::fmt;

use peresil::combinators::*;

use tokenizer::{Token, Tokens};

type Point<'s> = TokenPoint<'s, Token>;
type Master<'s> = peresil::ParseMaster<Point<'s>, Error, State<'s>>;
type Progress<'s, T> = peresil::Progress<Point<'s>, T, Error>;

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
#[derive(Debug)]
pub struct TokenPoint<'s, T: 's> {
    pub offset: usize,
    pub sub_offset: Option<u8>,
    pub s: &'s [T],
}

impl<'s, T: 's> TokenPoint<'s, T> {
    fn new(slice: &'s [T]) -> Self {
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
pub struct State<'s> {
    // Constructs like `if expr {}` will greedily match `StructName
    // {}` as a structure literal expression and then fail because the
    // body of the `if` isn't found. In these contexts, we disable
    // struct literals. You can re-enable them by entering something
    // like parenthesis or a block.
    ignore_struct_literals: bool,

    // When calculating the containing extent of an item, we need to
    // look back one token. Since that's already gone, we bundle all
    // the tokens around.
    tokens: &'s [Token],
}

impl<'s> State<'s> {
    #[allow(dead_code)]
    fn new(tokens: &'s [Token]) -> Self {
        State {
            tokens,
            ..State::default()
        }
    }

    fn ex(&self, start: Point, end: Point) -> Extent {
        if end.offset > start.offset {
            let (a, _) = self.tokens[start.offset].extent();
            let (_, b) = self.tokens[end.offset-1].extent();
            (a, b)
        } else {
            self.tokens[start.offset].extent()
        }
    }
}

// define an error type - emphasis on errors. Need to implement Recoverable (more to discuss.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Error {
    ExpectedAmpersand,
    ExpectedAmpersandEquals,
    ExpectedAs,
    ExpectedAsterisk,
    ExpectedAt,
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
    ExpectedDollar,
    ExpectedDoubleAmpersand,
    ExpectedDoubleColon,
    ExpectedDoubleEquals,
    ExpectedDoubleLeftAngle,
    ExpectedDoublePeriod,
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
}

impl peresil::Recoverable for Error {
    fn recoverable(&self) -> bool { true }
}

#[derive(Debug, PartialEq)]
pub enum ErrorDetail {
    Tokenizer(tokenizer::ErrorDetail),
    Parser(ParserErrorDetail),
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

impl From<ParserErrorDetail> for ErrorDetail {
    fn from(other: ParserErrorDetail) -> Self {
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

#[derive(Debug, PartialEq)]
pub struct ParserErrorDetail {
    location: usize,
    errors: BTreeSet<Error>,
}

impl ParserErrorDetail {
    pub fn with_text<'a>(&'a self, text: &'a str) -> ParserErrorDetailText<'a> {
        ParserErrorDetailText { detail: self, text }
    }
}

#[derive(Debug)]
pub struct ParserErrorDetailText<'a> {
    detail: &'a ParserErrorDetail,
    text: &'a str,
}

impl<'a> fmt::Display for ParserErrorDetailText<'a> {
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

pub fn parse_rust_file(file: &str) -> Result<File, ErrorDetail> {
    let tokens: Vec<_> = Tokens::new(file).collect::<Result<_, _>>()?;
    let (_ws, tokens): (Vec<_>, Vec<_>) = tokens.into_iter().partition(|t| {
        t.is_whitespace() || t.is_comment() || t.is_doc_comment() || t.is_comment_block() || t.is_doc_comment_block()
    });

    let mut pt = Point::new(&tokens);
    let mut pm = Master::with_state(State::new(&tokens));
    let mut items = Vec::new();

    loop {
        if pt.s.first().map(Token::is_end_of_file).unwrap_or(true) { break }

        let item = item(&mut pm, pt);
        let item = pm.finish(item);

        let next_pt = match item.status {
            peresil::Status::Success(s) => {
                items.push(s);
                item.point
            },
            peresil::Status::Failure(e) => {
                return Err(ErrorDetail::Parser(ParserErrorDetail {
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

    Ok(File { items: items })

    // TODO: add `expect` to progress?
}

// TODO: enum variants track whole extent, enum delegates

pub type Extent = (usize, usize);

#[derive(Debug, Visit)]
pub struct File {
    items: Vec<Item>,
}

#[derive(Debug, Visit, Decompose)]
pub enum Item {
    Attribute(Attribute),
    Const(Const),
    Enum(Enum),
    ExternCrate(Crate),
    ExternBlock(ExternBlock),
    Function(Function),
    Impl(Impl),
    MacroCall(MacroCall),
    Module(Module),
    Static(Static),
    Struct(Struct),
    Trait(Trait),
    TypeAlias(TypeAlias),
    Use(Use),
    Union(Union),
}

impl Item {
    #[allow(dead_code)]
    pub fn extent(&self) -> Extent {
        match *self {
            Item::Attribute(Attribute { extent, .. })     |
            Item::Const(Const { extent, .. })             |
            Item::Enum(Enum { extent, .. })               |
            Item::ExternCrate(Crate { extent, .. })       |
            Item::ExternBlock(ExternBlock { extent, .. }) |
            Item::Function(Function { extent, .. })       |
            Item::Impl(Impl { extent, .. })               |
            Item::MacroCall(MacroCall { extent, .. })     |
            Item::Module(Module { extent, .. })           |
            Item::Static(Static { extent, .. })           |
            Item::Struct(Struct { extent, .. })           |
            Item::Trait(Trait { extent, .. })             |
            Item::TypeAlias(TypeAlias { extent, .. })     |
            Item::Union(Union { extent, .. })             |
            Item::Use(Use { extent, .. })                 => extent,
        }
    }
}

#[derive(Debug, Visit)]
pub struct Attribute {
    extent: Extent,
    is_containing: Option<Extent>,
    text: Extent,
}

#[derive(Debug, Visit)]
pub struct Lifetime {
    extent: Extent,
    name: Ident,
}

#[derive(Debug, Visit, Decompose)]
pub enum Whitespace {
    Comment(Comment),
    Whitespace(Extent),
}

#[derive(Debug, Visit)]
pub struct Comment {
    extent: Extent,
    text: Extent,
}

#[derive(Debug, Visit)]
pub struct Use {
    extent: Extent,
    visibility: Option<Visibility>,
    path: Vec<Ident>,
    tail: UseTail,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)]
pub enum UseTail {
    Ident(UseTailIdent),
    Glob(UseTailGlob),
    Multi(UseTailMulti),
}

#[derive(Debug, Visit)]
pub struct UseTailIdent {
    extent: Extent,
    name: Ident,
    rename: Option<Ident>,
}

#[derive(Debug, Visit)]
pub struct UseTailGlob {
    extent: Extent,
}

#[derive(Debug, Visit)]
pub struct UseTailMulti {
    extent: Extent,
    names: Vec<UseTailIdent>,
}

#[derive(Debug, Visit)]
pub struct Function {
    pub extent: Extent,
    pub header: FunctionHeader,
    body: Block,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct FunctionHeader {
    pub extent: Extent,
    visibility: Option<Visibility>,
    qualifiers: FunctionQualifiers,
    pub name: Ident,
    generics: Option<GenericDeclarations>,
    arguments: Vec<Argument>,
    return_type: Option<Type>,
    wheres: Vec<Where>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct FunctionQualifiers {
    pub extent: Extent,
    is_const: Option<Extent>,
    is_unsafe: Option<Extent>,
    is_extern: Option<Extent>,
    abi: Option<String>,
}

#[derive(Debug, Visit)]
pub struct TraitImplFunctionHeader {
    extent: Extent,
    visibility: Option<Visibility>,
    qualifiers: FunctionQualifiers,
    pub name: Ident,
    generics: Option<GenericDeclarations>,
    arguments: Vec<TraitImplArgument>,
    return_type: Option<Type>,
    wheres: Vec<Where>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct GenericDeclarations {
    pub extent: Extent,
    lifetimes: Vec<GenericDeclarationLifetime>,
    types: Vec<GenericDeclarationType>,
}

#[derive(Debug, Visit)]
pub struct GenericDeclarationLifetime {
    extent: Extent,
    attributes: Vec<Attribute>,
    name: Lifetime,
    bounds: Vec<Lifetime>,
}

#[derive(Debug, Visit)]
pub struct GenericDeclarationType {
    extent: Extent,
    attributes: Vec<Attribute>,
    name: Ident,
    bounds: Option<TraitBounds>,
    default: Option<Type>,
}

#[derive(Debug, Visit, Decompose)]
pub enum Type {
    Array(TypeArray),
    Combination(TypeCombination),
    Function(TypeFunction),
    Pointer(TypePointer),
    Reference(TypeReference),
    Slice(TypeSlice),
    Tuple(TypeTuple),
    Uninhabited(Extent),
}

impl Type {
    pub fn extent(&self) -> Extent {
        match *self {
            Type::Array(TypeArray { extent, .. })             |
            Type::Combination(TypeCombination { extent, .. }) |
            Type::Function(TypeFunction { extent, .. })       |
            Type::Pointer(TypePointer { extent, .. })         |
            Type::Reference(TypeReference { extent, .. })     |
            Type::Slice(TypeSlice { extent, .. })             |
            Type::Tuple(TypeTuple { extent, .. })             => extent,
            Type::Uninhabited(extent) => extent,
        }
    }
}

#[derive(Debug, Visit)]
pub struct TypeReference {
    extent: Extent,
    kind: TypeReferenceKind,
    typ: Box<Type>,
}

#[derive(Debug, Visit)]
pub struct TypeReferenceKind {
    extent: Extent,
    lifetime: Option<Lifetime>,
    mutable: Option<Extent>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct TypePointer {
    extent: Extent,
    kind: TypePointerKind,
    typ: Box<Type>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug)]
pub enum TypePointerKind {
    Const,
    Mutable,
}

#[derive(Debug, Visit)]
pub struct TypeArray {
    extent: Extent,
    typ: Box<Type>,
    count: Box<Expression>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct TypeHigherRankedTraitBounds {
    extent: Extent,
    lifetimes: Vec<Lifetime>,
    child: TypeHigherRankedTraitBoundsChild,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)]
pub enum TypeHigherRankedTraitBoundsChild {
    Named(TypeNamed),
    Function(TypeFunction),
    Reference(TypeReference),
}

#[derive(Debug, Visit)]
pub struct TypeImplTrait {
    extent: Extent,
    name: TypeNamed,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct TypeCombination {
    extent: Extent,
    base: TypeCombinationBase,
    additional: Vec<TypeCombinationAdditional>,
}

#[derive(Debug, Visit, Decompose)]
pub enum TypeCombinationBase {
    Disambiguation(TypeDisambiguation),
    HigherRankedTraitBounds(TypeHigherRankedTraitBounds),
    ImplTrait(TypeImplTrait),
    Named(TypeNamed),
}

#[derive(Debug, Visit, Decompose)]
pub enum TypeCombinationAdditional {
    Named(TypeNamed),
    Lifetime(Lifetime),
}

#[derive(Debug, Visit)]
pub struct TypeNamed {
    extent: Extent,
    path: Vec<TypeNamedComponent>,
}

#[derive(Debug, Visit)]
pub struct TypeNamedComponent {
    extent: Extent,
    ident: Ident,
    generics: Option<TypeGenerics>,
}

#[derive(Debug, Visit)]
pub struct TypeDisambiguation {
    extent: Extent,
    from_type: Box<Type>,
    to_type: Option<Box<TypeNamed>>,
    path: Vec<TypeNamedComponent>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct TypeSlice {
    extent: Extent,
    typ: Box<Type>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct TypeTuple {
    extent: Extent,
    types: Vec<Type>,
}

#[derive(Debug, Visit, Decompose)]
pub enum TypeGenerics {
    Function(TypeGenericsFunction),
    Angle(TypeGenericsAngle),
}

#[derive(Debug, Visit)]
pub struct TypeGenericsFunction {
    extent: Extent,
    types: Vec<Type>,
    return_type: Option<Box<Type>>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct TypeGenericsAngle {
    extent: Extent,
    members: Vec<TypeGenericsAngleMember>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)]
pub enum TypeGenericsAngleMember {
    Lifetime(Lifetime),
    Type(Type),
    AssociatedType(AssociatedType)
}

#[derive(Debug, Visit)]
pub struct AssociatedType {
    extent: Extent,
    name: Ident,
    value: Type,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct TypeFunction {
    extent: Extent,
    qualifiers: FunctionQualifiers,
    arguments: Vec<TraitImplArgument>, // TODO: rename this indicating it doesn't require names
    return_type: Option<Box<Type>>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Copy, Clone, Visit)]
pub struct Ident {
    pub extent: Extent,
}

// TODO: Can we reuse the path from the `use` statement?
#[derive(Debug, Visit)]
pub struct Path {
    extent: Extent,
    components: Vec<Ident>,
}

// TODO: Can we reuse the path from the `use` statement?
#[derive(Debug, Visit)]
pub struct PathedIdent {
    extent: Extent,
    components: Vec<PathComponent>,
}

#[derive(Debug, Visit)]
pub struct PathComponent {
    extent: Extent,
    ident: Ident,
    turbofish: Option<Turbofish>,
}

#[derive(Debug, Visit)]
pub struct Turbofish {
    extent: Extent,
    lifetimes: Vec<Lifetime>,
    types: Vec<Type>,
}

impl From<Ident> for PathedIdent {
    fn from(other: Ident) -> PathedIdent {
        PathedIdent { extent: other.extent, components: vec![
            PathComponent { extent: other.extent, ident: other, turbofish: None },
        ] }
    }
}

#[derive(Debug, Visit)]
pub struct Const {
    extent: Extent,
    visibility: Option<Visibility>,
    name: Ident,
    typ: Type,
    value: Expression,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Static {
    extent: Extent,
    visibility: Option<Visibility>,
    is_mut: Option<Extent>,
    name: Ident,
    typ: Type,
    value: Expression,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Struct {
    pub extent: Extent,
    visibility: Option<Visibility>,
    name: Ident,
    generics: Option<GenericDeclarations>,
    wheres: Vec<Where>,
    body: StructDefinitionBody,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)]
pub enum StructDefinitionBody {
    Brace(StructDefinitionBodyBrace),
    Tuple(StructDefinitionBodyTuple),
    Empty(Extent),
}

#[derive(Debug, Visit)]
pub struct StructDefinitionBodyBrace {
    pub extent: Extent,
    fields: Vec<StructDefinitionFieldNamed>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct StructDefinitionFieldNamed {
    extent: Extent,
    attributes: Vec<Attribute>,
    visibility: Option<Visibility>,
    name: Ident,
    typ: Type,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct StructDefinitionBodyTuple {
    pub extent: Extent,
    fields: Vec<StructDefinitionFieldUnnamed>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct StructDefinitionFieldUnnamed {
    extent: Extent,
    attributes: Vec<Attribute>,
    visibility: Option<Visibility>,
    typ: Type,
}

#[derive(Debug, Visit)]
pub struct Union {
    pub extent: Extent,
    visibility: Option<Visibility>,
    name: Ident,
    generics: Option<GenericDeclarations>,
    wheres: Vec<Where>,
    fields: Vec<StructDefinitionFieldNamed>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Enum {
    pub extent: Extent,
    visibility: Option<Visibility>,
    name: Ident,
    generics: Option<GenericDeclarations>,
    wheres: Vec<Where>,
    variants: Vec<EnumVariant>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct EnumVariant {
    extent: Extent,
    attributes: Vec<Attribute>,
    name: Ident,
    body: EnumVariantBody,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)]
pub enum EnumVariantBody {
    Tuple(Vec<StructDefinitionFieldUnnamed>),
    Struct(StructDefinitionBodyBrace),
    Unit(Option<Expression>),
}

#[derive(Debug, Visit, Decompose)]
pub enum Argument {
    SelfArgument(SelfArgument),
    Named(NamedArgument),
}

#[derive(Debug, Visit, Decompose)]
pub enum SelfArgument {
    Longhand(SelfArgumentLonghand),
    Shorthand(SelfArgumentShorthand),
}

#[derive(Debug, Visit)]
pub struct SelfArgumentLonghand {
    extent: Extent,
    is_mut: Option<Extent>,
    name: Ident,
    typ: Type,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct SelfArgumentShorthand {
    extent: Extent,
    qualifier: Option<SelfArgumentShorthandQualifier>,
    name: Ident,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)]
pub enum SelfArgumentShorthandQualifier {
    Reference(TypeReferenceKind),
    Mut(Extent),
}

#[derive(Debug, Visit)]
pub struct NamedArgument {
    name: Pattern,
    typ: Type,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)]
pub enum TraitImplArgument {
    SelfArgument(SelfArgument),
    Named(TraitImplArgumentNamed),
}

#[derive(Debug, Visit)]
pub struct TraitImplArgumentNamed {
    name: Option<Pattern>,
    typ: Type,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)]
pub enum Where {
    Lifetime(WhereLifetime),
    Type(WhereType),
}

impl Where {
    pub fn extent(&self) -> Extent {
        match *self {
            Where::Lifetime(WhereLifetime { extent, .. }) |
            Where::Type(WhereType { extent, .. })         => extent,
        }
    }
}

#[derive(Debug, Visit)]
pub struct WhereLifetime {
    pub extent: Extent,
    name: Lifetime,
    bounds: Vec<Lifetime>,
}

#[derive(Debug, Visit)]
pub struct WhereType {
    pub extent: Extent,
    name: Type,
    bounds: TraitBounds,
}

#[derive(Debug, Visit)]
pub struct TraitBounds {
    pub extent: Extent,
    types: Vec<TraitBound>,
}

#[derive(Debug, Visit, Decompose)]
pub enum TraitBound {
    Lifetime(TraitBoundLifetime),
    Normal(TraitBoundNormal),
    Relaxed(TraitBoundRelaxed),
}

#[derive(Debug, Visit)]
pub struct TraitBoundLifetime {
    pub extent: Extent,
    lifetime: Lifetime,
}

#[derive(Debug, Visit)]
pub struct TraitBoundNormal {
    pub extent: Extent,
    typ: TraitBoundType,
}

#[derive(Debug, Visit)]
pub struct TraitBoundRelaxed {
    pub extent: Extent,
    typ: TraitBoundType,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)]
pub enum TraitBoundType {
    Named(TypeNamed),
    // TODO: HRTB Trait bounds don't really allow references or fn types, just named
    // We need to create a smaller enum here
    HigherRankedTraitBounds(TypeHigherRankedTraitBounds),
}

#[derive(Debug, Visit)]
pub struct Block {
    extent: Extent,
    statements: Vec<Statement>,
    expression: Option<Expression>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct UnsafeBlock {
    extent: Extent,
    body: Box<Block>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Parenthetical {
    extent: Extent,
    expression: Box<Expression>,
}

#[derive(Debug, Visit, Decompose)]
pub enum Statement {
    Expression(Expression),
    Item(Item),
    Empty(Extent),
}

impl Statement {
    #[allow(dead_code)]
    pub fn extent(&self) -> Extent {
        use Statement::*;
        match *self {
            Expression(ref e) => e.extent(),
            Item(ref i) => i.extent(),
            Empty(e) => e,
        }
    }
}

#[derive(Debug, Visit, Decompose)]
pub enum Expression {
    Array(Array),
    AsType(AsType),
    Binary(Binary),
    Block(Box<Block>),
    Box(ExpressionBox),
    Break(Break),
    Byte(Byte),
    ByteString(ByteString),
    Call(Call),
    Character(Character),
    Closure(Closure),
    Continue(Continue),
    Dereference(Dereference),
    Disambiguation(Disambiguation),
    FieldAccess(FieldAccess),
    ForLoop(ForLoop),
    If(If),
    IfLet(IfLet),
    Let(Let),
    Loop(Loop),
    MacroCall(MacroCall),
    Match(Match),
    Number(Number),
    Parenthetical(Parenthetical),
    Range(Range),
    RangeInclusive(RangeInclusive),
    Reference(Reference),
    Return(Return),
    Slice(Slice),
    String(String),
    Tuple(Tuple),
    TryOperator(TryOperator),
    Unary(Unary),
    UnsafeBlock(UnsafeBlock),
    Value(Value),
    While(While),
    WhileLet(WhileLet),
}

impl Expression {
    pub fn extent(&self) -> Extent {
        match *self {
            Expression::Block(ref x) => x.extent,

            Expression::Array(ref x) => x.extent(),
            Expression::Number(ref x) => x.extent(),

            Expression::AsType(AsType { extent, .. })                 |
            Expression::Binary(Binary { extent, .. })                 |
            Expression::Box(ExpressionBox { extent, .. })             |
            Expression::Break(Break { extent, .. })                   |
            Expression::Byte(Byte { extent, .. })                     |
            Expression::ByteString(ByteString { extent, .. })         |
            Expression::Call(Call { extent, .. })                     |
            Expression::Character(Character { extent, .. })           |
            Expression::Closure(Closure { extent, .. })               |
            Expression::Continue(Continue { extent, .. })             |
            Expression::Dereference(Dereference { extent, .. })       |
            Expression::Disambiguation(Disambiguation { extent, .. }) |
            Expression::FieldAccess(FieldAccess { extent, .. })       |
            Expression::ForLoop(ForLoop { extent, .. })               |
            Expression::If(If { extent, .. })                         |
            Expression::IfLet(IfLet { extent, .. })                   |
            Expression::Let(Let { extent, .. })                       |
            Expression::Loop(Loop { extent, .. })                     |
            Expression::MacroCall(MacroCall { extent, .. })           |
            Expression::Match(Match { extent, .. })                   |
            Expression::Parenthetical(Parenthetical { extent, .. })   |
            Expression::Range(Range { extent, .. })                   |
            Expression::RangeInclusive(RangeInclusive { extent, .. }) |
            Expression::Reference(Reference { extent, .. })           |
            Expression::Return(Return { extent, .. })                 |
            Expression::Slice(Slice { extent, .. })                   |
            Expression::String(String { extent, .. })                 |
            Expression::TryOperator(TryOperator { extent, .. })       |
            Expression::Tuple(Tuple { extent, .. })                   |
            Expression::Unary(Unary { extent, .. })                   |
            Expression::UnsafeBlock(UnsafeBlock { extent, .. })       |
            Expression::Value(Value { extent, .. })                   |
            Expression::While(While { extent, .. })                   |
            Expression::WhileLet(WhileLet { extent, .. })             => extent,
        }
    }

    fn may_terminate_implicit_statement(&self) -> bool {
        match *self {
            Expression::Block(_)       |
            Expression::Match(_)       |
            Expression::UnsafeBlock(_) => true,
            _ => self.may_only_be_followed_by_postfix(),
        }
    }

    fn may_only_be_followed_by_postfix(&self) -> bool {
        match *self {
            Expression::ForLoop(_)  |
            Expression::If(_)       |
            Expression::IfLet(_)    |
            Expression::Loop(_)     |
            Expression::While(_)    |
            Expression::WhileLet(_) |
            Expression::MacroCall(MacroCall { args: MacroCallArgs::Curly(_), .. }) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Visit)]
pub struct MacroCall {
    extent: Extent,
    name: Ident,
    arg: Option<Ident>,
    args: MacroCallArgs,
}

#[derive(Debug, Visit, Decompose)]
pub enum MacroCallArgs {
    Paren(Extent),
    Curly(Extent),
    Square(Extent),
}

#[derive(Debug, Visit)]
pub struct Let {
    extent: Extent,
    pattern: Pattern,
    typ: Option<Type>,
    value: Option<Box<Expression>>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Tuple {
    extent: Extent,
    members: Vec<Expression>,
}

#[derive(Debug, Visit)]
pub struct TryOperator {
    extent: Extent,
    target: Box<Expression>,
}

#[derive(Debug, Visit)]
pub struct FieldAccess {
    extent: Extent,
    target: Box<Expression>,
    field: FieldName,
}

#[derive(Debug, Decompose)]
pub enum FieldName {
    Path(PathComponent),
    Number(Extent),
}

#[derive(Debug, Visit)]
pub struct Number {
    extent: Extent,
    is_negative: Option<Extent>,
    value: NumberValue,
    whitespace: Vec<Whitespace>,
}

impl Number {
    #[allow(dead_code)]
    fn extent(&self) -> Extent {
        self.extent
    }
}

#[derive(Debug, Visit, Decompose)]
pub enum NumberValue {
    Binary(NumberBinary),
    Decimal(NumberDecimal),
    Hexadecimal(NumberHexadecimal),
    Octal(NumberOctal),
}

impl NumberValue {
    #[allow(dead_code)]
    fn extent(&self) -> Extent {
        match *self {
            NumberValue::Binary(NumberBinary { extent, .. })           |
            NumberValue::Decimal(NumberDecimal { extent, .. })         |
            NumberValue::Hexadecimal(NumberHexadecimal { extent, .. }) |
            NumberValue::Octal(NumberOctal { extent, .. })             => extent,
        }
    }
}

#[derive(Debug, Visit)]
pub struct NumberBinary {
    extent: Extent,
    decimal: Extent,
    fraction: Option<Extent>,
    exponent: Option<Extent>,
    suffix: Option<Extent>,
}

#[derive(Debug, Visit)]
pub struct NumberDecimal {
    extent: Extent,
    decimal: Extent,
    fraction: Option<Extent>,
    exponent: Option<Extent>,
    suffix: Option<Extent>,
}

#[derive(Debug, Visit)]
pub struct NumberHexadecimal {
    extent: Extent,
    decimal: Extent,
    fraction: Option<Extent>,
    exponent: Option<Extent>,
    suffix: Option<Extent>,
}

#[derive(Debug, Visit)]
pub struct NumberOctal {
    extent: Extent,
    decimal: Extent,
    fraction: Option<Extent>,
    exponent: Option<Extent>,
    suffix: Option<Extent>,
}

#[derive(Debug, Visit)]
pub struct Value {
    extent: Extent,
    name: PathedIdent,
    literal: Option<StructLiteral>,
}

#[derive(Debug, Visit)]
pub struct StructLiteral {
    extent: Extent,
    fields: Vec<StructLiteralField>,
    splat: Option<Box<Expression>>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct StructLiteralField {
    name: Ident,
    value: Expression,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Call {
    extent: Extent,
    target: Box<Expression>,
    args: Vec<Expression>,
}

#[derive(Debug, Visit)]
pub struct ForLoop {
    extent: Extent,
    label: Option<Lifetime>,
    pattern: Pattern,
    iter: Box<Expression>,
    body: Box<Block>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Loop {
    extent: Extent,
    label: Option<Lifetime>,
    body: Box<Block>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct IfLet {
    extent: Extent,
    pattern: Pattern,
    value: Box<Expression>,
    body: Box<Block>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct While {
    extent: Extent,
    label: Option<Lifetime>,
    value: Box<Expression>,
    body: Box<Block>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct WhileLet {
    extent: Extent,
    label: Option<Lifetime>,
    pattern: Pattern,
    value: Box<Expression>,
    body: Box<Block>,
    whitespace: Vec<Whitespace>,
}

// TODO: Should this be the same as dereference? What about reference?
#[derive(Debug, Visit)]
pub struct Unary {
    extent: Extent,
    op: UnaryOp,
    value: Box<Expression>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Debug, Visit)]
pub struct Binary {
    extent: Extent,
    op: BinaryOp,
    lhs: Box<Expression>,
    rhs: Box<Expression>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    AddAssign,
    Assign,
    BitwiseAnd,
    BitwiseAndAssign,
    BitwiseOr,
    BitwiseOrAssign,
    BitwiseXor,
    BitwiseXorAssign,
    BooleanAnd,
    BooleanOr,
    Div,
    DivAssign,
    Equal,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Mod,
    ModAssign,
    Mul,
    MulAssign,
    NotEqual,
    ShiftLeft,
    ShiftLeftAssign,
    ShiftRight,
    ShiftRightAssign,
    Sub,
    SubAssign,
}

#[derive(Debug, Visit)]
pub struct If {
    extent: Extent,
    condition: Box<Expression>,
    body: Box<Block>,
    more: Vec<If>,
    else_body: Option<Box<Block>>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Match {
    extent: Extent,
    head: Box<Expression>,
    arms: Vec<MatchArm>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct MatchArm {
    extent: Extent,
    attributes: Vec<Attribute>,
    pattern: Vec<Pattern>,
    guard: Option<Expression>,
    hand: MatchHand,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)]
pub enum MatchHand {
    Brace(Expression),
    Expression(Expression),
}

#[derive(Debug, Visit)]
pub struct Range {
    extent: Extent,
    lhs: Option<Box<Expression>>,
    rhs: Option<Box<Expression>>,
}

#[derive(Debug, Visit)]
pub struct RangeInclusive {
    extent: Extent,
    lhs: Option<Box<Expression>>,
    rhs: Option<Box<Expression>>,
}

#[derive(Debug, Visit, Decompose)]
pub enum Array {
    Explicit(ArrayExplicit),
    Repeated(ArrayRepeated),
}

impl Array {
    fn extent(&self) -> Extent {
        match *self {
            Array::Explicit(ArrayExplicit { extent, .. }) |
            Array::Repeated(ArrayRepeated { extent, .. }) => extent,
        }
    }
}

#[derive(Debug, Visit)]
pub struct ArrayExplicit {
    extent: Extent,
    values: Vec<Expression>,
}

#[derive(Debug, Visit)]
pub struct ArrayRepeated {
    extent: Extent,
    value: Box<Expression>,
    count: Box<Expression>,
    whitespace: Vec<Whitespace>,
}

// TODO: Rename this visitor function?
#[derive(Debug, Visit)]
pub struct ExpressionBox {
    extent: Extent,
    target: Box<Expression>,
}

#[derive(Debug, Visit)]
pub struct AsType {
    extent: Extent,
    target: Box<Expression>,
    typ: Type,
}

#[derive(Debug, Visit)]
pub struct Character {
    extent: Extent,
    value: Extent,
}

#[derive(Debug, Visit)]
pub struct String {
    extent: Extent,
    value: Extent,
}

#[derive(Debug, Visit)]
pub struct Byte {
    extent: Extent,
    value: Character,
}

#[derive(Debug, Visit)]
pub struct ByteString {
    extent: Extent,
    value: String,
}

#[derive(Debug, Visit)]
pub struct Slice {
    extent: Extent,
    target: Box<Expression>,
    index: Box<Expression>,
}

#[derive(Debug, Visit)]
pub struct Closure {
    extent: Extent,
    #[visit(ignore)]
    is_move: bool,
    args: Vec<ClosureArg>,
    return_type: Option<Type>,
    body: Box<Expression>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct ClosureArg {
    name: Pattern,
    typ: Option<Type>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Reference {
    extent: Extent,
    is_mutable: Option<Extent>,
    target: Box<Expression>,
}

#[derive(Debug, Visit)]
pub struct Dereference {
    extent: Extent,
    target: Box<Expression>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Disambiguation {
    extent: Extent,
    from_type: Type,
    to_type: Option<TypeNamed>,
    components: Vec<PathComponent>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Return {
    extent: Extent,
    value: Option<Box<Expression>>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Continue {
    extent: Extent,
    label: Option<Lifetime>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Break {
    extent: Extent,
    label: Option<Lifetime>,
    value: Option<Box<Expression>>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Pattern {
    extent: Extent,
    name: Option<PatternName>,
    kind: PatternKind,
}

#[derive(Debug, Visit)]
pub struct PatternName {
    extent: Extent,
    is_ref: Option<Extent>,
    is_mut: Option<Extent>,
    name: Ident,
    whitespace: Vec<Whitespace>,
}

impl Pattern {
    #[allow(dead_code)]
    fn extent(&self) -> Extent {
        self.extent
    }
}

#[derive(Debug, Visit, Decompose)]
pub enum PatternKind {
    Byte(PatternByte),
    ByteString(PatternByteString),
    Character(PatternCharacter),
    Ident(PatternIdent), // TODO: split into ident and enumtuple
    MacroCall(PatternMacroCall),
    Number(PatternNumber),
    RangeExclusive(PatternRangeExclusive),
    RangeInclusive(PatternRangeInclusive),
    Reference(PatternReference),
    Slice(PatternSlice),
    String(PatternString),
    Struct(PatternStruct),
    Tuple(PatternTuple),
}

impl PatternKind {
    #[allow(dead_code)]
    fn extent(&self) -> Extent {
        use PatternKind::*;

        match *self {
            Byte(PatternByte { extent, .. })                     |
            ByteString(PatternByteString { extent, .. })         |
            Character(PatternCharacter { extent, .. })           |
            Ident(PatternIdent { extent, .. })                   |
            Number(PatternNumber { extent, .. })                 |
            MacroCall(PatternMacroCall { extent, .. })           |
            RangeExclusive(PatternRangeExclusive { extent, .. }) |
            RangeInclusive(PatternRangeInclusive { extent, .. }) |
            Reference(PatternReference { extent, .. })           |
            Slice(PatternSlice { extent, .. })                   |
            String(PatternString { extent, .. })                 |
            Struct(PatternStruct { extent, .. })                 |
            Tuple(PatternTuple { extent, .. })                   => extent,
        }
    }
}

#[derive(Debug, Visit)]
pub struct PatternIdent {
    extent: Extent,
    is_ref: Option<Extent>,
    is_mut: Option<Extent>,
    ident: PathedIdent,
    tuple: Option<PatternTuple>,
}

#[derive(Debug, Visit)]
pub struct PatternStruct {
    extent: Extent,
    name: PathedIdent,
    fields: Vec<PatternStructField>,
    #[visit(ignore)]
    wildcard: bool,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)]
pub enum PatternStructField {
    Long(PatternStructFieldLong),
    Short(PatternStructFieldShort),
}

#[derive(Debug, Visit)]
pub struct PatternStructFieldLong {
    extent: Extent,
    name: Ident,
    pattern: Pattern,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct PatternStructFieldShort {
    ident: PatternIdent
}

#[derive(Debug, Visit)]
pub struct PatternTuple {
    extent: Extent,
    members: Vec<PatternBundleMember>,
}

#[derive(Debug, Visit)]
pub struct PatternSlice {
    extent: Extent,
    members: Vec<PatternBundleMember>,
}

#[derive(Debug, Visit, Decompose)]
pub enum PatternBundleMember {
    Pattern(Pattern),
    Wildcard(Extent),
}

#[derive(Debug, Visit)]
pub struct PatternWildcard {
    extent: Extent,
}

#[derive(Debug, Visit)]
pub struct PatternByte {
    extent: Extent,
    value: Byte,
}

#[derive(Debug, Visit)]
pub struct PatternCharacter {
    extent: Extent,
    value: Character,
}

#[derive(Debug, Visit)]
pub struct PatternByteString {
    extent: Extent,
    value: ByteString,
}

#[derive(Debug, Visit)]
pub struct PatternString {
    extent: Extent,
    value: String,
}

#[derive(Debug, Visit)]
pub struct PatternNumber {
    extent: Extent,
    is_negative: Option<Extent>,
    value: Number,
}

#[derive(Debug, Visit)]
pub struct PatternMacroCall {
    extent: Extent,
    value: MacroCall,
}

#[derive(Debug, Visit)]
pub struct PatternRangeExclusive {
    extent: Extent,
    start: PatternRangeComponent,
    end: PatternRangeComponent,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct PatternRangeInclusive {
    extent: Extent,
    start: PatternRangeComponent,
    end: PatternRangeComponent,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Decompose)]
pub enum PatternRangeComponent {
    Ident(PathedIdent),
    Byte(Byte),
    Character(Character),
    Number(PatternNumber),
}

#[derive(Debug, Visit)]
pub struct PatternReference {
    extent: Extent,
    is_mut: Option<Extent>,
    pattern: Box<Pattern>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Trait {
    extent: Extent,
    visibility: Option<Visibility>,
    is_unsafe: Option<Extent>,
    name: Ident,
    generics: Option<GenericDeclarations>,
    bounds: Option<TraitBounds>,
    wheres: Vec<Where>,
    members: Vec<TraitMember>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)]
pub enum TraitMember {
    Attribute(Attribute),
    Const(TraitMemberConst),
    Function(TraitMemberFunction),
    Type(TraitMemberType),
}

#[derive(Debug, Visit)]
pub struct TraitMemberFunction {
    extent: Extent,
    header: TraitImplFunctionHeader,
    body: Option<Block>,
}

#[derive(Debug, Visit)]
pub struct TraitMemberType {
    extent: Extent,
    name: Ident,
    bounds: Option<TraitBounds>,
    default: Option<Type>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct TraitMemberConst {
    extent: Extent,
    name: Ident,
    typ: Type,
    value: Option<Expression>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Impl {
    extent: Extent,
    is_unsafe: Option<Extent>,
    generics: Option<GenericDeclarations>,
    kind: ImplKind,
    wheres: Vec<Where>,
    body: Vec<ImplMember>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)]
pub enum ImplKind {
    Trait(ImplOfTrait),
    Inherent(ImplOfInherent),
}

#[derive(Debug, Visit)]
pub struct ImplOfTrait {
    extent: Extent,
    is_negative: Option<Extent>,
    trait_name: Type, // TODO: namedtype only?
    type_name: ImplOfTraitType,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct ImplOfInherent {
    extent: Extent,
    type_name: Type,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)]
pub enum ImplOfTraitType {
    Type(Type),
    Wildcard(Extent),
}

#[derive(Debug, Visit, Decompose)]
pub enum ImplMember {
    Attribute(Attribute),
    Const(ImplConst),
    Function(ImplFunction),
    Type(ImplType),
    MacroCall(MacroCall),
}

#[derive(Debug, Visit)]
pub struct ImplFunction {
    extent: Extent,
    header: FunctionHeader,
    body: Block,
}

#[derive(Debug, Visit)]
pub struct ImplType {
    extent: Extent,
    name: Ident,
    typ: Type,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct ImplConst {
    extent: Extent,
    name: Ident,
    typ: Type,
    value: Expression,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Crate {
    extent: Extent,
    visibility: Option<Visibility>,
    name: Ident,
    rename: Option<Ident>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct ExternBlock {
    extent: Extent,
    abi: Option<String>,
    members: Vec<ExternBlockMember>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)]
pub enum ExternBlockMember {
    Attribute(Attribute),
    Function(ExternBlockMemberFunction),
    Static(ExternBlockMemberStatic),
}

#[derive(Debug, Visit)]
pub struct ExternBlockMemberStatic {
    extent: Extent,
    visibility: Option<Visibility>,
    is_mut: Option<Extent>,
    name: Ident,
    typ: Type,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct ExternBlockMemberFunction {
    extent: Extent,
    visibility: Option<Visibility>,
    pub name: Ident,
    generics: Option<GenericDeclarations>,
    arguments: Vec<ExternBlockMemberFunctionArgument>,
    return_type: Option<Type>,
    wheres: Vec<Where>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)]
pub enum ExternBlockMemberFunctionArgument {
    Named(ExternBlockMemberFunctionArgumentNamed),
    Variadic(ExternBlockMemberFunctionArgumentVariadic),
}

#[derive(Debug, Visit)]
pub struct ExternBlockMemberFunctionArgumentNamed {
    extent: Extent,
    name: Pattern,
    typ: Type,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct ExternBlockMemberFunctionArgumentVariadic {
    extent: Extent,
}

#[derive(Debug, Visit)]
pub struct TypeAlias {
    extent: Extent,
    visibility: Option<Visibility>,
    name: Ident,
    generics: Option<GenericDeclarations>,
    wheres: Vec<Where>,
    defn: Type,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Module {
    extent: Extent,
    visibility: Option<Visibility>,
    name: Ident,
    body: Option<Vec<Item>>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)]
pub struct Visibility {
    extent: Extent,
    #[visit(ignore)]
    qualifier: Option<VisibilityQualifier>,
    whitespace: Vec<Whitespace>,
}

#[derive(Debug)]
pub enum VisibilityQualifier {
    Crate,
    SelfIdent,
    Path(Path),
}

// --------------------------------------------------

pub trait Visit {
    fn visit<V>(&self, &mut V)
        where V: Visitor;
}

impl<T> Visit for Box<T>
    where T: Visit
{
    fn visit<V>(&self, v: &mut V)
        where V: Visitor
    {
        (**self).visit(v)
    }
}

impl<T> Visit for Option<T>
    where T: Visit
{
    fn visit<V>(&self, v: &mut V)
        where V: Visitor
    {
        for i in self {
            i.visit(v)
        }
    }
}

impl<T> Visit for Vec<T>
    where T: Visit
{
    fn visit<V>(&self, v: &mut V)
        where V: Visitor
    {
        for i in self {
            i.visit(v)
        }
    }
}

// Cheap hacks to avoid having to annotate every terminal `Extent` and
// enum; just visit them and don't do anything.

// An extent without any context is pretty useless.
impl Visit for Extent {
    fn visit<V>(&self, _v: &mut V)
        where V: Visitor
    {}
}

// Can't imagine we'd ever want to count the number of additions;
// without the lhs/rhs there's not much benefit.
impl Visit for UnaryOp {
    fn visit<V>(&self, _v: &mut V)
        where V: Visitor
    {}
}
impl Visit for BinaryOp {
    fn visit<V>(&self, _v: &mut V)
        where V: Visitor
    {}
}

// We *might* want to visit this, to enable checking for "large" tuple
// indexes or poor variable names?
impl Visit for FieldName {
    fn visit<V>(&self, _v: &mut V)
        where V: Visitor
    {}
}

// We *might* want to continue visiting the children to be able to
// inspect the character / number?
impl Visit for PatternRangeComponent {
    fn visit<V>(&self, _v: &mut V)
        where V: Visitor
    {}
}

// Knowing if an unknown pointer is mutable has no benefit.
impl Visit for TypePointerKind {
    fn visit<V>(&self, _v: &mut V)
        where V: Visitor
    {}
}

pub trait Visitor {
    fn visit_argument(&mut self, &Argument) {}
    fn visit_array(&mut self, &Array) {}
    fn visit_array_explicit(&mut self, &ArrayExplicit) {}
    fn visit_array_repeated(&mut self, &ArrayRepeated) {}
    fn visit_as_type(&mut self, &AsType) {}
    fn visit_associated_type(&mut self, &AssociatedType) {}
    fn visit_attribute(&mut self, &Attribute) {}
    fn visit_binary(&mut self, &Binary) {}
    fn visit_block(&mut self, &Block) {}
    fn visit_break(&mut self, &Break) {}
    fn visit_byte(&mut self, &Byte) {}
    fn visit_byte_string(&mut self, &ByteString) {}
    fn visit_call(&mut self, &Call) {}
    fn visit_character(&mut self, &Character) {}
    fn visit_closure(&mut self, &Closure) {}
    fn visit_closure_arg(&mut self, &ClosureArg) {}
    fn visit_comment(&mut self, &Comment) {}
    fn visit_const(&mut self, &Const) {}
    fn visit_continue(&mut self, &Continue) {}
    fn visit_crate(&mut self, &Crate) {}
    fn visit_dereference(&mut self, &Dereference) {}
    fn visit_disambiguation(&mut self, &Disambiguation) {}
    fn visit_enum(&mut self, &Enum) {}
    fn visit_enum_variant(&mut self, &EnumVariant) {}
    fn visit_enum_variant_body(&mut self, &EnumVariantBody) {}
    fn visit_expression(&mut self, &Expression) {}
    fn visit_expression_box(&mut self, &ExpressionBox) {}
    fn visit_extern_block(&mut self, &ExternBlock) {}
    fn visit_extern_block_member(&mut self, &ExternBlockMember) {}
    fn visit_extern_block_member_function(&mut self, &ExternBlockMemberFunction) {}
    fn visit_extern_block_member_function_argument(&mut self, &ExternBlockMemberFunctionArgument) {}
    fn visit_extern_block_member_function_argument_named(&mut self, &ExternBlockMemberFunctionArgumentNamed) {}
    fn visit_extern_block_member_function_argument_variadic(&mut self, &ExternBlockMemberFunctionArgumentVariadic) {}
    fn visit_extern_block_member_static(&mut self, &ExternBlockMemberStatic) {}
    fn visit_field_access(&mut self, &FieldAccess) {}
    fn visit_file(&mut self, &File) {}
    fn visit_for_loop(&mut self, &ForLoop) {}
    fn visit_function(&mut self, &Function) {}
    fn visit_function_header(&mut self, &FunctionHeader) {}
    fn visit_function_qualifiers(&mut self, &FunctionQualifiers) {}
    fn visit_generic_declaration_lifetime(&mut self, &GenericDeclarationLifetime) {}
    fn visit_generic_declaration_type(&mut self, &GenericDeclarationType) {}
    fn visit_generic_declarations(&mut self, &GenericDeclarations) {}
    fn visit_ident(&mut self, &Ident) {}
    fn visit_if(&mut self, &If) {}
    fn visit_if_let(&mut self, &IfLet) {}
    fn visit_impl(&mut self, &Impl) {}
    fn visit_impl_const(&mut self, &ImplConst) {}
    fn visit_impl_function(&mut self, &ImplFunction) {}
    fn visit_impl_kind(&mut self, &ImplKind) {}
    fn visit_impl_member(&mut self, &ImplMember) {}
    fn visit_impl_of_inherent(&mut self, &ImplOfInherent) {}
    fn visit_impl_of_trait(&mut self, &ImplOfTrait) {}
    fn visit_impl_of_trait_type(&mut self, &ImplOfTraitType) {}
    fn visit_impl_type(&mut self, &ImplType) {}
    fn visit_item(&mut self, &Item) {}
    fn visit_let(&mut self, &Let) {}
    fn visit_lifetime(&mut self, &Lifetime) {}
    fn visit_loop(&mut self, &Loop) {}
    fn visit_macro_call(&mut self, &MacroCall) {}
    fn visit_macro_call_args(&mut self, &MacroCallArgs) {}
    fn visit_match(&mut self, &Match) {}
    fn visit_match_arm(&mut self, &MatchArm) {}
    fn visit_match_hand(&mut self, &MatchHand) {}
    fn visit_module(&mut self, &Module) {}
    fn visit_named_argument(&mut self, &NamedArgument) {}
    fn visit_number(&mut self, &Number) {}
    fn visit_number_value(&mut self, &NumberValue) {}
    fn visit_number_binary(&mut self, &NumberBinary) {}
    fn visit_number_decimal(&mut self, &NumberDecimal) {}
    fn visit_number_hexadecimal(&mut self, &NumberHexadecimal) {}
    fn visit_number_octal(&mut self, &NumberOctal) {}
    fn visit_parenthetical(&mut self, &Parenthetical) {}
    fn visit_path(&mut self, &Path) {}
    fn visit_path_component(&mut self, &PathComponent) {}
    fn visit_pathed_ident(&mut self, &PathedIdent) {}
    fn visit_pattern(&mut self, &Pattern) {}
    fn visit_pattern_name(&mut self, &PatternName) {}
    fn visit_pattern_bundle_member(&mut self, &PatternBundleMember) {}
    fn visit_pattern_byte(&mut self, &PatternByte) {}
    fn visit_pattern_byte_string(&mut self, &PatternByteString) {}
    fn visit_pattern_character(&mut self, &PatternCharacter) {}
    fn visit_pattern_ident(&mut self, &PatternIdent) {}
    fn visit_pattern_kind(&mut self, &PatternKind) {}
    fn visit_pattern_macro_call(&mut self, &PatternMacroCall) {}
    fn visit_pattern_number(&mut self, &PatternNumber) {}
    fn visit_pattern_range_exclusive(&mut self, &PatternRangeExclusive) {}
    fn visit_pattern_range_inclusive(&mut self, &PatternRangeInclusive) {}
    fn visit_pattern_reference(&mut self, &PatternReference) {}
    fn visit_pattern_slice(&mut self, &PatternSlice) {}
    fn visit_pattern_string(&mut self, &PatternString) {}
    fn visit_pattern_struct(&mut self, &PatternStruct) {}
    fn visit_pattern_struct_field(&mut self, &PatternStructField) {}
    fn visit_pattern_struct_field_long(&mut self, &PatternStructFieldLong) {}
    fn visit_pattern_struct_field_short(&mut self, &PatternStructFieldShort) {}
    fn visit_pattern_tuple(&mut self, &PatternTuple) {}
    fn visit_pattern_wildcard(&mut self, &PatternWildcard) {}
    fn visit_range(&mut self, &Range) {}
    fn visit_range_inclusive(&mut self, &RangeInclusive) {}
    fn visit_reference(&mut self, &Reference) {}
    fn visit_return(&mut self, &Return) {}
    fn visit_self_argument(&mut self, &SelfArgument) {}
    fn visit_self_argument_longhand(&mut self, &SelfArgumentLonghand) {}
    fn visit_self_argument_shorthand(&mut self, &SelfArgumentShorthand) {}
    fn visit_self_argument_shorthand_qualifier(&mut self, &SelfArgumentShorthandQualifier) {}
    fn visit_slice(&mut self, &Slice) {}
    fn visit_statement(&mut self, &Statement) {}
    fn visit_static(&mut self, &Static) {}
    fn visit_string(&mut self, &String) {}
    fn visit_struct(&mut self, &Struct) {}
    fn visit_struct_definition_body(&mut self, &StructDefinitionBody) {}
    fn visit_struct_definition_body_brace(&mut self, &StructDefinitionBodyBrace) {}
    fn visit_struct_definition_body_tuple(&mut self, &StructDefinitionBodyTuple) {}
    fn visit_struct_definition_field_named(&mut self, &StructDefinitionFieldNamed) {}
    fn visit_struct_definition_field_unnamed(&mut self, &StructDefinitionFieldUnnamed) {}
    fn visit_struct_literal(&mut self, &StructLiteral) {}
    fn visit_struct_literal_field(&mut self, &StructLiteralField) {}
    fn visit_trait(&mut self, &Trait) {}
    fn visit_trait_bound(&mut self, &TraitBound) {}
    fn visit_trait_bound_lifetime(&mut self, &TraitBoundLifetime) {}
    fn visit_trait_bound_normal(&mut self, &TraitBoundNormal) {}
    fn visit_trait_bound_relaxed(&mut self, &TraitBoundRelaxed) {}
    fn visit_trait_bound_type(&mut self, &TraitBoundType) {}
    fn visit_trait_bounds(&mut self, &TraitBounds) {}
    fn visit_trait_impl_argument(&mut self, &TraitImplArgument) {}
    fn visit_trait_impl_argument_named(&mut self, &TraitImplArgumentNamed) {}
    fn visit_trait_impl_function_header(&mut self, &TraitImplFunctionHeader) {}
    fn visit_trait_member(&mut self, &TraitMember) {}
    fn visit_trait_member_const(&mut self, &TraitMemberConst) {}
    fn visit_trait_member_function(&mut self, &TraitMemberFunction) {}
    fn visit_trait_member_type(&mut self, &TraitMemberType) {}
    fn visit_try_operator(&mut self, &TryOperator) {}
    fn visit_tuple(&mut self, &Tuple) {}
    fn visit_turbofish(&mut self, &Turbofish) {}
    fn visit_type(&mut self, &Type) {}
    fn visit_type_alias(&mut self, &TypeAlias) {}
    fn visit_type_array(&mut self, &TypeArray) {}
    fn visit_type_combination(&mut self, &TypeCombination) {}
    fn visit_type_combination_additional(&mut self, &TypeCombinationAdditional) {}
    fn visit_type_combination_base(&mut self, &TypeCombinationBase) {}
    fn visit_type_disambiguation(&mut self, &TypeDisambiguation) {}
    fn visit_type_function(&mut self, &TypeFunction) {}
    fn visit_type_generics(&mut self, &TypeGenerics) {}
    fn visit_type_generics_angle(&mut self, &TypeGenericsAngle) {}
    fn visit_type_generics_angle_member(&mut self, &TypeGenericsAngleMember) {}
    fn visit_type_generics_function(&mut self, &TypeGenericsFunction) {}
    fn visit_type_higher_ranked_trait_bounds(&mut self, &TypeHigherRankedTraitBounds) {}
    fn visit_type_higher_ranked_trait_bounds_child(&mut self, &TypeHigherRankedTraitBoundsChild) {}
    fn visit_type_impl_trait(&mut self, &TypeImplTrait) {}
    fn visit_type_named(&mut self, &TypeNamed) {}
    fn visit_type_named_component(&mut self, &TypeNamedComponent) {}
    fn visit_type_pointer(&mut self, &TypePointer) {}
    fn visit_type_reference(&mut self, &TypeReference) {}
    fn visit_type_reference_kind(&mut self, &TypeReferenceKind) {}
    fn visit_type_slice(&mut self, &TypeSlice) {}
    fn visit_type_tuple(&mut self, &TypeTuple) {}
    fn visit_unary(&mut self, &Unary) {}
    fn visit_union(&mut self, &Union) {}
    fn visit_unsafe_block(&mut self, &UnsafeBlock) {}
    fn visit_use(&mut self, &Use) {}
    fn visit_use_tail(&mut self, &UseTail) {}
    fn visit_use_tail_glob(&mut self, &UseTailGlob) {}
    fn visit_use_tail_ident(&mut self, &UseTailIdent) {}
    fn visit_use_tail_multi(&mut self, &UseTailMulti) {}
    fn visit_value(&mut self, &Value) {}
    fn visit_visibility(&mut self, &Visibility) {}
    fn visit_where(&mut self, &Where) {}
    fn visit_where_lifetime(&mut self, &WhereLifetime) {}
    fn visit_where_type(&mut self, &WhereType) {}
    fn visit_while(&mut self, &While) {}
    fn visit_while_let(&mut self, &WhileLet) {}
    fn visit_whitespace(&mut self, &Whitespace) {}

    fn exit_argument(&mut self, &Argument) {}
    fn exit_array(&mut self, &Array) {}
    fn exit_array_explicit(&mut self, &ArrayExplicit) {}
    fn exit_array_repeated(&mut self, &ArrayRepeated) {}
    fn exit_as_type(&mut self, &AsType) {}
    fn exit_associated_type(&mut self, &AssociatedType) {}
    fn exit_attribute(&mut self, &Attribute) {}
    fn exit_binary(&mut self, &Binary) {}
    fn exit_block(&mut self, &Block) {}
    fn exit_break(&mut self, &Break) {}
    fn exit_byte(&mut self, &Byte) {}
    fn exit_byte_string(&mut self, &ByteString) {}
    fn exit_call(&mut self, &Call) {}
    fn exit_character(&mut self, &Character) {}
    fn exit_closure(&mut self, &Closure) {}
    fn exit_closure_arg(&mut self, &ClosureArg) {}
    fn exit_comment(&mut self, &Comment) {}
    fn exit_const(&mut self, &Const) {}
    fn exit_continue(&mut self, &Continue) {}
    fn exit_crate(&mut self, &Crate) {}
    fn exit_dereference(&mut self, &Dereference) {}
    fn exit_disambiguation(&mut self, &Disambiguation) {}
    fn exit_enum(&mut self, &Enum) {}
    fn exit_enum_variant(&mut self, &EnumVariant) {}
    fn exit_enum_variant_body(&mut self, &EnumVariantBody) {}
    fn exit_expression(&mut self, &Expression) {}
    fn exit_expression_box(&mut self, &ExpressionBox) {}
    fn exit_extern_block(&mut self, &ExternBlock) {}
    fn exit_extern_block_member(&mut self, &ExternBlockMember) {}
    fn exit_extern_block_member_function(&mut self, &ExternBlockMemberFunction) {}
    fn exit_extern_block_member_function_argument(&mut self, &ExternBlockMemberFunctionArgument) {}
    fn exit_extern_block_member_function_argument_named(&mut self, &ExternBlockMemberFunctionArgumentNamed) {}
    fn exit_extern_block_member_function_argument_variadic(&mut self, &ExternBlockMemberFunctionArgumentVariadic) {}
    fn exit_extern_block_member_static(&mut self, &ExternBlockMemberStatic) {}
    fn exit_field_access(&mut self, &FieldAccess) {}
    fn exit_file(&mut self, &File) {}
    fn exit_for_loop(&mut self, &ForLoop) {}
    fn exit_function(&mut self, &Function) {}
    fn exit_function_header(&mut self, &FunctionHeader) {}
    fn exit_function_qualifiers(&mut self, &FunctionQualifiers) {}
    fn exit_generic_declaration_lifetime(&mut self, &GenericDeclarationLifetime) {}
    fn exit_generic_declaration_type(&mut self, &GenericDeclarationType) {}
    fn exit_generic_declarations(&mut self, &GenericDeclarations) {}
    fn exit_ident(&mut self, &Ident) {}
    fn exit_if(&mut self, &If) {}
    fn exit_if_let(&mut self, &IfLet) {}
    fn exit_impl(&mut self, &Impl) {}
    fn exit_impl_const(&mut self, &ImplConst) {}
    fn exit_impl_function(&mut self, &ImplFunction) {}
    fn exit_impl_kind(&mut self, &ImplKind) {}
    fn exit_impl_member(&mut self, &ImplMember) {}
    fn exit_impl_of_inherent(&mut self, &ImplOfInherent) {}
    fn exit_impl_of_trait(&mut self, &ImplOfTrait) {}
    fn exit_impl_of_trait_type(&mut self, &ImplOfTraitType) {}
    fn exit_impl_type(&mut self, &ImplType) {}
    fn exit_item(&mut self, &Item) {}
    fn exit_let(&mut self, &Let) {}
    fn exit_lifetime(&mut self, &Lifetime) {}
    fn exit_loop(&mut self, &Loop) {}
    fn exit_macro_call(&mut self, &MacroCall) {}
    fn exit_macro_call_args(&mut self, &MacroCallArgs) {}
    fn exit_match(&mut self, &Match) {}
    fn exit_match_arm(&mut self, &MatchArm) {}
    fn exit_match_hand(&mut self, &MatchHand) {}
    fn exit_module(&mut self, &Module) {}
    fn exit_named_argument(&mut self, &NamedArgument) {}
    fn exit_number(&mut self, &Number) {}
    fn exit_number_value(&mut self, &NumberValue) {}
    fn exit_number_binary(&mut self, &NumberBinary) {}
    fn exit_number_decimal(&mut self, &NumberDecimal) {}
    fn exit_number_hexadecimal(&mut self, &NumberHexadecimal) {}
    fn exit_number_octal(&mut self, &NumberOctal) {}
    fn exit_parenthetical(&mut self, &Parenthetical) {}
    fn exit_path(&mut self, &Path) {}
    fn exit_path_component(&mut self, &PathComponent) {}
    fn exit_pathed_ident(&mut self, &PathedIdent) {}
    fn exit_pattern(&mut self, &Pattern) {}
    fn exit_pattern_bundle_member(&mut self, &PatternBundleMember) {}
    fn exit_pattern_byte(&mut self, &PatternByte) {}
    fn exit_pattern_byte_string(&mut self, &PatternByteString) {}
    fn exit_pattern_character(&mut self, &PatternCharacter) {}
    fn exit_pattern_ident(&mut self, &PatternIdent) {}
    fn exit_pattern_kind(&mut self, &PatternKind) {}
    fn exit_pattern_macro_call(&mut self, &PatternMacroCall) {}
    fn exit_pattern_name(&mut self, &PatternName) {}
    fn exit_pattern_number(&mut self, &PatternNumber) {}
    fn exit_pattern_range_exclusive(&mut self, &PatternRangeExclusive) {}
    fn exit_pattern_range_inclusive(&mut self, &PatternRangeInclusive) {}
    fn exit_pattern_reference(&mut self, &PatternReference) {}
    fn exit_pattern_slice(&mut self, &PatternSlice) {}
    fn exit_pattern_string(&mut self, &PatternString) {}
    fn exit_pattern_struct(&mut self, &PatternStruct) {}
    fn exit_pattern_struct_field(&mut self, &PatternStructField) {}
    fn exit_pattern_struct_field_long(&mut self, &PatternStructFieldLong) {}
    fn exit_pattern_struct_field_short(&mut self, &PatternStructFieldShort) {}
    fn exit_pattern_tuple(&mut self, &PatternTuple) {}
    fn exit_pattern_wildcard(&mut self, &PatternWildcard) {}
    fn exit_range(&mut self, &Range) {}
    fn exit_range_inclusive(&mut self, &RangeInclusive) {}
    fn exit_reference(&mut self, &Reference) {}
    fn exit_return(&mut self, &Return) {}
    fn exit_self_argument(&mut self, &SelfArgument) {}
    fn exit_self_argument_longhand(&mut self, &SelfArgumentLonghand) {}
    fn exit_self_argument_shorthand(&mut self, &SelfArgumentShorthand) {}
    fn exit_self_argument_shorthand_qualifier(&mut self, &SelfArgumentShorthandQualifier) {}
    fn exit_slice(&mut self, &Slice) {}
    fn exit_statement(&mut self, &Statement) {}
    fn exit_static(&mut self, &Static) {}
    fn exit_string(&mut self, &String) {}
    fn exit_struct(&mut self, &Struct) {}
    fn exit_struct_definition_body(&mut self, &StructDefinitionBody) {}
    fn exit_struct_definition_body_brace(&mut self, &StructDefinitionBodyBrace) {}
    fn exit_struct_definition_body_tuple(&mut self, &StructDefinitionBodyTuple) {}
    fn exit_struct_definition_field_named(&mut self, &StructDefinitionFieldNamed) {}
    fn exit_struct_definition_field_unnamed(&mut self, &StructDefinitionFieldUnnamed) {}
    fn exit_struct_literal(&mut self, &StructLiteral) {}
    fn exit_struct_literal_field(&mut self, &StructLiteralField) {}
    fn exit_trait(&mut self, &Trait) {}
    fn exit_trait_bound(&mut self, &TraitBound) {}
    fn exit_trait_bound_lifetime(&mut self, &TraitBoundLifetime) {}
    fn exit_trait_bound_normal(&mut self, &TraitBoundNormal) {}
    fn exit_trait_bound_relaxed(&mut self, &TraitBoundRelaxed) {}
    fn exit_trait_bound_type(&mut self, &TraitBoundType) {}
    fn exit_trait_bounds(&mut self, &TraitBounds) {}
    fn exit_trait_impl_argument(&mut self, &TraitImplArgument) {}
    fn exit_trait_impl_argument_named(&mut self, &TraitImplArgumentNamed) {}
    fn exit_trait_impl_function_header(&mut self, &TraitImplFunctionHeader) {}
    fn exit_trait_member(&mut self, &TraitMember) {}
    fn exit_trait_member_const(&mut self, &TraitMemberConst) {}
    fn exit_trait_member_function(&mut self, &TraitMemberFunction) {}
    fn exit_trait_member_type(&mut self, &TraitMemberType) {}
    fn exit_try_operator(&mut self, &TryOperator) {}
    fn exit_tuple(&mut self, &Tuple) {}
    fn exit_turbofish(&mut self, &Turbofish) {}
    fn exit_type(&mut self, &Type) {}
    fn exit_type_alias(&mut self, &TypeAlias) {}
    fn exit_type_array(&mut self, &TypeArray) {}
    fn exit_type_combination(&mut self, &TypeCombination) {}
    fn exit_type_combination_additional(&mut self, &TypeCombinationAdditional) {}
    fn exit_type_combination_base(&mut self, &TypeCombinationBase) {}
    fn exit_type_disambiguation(&mut self, &TypeDisambiguation) {}
    fn exit_type_function(&mut self, &TypeFunction) {}
    fn exit_type_generics(&mut self, &TypeGenerics) {}
    fn exit_type_generics_angle(&mut self, &TypeGenericsAngle) {}
    fn exit_type_generics_angle_member(&mut self, &TypeGenericsAngleMember) {}
    fn exit_type_generics_function(&mut self, &TypeGenericsFunction) {}
    fn exit_type_higher_ranked_trait_bounds(&mut self, &TypeHigherRankedTraitBounds) {}
    fn exit_type_higher_ranked_trait_bounds_child(&mut self, &TypeHigherRankedTraitBoundsChild) {}
    fn exit_type_impl_trait(&mut self, &TypeImplTrait) {}
    fn exit_type_named(&mut self, &TypeNamed) {}
    fn exit_type_named_component(&mut self, &TypeNamedComponent) {}
    fn exit_type_pointer(&mut self, &TypePointer) {}
    fn exit_type_reference(&mut self, &TypeReference) {}
    fn exit_type_reference_kind(&mut self, &TypeReferenceKind) {}
    fn exit_type_slice(&mut self, &TypeSlice) {}
    fn exit_type_tuple(&mut self, &TypeTuple) {}
    fn exit_unary(&mut self, &Unary) {}
    fn exit_union(&mut self, &Union) {}
    fn exit_unsafe_block(&mut self, &UnsafeBlock) {}
    fn exit_use(&mut self, &Use) {}
    fn exit_use_tail(&mut self, &UseTail) {}
    fn exit_use_tail_glob(&mut self, &UseTailGlob) {}
    fn exit_use_tail_ident(&mut self, &UseTailIdent) {}
    fn exit_use_tail_multi(&mut self, &UseTailMulti) {}
    fn exit_value(&mut self, &Value) {}
    fn exit_visibility(&mut self, &Visibility) {}
    fn exit_where(&mut self, &Where) {}
    fn exit_where_lifetime(&mut self, &WhereLifetime) {}
    fn exit_where_type(&mut self, &WhereType) {}
    fn exit_while(&mut self, &While) {}
    fn exit_while_let(&mut self, &WhileLet) {}
    fn exit_whitespace(&mut self, &Whitespace) {}
}

// --------------------------------------------------

fn ext<'s, F, T>(f: F) -> impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Extent>
    where F: FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    move |pm, pt| {
        let spt = pt;
        let (pt, _) = try_parse!(f(pm, pt));
        Progress::success(pt, pm.state.ex(spt, pt))
    }
}

fn parse_nested_until<'s, O, C>(is_open: O, is_close: C) ->
    impl Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, Extent>
    where O: Fn(&Token) -> bool,
          C: Fn(&Token) -> bool,
{
    move |pm, spt| {
        let mut skipped: usize = 0;
        let mut depth: usize = 0;

        for token in spt.s {
            if token.is_end_of_file() {
                break;
            } else if is_open(token) {
                depth += 1;
            } else if is_close(token) {
                if depth == 0 {
                    break;
                } else {
                    depth -= 1;
                }
            }

            skipped += 1
        }

        let pt = spt.advance_by(skipped);
        Progress::success(pt, pm.state.ex(spt, pt))
    }
}

enum TailedState<P, T, E> {
    Nothing(P, E),
    ValueOnly(P, T),
    ValueAndSeparator(P, T),
}

fn parse_tailed<'s, F, S, T, U>(sep: S, f: F, pm: &mut Master<'s>, pt: Point<'s>) ->
    TailedState<Point<'s>, T, Error>
    where S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>,

{
    match f(pm, pt) {
        Progress { status: peresil::Status::Failure(f), point } => {
            TailedState::Nothing(point, f)
        }
        Progress { status: peresil::Status::Success(value), point } => {
            match sep(pm, point) {
                Progress { status: peresil::Status::Failure(_), point } => {
                    TailedState::ValueOnly(point, value)
                }
                Progress { status: peresil::Status::Success(_), point } => {
                    TailedState::ValueAndSeparator(point, value)
                }
            }
        }
    }
}

trait ImplicitSeparator {
    fn is_implicit_separator(&self) -> bool;
}

#[derive(Debug)]
struct Tailed<T> {
    values: Vec<T>,
    separator_count: usize,
    last_had_separator: bool,
}

impl<T> Default for Tailed<T> {
    fn default() -> Self {
        Tailed {
            values: Vec::new(),
            separator_count: 0,
            last_had_separator: false,
        }
    }
}

// Look for an expression that is followed by a separator. Each time
// the separator is found, another expression is attempted. Each
// expression is returned, along with the count of separators.
fn zero_or_more_tailed_append<'s, S, F, T, U>(append_to: Tailed<T>, sep: S, f: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Tailed<T>>
    where S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>,
{
    move |pm, mut pt| {
        let mut tailed = append_to;
        loop {
            match parse_tailed(&sep, &f, pm, pt) {
                TailedState::Nothing(pt, _) => {
                    return Progress::success(pt, tailed);
                }
                TailedState::ValueOnly(pt, v) => {
                    tailed.values.push(v);
                    tailed.last_had_separator = false;
                    return Progress::success(pt, tailed);
                }
                TailedState::ValueAndSeparator(pt2, v) => {
                    pt = pt2;
                    tailed.values.push(v);
                    tailed.separator_count += 1;
                    tailed.last_had_separator = true;
                }
            }
        }
    }
}

fn zero_or_more_tailed<'s, S, F, T, U>(sep: S, f: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Tailed<T>>
    where S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>,
{
    zero_or_more_tailed_append(Tailed::default(), sep, f)
}

fn zero_or_more_tailed_values<'s, S, F, T, U>(sep: S, f: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Vec<T>>
    where S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    map(zero_or_more_tailed(sep, f), |t| t.values)
}

fn zero_or_more_tailed_values_append<'s, A, S, F, T, U>(append_to: A, sep: S, f: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Vec<T>>
    where A: IntoAppend<T>,
          S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    let append_to = append_to.into();
    // TODO: How do we reset separator_count and last_had_separator?
    let tailed = Tailed { values: append_to, ..Tailed::default() };
    map(zero_or_more_tailed_append(tailed, sep, f), |t| t.values)
}

// Used after parsing a single value, but not the separator
// Foo + Bar
//    ^
fn zero_or_more_tailed_values_resume<'s, S, F, T, U>(sep: S, f: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Vec<T>>
    where S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>,
{
    move |pm, pt| {
        let spt = pt;
        let pt = match sep(pm, pt) {
            Progress { status: peresil::Status::Failure(_), point } => {
                return Progress::success(point, Vec::new())
            }
            Progress { status: peresil::Status::Success(_), point } => {
                point
            }
        };

        match one_or_more_tailed_values(sep, f)(pm, pt) {
            Progress { status: peresil::Status::Failure(_), .. } => {
                // We parsed the separator, but not another value. Rewind to before the separator
                Progress::success(spt, Vec::new())
            }
            other => other
        }
    }
}

fn zero_or_more_implicitly_tailed_append<'s, S, F, T, U>(append_to: Tailed<T>, sep: S, f: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Tailed<T>>
    where S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>,
          T: ImplicitSeparator,
{
    move |pm, mut pt| {
        let mut tailed = append_to;
        loop {
            match parse_tailed(&sep, &f, pm, pt) {
                TailedState::Nothing(pt, _) => {
                    return Progress::success(pt, tailed);
                }
                TailedState::ValueOnly(pt2, v) => {
                    if v.is_implicit_separator() {
                        pt = pt2;
                        tailed.values.push(v);
                        tailed.separator_count += 1;
                    } else {
                        tailed.values.push(v);
                        return Progress::success(pt2, tailed);
                    }
                    tailed.last_had_separator = false;
                }
                TailedState::ValueAndSeparator(pt2, v) => {
                    pt = pt2;
                    tailed.values.push(v);
                    tailed.separator_count += 1;
                    tailed.last_had_separator = true;
                }
            }
        }
    }
}

fn zero_or_more_implicitly_tailed_values<'s, S, F, T, U>(sep: S, f: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Vec<T>>
    where S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>,
          T: ImplicitSeparator
{
    map(zero_or_more_implicitly_tailed_append(Tailed::default(), sep, f), |t| t.values)
}

fn zero_or_more_implicitly_tailed_values_terminated<'s, S, F, T, U>(sep: S, f: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, (Vec<T>, bool)>
    where S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>,
          T: ImplicitSeparator
{
    map(zero_or_more_implicitly_tailed_append(Tailed::default(), sep, f), |t| {
        (t.values, t.last_had_separator)
    })
}

fn one_or_more_tailed<'s, S, F, T, U>(sep: S, f: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Tailed<T>>
    where S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    move |pm, pt| {
        let mut tailed = Tailed::default();

        match parse_tailed(&sep, &f, pm, pt) {
            TailedState::Nothing(pt, f) => {
                return Progress::failure(pt, f);
            }
            TailedState::ValueOnly(pt, v) => {
                tailed.values.push(v);
                return Progress::success(pt, tailed);
            }
            TailedState::ValueAndSeparator(pt, v) => {
                tailed.values.push(v);
                tailed.separator_count += 1;
                zero_or_more_tailed_append(tailed, sep, f)(pm, pt)
            }
        }
    }
}

fn one_or_more_tailed_values<'s, S, F, T, U>(sep: S, f: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Vec<T>>
    where S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    map(one_or_more_tailed(sep, f), |t| t.values)
}

// TODO: generic enough to move to library?
pub fn not<P, E, S, F, T>
    (parser: F, error: E)
     -> impl FnOnce(&mut peresil::ParseMaster<P, E, S>, P) -> peresil::Progress<P, (), E>
    where F: FnOnce(&mut peresil::ParseMaster<P, E, S>, P) -> peresil::Progress<P, T, E>,
          P: peresil::Point,
          E: peresil::Recoverable,
{
    move |pm, pt| {
        match parser(pm, pt) {
            peresil::Progress { status: peresil::Status::Success(_), .. } => {
                peresil::Progress::failure(pt, error)
            }
            peresil::Progress { status: peresil::Status::Failure(_), .. } => {
                peresil::Progress::success(pt, ())
            }
        }
    }
}

// TODO: generic enough to move to library?
pub fn peek<P, E, S, F, T>
    (parser: F)
     -> impl FnOnce(&mut peresil::ParseMaster<P, E, S>, P) -> peresil::Progress<P, T, E>
    where F: FnOnce(&mut peresil::ParseMaster<P, E, S>, P) -> peresil::Progress<P, T, E>,
          P: peresil::Point,
          E: peresil::Recoverable,
{
    move |pm, pt| {
        match parser(pm, pt) {
            peresil::Progress { status: peresil::Status::Success(val), .. } => {
                peresil::Progress::success(pt, val)
            }
            peresil::Progress { status: peresil::Status::Failure(f), .. } => {
                peresil::Progress::failure(pt, f)
            }
        }
    }
}

// --------------------------------------------------

fn item<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Item> {
    pm.alternate(pt)
        .one(map(attribute, Item::Attribute))
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
            let (s, e) = extent;
            let a = Token::LeftAngle((s, s+1));
            let b = Token::LeftAngle((s+1, e));
            Some((a, b))
        }
        (Token::DoubleRightAngle(extent), 0) => {
            let (s, e) = extent;
            let a = Token::RightAngle((s, s+1));
            let b = Token::RightAngle((s+1, e));
            Some((a, b))
        }
        (Token::ShiftRightEquals(extent), 0) => {
            let (s, e) = extent;
            let a = Token::RightAngle((s, s+1));
            let b = Token::GreaterThanOrEquals((s+1, e));
            Some((a, b))
        }
        (Token::ShiftRightEquals(extent), 1) => {
            let (s, e) = extent;
            let a = Token::RightAngle((s+1, s+2));
            let b = Token::Equals((s+2, e));
            Some((a, b))
        }
        (Token::DoublePipe(extent), 0) => {
            let (s, e) = extent;
            let a = Token::Pipe((s, s+1));
            let b = Token::Pipe((s+1, e));
            Some((a, b))
        }
        (Token::DoubleAmpersand(extent), 0) => {
            let (s, e) = extent;
            let a = Token::Ampersand((s, s+1));
            let b = Token::Ampersand((s+1, e));
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

fn function_qualifiers<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, FunctionQualifiers> {
    sequence!(pm, pt, {
        spt       = point;
        is_const  = optional(ext(kw_const));
        is_unsafe = optional(ext(kw_unsafe));
        is_extern = optional(function_qualifier_extern);
    }, |pm: &mut Master, pt| {
        let is_extern = is_extern;
        let (is_extern, abi) = match is_extern {
            Some((ex, abi)) => (Some(ex), abi),
            None => (None, None),
        };
        FunctionQualifiers { extent: pm.state.ex(spt, pt), is_const, is_unsafe, is_extern, abi }
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
        lifetimes = zero_or_more_tailed_values(comma, generic_declaration_lifetime);
        types     = zero_or_more_tailed_values(comma, generic_declaration_type);
        _         = right_angle;
    }, |pm: &mut Master, pt| GenericDeclarations { extent: pm.state.ex(spt, pt), lifetimes, types })
}

fn generic_declaration_lifetime<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, GenericDeclarationLifetime> {
    sequence!(pm, pt, {
        spt        = point;
        attributes = zero_or_more(attribute);
        name       = lifetime;
        bounds     = optional(generic_declaration_lifetime_bounds);
    }, |pm: &mut Master, pt| GenericDeclarationLifetime {
        extent: pm.state.ex(spt, pt),
        attributes,
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
        attributes = zero_or_more(attribute);
        name       = ident;
        // Over-permissive; allows interleaving trait bounds and default types
        bounds     = optional(generic_declaration_type_bounds);
        default    = optional(generic_declaration_type_default);
    }, |pm: &mut Master, pt| GenericDeclarationType { extent: pm.state.ex(spt, pt), attributes, name, bounds, default })
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
    pm.alternate(pt)
        .one(map(where_lifetime, Where::Lifetime))
        .one(map(where_type, Where::Type))
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
        .one(map(expression, Statement::Expression))
        .one(map(item, Statement::Item))
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
            Statement::Expression(ref e) => e.may_terminate_implicit_statement(),
            Statement::Item(_)           => true,
            Statement::Empty(_)          => false,
        }
    }
}

mod expression;
use expression::expression;

fn expr_macro_call<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, MacroCall> {
    sequence!(pm, pt, {
        spt  = point;
        name = ident;
        _    = bang;
        arg  = optional(ident);
        args = expr_macro_call_args;
    }, |pm: &mut Master, pt| MacroCall { extent: pm.state.ex(spt, pt), name, arg, args })
}

fn expr_macro_call_args<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, MacroCallArgs> {
    pm.alternate(pt)
        .one(map(expr_macro_call_paren, MacroCallArgs::Paren))
        .one(map(expr_macro_call_square, MacroCallArgs::Square))
        .one(map(expr_macro_call_curly, MacroCallArgs::Curly))
        .finish()
}

fn expr_macro_call_paren<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        _    = left_paren;
        args = parse_nested_until(Token::is_left_paren, Token::is_right_paren);
        _    = right_paren;
    }, |_, _| args)
}

fn expr_macro_call_square<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        _    = left_square;
        args = parse_nested_until(Token::is_left_square, Token::is_right_square);
        _    = right_square;
    }, |_, _| args)
}

fn expr_macro_call_curly<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        _    = left_curly;
        args = parse_nested_until(Token::is_left_curly, Token::is_right_curly);
        _    = right_curly;
    }, |_, _| args)
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

fn expr_let<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Let> {
    sequence!(pm, pt, {
        spt     = point;
        _       = kw_let;
        pattern = pattern;
        typ     = optional(expr_let_type);
        value   = optional(expr_let_rhs);
    }, |pm: &mut Master, pt| Let {
        extent: pm.state.ex(spt, pt),
        pattern,
        typ,
        value: value.map(Box::new),
        whitespace: Vec::new(),
    })
}

fn expr_let_type<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Type> {
    sequence!(pm, pt, {
        _   = colon;
        typ = typ;
    }, |_, _| typ)
}

fn expr_let_rhs<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Expression> {
    sequence!(pm, pt, {
        _     = equals;
        value = expression;
    }, |_, _| value)
}

fn expr_if<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, If> {
    sequence!(pm, pt, {
        spt               = point;
        _                 = kw_if;
        (condition, body) = expr_followed_by_block;
        more              = zero_or_more(expr_if_else_if);
        else_body         = optional(expr_if_else_end);
    }, move |pm: &mut Master, pt| If {
        extent: pm.state.ex(spt, pt),
        condition: Box::new(condition),
        body: Box::new(body),
        more,
        else_body: else_body.map(Box::new),
        whitespace: Vec::new(),
    })
}

fn expr_if_else_if<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, If> {
    sequence!(pm, pt, {
        _    = kw_else;
        tail = expr_if;
    }, |_, _| tail)
}

fn expr_if_else_end<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Block> {
    sequence!(pm, pt, {
        _         = kw_else;
        else_body = block;
    }, |_, _| else_body)
}

// To check if a given subexpression should re-allow struct literals,
// test something like this with the official compiler:
//
// ```rust
// fn main() {
//     struct Foo {a: u8}
//     if $parent_expression Foo {a: 42} {}
// }
// ```
//
// In general, anything that is inside some kind of enclosing
// container should re-enable them because it is no longer ambiguous.
fn allow_struct_literals<'s, F, T>(parser: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
    where F: FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    set_ignore_struct_literals(parser, false)
}

fn disallow_struct_literals<'s, F, T>(parser: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
    where F: FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    set_ignore_struct_literals(parser, true)
}

fn set_ignore_struct_literals<'s, F, T>(parser: F, ignore: bool) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
    where F: FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    move |pm, pt| {
        let old = pm.state.ignore_struct_literals;
        pm.state.ignore_struct_literals = ignore;

        let res = parser(pm, pt);

        pm.state.ignore_struct_literals = old;

        res
    }
}

fn expr_followed_by_block<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (Expression, Block)> {
    sequence!(pm, pt, {
        condition = disallow_struct_literals(expression);
        body      = block;
    }, |_, _| (condition, body))
}

fn expr_for_loop<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ForLoop> {
    sequence!(pm, pt, {
        spt          = point;
        label        = optional(loop_label);
        _            = kw_for;
        pattern      = pattern;
        _            = kw_in;
        (iter, body) = expr_followed_by_block;
    }, |pm: &mut Master, pt| ForLoop {
        extent: pm.state.ex(spt, pt),
        label,
        pattern,
        iter: Box::new(iter),
        body: Box::new(body),
        whitespace: Vec::new(),
    })
}

fn loop_label<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Lifetime> {
    sequence!(pm, pt, {
        lifetime = lifetime;
        _        = colon;
    }, |_, _| lifetime)
}

fn expr_loop<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Loop> {
    sequence!(pm, pt, {
        spt   = point;
        label = optional(loop_label);
        _     = kw_loop;
        body  = block;
    }, |pm: &mut Master, pt| Loop { extent: pm.state.ex(spt, pt), label, body: Box::new(body), whitespace: Vec::new() })
}

fn expr_if_let<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, IfLet> {
    sequence!(pm, pt, {
        spt           = point;
        _             = kw_if;
        _             = kw_let;
        pattern       = pattern;
        _             = equals;
        (value, body) = expr_followed_by_block;
    }, |pm: &mut Master, pt| IfLet {
        extent: pm.state.ex(spt, pt),
        pattern,
        value: Box::new(value),
        body: Box::new(body),
        whitespace: Vec::new(),
    })
}

fn expr_while<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, While> {
    sequence!(pm, pt, {
        spt           = point;
        label         = optional(loop_label);
        _             = kw_while;
        (value, body) = expr_followed_by_block;
    }, |pm: &mut Master, pt| While {
        extent: pm.state.ex(spt, pt),
        label,
        value: Box::new(value),
        body: Box::new(body),
        whitespace: Vec::new(),
    })
}

fn expr_while_let<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, WhileLet> {
    sequence!(pm, pt, {
        spt           = point;
        label         = optional(loop_label);
        _             = kw_while;
        _             = kw_let;
        pattern       = pattern;
        _             = equals;
        (value, body) = expr_followed_by_block;
    }, |pm: &mut Master, pt| WhileLet {
        extent: pm.state.ex(spt, pt),
        label,
        pattern,
        value: Box::new(value),
        body: Box::new(body),
        whitespace: Vec::new(),
    })
}

impl ImplicitSeparator for MatchArm {
    fn is_implicit_separator(&self) -> bool {
        match self.hand {
            MatchHand::Brace(..) => true,
            MatchHand::Expression(..) => false,
        }
    }
}

fn expr_match<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Match> {
    sequence!(pm, pt, {
        spt  = point;
        _    = kw_match;
        head = disallow_struct_literals(expression);
        _    = left_curly;
        arms = zero_or_more_implicitly_tailed_values(comma, match_arm);
        _    = right_curly;
    }, |pm: &mut Master, pt| Match { extent: pm.state.ex(spt, pt), head: Box::new(head), arms, whitespace: Vec::new() })
}

fn match_arm<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, MatchArm> {
    sequence!(pm, pt, {
        spt        = point;
        attributes = zero_or_more(attribute);
        pattern    = one_or_more_tailed_values(pipe, pattern);
        guard      = optional(match_arm_guard);
        _          = thick_arrow;
        hand       = match_arm_hand;
    }, |pm: &mut Master, pt| MatchArm { extent: pm.state.ex(spt, pt), attributes, pattern, guard, hand, whitespace: Vec::new() })
}

fn match_arm_guard<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Expression> {
    sequence!(pm, pt, {
        _     = kw_if;
        guard = allow_struct_literals(expression);
    }, |_, _| guard)
}

fn match_arm_hand<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, MatchHand> {
    pm.alternate(pt)
        .one(map(allow_struct_literals(expr_block), |b| MatchHand::Brace(Expression::Block(b))))
        .one(map(allow_struct_literals(expression), MatchHand::Expression))
        .finish()
}

fn expr_tuple_or_parenthetical<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Expression> {
    sequence!(pm, pt, {
        spt    = point;
        _      = left_paren;
        values = allow_struct_literals(zero_or_more_tailed(comma, expression));
        _      = right_paren;
    }, move |pm: &mut Master, pt| {
        let extent = pm.state.ex(spt, pt);
        let values = values;
        let Tailed { mut values, separator_count, .. } = values;
        match (values.len(), separator_count) {
            (1, 0) => Expression::Parenthetical(Parenthetical {
                extent,
                expression: Box::new(values.pop().expect("Must have one parenthesized value")),
            }),
            _ => Expression::Tuple(Tuple {
                extent,
                members: values,
            }),
        }
    })
}

fn expr_array<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Array> {
    pm.alternate(pt)
        .one(map(expr_array_explicit, Array::Explicit))
        .one(map(expr_array_repeated, Array::Repeated))
        .finish()
}

fn expr_array_explicit<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ArrayExplicit> {
    sequence!(pm, pt, {
        spt    = point;
        _      = left_square;
        values = allow_struct_literals(zero_or_more_tailed_values(comma, expression));
        _      = right_square;
    }, |pm: &mut Master, pt| ArrayExplicit { extent: pm.state.ex(spt, pt), values })
}

fn expr_array_repeated<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ArrayRepeated> {
    sequence!(pm, pt, {
        spt   = point;
        _     = left_square;
        value = allow_struct_literals(expression);
        _     = semicolon;
        count = expression;
        _     = right_square;
    }, |pm: &mut Master, pt| ArrayRepeated {
        extent: pm.state.ex(spt, pt),
        value: Box::new(value),
        count: Box::new(count),
        whitespace: Vec::new(),
    })
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

fn expr_byte<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Byte> {
    byte(pm, pt)
        .map(|extent| Byte { extent, value: Character { extent, value: extent } }) // FIXME: value
}

fn expr_byte_string<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ByteString> {
    pm.alternate(pt)
        .one(map(byte_string, |extent| {
            ByteString { extent, value: String { extent, value: extent } }  // FIXME: value
        }))
        .one(map(byte_string_raw, |extent| {
            ByteString { extent, value: String { extent, value: extent } }  // FIXME: value
        }))
        .finish()
}

fn expr_closure<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Closure> {
    sequence!(pm, pt, {
        spt                 = point;
        mov                 = optional(kw_move);
        _                   = pipe;
        args                = zero_or_more_tailed_values(comma, expr_closure_arg);
        _                   = pipe;
        (return_type, body) = expr_closure_return;
    }, |pm: &mut Master, pt| Closure {
        extent: pm.state.ex(spt, pt),
        is_move: mov.is_some(),
        args,
        return_type,
        body: Box::new(body),
        whitespace: Vec::new(),
    })
}

fn expr_closure_arg<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ClosureArg> {
    sequence!(pm, pt, {
        name = pattern;
        typ  = optional(expr_closure_arg_type);
    }, |_, _| ClosureArg { name, typ, whitespace: Vec::new() })
}

fn expr_closure_arg_type<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Type> {
    sequence!(pm, pt, {
        _   = colon;
        typ = typ;
    }, |_, _| typ)
}

fn expr_closure_return<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (Option<Type>, Expression)> {
    pm.alternate(pt)
        .one(expr_closure_return_explicit)
        .one(expr_closure_return_inferred)
        .finish()
}

fn expr_closure_return_explicit<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (Option<Type>, Expression)> {
    sequence!(pm, pt, {
        _    = thin_arrow;
        typ  = typ;
        body = expr_closure_return_body;
    }, |_, _| (Some(typ), body))
}

fn expr_closure_return_body<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Expression> {
    pm.alternate(pt)
        .one(expr_tuple_or_parenthetical)
        .one(map(expr_block, Expression::Block))
        .finish()
}

fn expr_closure_return_inferred<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (Option<Type>, Expression)> {
    map(expression, |body| (None, body))(pm, pt)
}

fn expr_return<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Return> {
    sequence!(pm, pt, {
        spt   = point;
        _     = kw_return;
        value = optional(expression);
    }, |pm: &mut Master, pt| Return {
        extent: pm.state.ex(spt, pt),
        value: value.map(Box::new),
        whitespace: Vec::new(),
    })
}

fn expr_continue<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Continue> {
    sequence!(pm, pt, {
        spt   = point;
        _     = kw_continue;
        label = optional(lifetime);
    }, |pm: &mut Master, pt| Continue { extent: pm.state.ex(spt, pt), label, whitespace: Vec::new() })
}

fn expr_break<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Break> {
    sequence!(pm, pt, {
        spt   = point;
        _     = kw_break;
        label = optional(lifetime);
        value = optional(expression);
    }, |pm: &mut Master, pt| Break {
        extent: pm.state.ex(spt, pt),
        label,
        value: value.map(Box::new),
        whitespace: Vec::new(),
    })
}

fn expr_block<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Box<Block>> {
    block(pm, pt).map(Box::new)
}

fn expr_unsafe_block<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, UnsafeBlock> {
    sequence!(pm, pt, {
        spt  = point;
        _    = kw_unsafe;
        body = block;
    }, |pm: &mut Master, pt| UnsafeBlock { extent: pm.state.ex(spt, pt), body: Box::new(body), whitespace: Vec::new() })
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

fn expr_value<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Value> {
    if pm.state.ignore_struct_literals {
        sequence!(pm, pt, {
            spt  = point;
            name = pathed_ident;
        }, |pm: &mut Master, pt| Value { extent: pm.state.ex(spt, pt), name, literal: None })
    } else {
        sequence!(pm, pt, {
            spt     = point;
            name    = pathed_ident;
            literal = optional(expr_value_struct_literal);
        }, |pm: &mut Master, pt| Value { extent: pm.state.ex(spt, pt), name, literal })
    }
}

fn expr_value_struct_literal<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, StructLiteral> {
    sequence!(pm, pt, {
        spt    = point;
        _      = left_curly;
        fields = zero_or_more_tailed_values(comma, expr_value_struct_literal_field);
        splat  = optional(expr_value_struct_literal_splat);
        _      = right_curly;
    }, |pm: &mut Master, pt| StructLiteral {
        extent: pm.state.ex(spt, pt),
        fields,
        splat: splat.map(Box::new),
        whitespace: Vec::new(),
    })
}

fn expr_value_struct_literal_field<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, StructLiteralField> {
    sequence!(pm, pt, {
        spt   = point;
        name  = ident;
        mpt   = point;
        value = optional(expr_value_struct_literal_field_value);
    }, |pm: &mut Master, _| {
        let value = value.unwrap_or_else(|| Expression::Value(Value {
            extent: pm.state.ex(spt, mpt),
            name: name.into(),
            literal: None,
        }));
        StructLiteralField { name, value, whitespace: Vec::new() }
    })
}

fn expr_value_struct_literal_field_value<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, Expression>
{
    sequence!(pm, pt, {
        _     = colon;
        value = allow_struct_literals(expression);
    }, |_, _| value)
}

fn expr_value_struct_literal_splat<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, Expression>
{
    sequence!(pm, pt, {
        _     = double_period;
        value = allow_struct_literals(expression);
    }, |_, _| value)
}

fn expr_disambiguation<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Disambiguation> {
    sequence!(pm, pt, {
        spt        = point;
        core       = disambiguation_core;
        components = zero_or_more_tailed_values_resume(double_colon, path_component);
    }, move |pm: &mut Master, pt| Disambiguation {
        extent: pm.state.ex(spt, pt),
        from_type: core.from_type,
        to_type: core.to_type,
        components,
        whitespace: core.whitespace,
    })
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
        members = zero_or_more_tailed_values(comma, pattern_bundle_member);
        _       = right_paren;
    }, |pm: &mut Master, pt| PatternTuple { extent: pm.state.ex(spt, pt), members })
}

fn pattern_slice<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, PatternSlice> {
    sequence!(pm, pt, {
        spt     = point;
        _       = left_square;
        members = zero_or_more_tailed_values(comma, pattern_bundle_member);
        _       = right_square;
    }, |pm: &mut Master, pt| PatternSlice { extent: pm.state.ex(spt, pt), members })
}

fn pattern_bundle_member<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, PatternBundleMember>
{
    pm.alternate(pt)
        .one(map(pattern, PatternBundleMember::Pattern))
        .one(map(ext(double_period), PatternBundleMember::Wildcard))
        .finish()
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

fn pattern_range_inclusive<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, PatternRangeInclusive>
{
    sequence!(pm, pt, {
        spt   = point;
        start = pattern_range_component;
        _     = triple_period;
        end   = pattern_range_component;
    }, |pm: &mut Master, pt| PatternRangeInclusive {
        extent: pm.state.ex(spt, pt),
        start,
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
        fields = zero_or_more_tailed_values(comma, struct_defn_field);
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

fn struct_defn_body_tuple_only<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<StructDefinitionFieldUnnamed>> {
    sequence!(pm, pt, {
        _     = left_paren;
        types = zero_or_more_tailed_values(comma, tuple_defn_field);
        _     = right_paren;
    }, |_, _| types)
}

fn tuple_defn_field<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, StructDefinitionFieldUnnamed> {
    sequence!(pm, pt, {
        spt        = point;
        attributes = zero_or_more(attribute);
        visibility = optional(visibility);
        typ        = typ;
    }, |pm: &mut Master, pt| StructDefinitionFieldUnnamed {
        extent: pm.state.ex(spt, pt),
        attributes,
        visibility,
        typ,
    })
}

fn struct_defn_field<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, StructDefinitionFieldNamed> {
    sequence!(pm, pt, {
        spt        = point;
        attributes = zero_or_more(attribute);
        visibility = optional(visibility);
        name       = ident;
        _          = colon;
        typ        = typ;
    }, |pm: &mut Master, pt| StructDefinitionFieldNamed {
        extent: pm.state.ex(spt, pt),
        visibility,
        attributes,
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
        fields     = zero_or_more_tailed_values(comma, struct_defn_field);
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
        variants   = zero_or_more_tailed_values(comma, enum_variant);
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
        spt        = point;
        attributes = zero_or_more(attribute);
        name       = ident;
        body       = enum_variant_body;
    }, |pm: &mut Master, pt| EnumVariant { extent: pm.state.ex(spt, pt), attributes, name, body, whitespace: Vec::new() })
}

fn enum_variant_body<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, EnumVariantBody> {
    pm.alternate(pt)
        .one(map(struct_defn_body_tuple_only, EnumVariantBody::Tuple))
        .one(map(struct_defn_body_brace_only, EnumVariantBody::Struct))
        .one(map(optional(enum_discriminant), EnumVariantBody::Unit))
        .finish()
}

fn enum_discriminant<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Expression> {
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
        _          = kw_trait;
        name       = ident;
        generics   = optional(generic_declarations);
        bounds     = optional(generic_declaration_type_bounds);
        wheres     = optional(where_clause);
        _          = left_curly;
        members    = zero_or_more(trait_impl_member);
        _          = right_curly;
    }, |pm: &mut Master, pt| Trait {
        extent: pm.state.ex(spt, pt),
        visibility,
        is_unsafe,
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
        .one(map(attribute, TraitMember::Attribute))
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

fn trait_member_const_value<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Expression> {
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
        qualifiers  = function_qualifiers;
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
        body      = zero_or_more(impl_member);
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
        .one(map(attribute, ImplMember::Attribute))
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
        spt   = point;
        _     = kw_const;
        name  = ident;
        _     = colon;
        typ   = typ;
        _     = equals;
        value = expression;
        _     = semicolon;
    }, |pm: &mut Master, pt| ImplConst { extent: pm.state.ex(spt, pt), name, typ, value, whitespace: Vec::new() })
}

// TODO: optional could take E that is `into`, or just a different one

fn attribute<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Attribute> {
    sequence!(pm, pt, {
        spt           = point;
        _             = hash;
        is_containing = optional(ext(bang));
        _             = left_square;
        text          = parse_nested_until(Token::is_left_square, Token::is_right_square);
        _             = right_square;
    }, |pm: &mut Master, pt| Attribute { extent: pm.state.ex(spt, pt), is_containing, text })
}

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
        members = zero_or_more(extern_block_member);
        _       = right_curly;
    }, |pm: &mut Master, pt| ExternBlock { extent: pm.state.ex(spt, pt), abi, members, whitespace: Vec::new() })
}

fn extern_block_member<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ExternBlockMember> {
    pm.alternate(pt)
        .one(map(attribute, ExternBlockMember::Attribute))
        .one(map(extern_block_static, ExternBlockMember::Static))
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
        path       = zero_or_more(use_path_component);
        tail       = use_tail;
        _          = semicolon;
    }, move |pm: &mut Master, pt| {
        Use { extent: pm.state.ex(spt, pt), visibility, path, tail, whitespace: Vec::new() }
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
        names = zero_or_more_tailed_values(comma, use_tail_ident);
        _     = right_curly;
    }, |pm: &mut Master, pt| UseTailMulti { extent: pm.state.ex(spt, pt), names })
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

fn module_body_or_not<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Option<Vec<Item>>> {
    pm.alternate(pt)
        .one(map(module_body, Some))
        .one(map(semicolon, |_| None))
        .finish()
}

fn module_body<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Vec<Item>> {
    sequence!(pm, pt, {
        _    = left_curly;
        body = zero_or_more(item);
        _    = right_curly;
    }, |_, _| body)
}

fn typ<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Type> {
    pm.alternate(pt)
        .one(map(typ_array, Type::Array))
        .one(map(typ_combination, Type::Combination))
        .one(map(typ_function, Type::Function))
        .one(map(typ_pointer, Type::Pointer))
        .one(map(typ_reference, Type::Reference))
        .one(map(typ_slice, Type::Slice))
        .one(map(typ_tuple, Type::Tuple))
        .one(map(ext(bang), Type::Uninhabited))
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
        _         = kw_for;
        _         = left_angle;
        lifetimes = zero_or_more_tailed_values(comma, lifetime);
        _         = right_angle;
        child     = typ_higher_ranked_trait_bounds_child;
    }, |pm: &mut Master, pt| TypeHigherRankedTraitBounds { extent: pm.state.ex(spt, pt), lifetimes, child, whitespace: Vec::new() })
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

fn typ_combination<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, TypeCombination> {
    sequence!(pm, pt, {
        spt        = point;
        base       = typ_combination_base;
        additional = zero_or_more_tailed_values_resume(plus, typ_combination_additional);
    }, move |pm: &mut Master, pt| TypeCombination { extent: pm.state.ex(spt, pt), base, additional })
}

fn typ_combination_base<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, TypeCombinationBase>
{
    pm.alternate(pt)
        .one(map(typ_disambiguation, TypeCombinationBase::Disambiguation))
        .one(map(typ_named, TypeCombinationBase::Named))
        .one(map(typ_higher_ranked_trait_bounds, TypeCombinationBase::HigherRankedTraitBounds))
        .one(map(typ_impl_trait, TypeCombinationBase::ImplTrait))
        .finish()
}

fn typ_combination_additional<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, TypeCombinationAdditional>
{
    pm.alternate(pt)
        .one(map(typ_named, TypeCombinationAdditional::Named))
        .one(map(lifetime, TypeCombinationAdditional::Lifetime))
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
        qualifiers  = function_qualifiers;
        _           = kw_fn;
        arguments   = trait_impl_function_arglist; // TODO: shouldn't allow `self`
        return_type = optional(function_return_type);
    }, |pm: &mut Master, pt| TypeFunction {
        extent: pm.state.ex(spt, pt),
        qualifiers,
        arguments,
        return_type: return_type.map(Box::new),
        whitespace: Vec::new(),
    })
}


fn lifetime<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Lifetime> {
    lifetime_normal(pm, pt)
        .map(|extent| Lifetime { extent: extent, name: Ident { extent } })
    // FIXME: value; can we actually have whitespace here?
}

#[cfg(test)]
mod test_utils {
    use super::*;

    type TestResult<T> = Result<(usize, T), (usize, Vec<Error>)>;

    pub fn qp<'s, F, T>(f: F, s: &'s str) -> TestResult<T>
        where F: for<'a> FnOnce(&mut Master<'a>, Point<'a>) -> Progress<'a, T>
    {
        // TODO: Master::once()?
        let tokens = Tokens::new(s).collect::<Result<Vec<_>, _>>().expect("Unable to tokenize");
        let (_ws, tokens): (Vec<_>, Vec<_>) = tokens.into_iter().partition(|t| {
            t.is_whitespace() || t.is_comment() || t.is_doc_comment() || t.is_comment_block() || t.is_doc_comment_block()
        });

        let mut pm = Master::with_state(State::new(&tokens));
        let pt = Point::new(&tokens);
        let r = f(&mut pm, pt);
        match pm.finish(r) {
            peresil::Progress { status: peresil::Status::Success(v), point } => {
                Ok((point.offset, v))
            }
            peresil::Progress { status: peresil::Status::Failure(v), point } => {
                Err((point.offset, v))
            }
        }
    }

    pub fn unwrap_progress<T>(p: TestResult<T>) -> T {
        match p {
            Ok((_, v)) => v,
            Err((offset, e)) => panic!("Failed parsing at token at index {}: {:?}", offset, e),
        }
    }

    pub fn unwrap_progress_err<T>(p: TestResult<T>) -> (usize, Vec<Error>) {
        match p {
            Ok(_) => panic!("Parsing should have failed, but it did not"),
            Err(x) => x
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use test_utils::*;

    #[test]
    fn parse_use() {
        let p = qp(p_use, "use foo::Bar;");
        assert_eq!(unwrap_progress(p).extent, (0, 13))
    }

    #[test]
    fn parse_use_public() {
        let p = qp(p_use, "pub use foo::Bar;");
        assert_eq!(unwrap_progress(p).extent, (0, 17))
    }

    #[test]
    fn parse_use_glob() {
        let p = qp(p_use, "use foo::*;");
        assert_eq!(unwrap_progress(p).extent, (0, 11))
    }

    #[test]
    fn parse_use_with_multi() {
        let p = qp(p_use, "use foo::{Bar, Baz};");
        assert_eq!(unwrap_progress(p).extent, (0, 20))
    }

    #[test]
    fn parse_use_no_path() {
        let p = qp(p_use, "use {Bar, Baz};");
        assert_eq!(unwrap_progress(p).extent, (0, 15))
    }

    #[test]
    fn parse_use_absolute_path() {
        let p = qp(p_use, "use ::{Bar, Baz};");
        assert_eq!(unwrap_progress(p).extent, (0, 17))
    }

    #[test]
    fn parse_use_rename() {
        let p = qp(p_use, "use foo as bar;");
        assert_eq!(unwrap_progress(p).extent, (0, 15))
    }

    #[test]
    fn parse_use_with_multi_rename() {
        let p = qp(p_use, "use foo::{bar as a, baz as b};");
        assert_eq!(unwrap_progress(p).extent, (0, 30))
    }

    #[test]
    fn parse_use_all_space() {
        let p = qp(p_use, "use foo :: { bar as a , baz as b } ;");
        assert_eq!(unwrap_progress(p).extent, (0, 36))
    }

    #[test]
    fn item_mod_multiple() {
        let p = qp(item, "mod foo { use super::*; }");
        assert_eq!(unwrap_progress(p).extent(), (0, 25))
    }

    #[test]
    fn item_macro_call_with_parens() {
        let p = qp(item, "foo!();");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn item_macro_call_with_square_brackets() {
        let p = qp(item, "foo![];");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn item_macro_call_with_curly_braces() {
        let p = qp(item, "foo! { }");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn item_macro_call_with_ident() {
        let p = qp(item, "macro_rules! name { }");
        assert_eq!(unwrap_progress(p).extent(), (0, 21))
    }

    #[test]
    fn item_macro_call_all_space() {
        let p = qp(item, "foo ! bar [ ] ;");
        assert_eq!(unwrap_progress(p).extent(), (0, 15))
    }

    #[test]
    fn item_mod() {
        let p = qp(module, "mod foo { }");
        assert_eq!(unwrap_progress(p).extent, (0, 11))
    }

    #[test]
    fn item_mod_public() {
        let p = qp(module, "pub mod foo;");
        assert_eq!(unwrap_progress(p).extent, (0, 12))
    }

    #[test]
    fn item_mod_another_file() {
        let p = qp(module, "mod foo;");
        assert_eq!(unwrap_progress(p).extent, (0, 8))
    }

    #[test]
    fn item_trait() {
        let p = qp(item, "trait Foo {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 12))
    }

    #[test]
    fn item_trait_public() {
        let p = qp(item, "pub trait Foo {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 16))
    }

    #[test]
    fn item_trait_unsafe() {
        let p = qp(item, "unsafe trait Foo {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 19))
    }

    #[test]
    fn item_trait_with_generics() {
        let p = qp(item, "trait Foo<T> {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 15))
    }

    #[test]
    fn item_trait_with_members() {
        let p = qp(item, "trait Foo { fn bar(&self) -> u8; }");
        assert_eq!(unwrap_progress(p).extent(), (0, 34))
    }

    #[test]
    fn item_trait_with_members_with_patterns() {
        let p = qp(item, "trait Foo { fn bar(&self, &a: &u8) -> u8; }");
        assert_eq!(unwrap_progress(p).extent(), (0, 43))
    }

    #[test]
    fn item_trait_with_members_with_body() {
        let p = qp(item, "trait Foo { fn bar(&self) -> u8 { 42 } }");
        assert_eq!(unwrap_progress(p).extent(), (0, 40))
    }

    #[test]
    fn item_trait_with_unnamed_parameters() {
        let p = qp(item, "trait Foo { fn bar(&self, u8); }");
        assert_eq!(unwrap_progress(p).extent(), (0, 32))
    }

    #[test]
    fn item_trait_with_qualified_function() {
        let p = qp(item, r#"trait Foo { const unsafe extern "C" fn bar(); }"#);
        assert_eq!(unwrap_progress(p).extent(), (0, 47))
    }

    #[test]
    fn item_trait_with_associated_type() {
        let p = qp(item, "trait Foo { type Bar; }");
        assert_eq!(unwrap_progress(p).extent(), (0, 23))
    }

    #[test]
    fn item_trait_with_associated_type_with_bounds() {
        let p = qp(item, "trait Foo { type Bar: Baz; }");
        assert_eq!(unwrap_progress(p).extent(), (0, 28))
    }

    #[test]
    fn item_trait_with_associated_type_with_default() {
        let p = qp(item, "trait Foo { type Bar = (); }");
        assert_eq!(unwrap_progress(p).extent(), (0, 28))
    }

    #[test]
    fn item_trait_with_associated_type_with_bounds_and_default() {
        let p = qp(item, "trait Foo { type Bar: Baz = (); }");
        assert_eq!(unwrap_progress(p).extent(), (0, 33))
    }

    #[test]
    fn item_trait_with_associated_const() {
        let p = qp(item, "trait Foo { const Bar: u8; }");
        assert_eq!(unwrap_progress(p).extent(), (0, 28))
    }

    #[test]
    fn item_trait_with_associated_const_with_default() {
        let p = qp(item, "trait Foo { const Bar: u8 = 42; }");
        assert_eq!(unwrap_progress(p).extent(), (0, 33))
    }

    #[test]
    fn item_trait_with_supertraits() {
        let p = qp(item, "trait Foo: Bar + Baz {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 23))
    }

    #[test]
    fn item_trait_with_where_clause() {
        let p = qp(item, "trait Foo where A: B {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 23))
    }

    #[test]
    fn item_trait_all_space() {
        let p = qp(item, "trait Foo : Bar { type A : B ; fn a ( a : u8) -> u8 { a } }");
        assert_eq!(unwrap_progress(p).extent(), (0, 59))
    }

    #[test]
    fn item_type_alias() {
        let p = qp(item, "type Foo<T> = Bar<T, u8>;");
        assert_eq!(unwrap_progress(p).extent(), (0, 25))
    }

    #[test]
    fn item_type_alias_public() {
        let p = qp(item, "pub type Foo<T> = Bar<T, u8>;");
        assert_eq!(unwrap_progress(p).extent(), (0, 29))
    }

    #[test]
    fn item_type_alias_with_trait_bounds() {
        let p = qp(item, "type X<T: Foo> where T: Bar = Option<T>;");
        assert_eq!(unwrap_progress(p).extent(), (0, 40))
    }

    #[test]
    fn item_const() {
        let p = qp(item, r#"const FOO: &'static str = "hi";"#);
        assert_eq!(unwrap_progress(p).extent(), (0, 31))
    }

    #[test]
    fn item_const_public() {
        let p = qp(item, "pub const FOO: u8 = 42;");
        assert_eq!(unwrap_progress(p).extent(), (0, 23))
    }

    #[test]
    fn item_static() {
        let p = qp(item, r#"static FOO: &'static str = "hi";"#);
        assert_eq!(unwrap_progress(p).extent(), (0, 32))
    }

    #[test]
    fn item_static_mut() {
        let p = qp(item, r#"static mut FOO: &'static str = "hi";"#);
        assert_eq!(unwrap_progress(p).extent(), (0, 36))
    }

    #[test]
    fn item_static_public() {
        let p = qp(item, "pub static FOO: u8 = 42;");
        assert_eq!(unwrap_progress(p).extent(), (0, 24))
    }

    #[test]
    fn item_extern_crate() {
        let p = qp(item, "extern crate foo;");
        assert_eq!(unwrap_progress(p).extent(), (0, 17))
    }

    #[test]
    fn item_extern_crate_public() {
        let p = qp(item, "pub extern crate foo;");
        assert_eq!(unwrap_progress(p).extent(), (0, 21))
    }

    #[test]
    fn item_extern_crate_rename() {
        let p = qp(item, "extern crate foo as bar;");
        assert_eq!(unwrap_progress(p).extent(), (0, 24))
    }

    #[test]
    fn item_extern_block() {
        let p = qp(item, r#"extern {}"#);
        assert_eq!(unwrap_progress(p).extent(), (0, 9))
    }

    #[test]
    fn item_extern_block_with_abi() {
        let p = qp(item, r#"extern "C" {}"#);
        assert_eq!(unwrap_progress(p).extent(), (0, 13))
    }

    #[test]
    fn item_extern_block_with_fn() {
        let p = qp(item, r#"extern { fn foo(bar: u8) -> bool; }"#);
        assert_eq!(unwrap_progress(p).extent(), (0, 35))
    }

    #[test]
    fn item_extern_block_with_variadic_fn() {
        let p = qp(item, r#"extern { fn foo(bar: u8, ...) -> bool; }"#);
        assert_eq!(unwrap_progress(p).extent(), (0, 40))
    }

    #[test]
    fn item_extern_block_with_fn_and_generics() {
        let p = qp(item, r#"extern { fn foo<A, B>(bar: A) -> B; }"#);
        assert_eq!(unwrap_progress(p).extent(), (0, 37))
    }

    #[test]
    fn item_extern_block_with_attribute() {
        let p = qp(item, r#"extern { #[wow] }"#);
        assert_eq!(unwrap_progress(p).extent(), (0, 17))
    }

    #[test]
    fn item_extern_block_with_static() {
        let p = qp(item, r#"extern { static FOO: u32; }"#);
        assert_eq!(unwrap_progress(p).extent(), (0, 27))
    }

    #[test]
    fn item_extern_block_with_static_and_qualifiers() {
        let p = qp(item, r#"extern { pub static mut FOO: u32; }"#);
        assert_eq!(unwrap_progress(p).extent(), (0, 35))
    }

    #[test]
    fn impl_without_trait() {
        let p = qp(p_impl, "impl Bar {}");
        assert_eq!(unwrap_progress(p).extent, (0, 11))
    }

    #[test]
    fn impl_with_trait() {
        let p = qp(p_impl, "impl Foo for Bar {}");
        assert_eq!(unwrap_progress(p).extent, (0, 19))
    }

    #[test]
    fn impl_with_negative_trait() {
        let p = qp(p_impl, "impl !Foo for Bar {}");
        assert_eq!(unwrap_progress(p).extent, (0, 20))
    }

    #[test]
    fn impl_trait_with_wildcard_type() {
        let p = qp(p_impl, "impl Foo for .. {}");
        assert_eq!(unwrap_progress(p).extent, (0, 18))
    }

    #[test]
    fn impl_with_generics() {
        let p = qp(p_impl, "impl<'a, T> Foo<'a, T> for Bar<'a, T> {}");
        assert_eq!(unwrap_progress(p).extent, (0, 40))
    }

    #[test]
    fn impl_with_generics_no_space() {
        let p = qp(p_impl, "impl<'a,T>Foo<'a,T>for Bar<'a,T>{}");
        assert_eq!(unwrap_progress(p).extent, (0, 34))
    }

    #[test]
    fn impl_with_trait_bounds() {
        let p = qp(p_impl, "impl<T> Foo for Bar<T> where T: Quux {}");
        assert_eq!(unwrap_progress(p).extent, (0, 39))
    }

    #[test]
    fn impl_with_attribute() {
        let p = qp(p_impl, "impl Foo { #[attribute] fn bar() {} }");
        assert_eq!(unwrap_progress(p).extent, (0, 37))
    }

    #[test]
    fn impl_with_attributes() {
        let p = qp(p_impl, "impl Foo { #[a] #[b] fn bar() {} }");
        assert_eq!(unwrap_progress(p).extent, (0, 34))
    }

    #[test]
    fn impl_with_associated_type() {
        let p = qp(p_impl, "impl Foo { type A = B; }");
        assert_eq!(unwrap_progress(p).extent, (0, 24))
    }

    #[test]
    fn impl_with_associated_const() {
        let p = qp(p_impl, "impl Foo { const A: i32 = 42; }");
        assert_eq!(unwrap_progress(p).extent, (0, 31))
    }

    #[test]
    fn impl_with_unsafe() {
        let p = qp(p_impl, "unsafe impl Foo {}");
        assert_eq!(unwrap_progress(p).extent, (0, 18))
    }

    #[test]
    fn impl_with_macro_call() {
        let p = qp(p_impl, "impl Foo { bar!(); }");
        assert_eq!(unwrap_progress(p).extent, (0, 20))
    }

    #[test]
    fn enum_with_trailing_stuff() {
        let p = qp(p_enum, "enum A {} impl Foo for Bar {}");
        assert_eq!(unwrap_progress(p).extent, (0, 9))
    }

    #[test]
    fn enum_with_generic_types() {
        let p = qp(p_enum, "enum A { Foo(Vec<u8>) }");
        assert_eq!(unwrap_progress(p).extent, (0, 23))
    }

    #[test]
    fn enum_with_generic_declarations() {
        let p = qp(p_enum, "enum A<T> { Foo(Vec<T>) }");
        assert_eq!(unwrap_progress(p).extent, (0, 25))
    }

    #[test]
    fn enum_with_struct_variant() {
        let p = qp(p_enum, "enum A { Foo { a: u8 } }");
        assert_eq!(unwrap_progress(p).extent, (0, 24))
    }

    #[test]
    fn enum_with_attribute() {
        let p = qp(p_enum, "enum Foo { #[attr] A(u8)}");
        assert_eq!(unwrap_progress(p).extent, (0, 25))
    }

    #[test]
    fn enum_with_attribute_on_value() {
        let p = qp(p_enum, "enum Foo { A(#[attr] u8) }");
        assert_eq!(unwrap_progress(p).extent, (0, 26))
    }

    #[test]
    fn enum_with_discriminant() {
        let p = qp(p_enum, "enum Foo { A = 1, B = 2 }");
        assert_eq!(unwrap_progress(p).extent, (0, 25))
    }

    #[test]
    fn enum_with_where_clause() {
        let p = qp(p_enum, "enum Foo<A> where A: Bar { Z }");
        assert_eq!(unwrap_progress(p).extent, (0, 30))
    }

    #[test]
    fn enum_public() {
        let p = qp(p_enum, "pub enum A {}");
        assert_eq!(unwrap_progress(p).extent, (0, 13))
    }

    #[test]
    fn fn_with_public_modifier() {
        let p = qp(function_header, "pub fn foo()");
        assert_eq!(unwrap_progress(p).extent, (0, 12))
    }

    #[test]
    fn fn_with_const_modifier() {
        let p = qp(function_header, "const fn foo()");
        assert_eq!(unwrap_progress(p).extent, (0, 14))
    }

    #[test]
    fn fn_with_extern_modifier() {
        let p = qp(function_header, "extern fn foo()");
        assert_eq!(unwrap_progress(p).extent, (0, 15))
    }

    #[test]
    fn fn_with_extern_modifier_and_abi() {
        let p = qp(function_header, r#"extern "C" fn foo()"#);
        assert_eq!(unwrap_progress(p).extent, (0, 19))
    }

    #[test]
    fn fn_with_self_type_reference() {
        let p = qp(function_header, "fn foo(&self)");
        assert_eq!(unwrap_progress(p).extent, (0, 13))
    }

    #[test]
    fn fn_with_self_type_value() {
        let p = qp(function_header, "fn foo(self)");
        assert_eq!(unwrap_progress(p).extent, (0, 12))
    }

    #[test]
    fn fn_with_self_type_value_mut() {
        let p = qp(function_header, "fn foo(mut self)");
        assert_eq!(unwrap_progress(p).extent, (0, 16))
    }

    #[test]
    fn fn_with_self_type_reference_mut() {
        let p = qp(function_header, "fn foo(&mut self)");
        assert_eq!(unwrap_progress(p).extent, (0, 17))
    }

    #[test]
    fn fn_with_self_type_with_lifetime() {
        let p = qp(function_header, "fn foo<'a>(&'a self)");
        assert_eq!(unwrap_progress(p).extent, (0, 20))
    }

    #[test]
    fn fn_with_self_type_and_regular() {
        let p = qp(function_header, "fn foo(&self, a: u8)");
        assert_eq!(unwrap_progress(p).extent, (0, 20))
    }

    #[test]
    fn fn_with_self_type_explicit_type() {
        let p = qp(function_header, "fn foo(self: &mut Self)");
        assert_eq!(unwrap_progress(p).extent, (0, 23))
    }

    #[test]
    fn fn_with_self_type_explicit_type_mutable() {
        let p = qp(function_header, "fn foo(mut self: &mut Self)");
        assert_eq!(unwrap_progress(p).extent, (0, 27))
    }

    #[test]
    fn fn_with_argument() {
        let p = qp(function_header, "fn foo(a: u8)");
        assert_eq!(unwrap_progress(p).extent, (0, 13))
    }

    #[test]
    fn fn_with_arguments_all_space() {
        let p = qp(function_header, "fn foo ( a : u8 )");
        assert_eq!(unwrap_progress(p).extent, (0, 17))
    }

    #[test]
    fn fn_with_argument_with_generic() {
        let p = qp(function_header, "fn foo(a: Vec<u8>)");
        assert_eq!(unwrap_progress(p).extent, (0, 18))
    }

    #[test]
    fn fn_with_arguments() {
        let p = qp(function_header, "fn foo(a: u8, b: u8)");
        assert_eq!(unwrap_progress(p).extent, (0, 20))
    }

    #[test]
    fn fn_with_arguments_with_patterns() {
        let p = qp(function_header, "fn foo(&a: &u8)");
        assert_eq!(unwrap_progress(p).extent, (0, 15))
    }

    #[test]
    fn fn_with_return_type() {
        let p = qp(function_header, "fn foo() -> bool");
        assert_eq!(unwrap_progress(p).extent, (0, 16))
    }

    #[test]
    fn fn_with_generics() {
        let p = qp(function_header, "fn foo<A, B>()");
        assert_eq!(unwrap_progress(p).extent, (0, 14))
    }

    #[test]
    fn fn_with_lifetimes() {
        let p = qp(function_header, "fn foo<'a, 'b>()");
        assert_eq!(unwrap_progress(p).extent, (0, 16))
    }

    #[test]
    fn fn_with_lifetimes_and_generics() {
        let p = qp(function_header, "fn foo<'a, T>()");
        assert_eq!(unwrap_progress(p).extent, (0, 15))
    }

    #[test]
    fn fn_with_whitespace_before_arguments() {
        let p = qp(function_header, "fn foo () -> ()");
        assert_eq!(unwrap_progress(p).extent, (0, 15))
    }

    #[test]
    fn fn_with_whitespace_before_generics() {
        let p = qp(function_header, "fn foo <'a, T>() -> ()");
        assert_eq!(unwrap_progress(p).extent, (0, 22))
    }

    #[test]
    fn fn_with_unsafe_qualifier() {
        let p = qp(function_header, "unsafe fn foo()");
        assert_eq!(unwrap_progress(p).extent, (0, 15))
    }

    #[test]
    fn block_with_multiple_implicit_statement_macro_calls() {
        let p = qp(block, "{ a! {} b! {} }");
        assert_eq!(unwrap_progress(p).extent, (0, 15));
    }

    #[test]
    fn block_promotes_implicit_statement_to_expression() {
        let p = qp(block, "{ if a {} }");
        let p = unwrap_progress(p);
        assert!(p.statements.is_empty());
        assert_eq!(p.expression.unwrap().extent(), (2, 9));
    }

    #[test]
    fn block_with_multiple_empty_statements() {
        let p = qp(block, "{ ; ; ; }");
        assert_eq!(unwrap_progress(p).extent, (0, 9));
    }

    #[test]
    fn statement_match_no_semicolon() {
        let p = qp(statement, "match a { _ => () }");
        assert_eq!(unwrap_progress(p).into_expression().unwrap().extent(), (0, 19))
    }

    #[test]
    fn statement_use() {
        let p = qp(statement, "use foo::Bar;");
        assert_eq!(unwrap_progress(p).extent(), (0, 13))
    }

    #[test]
    fn statement_any_item() {
        let p = qp(statement, "struct Foo {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 13))
    }

    #[test]
    fn statement_braced_expression_followed_by_method() {
        let p = qp(statement, "match 1 { _ => 1u8 }.count_ones()");
        assert_eq!(unwrap_progress(p).extent(), (0, 33))
    }

    #[test]
    fn expr_followed_by_block_disallows_struct_literal() {
        let p = qp(expr_followed_by_block, "a {}");
        let (e, b) = unwrap_progress(p);
        assert_eq!(e.extent(), (0, 1));
        assert_eq!(b.extent, (2, 4));
    }

    #[test]
    fn expr_followed_by_block_with_compound_condition() {
        let p = qp(expr_followed_by_block, "a && b {}");
        let (e, b) = unwrap_progress(p);
        assert_eq!(e.extent(), (0, 6));
        assert_eq!(b.extent, (7, 9));
    }

    #[test]
    fn expr_followed_by_block_with_parenthesized_struct_literal() {
        let p = qp(expr_followed_by_block, "(a {}) {}");
        let (e, b) = unwrap_progress(p);
        assert_eq!(e.extent(), (0, 6));
        let p = e.into_parenthetical().unwrap();
        assert!(p.expression.is_value());
        assert_eq!(b.extent, (7, 9));
    }

    #[test]
    fn pathed_ident_with_leading_separator() {
        let p = qp(pathed_ident, "::foo");
        assert_eq!(unwrap_progress(p).extent, (0, 5))
    }

    #[test]
    fn pathed_ident_with_turbofish() {
        let p = qp(pathed_ident, "foo::<Vec<u8>>");
        assert_eq!(unwrap_progress(p).extent, (0, 14))
    }

    #[test]
    fn pathed_ident_with_turbofish_with_lifetime() {
        let p = qp(pathed_ident, "StructWithLifetime::<'a, u8>");
        assert_eq!(unwrap_progress(p).extent, (0, 28))
    }

    #[test]
    fn pathed_ident_all_space() {
        let p = qp(pathed_ident, "foo :: < Vec < u8 > , Option < bool > >");
        assert_eq!(unwrap_progress(p).extent, (0, 39))
    }

    #[test]
    fn number_decimal_cannot_start_with_underscore() {
        let p = qp(number_literal, "_123");
        let (err_loc, errs) = unwrap_progress_err(p);
        assert_eq!(err_loc, 0);
        assert!(errs.contains(&Error::ExpectedNumber));
    }

    #[test]
    fn number_with_exponent() {
        let p = qp(number_literal, "1e2");
        assert_eq!(unwrap_progress(p).extent(), (0, 3))
    }

    #[test]
    fn number_with_prefix_and_exponent() {
        let p = qp(number_literal, "0x1e2");
        assert_eq!(unwrap_progress(p).extent(), (0, 5))
    }

    #[test]
    fn number_with_fractional() {
        let p = qp(number_literal, "1.2");
        assert_eq!(unwrap_progress(p).extent(), (0, 3))
    }

    #[test]
    fn number_with_fractional_with_suffix() {
        let p = qp(number_literal, "1.2f32");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn number_with_prefix_and_fractional() {
        let p = qp(number_literal, "0x1.2");
        assert_eq!(unwrap_progress(p).extent(), (0, 5))
    }

    #[test]
    fn number_with_prefix_exponent_and_fractional() {
        let p = qp(number_literal, "0o7.3e9");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn number_with_prefix_can_have_underscore_after_prefix() {
        let p = qp(number_literal, "0x_123");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn number_binary_can_have_suffix() {
        let p = qp(number_literal, "0b111u8");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn number_decimal_can_have_suffix() {
        let p = qp(number_literal, "123i16");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn number_hexadecimal_can_have_suffix() {
        let p = qp(number_literal, "0xBEEF__u32");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn number_octal_can_have_suffix() {
        let p = qp(number_literal, "0o777_isize");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn pattern_with_path() {
        let p = qp(pattern, "foo::Bar::Baz");
        assert_eq!(unwrap_progress(p).extent(), (0, 13))
    }

    #[test]
    fn pattern_with_ref() {
        let p = qp(pattern, "ref a");
        assert_eq!(unwrap_progress(p).extent(), (0, 5))
    }

    #[test]
    fn pattern_with_tuple() {
        let p = qp(pattern, "(a, b)");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn pattern_with_enum_tuple() {
        let p = qp(pattern, "Baz(a)");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn pattern_with_tuple_wildcard() {
        let p = qp(pattern, "(..)");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn pattern_with_tuple_wildcard_anywhere() {
        let p = qp(pattern, "(a, .., b)");
        assert_eq!(unwrap_progress(p).extent(), (0, 10))
    }

    #[test]
    fn pattern_with_tuple_all_space() {
        let p = qp(pattern, "( a , .. , b )");
        assert_eq!(unwrap_progress(p).extent(), (0, 14))
    }

    #[test]
    fn pattern_with_enum_struct() {
        let p = qp(pattern, "Baz { a: a }");
        assert_eq!(unwrap_progress(p).extent(), (0, 12))
    }

    #[test]
    fn pattern_with_enum_struct_shorthand() {
        let p = qp(pattern, "Baz { a }");
        assert_eq!(unwrap_progress(p).extent(), (0, 9))
    }

    #[test]
    fn pattern_with_enum_struct_shorthand_with_ref() {
        let p = qp(pattern, "Baz { ref a }");
        assert_eq!(unwrap_progress(p).extent(), (0, 13))
    }

    #[test]
    fn pattern_with_enum_struct_wildcard() {
        let p = qp(pattern, "Baz { .. }");
        assert_eq!(unwrap_progress(p).extent(), (0, 10))
    }

    #[test]
    fn pattern_with_byte_literal() {
        let p = qp(pattern, "b'a'");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn pattern_with_char_literal() {
        let p = qp(pattern, "'a'");
        assert_eq!(unwrap_progress(p).extent(), (0, 3))
    }

    #[test]
    fn pattern_with_byte_string_literal() {
        let p = qp(pattern, r#"b"hello""#);
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn pattern_with_string_literal() {
        let p = qp(pattern, r#""hello""#);
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn pattern_with_numeric_literal() {
        let p = qp(pattern, "42");
        assert_eq!(unwrap_progress(p).extent(), (0, 2))
    }

    #[test]
    fn pattern_with_numeric_literal_negative() {
        let p = qp(pattern, "- 42");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn pattern_with_slice() {
        let p = qp(pattern, "[]");
        assert_eq!(unwrap_progress(p).extent(), (0, 2))
    }

    #[test]
    fn pattern_with_reference() {
        let p = qp(pattern, "&a");
        assert_eq!(unwrap_progress(p).extent(), (0, 2))
    }

    #[test]
    fn pattern_with_reference_mutable() {
        let p = unwrap_progress(qp(pattern, "&mut ()"));
        assert!(p.kind.is_reference());
        assert_eq!(p.extent(), (0, 7));
    }

    #[test]
    fn pattern_with_named_subpattern() {
        let p = unwrap_progress(qp(pattern, "a @ 1"));
        assert_eq!(p.extent(), (0, 5));
    }

    #[test]
    fn pattern_with_named_subpattern_qualifiers() {
        let p = unwrap_progress(qp(pattern, "ref mut a @ 1"));
        assert_eq!(p.extent(), (0, 13));
    }

    #[test]
    fn pattern_with_numeric_inclusive_range() {
        let p = qp(pattern, "1 ... 10");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn pattern_with_numeric_inclusive_range_negative() {
        let p = qp(pattern, "-10 ... -1");
        assert_eq!(unwrap_progress(p).extent(), (0, 10))
    }

    #[test]
    fn pattern_with_character_inclusive_range() {
        let p = qp(pattern, "'a'...'z'");
        assert_eq!(unwrap_progress(p).extent(), (0, 9))
    }

    #[test]
    fn pattern_with_byte_inclusive_range() {
        let p = qp(pattern, "b'a'...b'z'");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn pattern_with_pathed_ident_inclusive_range() {
        let p = qp(pattern, "foo::a...z");
        assert_eq!(unwrap_progress(p).extent(), (0, 10))
    }

    #[test]
    fn pattern_with_numeric_exclusive_range() {
        let p = qp(pattern, "1 .. 10");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn pattern_with_numeric_exclusive_range_negative() {
        let p = qp(pattern, "-10 .. -1");
        assert_eq!(unwrap_progress(p).extent(), (0, 9))
    }

    #[test]
    fn pattern_with_character_exclusive_range() {
        let p = qp(pattern, "'a'..'z'");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn pattern_with_byte_exclusive_range() {
        let p = qp(pattern, "b'a'..b'z'");
        assert_eq!(unwrap_progress(p).extent(), (0, 10))
    }

    #[test]
    fn pattern_with_pathed_ident_exclusive_range() {
        let p = qp(pattern, "foo::a..z");
        assert_eq!(unwrap_progress(p).extent(), (0, 9))
    }

    #[test]
    fn pattern_with_macro_call() {
        let p = qp(pattern, "foo![]");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn match_arm_with_alternate() {
        let p = qp(match_arm, "a | b => 1");
        assert_eq!(unwrap_progress(p).extent, (0, 10))
    }

    #[test]
    fn match_arm_with_guard() {
        let p = qp(match_arm, "a if a > 2 => 1");
        assert_eq!(unwrap_progress(p).extent, (0, 15))
    }

    #[test]
    fn match_arm_with_attribute() {
        let p = qp(match_arm, "#[cfg(cool)] _ => 1");
        assert_eq!(unwrap_progress(p).extent, (0, 19))
    }

    #[test]
    fn type_tuple() {
        let p = qp(typ, "(u8, u8)");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn type_tuple_all_space() {
        let p = qp(typ, "( u8 , u8 )");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn type_with_generics() {
        let p = qp(typ, "A<T>");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn type_with_generics_all_space() {
        let p = qp(typ, "A < T >");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn type_impl_trait() {
        let p = qp(typ, "impl Foo");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn type_fn_trait() {
        let p = qp(typ, "Fn(u8) -> u8");
        assert_eq!(unwrap_progress(p).extent(), (0, 12))
    }

    #[test]
    fn type_ref() {
        let p = qp(typ, "&mut Foo");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn type_mut_ref() {
        let p = qp(typ, "&mut Foo");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn type_mut_ref_with_lifetime() {
        let p = qp(typ, "&'a mut Foo");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn type_const_pointer() {
        let p = qp(typ, "*const Foo");
        assert_eq!(unwrap_progress(p).extent(), (0, 10))
    }

    #[test]
    fn type_mut_pointer() {
        let p = qp(typ, "*mut Foo");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn type_uninhabited() {
        let p = qp(typ, "!");
        assert_eq!(unwrap_progress(p).extent(), (0, 1))
    }

    #[test]
    fn type_slice() {
        let p = qp(typ, "[u8]");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn type_array() {
        let p = qp(typ, "[u8; 42]");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn type_array_allows_expressions() {
        let p = qp(typ, "[u8; 1 + 1]");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn type_fn() {
        let p = qp(typ, "fn(u8) -> u8");
        assert_eq!(unwrap_progress(p).extent(), (0, 12))
    }

    #[test]
    fn type_fn_with_names() {
        let p = qp(typ, "fn(a: u8) -> u8");
        assert_eq!(unwrap_progress(p).extent(), (0, 15))
    }

    #[test]
    fn type_fn_with_const() {
        let p = qp(typ, "const fn()");
        assert_eq!(unwrap_progress(p).extent(), (0, 10))
    }

    #[test]
    fn type_fn_with_unsafe() {
        let p = qp(typ, "unsafe fn()");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn type_fn_with_extern() {
        let p = qp(typ, "extern fn()");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn type_fn_with_extern_and_abi() {
        let p = qp(typ, r#"extern "C" fn()"#);
        assert_eq!(unwrap_progress(p).extent(), (0, 15))
    }

    #[test]
    fn type_higher_ranked_trait_bounds() {
        let p = qp(typ, "for <'a> Foo<'a>");
        assert_eq!(unwrap_progress(p).extent(), (0, 16))
    }

    #[test]
    fn type_higher_ranked_trait_bounds_on_functions() {
        let p = qp(typ, "for <'a> fn(&'a u8)");
        assert_eq!(unwrap_progress(p).extent(), (0, 19))
    }

    #[test]
    fn type_higher_ranked_trait_bounds_on_references() {
        let p = qp(typ, "for <'a> &'a u8");
        assert_eq!(unwrap_progress(p).extent(), (0, 15))
    }

    #[test]
    fn type_combination() {
        let p = qp(typ, "Foo + Bar");
        assert_eq!(unwrap_progress(p).extent(), (0, 9))
    }

    #[test]
    fn type_combination_with_lifetimes() {
        let p = qp(typ, "Foo + 'a");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn type_combination_of_higher_ranked_trait_bounds_and_lifetimes() {
        let p = qp(typ, "for<'a> Foo + 'a");
        assert_eq!(unwrap_progress(p).extent(), (0, 16))
    }

    #[test]
    fn type_combination_of_impl_trait_and_lifetimes() {
        let p = qp(typ, "impl Foo + 'a");
        assert_eq!(unwrap_progress(p).extent(), (0, 13))
    }

    #[test]
    fn type_disambiguation() {
        let p = qp(typ, "<Foo as Bar>");
        assert_eq!(unwrap_progress(p).extent(), (0, 12))
    }

    #[test]
    fn type_disambiguation_with_associated_type() {
        let p = qp(typ, "<Foo as Bar>::Quux");
        assert_eq!(unwrap_progress(p).extent(), (0, 18))
    }

    #[test]
    fn type_disambiguation_without_disambiguation() {
        let p = qp(typ, "<Foo>");
        assert_eq!(unwrap_progress(p).extent(), (0, 5))
    }

    #[test]
    fn type_disambiguation_with_double_angle_brackets() {
        let p = qp(typ, "<<A as B> as Option<T>>");
        assert_eq!(unwrap_progress(p).extent(), (0, 23))
    }

    #[test]
    fn struct_basic() {
        let p = qp(p_struct, "struct S { field: TheType, other: OtherType }");
        assert_eq!(unwrap_progress(p).extent, (0, 45))
    }

    #[test]
    fn struct_with_generic_fields() {
        let p = qp(p_struct, "struct S { field: Option<u8> }");
        assert_eq!(unwrap_progress(p).extent, (0, 30))
    }

    #[test]
    fn struct_with_fields_with_no_space() {
        let p = qp(p_struct, "struct S{a:u8}");
        assert_eq!(unwrap_progress(p).extent, (0, 14))
    }

    #[test]
    fn struct_with_fields_with_all_space() {
        let p = qp(p_struct, "struct S { a : u8 }");
        assert_eq!(unwrap_progress(p).extent, (0, 19))
    }

    #[test]
    fn struct_with_generic_declarations() {
        let p = qp(p_struct, "struct S<T> { field: Option<T> }");
        assert_eq!(unwrap_progress(p).extent, (0, 32))
    }

    #[test]
    fn struct_public() {
        let p = qp(p_struct, "pub struct S {}");
        assert_eq!(unwrap_progress(p).extent, (0, 15))
    }

    #[test]
    fn struct_public_field() {
        let p = qp(p_struct, "struct S { pub age: u8 }");
        assert_eq!(unwrap_progress(p).extent, (0, 24))
    }

    #[test]
    fn struct_with_attributed_field() {
        let p = qp(p_struct, "struct S { #[foo(bar)] #[baz(quux)] field: u8 }");
        assert_eq!(unwrap_progress(p).extent, (0, 47))
    }

    #[test]
    fn struct_with_tuple() {
        let p = qp(p_struct, "struct S(u8);");
        assert_eq!(unwrap_progress(p).extent, (0, 13))
    }

    #[test]
    fn struct_with_tuple_and_annotation() {
        let p = qp(p_struct, "struct S(#[foo] u8);");
        assert_eq!(unwrap_progress(p).extent, (0, 20))
    }

    #[test]
    fn struct_with_tuple_and_visibility() {
        let p = qp(p_struct, "struct S(pub u8);");
        assert_eq!(unwrap_progress(p).extent, (0, 17))
    }

    #[test]
    fn struct_empty() {
        let p = qp(p_struct, "struct S;");
        assert_eq!(unwrap_progress(p).extent, (0, 9))
    }

    #[test]
    fn struct_with_where_clause() {
        let p = qp(p_struct, "struct S<A> where A: Foo { a: A }");
        assert_eq!(unwrap_progress(p).extent, (0, 33))
    }

    #[test]
    fn struct_with_tuple_and_where_clause() {
        let p = qp(p_struct, "struct S<A>(A) where A: Foo;");
        assert_eq!(unwrap_progress(p).extent, (0, 28))
    }

    #[test]
    fn union_basic() {
        let p = qp(p_union, "union U { field: TheType, other: OtherType }");
        assert_eq!(unwrap_progress(p).extent, (0, 44))
    }

    #[test]
    fn union_is_still_an_ident() {
        let p = qp(p_union, "union union { union: union }");
        assert_eq!(unwrap_progress(p).extent, (0, 28))
    }

    #[test]
    fn union_with_generic_declarations() {
        let p = qp(p_union, "union S<T> { field: Option<T> }");
        assert_eq!(unwrap_progress(p).extent, (0, 31))
    }

    #[test]
    fn union_with_where_clause() {
        let p = qp(p_union, "union S<A> where A: Foo { a: A }");
        assert_eq!(unwrap_progress(p).extent, (0, 32))
    }

    #[test]
    fn union_public() {
        let p = qp(p_union, "pub union S {}");
        assert_eq!(unwrap_progress(p).extent, (0, 14))
    }

    #[test]
    fn union_public_field() {
        let p = qp(p_union, "union S { pub age: u8 }");
        assert_eq!(unwrap_progress(p).extent, (0, 23))
    }

    #[test]
    fn union_with_attributed_field() {
        let p = qp(p_union, "union S { #[foo(bar)] #[baz(quux)] field: u8 }");
        assert_eq!(unwrap_progress(p).extent, (0, 46))
    }

    #[test]
    fn where_clause_with_path() {
        let p = qp(where_clause_item, "P: foo::bar::baz::Quux<'a>");
        assert_eq!(unwrap_progress(p).extent(), (0, 26))
    }

    #[test]
    fn where_clause_with_multiple_bounds() {
        let p = qp(where_clause_item, "P: A + B");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn where_clause_with_multiple_types() {
        let p = qp(where_clause, "where P: A, Q: B");
        let p = unwrap_progress(p);
        assert_eq!(p[1].extent(), (12, 16))
    }

    #[test]
    fn where_clause_with_lifetimes() {
        let p = qp(where_clause_item, "'a: 'b + 'c");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn ident_with_leading_underscore() {
        let p = qp(ident, "_foo");
        assert_eq!(unwrap_progress(p).extent, (0, 4))
    }

    #[test]
    fn ident_can_have_keyword_substring() {
        let p = qp(ident, "form");
        assert_eq!(unwrap_progress(p).extent, (0, 4))
    }

    #[test]
    fn lifetime_ident() {
        let p = qp(lifetime, "'a");
        assert_eq!(unwrap_progress(p).extent, (0, 2))
    }

    #[test]
    fn lifetime_static() {
        let p = qp(lifetime, "'static");
        assert_eq!(unwrap_progress(p).extent, (0, 7))
    }

    #[test]
    fn generic_declarations_() {
        let p = qp(generic_declarations, "<A>");
        assert_eq!(unwrap_progress(p).extent, (0, 3))
    }

    #[test]
    fn generic_declarations_allow_type_bounds() {
        let p = qp(generic_declarations, "<A: Foo>");
        assert_eq!(unwrap_progress(p).extent, (0, 8))
    }

    #[test]
    fn generic_declarations_with_default_types() {
        let p = qp(generic_declarations, "<A = Bar>");
        assert_eq!(unwrap_progress(p).extent, (0, 9))
    }

    #[test]
    fn generic_declarations_with_type_bounds_and_default_types() {
        let p = qp(generic_declarations, "<A: Foo = Bar>");
        assert_eq!(unwrap_progress(p).extent, (0, 14))
    }

    #[test]
    fn generic_declarations_allow_lifetime_bounds() {
        let p = qp(generic_declarations, "<'a: 'b>");
        assert_eq!(unwrap_progress(p).extent, (0, 8))
    }

    #[test]
    fn generic_declarations_with_attributes() {
        let p = qp(generic_declarations, "<#[foo] 'a, #[bar] B>");
        assert_eq!(unwrap_progress(p).extent, (0, 21))
    }

    #[test]
    fn generic_declarations_all_space() {
        let p = qp(generic_declarations, "< 'a : 'b , A : Foo >");
        assert_eq!(unwrap_progress(p).extent, (0, 21))
    }

    #[test]
    fn trait_bounds_with_lifetime() {
        let p = qp(trait_bounds, "'a + 'b");
        assert_eq!(unwrap_progress(p).extent, (0, 7))
    }

    #[test]
    fn trait_bounds_with_relaxed() {
        let p = qp(trait_bounds, "?A + ?B");
        assert_eq!(unwrap_progress(p).extent, (0, 7))
    }

    #[test]
    fn trait_bounds_with_associated_types() {
        let p = qp(trait_bounds, "A<B, C = D>");
        assert_eq!(unwrap_progress(p).extent, (0, 11))
    }

    #[test]
    fn visibility_self() {
        let p = qp(visibility, "pub(self)");
        assert_eq!(unwrap_progress(p).extent, (0, 9))
    }

    #[test]
    fn visibility_super() {
        let p = qp(visibility, "pub(super)");
        assert_eq!(unwrap_progress(p).extent, (0, 10))
    }

    #[test]
    fn visibility_crate() {
        let p = qp(visibility, "pub(crate)");
        assert_eq!(unwrap_progress(p).extent, (0, 10))
    }

    #[test]
    fn visibility_path() {
        let p = qp(visibility, "pub(::foo::bar)");
        assert_eq!(unwrap_progress(p).extent, (0, 15))
    }

    fn zero_or_more_tailed_test<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
        Progress<'s, Tailed<Ident>>
    {
        zero_or_more_tailed(comma, ident)(pm, pt)
    }

    #[test]
    fn zero_or_more_tailed_with_zero() {
        let p = qp(zero_or_more_tailed_test, "");
        let p = unwrap_progress(p);
        assert_eq!(p.values.len(), 0);
        assert_eq!(p.separator_count, 0);
    }

    #[test]
    fn zero_or_more_tailed_with_one() {
        let p = qp(zero_or_more_tailed_test, "X");
        let p = unwrap_progress(p);
        assert_eq!(p.values.len(), 1);
        assert_eq!(p.separator_count, 0);
    }

    #[test]
    fn zero_or_more_tailed_with_one_trailing() {
        let p = qp(zero_or_more_tailed_test, "X,");
        let p = unwrap_progress(p);
        assert_eq!(p.values.len(), 1);
        assert_eq!(p.separator_count, 1);
    }

    #[test]
    fn zero_or_more_tailed_with_two() {
        let p = qp(zero_or_more_tailed_test, "X, X");
        let p = unwrap_progress(p);
        assert_eq!(p.values.len(), 2);
        assert_eq!(p.separator_count, 1);
    }

    #[test]
    fn zero_or_more_tailed_with_two_trailing() {
        let p = qp(zero_or_more_tailed_test, "X, X,");
        let p = unwrap_progress(p);
        assert_eq!(p.values.len(), 2);
        assert_eq!(p.separator_count, 2);
    }

    #[test]
    fn zero_or_more_tailed_with_all_space() {
        let p = qp(zero_or_more_tailed_test, "X , X , ");
        let p = unwrap_progress(p);
        assert_eq!(p.values.len(), 2);
        assert_eq!(p.separator_count, 2);
    }

    #[test]
    fn zero_or_more_tailed_doesnt_allow_space_separator() {
        let p = qp(zero_or_more_tailed_test, "X X");
        let p = unwrap_progress(p);
        assert_eq!(p.values.len(), 1);
        assert_eq!(p.separator_count, 0);
    }

    fn one_or_more_tailed_test<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
        Progress<'s, Tailed<Ident>>
    {
        one_or_more_tailed(comma, ident)(pm, pt)
    }

    #[test]
    fn one_or_more_tailed_with_zero() {
        let p = qp(one_or_more_tailed_test, "");
        let (err_loc, errs) = unwrap_progress_err(p);
        assert_eq!(err_loc, 0);
        assert!(errs.contains(&Error::ExpectedIdent));
    }

    #[test]
    fn one_or_more_tailed_with_one() {
        let p = qp(one_or_more_tailed_test, "X");
        let p = unwrap_progress(p);
        assert_eq!(p.values.len(), 1);
        assert_eq!(p.separator_count, 0);
    }

    #[test]
    fn one_or_more_tailed_with_one_trailing() {
        let p = qp(one_or_more_tailed_test, "X,");
        let p = unwrap_progress(p);
        assert_eq!(p.values.len(), 1);
        assert_eq!(p.separator_count, 1);
    }

    #[test]
    fn one_or_more_tailed_with_two() {
        let p = qp(one_or_more_tailed_test, "X, X");
        let p = unwrap_progress(p);
        assert_eq!(p.values.len(), 2);
        assert_eq!(p.separator_count, 1);
    }

    #[test]
    fn one_or_more_tailed_with_two_trailing() {
        let p = qp(one_or_more_tailed_test, "X, X,");
        let p = unwrap_progress(p);
        assert_eq!(p.values.len(), 2);
        assert_eq!(p.separator_count, 2);
    }

    #[test]
    fn one_or_more_tailed_with_all_space() {
        let p = qp(one_or_more_tailed_test, "X , X , ");
        let p = unwrap_progress(p);
        assert_eq!(p.values.len(), 2);
        assert_eq!(p.separator_count, 2);
    }

    #[test]
    fn one_or_more_tailed_with_two_doesnt_allow_space_separator() {
        let p = qp(one_or_more_tailed_test, "X X");
        let p = unwrap_progress(p);
        assert_eq!(p.values.len(), 1);
        assert_eq!(p.separator_count, 0);
    }

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
