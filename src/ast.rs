use std;

use {Extent, HasExtent};
use visit::{Visit, Visitor};

#[derive(Debug, Visit)]
pub struct File {
    pub items: Vec<Attributed<Item>>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum Item {
    AttributeContaining(AttributeContaining),
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

#[derive(Debug, HasExtent, Visit)]
pub struct Attribute {
    pub extent: Extent,
    pub text: Extent,
}

#[derive(Debug, HasExtent, Visit)]
pub struct AttributeContaining {
    pub extent: Extent,
    pub text: Extent,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Lifetime {
    pub extent: Extent,
    pub name: Ident,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum Whitespace {
    Comment(Comment),
    Whitespace(Extent),
}

#[derive(Debug, HasExtent, Visit)]
pub struct Comment {
    pub extent: Extent,
    pub text: Extent,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Use {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub path: UsePath,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct UsePath {
    pub extent: Extent,
    pub path: Vec<Ident>,
    pub tail: UseTail,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum UseTail {
    Ident(UseTailIdent),
    Glob(UseTailGlob),
    Multi(UseTailMulti),
}

#[derive(Debug, HasExtent, Visit)]
pub struct UseTailIdent {
    pub extent: Extent,
    pub name: Ident,
    pub rename: Option<Ident>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct UseTailGlob {
    pub extent: Extent,
}

#[derive(Debug, HasExtent, Visit)]
pub struct UseTailMulti {
    pub extent: Extent,
    pub paths: Vec<UsePath>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Function {
    pub extent: Extent,
    pub header: FunctionHeader,
    pub body: Block,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct FunctionHeader {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub qualifiers: FunctionQualifiers,
    pub name: Ident,
    pub generics: Option<GenericDeclarations>,
    pub arguments: Vec<Argument>,
    pub return_type: Option<Type>,
    pub wheres: Vec<Where>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct FunctionQualifiers {
    pub extent: Extent,
    pub is_default: Option<Extent>,
    pub is_const: Option<Extent>,
    pub is_unsafe: Option<Extent>,
    pub is_extern: Option<Extent>,
    pub abi: Option<String>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct TraitImplFunctionHeader {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub qualifiers: FunctionQualifiers,
    pub name: Ident,
    pub generics: Option<GenericDeclarations>,
    pub arguments: Vec<TraitImplArgument>,
    pub return_type: Option<Type>,
    pub wheres: Vec<Where>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct GenericDeclarations {
    pub extent: Extent,
    pub lifetimes: Vec<Attributed<GenericDeclarationLifetime>>,
    pub types: Vec<Attributed<GenericDeclarationType>>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct GenericDeclarationLifetime {
    pub extent: Extent,
    pub name: Lifetime,
    pub bounds: Vec<Lifetime>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct GenericDeclarationType {
    pub extent: Extent,
    pub name: Ident,
    pub bounds: Option<TraitBounds>,
    pub default: Option<Type>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Type {
    pub extent: Extent,
    pub kind: TypeKind,
    pub additional: Vec<TypeAdditional>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum TypeKind {
    Array(TypeArray),
    Disambiguation(TypeDisambiguation),
    Function(TypeFunction),
    HigherRankedTraitBounds(TypeHigherRankedTraitBounds),
    ImplTrait(TypeImplTrait),
    Named(TypeNamed),
    Pointer(TypePointer),
    Reference(TypeReference),
    Slice(TypeSlice),
    Tuple(TypeTuple),
    Uninhabited(Extent),
}

#[derive(Debug, HasExtent, Visit)]
pub struct TypeReference {
    pub extent: Extent,
    pub kind: TypeReferenceKind,
    pub typ: Box<Type>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct TypeReferenceKind {
    pub extent: Extent,
    pub lifetime: Option<Lifetime>,
    pub mutable: Option<Extent>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct TypePointer {
    pub extent: Extent,
    pub kind: TypePointerKind,
    pub typ: Box<Type>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug)]
pub enum TypePointerKind {
    Const,
    Mutable,
}

#[derive(Debug, HasExtent, Visit)]
pub struct TypeArray {
    pub extent: Extent,
    pub typ: Box<Type>,
    pub count: Box<Attributed<Expression>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct TypeHigherRankedTraitBounds {
    pub extent: Extent,
    pub lifetimes: Vec<Lifetime>,
    pub child: TypeHigherRankedTraitBoundsChild,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum TypeHigherRankedTraitBoundsChild {
    Named(TypeNamed),
    Function(TypeFunction),
    Reference(TypeReference),
}

#[derive(Debug, HasExtent, Visit)]
pub struct TypeImplTrait {
    pub extent: Extent,
    pub name: TypeNamed,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum TypeAdditional {
    Named(TypeNamed),
    Lifetime(Lifetime),
}

#[derive(Debug, HasExtent, Visit)]
pub struct TypeNamed {
    pub extent: Extent,
    pub path: Vec<TypeNamedComponent>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct TypeNamedComponent {
    pub extent: Extent,
    pub ident: Ident,
    pub generics: Option<TypeGenerics>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct TypeDisambiguation {
    pub extent: Extent,
    pub from_type: Box<Type>,
    pub to_type: Option<Box<TypeNamed>>,
    pub path: Vec<TypeNamedComponent>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct TypeSlice {
    pub extent: Extent,
    pub typ: Box<Type>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct TypeTuple {
    pub extent: Extent,
    pub types: Vec<Type>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum TypeGenerics {
    Function(TypeGenericsFunction),
    Angle(TypeGenericsAngle),
}

#[derive(Debug, HasExtent, Visit)]
pub struct TypeGenericsFunction {
    pub extent: Extent,
    pub types: Vec<Type>,
    pub return_type: Option<Box<Type>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct TypeGenericsAngle {
    pub extent: Extent,
    pub members: Vec<TypeGenericsAngleMember>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum TypeGenericsAngleMember {
    Lifetime(Lifetime),
    Type(Type),
    AssociatedType(AssociatedType)
}

#[derive(Debug, HasExtent, Visit)]
pub struct AssociatedType {
    pub extent: Extent,
    pub name: Ident,
    pub value: Type,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct TypeFunction {
    pub extent: Extent,
    pub qualifiers: FunctionQualifiers,
    pub arguments: Vec<TypeFunctionArgument>,
    pub return_type: Option<Box<Type>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum TypeFunctionArgument {
    Named(TypeFunctionArgumentNamed),
    Variadic(Extent),
}

#[derive(Debug, HasExtent, Visit)]
pub struct TypeFunctionArgumentNamed {
    pub extent: Extent,
    pub name: Option<Ident>,
    pub typ: Type
}

#[derive(Debug, Copy, Clone, HasExtent, Visit)]
pub struct Ident {
    pub extent: Extent,
}

// TODO: Can we reuse the path from the `use` statement?
#[derive(Debug, HasExtent, Visit)]
pub struct Path {
    pub extent: Extent,
    pub components: Vec<Ident>,
}

// TODO: Can we reuse the path from the `use` statement?
#[derive(Debug, HasExtent, Visit)]
pub struct PathedIdent {
    pub extent: Extent,
    pub components: Vec<PathComponent>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct PathComponent {
    pub extent: Extent,
    pub ident: Ident,
    pub turbofish: Option<Turbofish>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Turbofish {
    pub extent: Extent,
    pub lifetimes: Vec<Lifetime>,
    pub types: Vec<Type>,
}

impl From<Ident> for PathedIdent {
    fn from(other: Ident) -> PathedIdent {
        PathedIdent { extent: other.extent, components: vec![
            PathComponent { extent: other.extent, ident: other, turbofish: None },
        ] }
    }
}

#[derive(Debug, HasExtent, Visit)]
pub struct Const {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub name: Ident,
    pub typ: Type,
    pub value: Attributed<Expression>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Static {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub is_mut: Option<Extent>,
    pub name: Ident,
    pub typ: Type,
    pub value: Attributed<Expression>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Struct {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub name: Ident,
    pub generics: Option<GenericDeclarations>,
    pub wheres: Vec<Where>,
    pub body: StructDefinitionBody,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum StructDefinitionBody {
    Brace(StructDefinitionBodyBrace),
    Tuple(StructDefinitionBodyTuple),
    Empty(Extent),
}

#[derive(Debug, HasExtent, Visit)]
pub struct StructDefinitionBodyBrace {
    pub extent: Extent,
    pub fields: Vec<Attributed<StructDefinitionFieldNamed>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct StructDefinitionFieldNamed {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub name: Ident,
    pub typ: Type,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct StructDefinitionBodyTuple {
    pub extent: Extent,
    pub fields: Vec<Attributed<StructDefinitionFieldUnnamed>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct StructDefinitionFieldUnnamed {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub typ: Type,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Union {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub name: Ident,
    pub generics: Option<GenericDeclarations>,
    pub wheres: Vec<Where>,
    pub fields: Vec<Attributed<StructDefinitionFieldNamed>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Enum {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub name: Ident,
    pub generics: Option<GenericDeclarations>,
    pub wheres: Vec<Where>,
    pub variants: Vec<Attributed<EnumVariant>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct EnumVariant {
    pub extent: Extent,
    pub name: Ident,
    pub body: EnumVariantBody,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)] // HasExtent?
pub enum EnumVariantBody {
    Tuple(Vec<Attributed<StructDefinitionFieldUnnamed>>),
    Struct(StructDefinitionBodyBrace),
    Unit(Option<Attributed<Expression>>),
}

#[derive(Debug, Visit, Decompose)] // HasExtent?
pub enum Argument {
    SelfArgument(SelfArgument),
    Named(NamedArgument),
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum SelfArgument {
    Longhand(SelfArgumentLonghand),
    Shorthand(SelfArgumentShorthand),
}

#[derive(Debug, HasExtent, Visit)]
pub struct SelfArgumentLonghand {
    pub extent: Extent,
    pub is_mut: Option<Extent>,
    pub name: Ident,
    pub typ: Type,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct SelfArgumentShorthand {
    pub extent: Extent,
    pub qualifier: Option<SelfArgumentShorthandQualifier>,
    pub name: Ident,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum SelfArgumentShorthandQualifier {
    Reference(TypeReferenceKind),
    Mut(Extent),
}

#[derive(Debug, Visit)] // HasExtent?
pub struct NamedArgument {
    pub name: Pattern,
    pub typ: Type,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)] // HasExtent?
pub enum TraitImplArgument {
    SelfArgument(SelfArgument),
    Named(TraitImplArgumentNamed),
}

#[derive(Debug, Visit)] // HasExtent?
pub struct TraitImplArgumentNamed {
    pub name: Option<Pattern>,
    pub typ: Type,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Where {
    pub extent: Extent,
    pub higher_ranked_trait_bounds: Vec<Lifetime>,
    pub kind: WhereKind,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum WhereKind {
    Lifetime(WhereLifetime),
    Type(WhereType),
}

#[derive(Debug, HasExtent, Visit)]
pub struct WhereLifetime {
    pub extent: Extent,
    pub name: Lifetime,
    pub bounds: Vec<Lifetime>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct WhereType {
    pub extent: Extent,
    pub name: Type,
    pub bounds: TraitBounds,
}

#[derive(Debug, HasExtent, Visit)]
pub struct TraitBounds {
    pub extent: Extent,
    pub types: Vec<TraitBound>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum TraitBound {
    Lifetime(TraitBoundLifetime),
    Normal(TraitBoundNormal),
    Relaxed(TraitBoundRelaxed),
}

#[derive(Debug, HasExtent, Visit)]
pub struct TraitBoundLifetime {
    pub extent: Extent,
    pub lifetime: Lifetime,
}

#[derive(Debug, HasExtent, Visit)]
pub struct TraitBoundNormal {
    pub extent: Extent,
    pub typ: TraitBoundType,
}

#[derive(Debug, HasExtent, Visit)]
pub struct TraitBoundRelaxed {
    pub extent: Extent,
    pub typ: TraitBoundType,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum TraitBoundType {
    Named(TypeNamed),
    // TODO: HRTB Trait bounds don't really allow references or fn types, just named
    // We need to create a smaller enum here
    HigherRankedTraitBounds(TypeHigherRankedTraitBounds),
}

#[derive(Debug, HasExtent, Visit)]
pub struct Block {
    pub extent: Extent,
    pub statements: Vec<Statement>,
    pub expression: Option<Attributed<Expression>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct UnsafeBlock {
    pub extent: Extent,
    pub body: Box<Block>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Parenthetical {
    pub extent: Extent,
    pub expression: Box<Attributed<Expression>>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum Statement {
    Expression(Attributed<Expression>),
    Item(Attributed<Item>),
    Empty(Extent),
}

#[derive(Debug)]
pub struct Attributed<T> {
    pub extent: Extent,
    pub attributes: Vec<Attribute>,
    pub value: T,
}

impl<T> HasExtent for Attributed<T> {
    fn extent(&self) -> Extent {
        self.extent
    }
}

impl<T> std::ops::Deref for Attributed<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target { &self.value }
}

macro_rules! visit_attributed {
    ($typ:ty, $visit:ident, $exit:ident) => {
        impl Visit for Attributed<$typ> {
            fn visit<V>(&self, v: &mut V)
                where V: Visitor
            {
                v.$visit(self);
                self.attributes.visit(v);
                self.value.visit(v);
                v.$exit(self);
            }
        }
    };
}

visit_attributed!(EnumVariant, visit_attributed_enum_variant, exit_attributed_enum_variant);
visit_attributed!(Expression, visit_attributed_expression, exit_attributed_expression);
visit_attributed!(ExternBlockMember, visit_attributed_extern_block_member, exit_attributed_extern_block_member);
visit_attributed!(GenericDeclarationLifetime, visit_attributed_generic_declaration_lifetime, exit_attributed_generic_declaration_lifetime);
visit_attributed!(GenericDeclarationType, visit_attributed_generic_declaration_type, exit_attributed_generic_declaration_type);
visit_attributed!(ImplMember, visit_attributed_impl_member, exit_attributed_impl_member);
visit_attributed!(Item, visit_attributed_item, exit_attributed_item);
visit_attributed!(StructDefinitionFieldNamed, visit_attributed_struct_definition_field_named, exit_attributed_struct_definition_field_named);
visit_attributed!(StructDefinitionFieldUnnamed, visit_attributed_struct_definition_field_unnamed, exit_attributed_struct_definition_field_unnamed);
visit_attributed!(TraitMember, visit_attributed_trait_member, exit_attributed_trait_member);

// Assumes that there are no attributes
impl From<Expression> for Attributed<Expression> {
    fn from(value: Expression) -> Attributed<Expression> {
        Attributed {
            extent: value.extent(),
            attributes: vec![],
            value,
        }
    }
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum Expression {
    Array(Array),
    AsType(AsType),
    Ascription(Ascription),
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
    pub(crate) fn may_terminate_statement(&self) -> bool {
        match *self {
            Expression::Block(_)       |
            Expression::ForLoop(_)     |
            Expression::If(_)          |
            Expression::IfLet(_)       |
            Expression::Loop(_)        |
            Expression::Match(_)       |
            Expression::UnsafeBlock(_) |
            Expression::While(_)       |
            Expression::WhileLet(_)    |
            Expression::MacroCall(MacroCall { args: MacroCallArgs::Curly(_), .. }) => true,
            _ => false,
        }
    }
}

#[derive(Debug, HasExtent, Visit)]
pub struct MacroCall {
    pub extent: Extent,
    pub name: Ident,
    pub arg: Option<Ident>,
    pub args: MacroCallArgs,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum MacroCallArgs {
    Paren(Extent),
    Curly(Extent),
    Square(Extent),
}

#[derive(Debug, HasExtent, Visit)]
pub struct Let {
    pub extent: Extent,
    pub pattern: Pattern,
    pub typ: Option<Type>,
    pub value: Option<Box<Attributed<Expression>>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Tuple {
    pub extent: Extent,
    pub members: Vec<Attributed<Expression>>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct TryOperator {
    pub extent: Extent,
    pub target: Box<Attributed<Expression>>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct FieldAccess {
    pub extent: Extent,
    pub target: Box<Attributed<Expression>>,
    pub field: FieldName,
}

#[derive(Debug, HasExtent, Decompose)]
pub enum FieldName {
    Path(PathComponent),
    Number(Extent),
}

#[derive(Debug, HasExtent, Visit)]
pub struct Number {
    pub extent: Extent,
    pub is_negative: Option<Extent>,
    pub value: NumberValue,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum NumberValue {
    Binary(NumberBinary),
    Decimal(NumberDecimal),
    Hexadecimal(NumberHexadecimal),
    Octal(NumberOctal),
}

#[derive(Debug, HasExtent, Visit)]
pub struct NumberBinary {
    pub extent: Extent,
    pub decimal: Extent,
    pub fraction: Option<Extent>,
    pub exponent: Option<Extent>,
    pub suffix: Option<Extent>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct NumberDecimal {
    pub extent: Extent,
    pub decimal: Extent,
    pub fraction: Option<Extent>,
    pub exponent: Option<Extent>,
    pub suffix: Option<Extent>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct NumberHexadecimal {
    pub extent: Extent,
    pub decimal: Extent,
    pub fraction: Option<Extent>,
    pub exponent: Option<Extent>,
    pub suffix: Option<Extent>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct NumberOctal {
    pub extent: Extent,
    pub decimal: Extent,
    pub fraction: Option<Extent>,
    pub exponent: Option<Extent>,
    pub suffix: Option<Extent>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Value {
    pub extent: Extent,
    pub name: PathedIdent,
    pub literal: Option<StructLiteral>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct StructLiteral {
    pub extent: Extent,
    pub fields: Vec<StructLiteralField>,
    pub splat: Option<Box<Attributed<Expression>>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)] // HasExtent?
pub struct StructLiteralField {
    pub name: Ident,
    pub value: Attributed<Expression>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Call {
    pub extent: Extent,
    pub target: Box<Attributed<Expression>>,
    pub args: Vec<Attributed<Expression>>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct ForLoop {
    pub extent: Extent,
    pub label: Option<Lifetime>,
    pub pattern: Pattern,
    pub iter: Box<Attributed<Expression>>,
    pub body: Box<Block>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Loop {
    pub extent: Extent,
    pub label: Option<Lifetime>,
    pub body: Box<Block>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct IfLet {
    pub extent: Extent,
    pub pattern: Pattern,
    pub value: Box<Attributed<Expression>>,
    pub body: Box<Block>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct While {
    pub extent: Extent,
    pub label: Option<Lifetime>,
    pub value: Box<Attributed<Expression>>,
    pub body: Box<Block>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct WhileLet {
    pub extent: Extent,
    pub label: Option<Lifetime>,
    pub pattern: Pattern,
    pub value: Box<Attributed<Expression>>,
    pub body: Box<Block>,
    pub whitespace: Vec<Whitespace>,
}

// TODO: Should this be the same as dereference? What about reference?
#[derive(Debug, HasExtent, Visit)]
pub struct Unary {
    pub extent: Extent,
    pub op: UnaryOp,
    pub value: Box<Attributed<Expression>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Binary {
    pub extent: Extent,
    pub op: BinaryOp,
    pub lhs: Box<Attributed<Expression>>,
    pub rhs: Box<Attributed<Expression>>,
    pub whitespace: Vec<Whitespace>,
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

#[derive(Debug, HasExtent, Visit)]
pub struct If {
    pub extent: Extent,
    pub condition: Box<Attributed<Expression>>,
    pub body: Box<Block>,
    pub more: Vec<If>,
    pub else_body: Option<Box<Block>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Match {
    pub extent: Extent,
    pub head: Box<Attributed<Expression>>,
    pub arms: Vec<MatchArm>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct MatchArm {
    pub extent: Extent,
    pub attributes: Vec<Attribute>,
    pub pattern: Vec<Pattern>,
    pub guard: Option<Attributed<Expression>>,
    pub hand: MatchHand,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum MatchHand {
    Brace(Attributed<Expression>),
    Expression(Attributed<Expression>),
}

#[derive(Debug, HasExtent, Visit)]
pub struct Range {
    pub extent: Extent,
    pub lhs: Option<Box<Attributed<Expression>>>,
    pub rhs: Option<Box<Attributed<Expression>>>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct RangeInclusive {
    pub extent: Extent,
    pub lhs: Option<Box<Attributed<Expression>>>,
    pub rhs: Option<Box<Attributed<Expression>>>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum Array {
    Explicit(ArrayExplicit),
    Repeated(ArrayRepeated),
}

#[derive(Debug, HasExtent, Visit)]
pub struct ArrayExplicit {
    pub extent: Extent,
    pub values: Vec<Attributed<Expression>>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct ArrayRepeated {
    pub extent: Extent,
    pub value: Box<Attributed<Expression>>,
    pub count: Box<Attributed<Expression>>,
    pub whitespace: Vec<Whitespace>,
}

// TODO: Rename this visitor function?
#[derive(Debug, HasExtent, Visit)]
pub struct ExpressionBox {
    pub extent: Extent,
    pub target: Box<Attributed<Expression>>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct AsType {
    pub extent: Extent,
    pub target: Box<Attributed<Expression>>,
    pub typ: Type,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Ascription {
    pub extent: Extent,
    pub target: Box<Attributed<Expression>>,
    pub typ: Type,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Character {
    pub extent: Extent,
    pub value: Extent,
}

#[derive(Debug, HasExtent, Visit)]
pub struct String {
    pub extent: Extent,
    pub value: Extent,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Byte {
    pub extent: Extent,
    pub value: Character,
}

#[derive(Debug, HasExtent, Visit)]
pub struct ByteString {
    pub extent: Extent,
    pub value: String,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Slice {
    pub extent: Extent,
    pub target: Box<Attributed<Expression>>,
    pub index: Box<Attributed<Expression>>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Closure {
    pub extent: Extent,
    #[visit(ignore)]
    pub is_move: bool,
    pub args: Vec<ClosureArg>,
    pub return_type: Option<Type>,
    pub body: Box<Attributed<Expression>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)] // HasExtent?
pub struct ClosureArg {
    pub name: Pattern,
    pub typ: Option<Type>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Reference {
    pub extent: Extent,
    pub is_mutable: Option<Extent>,
    pub target: Box<Attributed<Expression>>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Dereference {
    pub extent: Extent,
    pub target: Box<Attributed<Expression>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Disambiguation {
    pub extent: Extent,
    pub from_type: Type,
    pub to_type: Option<TypeNamed>,
    pub components: Vec<PathComponent>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Return {
    pub extent: Extent,
    pub value: Option<Box<Attributed<Expression>>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Continue {
    pub extent: Extent,
    pub label: Option<Lifetime>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Break {
    pub extent: Extent,
    pub label: Option<Lifetime>,
    pub value: Option<Box<Attributed<Expression>>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Pattern {
    pub extent: Extent,
    pub name: Option<PatternName>,
    pub kind: PatternKind,
}

#[derive(Debug, HasExtent, Visit)]
pub struct PatternName {
    pub extent: Extent,
    pub is_ref: Option<Extent>,
    pub is_mut: Option<Extent>,
    pub name: Ident,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum PatternKind {
    Box(PatternBox),
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

#[derive(Debug, HasExtent, Visit)]
pub struct PatternIdent {
    pub extent: Extent,
    pub is_ref: Option<Extent>,
    pub is_mut: Option<Extent>,
    pub ident: PathedIdent,
    pub tuple: Option<PatternTuple>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct PatternStruct {
    pub extent: Extent,
    pub name: PathedIdent,
    pub fields: Vec<PatternStructField>,
    #[visit(ignore)]
    pub wildcard: bool,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)] // HasExtent?
pub enum PatternStructField {
    Long(PatternStructFieldLong),
    Short(PatternStructFieldShort),
}

#[derive(Debug, HasExtent, Visit)]
pub struct PatternStructFieldLong {
    pub extent: Extent,
    pub name: Ident,
    pub pattern: Pattern,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit)] // HasExtent?
pub struct PatternStructFieldShort {
    pub ident: PatternIdent
}

#[derive(Debug, HasExtent, Visit)]
pub struct PatternTuple {
    pub extent: Extent,
    pub members: Vec<PatternBundleMember>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct PatternSlice {
    pub extent: Extent,
    pub members: Vec<PatternBundleMember>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum PatternBundleMember {
    Pattern(Pattern),
    Wildcard(Extent),
}

#[derive(Debug, HasExtent, Visit)]
pub struct PatternWildcard {
    pub extent: Extent,
}

#[derive(Debug, HasExtent, Visit)]
pub struct PatternByte {
    pub extent: Extent,
    pub value: Byte,
}

#[derive(Debug, HasExtent, Visit)]
pub struct PatternCharacter {
    pub extent: Extent,
    pub value: Character,
}

#[derive(Debug, HasExtent, Visit)]
pub struct PatternByteString {
    pub extent: Extent,
    pub value: ByteString,
}

#[derive(Debug, HasExtent, Visit)]
pub struct PatternString {
    pub extent: Extent,
    pub value: String,
}

#[derive(Debug, HasExtent, Visit)]
pub struct PatternNumber {
    pub extent: Extent,
    pub is_negative: Option<Extent>,
    pub value: Number,
}

#[derive(Debug, HasExtent, Visit)]
pub struct PatternMacroCall {
    pub extent: Extent,
    pub value: MacroCall,
}

#[derive(Debug, HasExtent, Visit)]
pub struct PatternRangeExclusive {
    pub extent: Extent,
    pub start: PatternRangeComponent,
    pub end: PatternRangeComponent,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct PatternRangeInclusive {
    pub extent: Extent,
    pub start: PatternRangeComponent,
    pub end: PatternRangeComponent,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, Decompose)]
pub enum PatternRangeComponent {
    Ident(PathedIdent),
    Byte(Byte),
    Character(Character),
    Number(PatternNumber),
}

#[derive(Debug, HasExtent, Visit)]
pub struct PatternReference {
    pub extent: Extent,
    pub is_mut: Option<Extent>,
    pub pattern: Box<Pattern>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct PatternBox {
    pub extent: Extent,
    pub pattern: Box<Pattern>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Trait {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub is_unsafe: Option<Extent>,
    pub is_auto: Option<Extent>,
    pub name: Ident,
    pub generics: Option<GenericDeclarations>,
    pub bounds: Option<TraitBounds>,
    pub wheres: Vec<Where>,
    pub members: Vec<Attributed<TraitMember>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum TraitMember {
    Const(TraitMemberConst),
    Function(TraitMemberFunction),
    Type(TraitMemberType),
    MacroCall(MacroCall),
}

#[derive(Debug, HasExtent, Visit)]
pub struct TraitMemberFunction {
    pub extent: Extent,
    pub header: TraitImplFunctionHeader,
    pub body: Option<Block>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct TraitMemberType {
    pub extent: Extent,
    pub name: Ident,
    pub bounds: Option<TraitBounds>,
    pub default: Option<Type>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct TraitMemberConst {
    pub extent: Extent,
    pub name: Ident,
    pub typ: Type,
    pub value: Option<Attributed<Expression>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Impl {
    pub extent: Extent,
    pub is_unsafe: Option<Extent>,
    pub generics: Option<GenericDeclarations>,
    pub kind: ImplKind,
    pub wheres: Vec<Where>,
    pub body: Vec<Attributed<ImplMember>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum ImplKind {
    Trait(ImplOfTrait),
    Inherent(ImplOfInherent),
}

#[derive(Debug, HasExtent, Visit)]
pub struct ImplOfTrait {
    pub extent: Extent,
    pub is_negative: Option<Extent>,
    pub trait_name: Type, // TODO: namedtype only?
    pub type_name: ImplOfTraitType,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct ImplOfInherent {
    pub extent: Extent,
    pub type_name: Type,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum ImplOfTraitType {
    Type(Type),
    Wildcard(Extent),
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum ImplMember {
    Const(ImplConst),
    Function(ImplFunction),
    Type(ImplType),
    MacroCall(MacroCall),
}

#[derive(Debug, HasExtent, Visit)]
pub struct ImplFunction {
    pub extent: Extent,
    pub header: FunctionHeader,
    pub body: Block,
}

#[derive(Debug, HasExtent, Visit)]
pub struct ImplType {
    pub extent: Extent,
    pub name: Ident,
    pub typ: Type,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct ImplConst {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub name: Ident,
    pub typ: Type,
    pub value: Attributed<Expression>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Crate {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub name: Ident,
    pub rename: Option<Ident>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct ExternBlock {
    pub extent: Extent,
    pub abi: Option<String>,
    pub members: Vec<Attributed<ExternBlockMember>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum ExternBlockMember {
    Function(ExternBlockMemberFunction),
    Static(ExternBlockMemberStatic),
}

#[derive(Debug, HasExtent, Visit)]
pub struct ExternBlockMemberStatic {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub is_mut: Option<Extent>,
    pub name: Ident,
    pub typ: Type,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct ExternBlockMemberFunction {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub name: Ident,
    pub generics: Option<GenericDeclarations>,
    pub arguments: Vec<ExternBlockMemberFunctionArgument>,
    pub return_type: Option<Type>,
    pub wheres: Vec<Where>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit, Decompose)]
pub enum ExternBlockMemberFunctionArgument {
    Named(ExternBlockMemberFunctionArgumentNamed),
    Variadic(ExternBlockMemberFunctionArgumentVariadic),
}

#[derive(Debug, HasExtent, Visit)]
pub struct ExternBlockMemberFunctionArgumentNamed {
    pub extent: Extent,
    pub name: Pattern,
    pub typ: Type,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct ExternBlockMemberFunctionArgumentVariadic {
    pub extent: Extent,
}

#[derive(Debug, HasExtent, Visit)]
pub struct TypeAlias {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub name: Ident,
    pub generics: Option<GenericDeclarations>,
    pub wheres: Vec<Where>,
    pub defn: Type,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Module {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub name: Ident,
    pub body: Option<Vec<Attributed<Item>>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, Visit)]
pub struct Visibility {
    pub extent: Extent,
    #[visit(ignore)]
    pub qualifier: Option<VisibilityQualifier>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug)]
pub enum VisibilityQualifier {
    Crate,
    SelfIdent,
    Path(Path),
}
