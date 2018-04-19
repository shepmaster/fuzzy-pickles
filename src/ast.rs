//! The components of a Rust Abstract Syntax Tree (AST)

// Potential cleanups:
//
// - Split expressions, patterns, types into separate modules?

use std;

use {Extent, HasExtent};
use visit::{Visit, Visitor};

/// An entire Rust file
#[derive(Debug, Visit)]
pub struct File {
    pub items: Vec<Attributed<Item>>,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
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

/// An attribute that applies to the subsequent element
///
/// ### Example Source
///
/// ```rust,ignore
/// #[derive(Debug)]
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Attribute {
    pub extent: Extent,
    pub text: Extent,
}

/// An attribute that applies to the containing element
///
/// ### Example Source
///
/// ```rust,ignore
/// #![feature(nll)]
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct AttributeContaining {
    pub extent: Extent,
    pub text: Extent,
}

/// A lifetime identifier
///
/// ### Example Source
///
/// ```rust,ignore
/// 'static
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Lifetime {
    pub extent: Extent,
    pub name: Ident,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum Whitespace {
    Comment(Comment),
    Whitespace(Extent),
}

/// A single-line comment
///
/// ### Example Source
///
/// ```rust,ignore
/// // Hello, world!
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Comment {
    pub extent: Extent,
    pub text: Extent,
}

/// A `use` item
///
/// ### Example Source
///
/// ```rust,ignore
/// use std::collections::HashMap;
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Use {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub path: UsePath,
    pub whitespace: Vec<Whitespace>,
}

/// The names imported by the `use` item
///
/// ### Example Source
///
/// ```rust,ignore
/// use std::collections::HashMap;
/// //  ^^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct UsePath {
    pub extent: Extent,
    pub path: Vec<Ident>,
    pub tail: UseTail,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum UseTail {
    Ident(UseTailIdent),
    Glob(UseTailGlob),
    Multi(UseTailMulti),
}

/// A single name imported by the `use` item
///
/// ### Example Source
///
/// ```rust,ignore
/// use std::collections::HashMap;
/// //                    ^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct UseTailIdent {
    pub extent: Extent,
    pub name: Ident,
    pub rename: Option<Ident>,
}

/// A wildcard name imported by the `use` item
///
/// ### Example Source
///
/// ```rust,ignore
/// use std::collections::*;
/// //                    ^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct UseTailGlob {
    pub extent: Extent,
}

/// A collection of names imported by the `use` item
///
/// ### Example Source
///
/// ```rust,ignore
/// use std::collections::{BTreeMap, HashMap};
/// //                    ^^^^^^^^^^^^^^^^^^^
/// ```
// TODO: rename to "collection"?
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct UseTailMulti {
    pub extent: Extent,
    pub paths: Vec<UsePath>,
}

/// A function definition
///
/// ### Example Source
///
/// ```rust,ignore
/// fn hello() {}
/// ```
// TODO: rename to "function definition"?
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Function {
    pub extent: Extent,
    pub header: FunctionHeader,
    pub body: Block,
    pub whitespace: Vec<Whitespace>,
}

/// A function definition's signature
///
/// ### Example Source
///
/// ```rust,ignore
///     pub fn hello(x: i32) -> bool { false }
/// //  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
// TODO: rename to "function signature"?
// TODO: are we allowing `self` in free functions?
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
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

/// Qualifiers that apply to functions
///
/// ### Example Source
///
/// ```rust,ignore
///     const unsafe extern "C" fn example() {}
/// //  ^^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct FunctionQualifiers {
    pub extent: Extent,
    // TODO: do we allow parsing this on a free function?
    pub is_default: Option<Extent>,
    pub is_const: Option<Extent>,
    pub is_unsafe: Option<Extent>,
    pub is_extern: Option<Extent>,
    // TODO: abi should be predicated on `extern` being present
    pub abi: Option<String>,
}

/// The signature of a function in a trait declaration
///
/// ### Example Source
///
/// ```rust,ignore
/// pub trait Monster { fn roar(&self) {} }
/// //                  ^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
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

/// Generic lifetime and type parameters
///
/// ### Example Source
///
/// ```rust,ignore
/// struct A<'a, T> {}
/// //      ^^^^^^^
/// ```
// TODO: rename to "parameters"?
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct GenericDeclarations {
    pub extent: Extent,
    pub lifetimes: Vec<Attributed<GenericDeclarationLifetime>>,
    pub types: Vec<Attributed<GenericDeclarationType>>,
}

/// Generic lifetime parameters
///
/// ### Example Source
///
/// ```rust,ignore
/// struct A<'a, 'b: 'a> {}
/// //       ^^  ^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct GenericDeclarationLifetime {
    pub extent: Extent,
    pub name: Lifetime,
    pub bounds: Vec<Lifetime>,
}

/// Generic type parameters
///
/// ### Example Source
///
/// ```rust,ignore
/// struct A<T, U: Debug, V = i32> {}
/// //       ^  ^^^^^^^^  ^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct GenericDeclarationType {
    pub extent: Extent,
    pub name: Ident,
    pub bounds: Option<TraitBounds>,
    pub default: Option<Type>,
}

/// A concrete type
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a(_: i32, _: [bool; 4]) -> Option<(f32, f64)>
/// //      ^^^      ^^^^                 ^^^  ^^^
/// //              ^^^^^^^^^            ^^^^^^^^^^
/// //                            ^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Type {
    pub extent: Extent,
    pub kind: TypeKind,
    pub additional: Vec<TypeAdditional>,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
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

/// A reference in a type
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() -> &'a mut i32 {}
/// //        ^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TypeReference {
    pub extent: Extent,
    pub kind: TypeReferenceKind,
    pub typ: Box<Type>,
}

/// The qualifiers for a reference in a type
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() -> &'a mut i32 {}
/// //        ^^^^^^^
/// ```
// TODO: rename to qualifiers?
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TypeReferenceKind {
    pub extent: Extent,
    pub lifetime: Option<Lifetime>,
    pub mutable: Option<Extent>,
    pub whitespace: Vec<Whitespace>,
}

/// A pointer in a type
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() -> *const bool {}
/// //        ^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
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

/// An array in a type
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() -> [u8; 16] {}
/// //        ^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TypeArray {
    pub extent: Extent,
    pub typ: Box<Type>,
    pub count: Box<Attributed<Expression>>,
    pub whitespace: Vec<Whitespace>,
}

/// A type with a higher-ranked trait bound
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() -> for<'a> &'a i16 {}
/// //        ^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TypeHigherRankedTraitBounds {
    pub extent: Extent,
    pub lifetimes: Vec<Lifetime>,
    pub child: TypeHigherRankedTraitBoundsChild,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum TypeHigherRankedTraitBoundsChild {
    Named(TypeNamed),
    Function(TypeFunction),
    Reference(TypeReference),
}

/// An unnamed implementation of a trait
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() -> impl Iterator<Item = u8> {}
/// //        ^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TypeImplTrait {
    pub extent: Extent,
    pub name: TypeNamed,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum TypeAdditional {
    Named(TypeNamed),
    Lifetime(Lifetime),
}

/// A named type
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() -> ::std::collections::HashMap<u8, u8> {}
/// //        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TypeNamed {
    pub extent: Extent,
    pub path: Vec<TypeNamedComponent>,
}

/// A component of a named type
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() -> ::std::collections::HashMap<u8, u8> {}
/// //          ^^^  ^^^^^^^^^^^  ^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TypeNamedComponent {
    pub extent: Extent,
    pub ident: Ident,
    pub generics: Option<TypeGenerics>,
}

/// A disambiguation of a type
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() -> <Vec<u8> as IntoIterator>::Item {}
/// //        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TypeDisambiguation {
    pub extent: Extent,
    pub from_type: Box<Type>,
    pub to_type: Option<Box<TypeNamed>>,
    pub path: Vec<TypeNamedComponent>,
    pub whitespace: Vec<Whitespace>,
}

/// A slice as a type
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() -> &[u8] {}
/// //         ^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TypeSlice {
    pub extent: Extent,
    pub typ: Box<Type>,
    pub whitespace: Vec<Whitespace>,
}

/// A tuple as a type
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() -> (i32, u8) {}
/// //        ^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TypeTuple {
    pub extent: Extent,
    pub types: Vec<Type>,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum TypeGenerics {
    Function(TypeGenericsFunction),
    Angle(TypeGenericsAngle),
}

/// Generic parameter declarations in a function style
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() -> Fn(i32) -> bool {}
/// //          ^^^^^^^^^^^^^
/// ```
// TODO: rename to "parameters" / "declaration"?
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TypeGenericsFunction {
    pub extent: Extent,
    pub types: Vec<Type>,
    pub return_type: Option<Box<Type>>,
    pub whitespace: Vec<Whitespace>,
}

/// Generic parameter declarations in the basic style
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() -> HashMap<i32, u8> {}
/// //               ^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TypeGenericsAngle {
    pub extent: Extent,
    pub members: Vec<TypeGenericsAngleMember>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum TypeGenericsAngleMember {
    Lifetime(Lifetime),
    Type(Type),
    AssociatedType(AssociatedType)
}

/// An associated item in a type with generics
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() -> impl Iterator<Item = bool> {}
/// //                      ^^^^^^^^^^^
/// ```
// TODO: add "type" to the name?
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct AssociatedType {
    pub extent: Extent,
    pub name: Ident,
    pub value: Type,
    pub whitespace: Vec<Whitespace>,
}

/// A function pointer as a type
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() -> fn(i8) -> bool {}
/// //        ^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TypeFunction {
    pub extent: Extent,
    pub qualifiers: FunctionQualifiers,
    pub arguments: Vec<TypeFunctionArgument>,
    pub return_type: Option<Box<Type>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum TypeFunctionArgument {
    Named(TypeFunctionArgumentNamed),
    Variadic(Extent),
}

/// The named argument of a function pointer
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() -> fn(a: i32) {}
/// //           ^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TypeFunctionArgumentNamed {
    pub extent: Extent,
    pub name: Option<Ident>,
    pub typ: Type
}

/// A single identifier
///
/// ### Example Source
///
/// ```rust,ignore
/// fn main() {}
/// // ^^^^
/// ```
#[derive(Debug, Copy, Clone, HasExtent, ExtentIndex, Visit)]
pub struct Ident {
    pub extent: Extent,
}

/// The path that an item is visible in
///
/// ### Example Source
///
/// ```rust,ignore
/// pub(in foo) struct A;
/// //     ^^^
/// ```
// TODO: make this a more specific name; `Path` is overly generic for this usage
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Path {
    pub extent: Extent,
    // TODO: Can we reuse the path from the `use` statement?
    pub components: Vec<Ident>,
}

/// A module-qualified identifier
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { ::std::thread::spawn(); }
/// //       ^^^^^^^^^^^^^^^^^^^^
/// ```
// TODO: Can we reuse the path from the `use` statement?
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct PathedIdent {
    pub extent: Extent,
    pub components: Vec<PathComponent>,
}

/// A component of a module-qualified identifier
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { ::std::thread::spawn(); }
/// //         ^^^  ^^^^^^  ^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct PathComponent {
    pub extent: Extent,
    pub ident: Ident,
    pub turbofish: Option<Turbofish>,
}

/// Allows specifying concrete types
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { None::<u8>; }
/// //           ^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
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

/// A constant value
///
/// ### Example Source
///
/// ```rust,ignore
/// pub const NAME: &str = "Rust";
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Const {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub name: Ident,
    pub typ: Type,
    pub value: Attributed<Expression>,
    pub whitespace: Vec<Whitespace>,
}

/// A static value
///
/// ### Example Source
///
/// ```rust,ignore
/// pub static NAME: &str = "Rust";
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Static {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub is_mut: Option<Extent>,
    pub name: Ident,
    pub typ: Type,
    pub value: Attributed<Expression>,
    pub whitespace: Vec<Whitespace>,
}

/// A struct definition
///
/// ### Example Source
///
/// ```rust,ignore
/// pub struct A { count: i32 }
/// ```
// TODO: rename to "definition"?
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Struct {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub name: Ident,
    pub generics: Option<GenericDeclarations>,
    pub wheres: Vec<Where>,
    pub body: StructDefinitionBody,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum StructDefinitionBody {
    Brace(StructDefinitionBodyBrace),
    Tuple(StructDefinitionBodyTuple),
    Empty(Extent),
}

/// A struct defined using curly braces
///
/// ### Example Source
///
/// ```rust,ignore
/// struct A { count: i32 }
/// //       ^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct StructDefinitionBodyBrace {
    pub extent: Extent,
    pub fields: Vec<Attributed<StructDefinitionFieldNamed>>,
    pub whitespace: Vec<Whitespace>,
}

/// A named field of a struct
///
/// ### Example Source
///
/// ```rust,ignore
/// struct A { pub count: i32 }
/// //         ^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct StructDefinitionFieldNamed {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub name: Ident,
    pub typ: Type,
    pub whitespace: Vec<Whitespace>,
}

/// A struct defined using parenthesis
///
/// ### Example Source
///
/// ```rust,ignore
/// struct Meters(u32);
/// //           ^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct StructDefinitionBodyTuple {
    pub extent: Extent,
    pub fields: Vec<Attributed<StructDefinitionFieldUnnamed>>,
    pub whitespace: Vec<Whitespace>,
}

/// An unnamed field of a struct
///
/// ### Example Source
///
/// ```rust,ignore
/// struct Meters(pub u32);
/// //            ^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct StructDefinitionFieldUnnamed {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub typ: Type,
}

/// A union definition
///
/// ### Example Source
///
/// ```rust,ignore
/// union Bits { big: u32, little: [u8; 4] }
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Union {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub name: Ident,
    pub generics: Option<GenericDeclarations>,
    pub wheres: Vec<Where>,
    pub fields: Vec<Attributed<StructDefinitionFieldNamed>>,
    pub whitespace: Vec<Whitespace>,
}

/// An enumeration of multiple types
///
/// ### Example Source
///
/// ```rust,ignore
/// pub enum Option<T> { Some(T), None }
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Enum {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub name: Ident,
    pub generics: Option<GenericDeclarations>,
    pub wheres: Vec<Where>,
    pub variants: Vec<Attributed<EnumVariant>>,
    pub whitespace: Vec<Whitespace>,
}

/// A single member of an enum
///
/// ### Example Source
///
/// ```rust,ignore
/// pub enum Option<T> { Some(T), None }
/// //                   ^^^^^^^  ^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct EnumVariant {
    pub extent: Extent,
    pub name: Ident,
    pub body: EnumVariantBody,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)] // TODO: HasExtent?
pub enum EnumVariantBody {
    Tuple(Vec<Attributed<StructDefinitionFieldUnnamed>>),
    Struct(StructDefinitionBodyBrace),
    Unit(Option<Attributed<Expression>>),
}

#[derive(Debug, Visit, Decompose)] // TODO: HasExtent?
pub enum Argument {
    SelfArgument(SelfArgument),
    Named(NamedArgument),
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum SelfArgument {
    Longhand(SelfArgumentLonghand),
    Shorthand(SelfArgumentShorthand),
}

/// The `self` argument with an explicit type
///
/// ### Example Source
///
/// ```rust,ignore
/// impl A { fn b(self: Box<Self>) {} }
/// //            ^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct SelfArgumentLonghand {
    pub extent: Extent,
    pub is_mut: Option<Extent>,
    pub name: Ident,
    pub typ: Type,
    pub whitespace: Vec<Whitespace>,
}

/// The `self` argument with an implicit type
///
/// ### Example Source
///
/// ```rust,ignore
/// impl A { fn b(&mut self) {} }
/// //            ^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct SelfArgumentShorthand {
    pub extent: Extent,
    pub qualifier: Option<SelfArgumentShorthandQualifier>,
    pub name: Ident,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum SelfArgumentShorthandQualifier {
    Reference(TypeReferenceKind),
    Mut(Extent),
}

/// A function argument with a name
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a(age: u8) {}
/// //   ^^^^^^^
/// ```
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

/// An argument of a trait's function declaration
///
/// ### Example Source
///
/// ```rust,ignore
/// trait X { fn a(b: bool, i32); }
/// //             ^^^^^^^  ^^^
/// ```
// TODO: "Trait impl" sounds confusing; why not just trait or trait defn?
// TODO: "named" is a lie here, as well
#[derive(Debug, Visit)] // HasExtent?
pub struct TraitImplArgumentNamed {
    pub name: Option<Pattern>,
    pub typ: Type,
    pub whitespace: Vec<Whitespace>,
}

/// A single where clause
///
/// ### Example Source
///
/// ```rust,ignore
/// struct A where for<'a> &'a str: Sized {}
/// //             ^^^^^^^^^^^^^^^^^^^^^^
/// ```
// TODO: rename to where clause?
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Where {
    pub extent: Extent,
    pub higher_ranked_trait_bounds: Vec<Lifetime>,
    pub kind: WhereKind,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum WhereKind {
    Lifetime(WhereLifetime),
    Type(WhereType),
}

/// A single where clause applying to a lifetime
///
/// ### Example Source
///
/// ```rust,ignore
/// struct A<'a, 'b> where &'a: &'b {}
/// //                     ^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct WhereLifetime {
    pub extent: Extent,
    pub name: Lifetime,
    pub bounds: Vec<Lifetime>,
}

/// A single where clause applying to a type
///
/// ### Example Source
///
/// ```rust,ignore
/// struct A<T> where A: Debug {}
/// //                ^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct WhereType {
    pub extent: Extent,
    pub name: Type,
    pub bounds: TraitBounds,
}

/// The trait bounds of a type
///
/// ### Example Source
///
/// ```rust,ignore
/// struct A<'a, T> where A: 'a + ?Sized + Debug {}
/// //                       ^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TraitBounds {
    pub extent: Extent,
    pub types: Vec<TraitBound>,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum TraitBound {
    Lifetime(TraitBoundLifetime),
    Normal(TraitBoundNormal),
    Relaxed(TraitBoundRelaxed),
}

/// A lifetime bound on a type
///
/// ### Example Source
///
/// ```rust,ignore
/// struct A<'a, T> where A: 'a + ?Sized + Debug {}
/// //                       ^^
///
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TraitBoundLifetime {
    pub extent: Extent,
    pub lifetime: Lifetime,
}

/// A standard trait bound on a type
///
/// ### Example Source
///
/// ```rust,ignore
/// struct A<'a, T> where A: 'a + ?Sized + Debug {}
/// //                                     ^^^^^
///
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TraitBoundNormal {
    pub extent: Extent,
    pub typ: TraitBoundType,
}

/// A relaxed trait bound on a type
///
/// ### Example Source
///
/// ```rust,ignore
/// struct A<'a, T> where A: 'a + ?Sized + Debug {}
/// //                            ^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TraitBoundRelaxed {
    pub extent: Extent,
    pub typ: TraitBoundType,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum TraitBoundType {
    Named(TypeNamed),
    // TODO: HRTB Trait bounds don't really allow references or fn types, just named
    // We need to create a smaller enum here
    HigherRankedTraitBounds(TypeHigherRankedTraitBounds),
}

/// A collection of statements and an optional final expression
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { if true {} else {} }
/// //               ^^      ^^
/// //     ^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Block {
    pub extent: Extent,
    pub statements: Vec<Statement>,
    pub expression: Option<Attributed<Expression>>,
    pub whitespace: Vec<Whitespace>,
}

/// A block which allows calling unsafe code
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { unsafe {} }
/// //       ^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct UnsafeBlock {
    pub extent: Extent,
    pub body: Box<Block>,
    pub whitespace: Vec<Whitespace>,
}

/// An expression surrounded by parenthesis
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { (1 + 1) }
/// //       ^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Parenthetical {
    pub extent: Extent,
    pub expression: Box<Attributed<Expression>>,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum Statement {
    Expression(Attributed<Expression>),
    Item(Attributed<Item>),
    Empty(Extent),
}

/// An element that can have attributes applied to it.
///
/// ### Example Source
///
/// ```rust,ignore
/// #[inline(never)] fn a() {}
/// ```
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

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
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

/// A single unexpanded macro
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { println!("Hello, world!"); }
/// //       ^^^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct MacroCall {
    pub extent: Extent,
    pub name: Ident,
    pub arg: Option<Ident>,
    pub args: MacroCallArgs,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum MacroCallArgs {
    Paren(Extent),
    Curly(Extent),
    Square(Extent),
}

/// A variable declaration
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { let b: u8 = 42; }
/// //       ^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Let {
    pub extent: Extent,
    pub pattern: Pattern,
    pub typ: Option<Type>,
    pub value: Option<Box<Attributed<Expression>>>,
    pub whitespace: Vec<Whitespace>,
}

/// A tuple expression
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { (100, true, 42.42); }
/// //       ^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Tuple {
    pub extent: Extent,
    pub members: Vec<Attributed<Expression>>,
}

/// The question mark / try / early exit operator
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { 42?; }
/// //       ^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TryOperator {
    pub extent: Extent,
    pub target: Box<Attributed<Expression>>,
}

/// Access to a field of a struct
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { thing.one; }
/// //       ^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct FieldAccess {
    pub extent: Extent,
    pub target: Box<Attributed<Expression>>,
    pub field: FieldName,
}

#[derive(Debug, HasExtent, ExtentIndex, Decompose)]
pub enum FieldName {
    Path(PathComponent),
    Number(Extent),
}

/// A number literal
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { 0xDEAD_BEEF; }
/// //       ^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Number {
    pub extent: Extent,
    pub is_negative: Option<Extent>,
    pub value: NumberValue,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum NumberValue {
    Binary(NumberBinary),
    Decimal(NumberDecimal),
    Hexadecimal(NumberHexadecimal),
    Octal(NumberOctal),
}

/// A binary number literal
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { 0b0110_u8; }
/// //       ^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct NumberBinary {
    pub extent: Extent,
    pub decimal: Extent,
    pub fraction: Option<Extent>,
    pub exponent: Option<Extent>,
    pub suffix: Option<Extent>,
}

/// A decimal number literal
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { 1234.5678_f32; }
/// //       ^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct NumberDecimal {
    pub extent: Extent,
    pub decimal: Extent,
    pub fraction: Option<Extent>,
    pub exponent: Option<Extent>,
    pub suffix: Option<Extent>,
}

/// A hexadecimal number literal
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { 0xAA_BB_CC_DD; }
/// //       ^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct NumberHexadecimal {
    pub extent: Extent,
    pub decimal: Extent,
    pub fraction: Option<Extent>,
    pub exponent: Option<Extent>,
    pub suffix: Option<Extent>,
}

/// An octal number literal
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { 0o0755; }
/// //       ^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct NumberOctal {
    pub extent: Extent,
    pub decimal: Extent,
    pub fraction: Option<Extent>,
    pub exponent: Option<Extent>,
    pub suffix: Option<Extent>,
}

/// A variable expression
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { my_variable; }
/// //       ^^^^^^^^^^^
/// ```
// TODO: This name is too generic
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Value {
    pub extent: Extent,
    pub name: PathedIdent,
    pub literal: Option<StructLiteral>,
}

/// Literal creation of a struct
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { Monster { hp: 42, gold: 100 } }
/// //       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct StructLiteral {
    pub extent: Extent,
    pub fields: Vec<StructLiteralField>,
    pub splat: Option<Box<Attributed<Expression>>>,
    pub whitespace: Vec<Whitespace>,
}

/// A field of a struct literal
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { Monster { hp: 42, gold: 100 } }
/// //                 ^^^^^^  ^^^^^^^^^
/// ```
#[derive(Debug, Visit)] // TODO: HasExtent?
pub struct StructLiteralField {
    pub name: Ident,
    pub value: Attributed<Expression>,
    pub whitespace: Vec<Whitespace>,
}

/// A function, method, or closure call
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { greet_user("Vivian"); }
/// //       ^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Call {
    pub extent: Extent,
    pub target: Box<Attributed<Expression>>,
    pub args: Vec<Attributed<Expression>>,
}

/// The iterator-based loop
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { for i in 0..10 {} }
/// //       ^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct ForLoop {
    pub extent: Extent,
    pub label: Option<Lifetime>,
    pub pattern: Pattern,
    pub iter: Box<Attributed<Expression>>,
    pub body: Box<Block>,
    pub whitespace: Vec<Whitespace>,
}

/// The infinite loop
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { loop {} }
/// //       ^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Loop {
    pub extent: Extent,
    pub label: Option<Lifetime>,
    pub body: Box<Block>,
    pub whitespace: Vec<Whitespace>,
}

/// The conditional, pattern-matching variable scope
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { if let Some(name) = current_player {} }
/// //       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct IfLet {
    pub extent: Extent,
    pub pattern: Pattern,
    pub value: Box<Attributed<Expression>>,
    pub body: Box<Block>,
    pub whitespace: Vec<Whitespace>,
}

/// The boolean-based loop
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { while players_count < 1 {} }
/// //       ^^^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct While {
    pub extent: Extent,
    pub label: Option<Lifetime>,
    pub value: Box<Attributed<Expression>>,
    pub body: Box<Block>,
    pub whitespace: Vec<Whitespace>,
}

/// The conditional, pattern-matching loop
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { while let Some(i) = iterator.next() {} }
/// //       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct WhileLet {
    pub extent: Extent,
    pub label: Option<Lifetime>,
    pub pattern: Pattern,
    pub value: Box<Attributed<Expression>>,
    pub body: Box<Block>,
    pub whitespace: Vec<Whitespace>,
}

/// A unary operator
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { !false; }
/// //       ^^^^^^
/// ```
// TODO: Should this be the same as dereference? What about reference?
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
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

/// A binary operator
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { 1 + 1; }
/// //       ^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Binary {
    pub extent: Extent,
    pub op: BinaryOp,
    pub lhs: Box<Attributed<Expression>>,
    pub rhs: Box<Attributed<Expression>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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

/// Boolean conditional control flow
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { if a {} else if b {} else {} }
/// //       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct If {
    pub extent: Extent,
    pub condition: Box<Attributed<Expression>>,
    pub body: Box<Block>,
    pub more: Vec<If>,
    pub else_body: Option<Box<Block>>,
    pub whitespace: Vec<Whitespace>,
}

/// Pattern-matching conditional control flow
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { match 1 { 0 => true, 1 => { false } _ => true } }
/// //       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Match {
    pub extent: Extent,
    pub head: Box<Attributed<Expression>>,
    pub arms: Vec<MatchArm>,
    pub whitespace: Vec<Whitespace>,
}

/// A single pattern of a `match`
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { match 1 { 0 if false => true, _ => { true } } }
/// //                 ^^^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct MatchArm {
    pub extent: Extent,
    pub attributes: Vec<Attribute>,
    pub pattern: Vec<Pattern>,
    pub guard: Option<Attributed<Expression>>,
    pub hand: MatchHand,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum MatchHand {
    Brace(Attributed<Expression>),
    Expression(Attributed<Expression>),
}

/// An exclusive range
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { 0..10; }
/// //       ^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
// TODO: rename "exclusive"
pub struct Range {
    pub extent: Extent,
    pub lhs: Option<Box<Attributed<Expression>>>,
    pub rhs: Option<Box<Attributed<Expression>>>,
}

/// An inclusive range
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { 0..=10; }
/// //       ^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct RangeInclusive {
    pub extent: Extent,
    pub lhs: Option<Box<Attributed<Expression>>>,
    #[visit(ignore)]
    pub operator: RangeInclusiveOperator,
    pub rhs: Option<Box<Attributed<Expression>>>,
}

#[derive(Debug, HasExtent, ExtentIndex, Decompose)]
pub enum RangeInclusiveOperator {
    Legacy(Extent),
    Recommended(Extent),
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum Array {
    Explicit(ArrayExplicit),
    Repeated(ArrayRepeated),
}

/// An array with all members explcitly listed
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { [1, 2, 3]; }
/// //       ^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct ArrayExplicit {
    pub extent: Extent,
    pub values: Vec<Attributed<Expression>>,
}

/// An array with an example value and a length
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { [42; 10]; }
/// //       ^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct ArrayRepeated {
    pub extent: Extent,
    pub value: Box<Attributed<Expression>>,
    pub count: Box<Attributed<Expression>>,
    pub whitespace: Vec<Whitespace>,
}

/// The `box` keyword expression
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { box 42; }
/// //       ^^^^^^
/// ```
// TODO: Rename this visitor function?
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct ExpressionBox {
    pub extent: Extent,
    pub target: Box<Attributed<Expression>>,
}

/// Inline type conversion
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { 42u8 as u64; }
/// //       ^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct AsType {
    pub extent: Extent,
    pub target: Box<Attributed<Expression>>,
    pub typ: Type,
}

/// Inline type specification
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { iterator.collect() : Vec<_>; }
/// //       ^^^^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Ascription {
    pub extent: Extent,
    pub target: Box<Attributed<Expression>>,
    pub typ: Type,
}

/// A character literal
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { 'x'; }
/// //       ^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Character {
    pub extent: Extent,
    pub value: Extent,
}

/// A string literal
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { "hello"; }
/// //       ^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct String {
    pub extent: Extent,
    pub value: Extent,
}

/// A byte literal
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { b'x'; }
/// //       ^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Byte {
    pub extent: Extent,
    pub value: Character,
}

/// A byte string literal
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { b"hello"; }
/// //       ^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct ByteString {
    pub extent: Extent,
    pub value: String,
}

/// The square-bracket operator for slicing and indexing
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { value[0]; }
/// //       ^^^^^^^^
/// ```
// TODO: rename to "index"?
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Slice {
    pub extent: Extent,
    pub target: Box<Attributed<Expression>>,
    pub index: Box<Attributed<Expression>>,
}

/// A closure
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { move |a| a + 2; }
/// //       ^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Closure {
    pub extent: Extent,
    #[visit(ignore)]
    pub is_move: bool,
    pub args: Vec<ClosureArg>,
    pub return_type: Option<Type>,
    pub body: Box<Attributed<Expression>>,
    pub whitespace: Vec<Whitespace>,
}

/// A closure's argument
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { |a, b: i32| a + b; }
/// //        ^  ^^^^^^
/// ```
#[derive(Debug, Visit)] // TODO: HasExtent?
pub struct ClosureArg {
    pub name: Pattern,
    pub typ: Option<Type>,
    pub whitespace: Vec<Whitespace>,
}

/// A reference of an expression
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { & mut 42; }
/// //       ^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Reference {
    pub extent: Extent,
    pub is_mutable: Option<Extent>,
    pub target: Box<Attributed<Expression>>,
}

/// A dereference of an expression
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { *42; }
/// //       ^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Dereference {
    pub extent: Extent,
    pub target: Box<Attributed<Expression>>,
    pub whitespace: Vec<Whitespace>,
}

/// Type disambiguation
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { <Vec<u8> as IntoIterator>::into_iter(scores); }
/// //       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Disambiguation {
    pub extent: Extent,
    pub from_type: Type,
    pub to_type: Option<TypeNamed>,
    pub components: Vec<PathComponent>,
    pub whitespace: Vec<Whitespace>,
}

/// Exit from a function or closure
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { return 42; }
/// //       ^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Return {
    pub extent: Extent,
    pub value: Option<Box<Attributed<Expression>>>,
    pub whitespace: Vec<Whitespace>,
}

/// Advance to the next iteration of a loop
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { 'v: loop { continue 'v; } }
/// //                  ^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Continue {
    pub extent: Extent,
    pub label: Option<Lifetime>,
    pub whitespace: Vec<Whitespace>,
}

/// Exit from a loop
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { loop { break 42; } }
/// //              ^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Break {
    pub extent: Extent,
    pub label: Option<Lifetime>,
    pub value: Option<Box<Attributed<Expression>>>,
    pub whitespace: Vec<Whitespace>,
}

/// A component used in pattern matching
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a(mut b: i32) { if let Some(foo) = x {} }
/// //   ^^^^^                ^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Pattern {
    pub extent: Extent,
    pub name: Option<PatternName>,
    pub kind: PatternKind,
}

/// A renaming of a matched pattern
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { let ref mut a @ _; }
/// //           ^^^^^^^^^^^
/// ```
// TODO: clarify name to show it's stranger?
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct PatternName {
    pub extent: Extent,
    pub is_ref: Option<Extent>,
    pub is_mut: Option<Extent>,
    pub name: Ident,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
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

/// A basic pattern match
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { let ref mut b; }
/// //           ^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct PatternIdent {
    pub extent: Extent,
    pub is_ref: Option<Extent>,
    pub is_mut: Option<Extent>,
    pub ident: PathedIdent,
    pub tuple: Option<PatternTuple>,
}

/// Pattern matching against a struct
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { let Monster { name, .. }; }
/// //           ^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct PatternStruct {
    pub extent: Extent,
    pub name: PathedIdent,
    pub fields: Vec<PatternStructField>,
    #[visit(ignore)]
    pub wildcard: bool,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, Visit, Decompose)] // TODO: HasExtent?
pub enum PatternStructField {
    Long(PatternStructFieldLong),
    Short(PatternStructFieldShort),
}

/// Pattern matching a struct's field with recursive patterns
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { let Monster { name: scary_name }; }
/// //                     ^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct PatternStructFieldLong {
    pub extent: Extent,
    pub name: Ident,
    pub pattern: Pattern,
    pub whitespace: Vec<Whitespace>,
}

/// Pattern matching a struct's field with simple names
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { let Monster { name }; }
/// //                     ^^^^
/// ```
#[derive(Debug, Visit)] // TODO: HasExtent?
pub struct PatternStructFieldShort {
    pub ident: PatternIdent
}

/// Pattern matching a tuple
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { let (tx, rx); }
/// //           ^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct PatternTuple {
    pub extent: Extent,
    pub members: Vec<PatternTupleMember>,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum PatternTupleMember {
    Pattern(Pattern),
    Wildcard(Extent),
}

/// Pattern matching a slice
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { let [a, b, ..]; }
/// //           ^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct PatternSlice {
    pub extent: Extent,
    pub members: Vec<PatternSliceMember>,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum PatternSliceMember {
    Pattern(Pattern),
    Subslice(PatternSliceSubslice),
    Wildcard(Extent),
}

/// Pattern matching a subslice
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { let [a, ref mut b..]; }
/// //               ^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct PatternSliceSubslice {
    pub extent: Extent,
    pub is_ref: Option<Extent>,
    pub is_mut: Option<Extent>,
    pub name: Ident,
}

/// Pattern matching a byte literal
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { let Some(b'x'); }
/// //                ^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct PatternByte {
    pub extent: Extent,
    pub value: Byte,
}

/// Pattern matching a character literal
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { let Some('x') }
/// //                ^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct PatternCharacter {
    pub extent: Extent,
    pub value: Character,
}

/// Pattern matching a byte string literal
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { let Some(b"abc") }
/// //                ^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct PatternByteString {
    pub extent: Extent,
    pub value: ByteString,
}

/// Pattern matching a string literal
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { let Some("abc"); }
/// //                ^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct PatternString {
    pub extent: Extent,
    pub value: String,
}

/// Pattern matching a number literal
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { let 0xDEAD_BEEF; }
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct PatternNumber {
    pub extent: Extent,
    pub is_negative: Option<Extent>,
    pub value: Number,
}

/// A macro call that expands into a pattern
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { let magic!(); }
/// //           ^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct PatternMacroCall {
    pub extent: Extent,
    pub value: MacroCall,
}

/// Pattern matching against an exclusive range
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { let 0..10; }
/// //           ^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct PatternRangeExclusive {
    pub extent: Extent,
    pub start: PatternRangeComponent,
    pub end: PatternRangeComponent,
    pub whitespace: Vec<Whitespace>,
}

/// Pattern matching against an inclusive range
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { let 0..=10; }
/// //           ^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct PatternRangeInclusive {
    pub extent: Extent,
    pub start: PatternRangeComponent,
    #[visit(ignore)]
    pub operator: RangeInclusiveOperator,
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

/// Pattern matching against a reference
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { let &mut x; }
/// //           ^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct PatternReference {
    pub extent: Extent,
    pub is_mut: Option<Extent>,
    pub pattern: Box<Pattern>,
    pub whitespace: Vec<Whitespace>,
}

/// Pattern matching against a box
///
/// ### Example Source
///
/// ```rust,ignore
/// fn a() { let box x; }
/// //           ^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct PatternBox {
    pub extent: Extent,
    pub pattern: Box<Pattern>,
    pub whitespace: Vec<Whitespace>,
}

/// Defines a trait
///
/// ### Example Source
///
/// ```rust,ignore
/// pub trait Iterator { type Item; fn next(&mut self) -> Option<Self::Item>; }
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
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

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum TraitMember {
    Const(TraitMemberConst),
    Function(TraitMemberFunction),
    Type(TraitMemberType),
    MacroCall(MacroCall),
}

/// A trait's function
///
/// ### Example Source
///
/// ```rust,ignore
/// pub trait Iterator { type Item; fn next(&mut self) -> Option<Self::Item>; }
/// //                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TraitMemberFunction {
    pub extent: Extent,
    pub header: TraitImplFunctionHeader,
    pub body: Option<Block>,
}

/// A trait's associated type
///
/// ### Example Source
///
/// ```rust,ignore
/// pub trait Iterator { type Item; fn next(&mut self) -> Option<Self::Item>; }
/// //                   ^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TraitMemberType {
    pub extent: Extent,
    pub name: Ident,
    pub bounds: Option<TraitBounds>,
    pub default: Option<Type>,
    pub whitespace: Vec<Whitespace>,
}

/// A trait's associated constant
///
/// ### Example Source
///
/// ```rust,ignore
/// pub trait Number { const MAX: Self; }
/// //                 ^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TraitMemberConst {
    pub extent: Extent,
    pub name: Ident,
    pub typ: Type,
    pub value: Option<Attributed<Expression>>,
    pub whitespace: Vec<Whitespace>,
}

/// Implementation details for a type
///
/// ### Example Source
///
/// ```rust,ignore
/// impl Ogre { const GOLD: u8 = 200; fn health(&self) -> u16 { 42 } }
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Impl {
    pub extent: Extent,
    pub is_unsafe: Option<Extent>,
    pub generics: Option<GenericDeclarations>,
    pub kind: ImplKind,
    pub wheres: Vec<Where>,
    pub body: Vec<Attributed<ImplMember>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum ImplKind {
    Trait(ImplOfTrait),
    Inherent(ImplOfInherent),
}

/// An implementation of a trait for a type
///
/// ### Example Source
///
/// ```rust,ignore
/// impl Monster for Ogre { }
/// //   ^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct ImplOfTrait {
    pub extent: Extent,
    pub is_negative: Option<Extent>,
    pub trait_name: Type, // TODO: namedtype only?
    pub type_name: ImplOfTraitType,
    pub whitespace: Vec<Whitespace>,
}

/// Inherent implementation for a type
///
/// ### Example Source
///
/// ```rust,ignore
/// impl Ogre {}
/// //   ^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct ImplOfInherent {
    pub extent: Extent,
    pub type_name: Type,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum ImplOfTraitType {
    Type(Type),
    Wildcard(Extent),
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum ImplMember {
    Const(ImplConst),
    Function(ImplFunction),
    Type(ImplType),
    MacroCall(MacroCall),
}

/// A function in an implementation block
///
/// ### Example Source
///
/// ```rust,ignore
/// impl Ogre { fn roar(&self) {} }
/// //          ^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct ImplFunction {
    pub extent: Extent,
    pub header: FunctionHeader,
    pub body: Block,
}

/// A type in an implementation block
///
/// ### Example Source
///
/// ```rust,ignore
/// impl Monster for Ogre { type Gold = u8; }
/// //                      ^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct ImplType {
    pub extent: Extent,
    pub name: Ident,
    pub typ: Type,
    pub whitespace: Vec<Whitespace>,
}

/// A constant in an implementation block
///
/// ### Example Source
///
/// ```rust,ignore
/// impl Ogre { const GOLD: u8 = 42; }
/// //          ^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct ImplConst {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub name: Ident,
    pub typ: Type,
    pub value: Attributed<Expression>,
    pub whitespace: Vec<Whitespace>,
}

/// An extern crate declaration
///
/// ### Example Source
///
/// ```rust,ignore
/// pub extern crate fuzzy_pickles as neat_code;
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Crate {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub name: Ident,
    pub rename: Option<Ident>,
    pub whitespace: Vec<Whitespace>,
}

/// Functions, types, and variables provided via FFI
///
/// ### Example Source
///
/// ```rust,ignore
/// extern "C" { fn putc(c: u8); }
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct ExternBlock {
    pub extent: Extent,
    pub abi: Option<String>,
    pub members: Vec<Attributed<ExternBlockMember>>,
    pub whitespace: Vec<Whitespace>,
}

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum ExternBlockMember {
    Function(ExternBlockMemberFunction),
    Static(ExternBlockMemberStatic),
    Type(ExternBlockMemberType),
}

/// A static variable accessed via FFI
///
/// ### Example Source
///
/// ```rust,ignore
/// extern "C" { static VERSION: *const u8; }
/// //           ^^^^^^^^^^^^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct ExternBlockMemberStatic {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub is_mut: Option<Extent>,
    pub name: Ident,
    pub typ: Type,
    pub whitespace: Vec<Whitespace>,
}

/// An opaque FFI type
///
/// ### Example Source
///
/// ```rust,ignore
/// extern "C" { type CoolType; }
/// //           ^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct ExternBlockMemberType {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub name: Ident,
}

/// A function provided from FFI
///
/// ### Example Source
///
/// ```rust,ignore
/// extern "C" { fn putc(c: u8); }
/// //           ^^^^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
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

#[derive(Debug, HasExtent, ExtentIndex, Visit, Decompose)]
pub enum ExternBlockMemberFunctionArgument {
    Named(ExternBlockMemberFunctionArgumentNamed),
    Variadic(ExternBlockMemberFunctionArgumentVariadic),
}

/// A named argument to a FFI function
///
/// ### Example Source
///
/// ```rust,ignore
/// extern "C" { fn printf(s: *const u8, ...); }
/// //                     ^^^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct ExternBlockMemberFunctionArgumentNamed {
    pub extent: Extent,
    pub name: Pattern,
    pub typ: Type,
    pub whitespace: Vec<Whitespace>,
}

/// A variadic list of arguments to a FFI function
///
/// ### Example Source
///
/// ```rust,ignore
/// extern "C" { fn printf(s: *const u8, ...); }
/// //                                   ^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct ExternBlockMemberFunctionArgumentVariadic {
    pub extent: Extent,
}

/// A type alias
///
/// ### Example Source
///
/// ```rust,ignore
/// type Point = (i32, i32);
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct TypeAlias {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub name: Ident,
    pub generics: Option<GenericDeclarations>,
    pub wheres: Vec<Where>,
    pub defn: Type,
    pub whitespace: Vec<Whitespace>,
}

/// A module of code
///
/// ### Example Source
///
/// ```rust,ignore
/// mod details {}
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
pub struct Module {
    pub extent: Extent,
    pub visibility: Option<Visibility>,
    pub name: Ident,
    pub body: Option<Vec<Attributed<Item>>>,
    pub whitespace: Vec<Whitespace>,
}

/// Visibility modifiers for an element
///
/// ### Example Source
///
/// ```rust,ignore
///     pub(crate) struct Ogre;
/// //  ^^^^^^^^^^
/// ```
#[derive(Debug, HasExtent, ExtentIndex, Visit)]
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
