//! Visitors of the AST

use crate::ast::*;
use crate::Extent;

/// An AST node that can be visited
pub trait Visit {
    fn visit<'ast, V>(&'ast self, _: &mut V)
    where
        V: Visitor<'ast>;

    fn visit_mut<V>(&mut self, _: &mut V)
    where
        V: VisitorMut;
}

impl<T> Visit for Box<T>
where
    T: Visit,
{
    fn visit<'ast, V>(&'ast self, v: &mut V)
    where
        V: Visitor<'ast>,
    {
        (**self).visit(v)
    }

    fn visit_mut<V>(&mut self, v: &mut V)
    where
        V: VisitorMut,
    {
        (**self).visit_mut(v)
    }
}

impl<T> Visit for Option<T>
where
    T: Visit,
{
    fn visit<'ast, V>(&'ast self, v: &mut V)
    where
        V: Visitor<'ast>,
    {
        if let Some(i) = self {
            i.visit(v)
        }
    }

    fn visit_mut<V>(&mut self, v: &mut V)
    where
        V: VisitorMut,
    {
        if let Some(i) = self {
            i.visit_mut(v)
        }
    }
}

impl<T> Visit for Vec<T>
where
    T: Visit,
{
    fn visit<'ast, V>(&'ast self, v: &mut V)
    where
        V: Visitor<'ast>
    {
        for i in self {
            i.visit(v)
        }
    }

    fn visit_mut<V>(&mut self, v: &mut V)
    where
        V: VisitorMut
    {
        for i in self {
            i.visit_mut(v)
        }
    }
}

// Cheap hack to avoid having to annotate every terminal `Extent`;
// just visit them and don't do anything. An extent without any
// context is pretty useless.
impl Visit for Extent {
    fn visit<'ast, V>(&'ast self, _v: &mut V)
    where
        V: Visitor<'ast>
    {}

    fn visit_mut<V>(&mut self, _v: &mut V)
    where
        V: VisitorMut
    {}
}

/// Directs the visitor to continue processing the children of the
/// current code or not.
#[derive(Debug, PartialEq)]
pub enum Control {
    Continue,
    Break
}

/// A visitor of AST nodes
///
/// On entry of a node, the corresponding `visit_*` method will be
/// called. This method may return a [`Control`] to avoid descending
/// into child nodes.
///
/// After visiting all the children of a node, the corresponding
/// `exit_*` method will be called.
///
/// For ease-of-use, all methods in this trait have a default
/// implementation. Consumers of the trait only need to implement
/// methods for the nodes they are interested in. The default
/// implementations for the `visit_*` methods continues processing on
/// every node.
// TODO: rename `visit_*` to `entry_*`?
pub trait Visitor<'ast> {
    fn visit_argument(&mut self, _: &'ast Argument) -> Control { Control::Continue }
    fn visit_array(&mut self, _: &'ast Array) -> Control { Control::Continue }
    fn visit_array_explicit(&mut self, _: &'ast ArrayExplicit) -> Control { Control::Continue }
    fn visit_array_repeated(&mut self, _: &'ast ArrayRepeated) -> Control { Control::Continue }
    fn visit_as_type(&mut self, _: &'ast AsType) -> Control { Control::Continue }
    fn visit_ascription(&mut self, _: &'ast Ascription) -> Control { Control::Continue }
    fn visit_associated_type(&mut self, _: &'ast AssociatedType) -> Control { Control::Continue }
    fn visit_associated_type_value(&mut self, _: &'ast AssociatedTypeValue) -> Control { Control::Continue }
    fn visit_associated_type_value_equal(&mut self, _: &'ast AssociatedTypeValueEqual) -> Control { Control::Continue }
    fn visit_associated_type_value_bound(&mut self, _: &'ast AssociatedTypeValueBound) -> Control { Control::Continue }
    fn visit_async_block(&mut self, _: &'ast AsyncBlock) -> Control { Control::Continue }
    fn visit_attribute(&mut self, _: &'ast Attribute) -> Control { Control::Continue }
    fn visit_attribute_literal(&mut self, _: &'ast AttributeLiteral) -> Control { Control::Continue }
    fn visit_attribute_containing(&mut self, _: &'ast AttributeContaining) -> Control { Control::Continue }
    fn visit_attribute_containing_literal(&mut self, _: &'ast AttributeContainingLiteral) -> Control { Control::Continue }
    fn visit_attributed_enum_variant(&mut self, _: &'ast Attributed<EnumVariant>) -> Control { Control::Continue }
    fn visit_attributed_expression(&mut self, _: &'ast Attributed<Expression>) -> Control { Control::Continue }
    fn visit_attributed_extern_block_member(&mut self, _: &'ast Attributed<ExternBlockMember>) -> Control { Control::Continue }
    fn visit_attributed_generic_declaration_const(&mut self, _: &'ast Attributed<GenericDeclarationConst>) -> Control { Control::Continue }
    fn visit_attributed_generic_declaration_lifetime(&mut self, _: &'ast Attributed<GenericDeclarationLifetime>) -> Control { Control::Continue }
    fn visit_attributed_generic_declaration_type(&mut self, _: &'ast Attributed<GenericDeclarationType>) -> Control { Control::Continue }
    fn visit_attributed_impl_member(&mut self, _: &'ast Attributed<ImplMember>) -> Control { Control::Continue }
    fn visit_attributed_item(&mut self, _: &'ast Attributed<Item>) -> Control { Control::Continue }
    fn visit_attributed_pattern_struct_field(&mut self, _: &'ast Attributed<PatternStructField>) -> Control { Control::Continue }
    fn visit_attributed_struct_definition_field_named(&mut self, _: &'ast Attributed<StructDefinitionFieldNamed>) -> Control { Control::Continue }
    fn visit_attributed_struct_definition_field_unnamed(&mut self, _: &'ast Attributed<StructDefinitionFieldUnnamed>) -> Control { Control::Continue }
    fn visit_attributed_struct_literal_field(&mut self, _: &'ast Attributed<StructLiteralField>) -> Control { Control::Continue }
    fn visit_attributed_trait_member(&mut self, _: &'ast Attributed<TraitMember>) -> Control { Control::Continue }
    fn visit_binary(&mut self, _: &'ast Binary) -> Control { Control::Continue }
    fn visit_block(&mut self, _: &'ast Block) -> Control { Control::Continue }
    fn visit_break(&mut self, _: &'ast Break) -> Control { Control::Continue }
    fn visit_byte(&mut self, _: &'ast Byte) -> Control { Control::Continue }
    fn visit_byte_string(&mut self, _: &'ast ByteString) -> Control { Control::Continue }
    fn visit_call(&mut self, _: &'ast Call) -> Control { Control::Continue }
    fn visit_character(&mut self, _: &'ast Character) -> Control { Control::Continue }
    fn visit_closure(&mut self, _: &'ast Closure) -> Control { Control::Continue }
    fn visit_closure_arg(&mut self, _: &'ast ClosureArg) -> Control { Control::Continue }
    fn visit_comment(&mut self, _: &'ast Comment) -> Control { Control::Continue }
    fn visit_const(&mut self, _: &'ast Const) -> Control { Control::Continue }
    fn visit_const_block(&mut self, _: &'ast ConstBlock) -> Control { Control::Continue }
    fn visit_continue(&mut self, _: &'ast Continue) -> Control { Control::Continue }
    fn visit_crate(&mut self, _: &'ast Crate) -> Control { Control::Continue }
    fn visit_dereference(&mut self, _: &'ast Dereference) -> Control { Control::Continue }
    fn visit_disambiguation(&mut self, _: &'ast Disambiguation) -> Control { Control::Continue }
    fn visit_enum(&mut self, _: &'ast Enum) -> Control { Control::Continue }
    fn visit_enum_variant(&mut self, _: &'ast EnumVariant) -> Control { Control::Continue }
    fn visit_enum_variant_body(&mut self, _: &'ast EnumVariantBody) -> Control { Control::Continue }
    fn visit_expression(&mut self, _: &'ast Expression) -> Control { Control::Continue }
    fn visit_expression_box(&mut self, _: &'ast ExpressionBox) -> Control { Control::Continue }
    fn visit_extern_block(&mut self, _: &'ast ExternBlock) -> Control { Control::Continue }
    fn visit_extern_block_member(&mut self, _: &'ast ExternBlockMember) -> Control { Control::Continue }
    fn visit_extern_block_member_function(&mut self, _: &'ast ExternBlockMemberFunction) -> Control { Control::Continue }
    fn visit_extern_block_member_function_argument(&mut self, _: &'ast ExternBlockMemberFunctionArgument) -> Control { Control::Continue }
    fn visit_extern_block_member_function_argument_named(&mut self, _: &'ast ExternBlockMemberFunctionArgumentNamed) -> Control { Control::Continue }
    fn visit_extern_block_member_function_argument_variadic(&mut self, _: &'ast ExternBlockMemberFunctionArgumentVariadic) -> Control { Control::Continue }
    fn visit_extern_block_member_static(&mut self, _: &'ast ExternBlockMemberStatic) -> Control { Control::Continue }
    fn visit_extern_block_member_type(&mut self, _: &'ast ExternBlockMemberType) -> Control { Control::Continue }
    fn visit_field_access(&mut self, _: &'ast FieldAccess) -> Control { Control::Continue }
    fn visit_field_name(&mut self, _: &'ast FieldName) -> Control { Control::Continue }
    fn visit_file(&mut self, _: &'ast File) -> Control { Control::Continue }
    fn visit_for_loop(&mut self, _: &'ast ForLoop) -> Control { Control::Continue }
    fn visit_function(&mut self, _: &'ast Function) -> Control { Control::Continue }
    fn visit_function_header(&mut self, _: &'ast FunctionHeader) -> Control { Control::Continue }
    fn visit_function_qualifiers(&mut self, _: &'ast FunctionQualifiers) -> Control { Control::Continue }
    fn visit_generic_declaration_const(&mut self, _: &'ast GenericDeclarationConst) -> Control { Control::Continue }
    fn visit_generic_declaration_lifetime(&mut self, _: &'ast GenericDeclarationLifetime) -> Control { Control::Continue }
    fn visit_generic_declaration_type(&mut self, _: &'ast GenericDeclarationType) -> Control { Control::Continue }
    fn visit_generic_declarations(&mut self, _: &'ast GenericDeclarations) -> Control { Control::Continue }
    fn visit_ident(&mut self, _: &'ast Ident) -> Control { Control::Continue }
    fn visit_if(&mut self, _: &'ast If) -> Control { Control::Continue }
    fn visit_if_let(&mut self, _: &'ast IfLet) -> Control { Control::Continue }
    fn visit_impl(&mut self, _: &'ast Impl) -> Control { Control::Continue }
    fn visit_impl_const(&mut self, _: &'ast ImplConst) -> Control { Control::Continue }
    fn visit_impl_function(&mut self, _: &'ast ImplFunction) -> Control { Control::Continue }
    fn visit_impl_kind(&mut self, _: &'ast ImplKind) -> Control { Control::Continue }
    fn visit_impl_member(&mut self, _: &'ast ImplMember) -> Control { Control::Continue }
    fn visit_impl_of_inherent(&mut self, _: &'ast ImplOfInherent) -> Control { Control::Continue }
    fn visit_impl_of_trait(&mut self, _: &'ast ImplOfTrait) -> Control { Control::Continue }
    fn visit_impl_of_trait_type(&mut self, _: &'ast ImplOfTraitType) -> Control { Control::Continue }
    fn visit_impl_type(&mut self, _: &'ast ImplType) -> Control { Control::Continue }
    fn visit_item(&mut self, _: &'ast Item) -> Control { Control::Continue }
    fn visit_let(&mut self, _: &'ast Let) -> Control { Control::Continue }
    fn visit_lifetime(&mut self, _: &'ast Lifetime) -> Control { Control::Continue }
    fn visit_loop(&mut self, _: &'ast Loop) -> Control { Control::Continue }
    fn visit_macro_call(&mut self, _: &'ast MacroCall) -> Control { Control::Continue }
    fn visit_macro_call_args(&mut self, _: &'ast MacroCallArgs) -> Control { Control::Continue }
    fn visit_match(&mut self, _: &'ast Match) -> Control { Control::Continue }
    fn visit_match_arm(&mut self, _: &'ast MatchArm) -> Control { Control::Continue }
    fn visit_match_hand(&mut self, _: &'ast MatchHand) -> Control { Control::Continue }
    fn visit_module(&mut self, _: &'ast Module) -> Control { Control::Continue }
    fn visit_named_argument(&mut self, _: &'ast NamedArgument) -> Control { Control::Continue }
    fn visit_number(&mut self, _: &'ast Number) -> Control { Control::Continue }
    fn visit_number_value(&mut self, _: &'ast NumberValue) -> Control { Control::Continue }
    fn visit_number_binary(&mut self, _: &'ast NumberBinary) -> Control { Control::Continue }
    fn visit_number_decimal(&mut self, _: &'ast NumberDecimal) -> Control { Control::Continue }
    fn visit_number_hexadecimal(&mut self, _: &'ast NumberHexadecimal) -> Control { Control::Continue }
    fn visit_number_octal(&mut self, _: &'ast NumberOctal) -> Control { Control::Continue }
    fn visit_parenthetical(&mut self, _: &'ast Parenthetical) -> Control { Control::Continue }
    fn visit_path(&mut self, _: &'ast Path) -> Control { Control::Continue }
    fn visit_path_component(&mut self, _: &'ast PathComponent) -> Control { Control::Continue }
    fn visit_pathed_ident(&mut self, _: &'ast PathedIdent) -> Control { Control::Continue }
    fn visit_pattern(&mut self, _: &'ast Pattern) -> Control { Control::Continue }
    fn visit_pattern_box(&mut self, _: &'ast PatternBox) -> Control { Control::Continue }
    fn visit_pattern_byte(&mut self, _: &'ast PatternByte) -> Control { Control::Continue }
    fn visit_pattern_byte_string(&mut self, _: &'ast PatternByteString) -> Control { Control::Continue }
    fn visit_pattern_character(&mut self, _: &'ast PatternCharacter) -> Control { Control::Continue }
    fn visit_pattern_ident(&mut self, _: &'ast PatternIdent) -> Control { Control::Continue }
    fn visit_pattern_kind(&mut self, _: &'ast PatternKind) -> Control { Control::Continue }
    fn visit_pattern_macro_call(&mut self, _: &'ast PatternMacroCall) -> Control { Control::Continue }
    fn visit_pattern_name(&mut self, _: &'ast PatternName) -> Control { Control::Continue }
    fn visit_pattern_number(&mut self, _: &'ast PatternNumber) -> Control { Control::Continue }
    fn visit_pattern_range_component(&mut self, _: &'ast PatternRangeComponent) -> Control { Control::Continue }
    fn visit_pattern_range_exclusive(&mut self, _: &'ast PatternRangeExclusive) -> Control { Control::Continue }
    fn visit_pattern_range_inclusive(&mut self, _: &'ast PatternRangeInclusive) -> Control { Control::Continue }
    fn visit_pattern_reference(&mut self, _: &'ast PatternReference) -> Control { Control::Continue }
    fn visit_pattern_slice(&mut self, _: &'ast PatternSlice) -> Control { Control::Continue }
    fn visit_pattern_slice_member(&mut self, _: &'ast PatternSliceMember) -> Control { Control::Continue }
    fn visit_pattern_slice_subslice(&mut self, _: &'ast PatternSliceSubslice) -> Control { Control::Continue }
    fn visit_pattern_string(&mut self, _: &'ast PatternString) -> Control { Control::Continue }
    fn visit_pattern_struct(&mut self, _: &'ast PatternStruct) -> Control { Control::Continue }
    fn visit_pattern_struct_field(&mut self, _: &'ast PatternStructField) -> Control { Control::Continue }
    fn visit_pattern_struct_field_long(&mut self, _: &'ast PatternStructFieldLong) -> Control { Control::Continue }
    fn visit_pattern_struct_field_short(&mut self, _: &'ast PatternStructFieldShort) -> Control { Control::Continue }
    fn visit_pattern_tuple(&mut self, _: &'ast PatternTuple) -> Control { Control::Continue }
    fn visit_pattern_tuple_member(&mut self, _: &'ast PatternTupleMember) -> Control { Control::Continue }
    fn visit_range(&mut self, _: &'ast Range) -> Control { Control::Continue }
    fn visit_range_inclusive(&mut self, _: &'ast RangeInclusive) -> Control { Control::Continue }
    fn visit_reference(&mut self, _: &'ast Reference) -> Control { Control::Continue }
    fn visit_return(&mut self, _: &'ast Return) -> Control { Control::Continue }
    fn visit_self_argument(&mut self, _: &'ast SelfArgument) -> Control { Control::Continue }
    fn visit_self_argument_longhand(&mut self, _: &'ast SelfArgumentLonghand) -> Control { Control::Continue }
    fn visit_self_argument_shorthand(&mut self, _: &'ast SelfArgumentShorthand) -> Control { Control::Continue }
    fn visit_self_argument_shorthand_qualifier(&mut self, _: &'ast SelfArgumentShorthandQualifier) -> Control { Control::Continue }
    fn visit_slice(&mut self, _: &'ast Slice) -> Control { Control::Continue }
    fn visit_statement(&mut self, _: &'ast Statement) -> Control { Control::Continue }
    fn visit_static(&mut self, _: &'ast Static) -> Control { Control::Continue }
    fn visit_string(&mut self, _: &'ast String) -> Control { Control::Continue }
    fn visit_struct(&mut self, _: &'ast Struct) -> Control { Control::Continue }
    fn visit_struct_definition_body(&mut self, _: &'ast StructDefinitionBody) -> Control { Control::Continue }
    fn visit_struct_definition_body_brace(&mut self, _: &'ast StructDefinitionBodyBrace) -> Control { Control::Continue }
    fn visit_struct_definition_body_tuple(&mut self, _: &'ast StructDefinitionBodyTuple) -> Control { Control::Continue }
    fn visit_struct_definition_field_named(&mut self, _: &'ast StructDefinitionFieldNamed) -> Control { Control::Continue }
    fn visit_struct_definition_field_unnamed(&mut self, _: &'ast StructDefinitionFieldUnnamed) -> Control { Control::Continue }
    fn visit_struct_literal(&mut self, _: &'ast StructLiteral) -> Control { Control::Continue }
    fn visit_struct_literal_field(&mut self, _: &'ast StructLiteralField) -> Control { Control::Continue }
    fn visit_trait(&mut self, _: &'ast Trait) -> Control { Control::Continue }
    fn visit_trait_bound(&mut self, _: &'ast TraitBound) -> Control { Control::Continue }
    fn visit_trait_bound_lifetime(&mut self, _: &'ast TraitBoundLifetime) -> Control { Control::Continue }
    fn visit_trait_bound_normal(&mut self, _: &'ast TraitBoundNormal) -> Control { Control::Continue }
    fn visit_trait_bound_relaxed(&mut self, _: &'ast TraitBoundRelaxed) -> Control { Control::Continue }
    fn visit_trait_bound_type(&mut self, _: &'ast TraitBoundType) -> Control { Control::Continue }
    fn visit_trait_bounds(&mut self, _: &'ast TraitBounds) -> Control { Control::Continue }
    fn visit_trait_impl_argument(&mut self, _: &'ast TraitImplArgument) -> Control { Control::Continue }
    fn visit_trait_impl_argument_named(&mut self, _: &'ast TraitImplArgumentNamed) -> Control { Control::Continue }
    fn visit_trait_impl_function_header(&mut self, _: &'ast TraitImplFunctionHeader) -> Control { Control::Continue }
    fn visit_trait_member(&mut self, _: &'ast TraitMember) -> Control { Control::Continue }
    fn visit_trait_member_const(&mut self, _: &'ast TraitMemberConst) -> Control { Control::Continue }
    fn visit_trait_member_function(&mut self, _: &'ast TraitMemberFunction) -> Control { Control::Continue }
    fn visit_trait_member_type(&mut self, _: &'ast TraitMemberType) -> Control { Control::Continue }
    fn visit_try_operator(&mut self, _: &'ast TryOperator) -> Control { Control::Continue }
    fn visit_tuple(&mut self, _: &'ast Tuple) -> Control { Control::Continue }
    fn visit_turbofish(&mut self, _: &'ast Turbofish) -> Control { Control::Continue }
    fn visit_type(&mut self, _: &'ast Type) -> Control { Control::Continue }
    fn visit_type_additional(&mut self, _: &'ast TypeAdditional) -> Control { Control::Continue }
    fn visit_type_alias(&mut self, _: &'ast TypeAlias) -> Control { Control::Continue }
    fn visit_type_array(&mut self, _: &'ast TypeArray) -> Control { Control::Continue }
    fn visit_type_disambiguation(&mut self, _: &'ast TypeDisambiguation) -> Control { Control::Continue }
    fn visit_type_dyn_trait(&mut self, _: &'ast TypeDynTrait) -> Control { Control::Continue }
    fn visit_type_function(&mut self, _: &'ast TypeFunction) -> Control { Control::Continue }
    fn visit_type_function_argument(&mut self, _: &'ast TypeFunctionArgument) -> Control { Control::Continue }
    fn visit_type_function_argument_named(&mut self, _: &'ast TypeFunctionArgumentNamed) -> Control { Control::Continue }
    fn visit_type_generics(&mut self, _: &'ast TypeGenerics) -> Control { Control::Continue }
    fn visit_type_generics_angle(&mut self, _: &'ast TypeGenericsAngle) -> Control { Control::Continue }
    fn visit_type_generics_angle_member(&mut self, _: &'ast TypeGenericsAngleMember) -> Control { Control::Continue }
    fn visit_type_generics_function(&mut self, _: &'ast TypeGenericsFunction) -> Control { Control::Continue }
    fn visit_type_higher_ranked_trait_bounds(&mut self, _: &'ast TypeHigherRankedTraitBounds) -> Control { Control::Continue }
    fn visit_type_higher_ranked_trait_bounds_child(&mut self, _: &'ast TypeHigherRankedTraitBoundsChild) -> Control { Control::Continue }
    fn visit_type_impl_trait(&mut self, _: &'ast TypeImplTrait) -> Control { Control::Continue }
    fn visit_type_kind(&mut self, _: &'ast TypeKind) -> Control { Control::Continue }
    fn visit_type_named(&mut self, _: &'ast TypeNamed) -> Control { Control::Continue }
    fn visit_type_named_component(&mut self, _: &'ast TypeNamedComponent) -> Control { Control::Continue }
    fn visit_type_pointer(&mut self, _: &'ast TypePointer) -> Control { Control::Continue }
    fn visit_type_reference(&mut self, _: &'ast TypeReference) -> Control { Control::Continue }
    fn visit_type_reference_kind(&mut self, _: &'ast TypeReferenceKind) -> Control { Control::Continue }
    fn visit_type_slice(&mut self, _: &'ast TypeSlice) -> Control { Control::Continue }
    fn visit_type_tuple(&mut self, _: &'ast TypeTuple) -> Control { Control::Continue }
    fn visit_unary(&mut self, _: &'ast Unary) -> Control { Control::Continue }
    fn visit_union(&mut self, _: &'ast Union) -> Control { Control::Continue }
    fn visit_unsafe_block(&mut self, _: &'ast UnsafeBlock) -> Control { Control::Continue }
    fn visit_use(&mut self, _: &'ast Use) -> Control { Control::Continue }
    fn visit_use_path(&mut self, _: &'ast UsePath) -> Control { Control::Continue }
    fn visit_use_tail(&mut self, _: &'ast UseTail) -> Control { Control::Continue }
    fn visit_use_tail_glob(&mut self, _: &'ast UseTailGlob) -> Control { Control::Continue }
    fn visit_use_tail_ident(&mut self, _: &'ast UseTailIdent) -> Control { Control::Continue }
    fn visit_use_tail_multi(&mut self, _: &'ast UseTailMulti) -> Control { Control::Continue }
    fn visit_value(&mut self, _: &'ast Value) -> Control { Control::Continue }
    fn visit_visibility(&mut self, _: &'ast Visibility) -> Control { Control::Continue }
    fn visit_visibility_public(&mut self, _: &'ast VisibilityPublic) -> Control { Control::Continue }
    fn visit_where(&mut self, _: &'ast Where) -> Control { Control::Continue }
    fn visit_where_kind(&mut self, _: &'ast WhereKind) -> Control { Control::Continue }
    fn visit_where_lifetime(&mut self, _: &'ast WhereLifetime) -> Control { Control::Continue }
    fn visit_where_type(&mut self, _: &'ast WhereType) -> Control { Control::Continue }
    fn visit_while(&mut self, _: &'ast While) -> Control { Control::Continue }
    fn visit_while_let(&mut self, _: &'ast WhileLet) -> Control { Control::Continue }
    fn visit_whitespace(&mut self, _: &'ast Whitespace) -> Control { Control::Continue }

    fn exit_argument(&mut self, _: &'ast Argument) {}
    fn exit_array(&mut self, _: &'ast Array) {}
    fn exit_array_explicit(&mut self, _: &'ast ArrayExplicit) {}
    fn exit_array_repeated(&mut self, _: &'ast ArrayRepeated) {}
    fn exit_as_type(&mut self, _: &'ast AsType) {}
    fn exit_ascription(&mut self, _: &'ast Ascription) {}
    fn exit_associated_type(&mut self, _: &'ast AssociatedType) {}
    fn exit_associated_type_value(&mut self, _: &'ast AssociatedTypeValue) {}
    fn exit_associated_type_value_equal(&mut self, _: &'ast AssociatedTypeValueEqual) {}
    fn exit_associated_type_value_bound(&mut self, _: &'ast AssociatedTypeValueBound) {}
    fn exit_async_block(&mut self, _: &'ast AsyncBlock) {}
    fn exit_attribute(&mut self, _: &'ast Attribute) {}
    fn exit_attribute_literal(&mut self, _: &'ast AttributeLiteral) {}
    fn exit_attribute_containing(&mut self, _: &'ast AttributeContaining) {}
    fn exit_attribute_containing_literal(&mut self, _: &'ast AttributeContainingLiteral) {}
    fn exit_attributed_enum_variant(&mut self, _: &'ast Attributed<EnumVariant>) {}
    fn exit_attributed_expression(&mut self, _: &'ast Attributed<Expression>) {}
    fn exit_attributed_extern_block_member(&mut self, _: &'ast Attributed<ExternBlockMember>) {}
    fn exit_attributed_generic_declaration_const(&mut self, _: &'ast Attributed<GenericDeclarationConst>) {}
    fn exit_attributed_generic_declaration_lifetime(&mut self, _: &'ast Attributed<GenericDeclarationLifetime>) {}
    fn exit_attributed_generic_declaration_type(&mut self, _: &'ast Attributed<GenericDeclarationType>) {}
    fn exit_attributed_impl_member(&mut self, _: &'ast Attributed<ImplMember>) {}
    fn exit_attributed_item(&mut self, _: &'ast Attributed<Item>) {}
    fn exit_attributed_pattern_struct_field(&mut self, _: &'ast Attributed<PatternStructField>) {}
    fn exit_attributed_struct_definition_field_named(&mut self, _: &'ast Attributed<StructDefinitionFieldNamed>) {}
    fn exit_attributed_struct_definition_field_unnamed(&mut self, _: &'ast Attributed<StructDefinitionFieldUnnamed>) {}
    fn exit_attributed_struct_literal_field(&mut self, _: &'ast Attributed<StructLiteralField>) {}
    fn exit_attributed_trait_member(&mut self, _: &'ast Attributed<TraitMember>) {}
    fn exit_binary(&mut self, _: &'ast Binary) {}
    fn exit_block(&mut self, _: &'ast Block) {}
    fn exit_break(&mut self, _: &'ast Break) {}
    fn exit_byte(&mut self, _: &'ast Byte) {}
    fn exit_byte_string(&mut self, _: &'ast ByteString) {}
    fn exit_call(&mut self, _: &'ast Call) {}
    fn exit_character(&mut self, _: &'ast Character) {}
    fn exit_closure(&mut self, _: &'ast Closure) {}
    fn exit_closure_arg(&mut self, _: &'ast ClosureArg) {}
    fn exit_comment(&mut self, _: &'ast Comment) {}
    fn exit_const(&mut self, _: &'ast Const) {}
    fn exit_const_block(&mut self, _: &'ast ConstBlock) {}
    fn exit_continue(&mut self, _: &'ast Continue) {}
    fn exit_crate(&mut self, _: &'ast Crate) {}
    fn exit_dereference(&mut self, _: &'ast Dereference) {}
    fn exit_disambiguation(&mut self, _: &'ast Disambiguation) {}
    fn exit_enum(&mut self, _: &'ast Enum) {}
    fn exit_enum_variant(&mut self, _: &'ast EnumVariant) {}
    fn exit_enum_variant_body(&mut self, _: &'ast EnumVariantBody) {}
    fn exit_expression(&mut self, _: &'ast Expression) {}
    fn exit_expression_box(&mut self, _: &'ast ExpressionBox) {}
    fn exit_extern_block(&mut self, _: &'ast ExternBlock) {}
    fn exit_extern_block_member(&mut self, _: &'ast ExternBlockMember) {}
    fn exit_extern_block_member_function(&mut self, _: &'ast ExternBlockMemberFunction) {}
    fn exit_extern_block_member_function_argument(&mut self, _: &'ast ExternBlockMemberFunctionArgument) {}
    fn exit_extern_block_member_function_argument_named(&mut self, _: &'ast ExternBlockMemberFunctionArgumentNamed) {}
    fn exit_extern_block_member_function_argument_variadic(&mut self, _: &'ast ExternBlockMemberFunctionArgumentVariadic) {}
    fn exit_extern_block_member_static(&mut self, _: &'ast ExternBlockMemberStatic) {}
    fn exit_extern_block_member_type(&mut self, _: &'ast ExternBlockMemberType) {}
    fn exit_field_access(&mut self, _: &'ast FieldAccess) {}
    fn exit_field_name(&mut self, _: &'ast FieldName) {}
    fn exit_file(&mut self, _: &'ast File) {}
    fn exit_for_loop(&mut self, _: &'ast ForLoop) {}
    fn exit_function(&mut self, _: &'ast Function) {}
    fn exit_function_header(&mut self, _: &'ast FunctionHeader) {}
    fn exit_function_qualifiers(&mut self, _: &'ast FunctionQualifiers) {}
    fn exit_generic_declaration_const(&mut self, _: &'ast GenericDeclarationConst) {}
    fn exit_generic_declaration_lifetime(&mut self, _: &'ast GenericDeclarationLifetime) {}
    fn exit_generic_declaration_type(&mut self, _: &'ast GenericDeclarationType) {}
    fn exit_generic_declarations(&mut self, _: &'ast GenericDeclarations) {}
    fn exit_ident(&mut self, _: &'ast Ident) {}
    fn exit_if(&mut self, _: &'ast If) {}
    fn exit_if_let(&mut self, _: &'ast IfLet) {}
    fn exit_impl(&mut self, _: &'ast Impl) {}
    fn exit_impl_const(&mut self, _: &'ast ImplConst) {}
    fn exit_impl_function(&mut self, _: &'ast ImplFunction) {}
    fn exit_impl_kind(&mut self, _: &'ast ImplKind) {}
    fn exit_impl_member(&mut self, _: &'ast ImplMember) {}
    fn exit_impl_of_inherent(&mut self, _: &'ast ImplOfInherent) {}
    fn exit_impl_of_trait(&mut self, _: &'ast ImplOfTrait) {}
    fn exit_impl_of_trait_type(&mut self, _: &'ast ImplOfTraitType) {}
    fn exit_impl_type(&mut self, _: &'ast ImplType) {}
    fn exit_item(&mut self, _: &'ast Item) {}
    fn exit_let(&mut self, _: &'ast Let) {}
    fn exit_lifetime(&mut self, _: &'ast Lifetime) {}
    fn exit_loop(&mut self, _: &'ast Loop) {}
    fn exit_macro_call(&mut self, _: &'ast MacroCall) {}
    fn exit_macro_call_args(&mut self, _: &'ast MacroCallArgs) {}
    fn exit_match(&mut self, _: &'ast Match) {}
    fn exit_match_arm(&mut self, _: &'ast MatchArm) {}
    fn exit_match_hand(&mut self, _: &'ast MatchHand) {}
    fn exit_module(&mut self, _: &'ast Module) {}
    fn exit_named_argument(&mut self, _: &'ast NamedArgument) {}
    fn exit_number(&mut self, _: &'ast Number) {}
    fn exit_number_value(&mut self, _: &'ast NumberValue) {}
    fn exit_number_binary(&mut self, _: &'ast NumberBinary) {}
    fn exit_number_decimal(&mut self, _: &'ast NumberDecimal) {}
    fn exit_number_hexadecimal(&mut self, _: &'ast NumberHexadecimal) {}
    fn exit_number_octal(&mut self, _: &'ast NumberOctal) {}
    fn exit_parenthetical(&mut self, _: &'ast Parenthetical) {}
    fn exit_path(&mut self, _: &'ast Path) {}
    fn exit_path_component(&mut self, _: &'ast PathComponent) {}
    fn exit_pathed_ident(&mut self, _: &'ast PathedIdent) {}
    fn exit_pattern(&mut self, _: &'ast Pattern) {}
    fn exit_pattern_box(&mut self, _: &'ast PatternBox) {}
    fn exit_pattern_byte(&mut self, _: &'ast PatternByte) {}
    fn exit_pattern_byte_string(&mut self, _: &'ast PatternByteString) {}
    fn exit_pattern_character(&mut self, _: &'ast PatternCharacter) {}
    fn exit_pattern_ident(&mut self, _: &'ast PatternIdent) {}
    fn exit_pattern_kind(&mut self, _: &'ast PatternKind) {}
    fn exit_pattern_macro_call(&mut self, _: &'ast PatternMacroCall) {}
    fn exit_pattern_name(&mut self, _: &'ast PatternName) {}
    fn exit_pattern_number(&mut self, _: &'ast PatternNumber) {}
    fn exit_pattern_range_component(&mut self, _: &'ast PatternRangeComponent) {}
    fn exit_pattern_range_exclusive(&mut self, _: &'ast PatternRangeExclusive) {}
    fn exit_pattern_range_inclusive(&mut self, _: &'ast PatternRangeInclusive) {}
    fn exit_pattern_reference(&mut self, _: &'ast PatternReference) {}
    fn exit_pattern_slice(&mut self, _: &'ast PatternSlice) {}
    fn exit_pattern_slice_member(&mut self, _: &'ast PatternSliceMember) {}
    fn exit_pattern_slice_subslice(&mut self, _: &'ast PatternSliceSubslice) {}
    fn exit_pattern_string(&mut self, _: &'ast PatternString) {}
    fn exit_pattern_struct(&mut self, _: &'ast PatternStruct) {}
    fn exit_pattern_struct_field(&mut self, _: &'ast PatternStructField) {}
    fn exit_pattern_struct_field_long(&mut self, _: &'ast PatternStructFieldLong) {}
    fn exit_pattern_struct_field_short(&mut self, _: &'ast PatternStructFieldShort) {}
    fn exit_pattern_tuple(&mut self, _: &'ast PatternTuple) {}
    fn exit_pattern_tuple_member(&mut self, _: &'ast PatternTupleMember) {}
    fn exit_range(&mut self, _: &'ast Range) {}
    fn exit_range_inclusive(&mut self, _: &'ast RangeInclusive) {}
    fn exit_reference(&mut self, _: &'ast Reference) {}
    fn exit_return(&mut self, _: &'ast Return) {}
    fn exit_self_argument(&mut self, _: &'ast SelfArgument) {}
    fn exit_self_argument_longhand(&mut self, _: &'ast SelfArgumentLonghand) {}
    fn exit_self_argument_shorthand(&mut self, _: &'ast SelfArgumentShorthand) {}
    fn exit_self_argument_shorthand_qualifier(&mut self, _: &'ast SelfArgumentShorthandQualifier) {}
    fn exit_slice(&mut self, _: &'ast Slice) {}
    fn exit_statement(&mut self, _: &'ast Statement) {}
    fn exit_static(&mut self, _: &'ast Static) {}
    fn exit_string(&mut self, _: &'ast String) {}
    fn exit_struct(&mut self, _: &'ast Struct) {}
    fn exit_struct_definition_body(&mut self, _: &'ast StructDefinitionBody) {}
    fn exit_struct_definition_body_brace(&mut self, _: &'ast StructDefinitionBodyBrace) {}
    fn exit_struct_definition_body_tuple(&mut self, _: &'ast StructDefinitionBodyTuple) {}
    fn exit_struct_definition_field_named(&mut self, _: &'ast StructDefinitionFieldNamed) {}
    fn exit_struct_definition_field_unnamed(&mut self, _: &'ast StructDefinitionFieldUnnamed) {}
    fn exit_struct_literal(&mut self, _: &'ast StructLiteral) {}
    fn exit_struct_literal_field(&mut self, _: &'ast StructLiteralField) {}
    fn exit_trait(&mut self, _: &'ast Trait) {}
    fn exit_trait_bound(&mut self, _: &'ast TraitBound) {}
    fn exit_trait_bound_lifetime(&mut self, _: &'ast TraitBoundLifetime) {}
    fn exit_trait_bound_normal(&mut self, _: &'ast TraitBoundNormal) {}
    fn exit_trait_bound_relaxed(&mut self, _: &'ast TraitBoundRelaxed) {}
    fn exit_trait_bound_type(&mut self, _: &'ast TraitBoundType) {}
    fn exit_trait_bounds(&mut self, _: &'ast TraitBounds) {}
    fn exit_trait_impl_argument(&mut self, _: &'ast TraitImplArgument) {}
    fn exit_trait_impl_argument_named(&mut self, _: &'ast TraitImplArgumentNamed) {}
    fn exit_trait_impl_function_header(&mut self, _: &'ast TraitImplFunctionHeader) {}
    fn exit_trait_member(&mut self, _: &'ast TraitMember) {}
    fn exit_trait_member_const(&mut self, _: &'ast TraitMemberConst) {}
    fn exit_trait_member_function(&mut self, _: &'ast TraitMemberFunction) {}
    fn exit_trait_member_type(&mut self, _: &'ast TraitMemberType) {}
    fn exit_try_operator(&mut self, _: &'ast TryOperator) {}
    fn exit_tuple(&mut self, _: &'ast Tuple) {}
    fn exit_turbofish(&mut self, _: &'ast Turbofish) {}
    fn exit_type(&mut self, _: &'ast Type) {}
    fn exit_type_additional(&mut self, _: &'ast TypeAdditional) {}
    fn exit_type_alias(&mut self, _: &'ast TypeAlias) {}
    fn exit_type_array(&mut self, _: &'ast TypeArray) {}
    fn exit_type_disambiguation(&mut self, _: &'ast TypeDisambiguation) {}
    fn exit_type_dyn_trait(&mut self, _: &'ast TypeDynTrait) {}
    fn exit_type_function(&mut self, _: &'ast TypeFunction) {}
    fn exit_type_function_argument(&mut self, _: &'ast TypeFunctionArgument) {}
    fn exit_type_function_argument_named(&mut self, _: &'ast TypeFunctionArgumentNamed) {}
    fn exit_type_generics(&mut self, _: &'ast TypeGenerics) {}
    fn exit_type_generics_angle(&mut self, _: &'ast TypeGenericsAngle) {}
    fn exit_type_generics_angle_member(&mut self, _: &'ast TypeGenericsAngleMember) {}
    fn exit_type_generics_function(&mut self, _: &'ast TypeGenericsFunction) {}
    fn exit_type_higher_ranked_trait_bounds(&mut self, _: &'ast TypeHigherRankedTraitBounds) {}
    fn exit_type_higher_ranked_trait_bounds_child(&mut self, _: &'ast TypeHigherRankedTraitBoundsChild) {}
    fn exit_type_impl_trait(&mut self, _: &'ast TypeImplTrait) {}
    fn exit_type_kind(&mut self, _: &'ast TypeKind) {}
    fn exit_type_named(&mut self, _: &'ast TypeNamed) {}
    fn exit_type_named_component(&mut self, _: &'ast TypeNamedComponent) {}
    fn exit_type_pointer(&mut self, _: &'ast TypePointer) {}
    fn exit_type_reference(&mut self, _: &'ast TypeReference) {}
    fn exit_type_reference_kind(&mut self, _: &'ast TypeReferenceKind) {}
    fn exit_type_slice(&mut self, _: &'ast TypeSlice) {}
    fn exit_type_tuple(&mut self, _: &'ast TypeTuple) {}
    fn exit_unary(&mut self, _: &'ast Unary) {}
    fn exit_union(&mut self, _: &'ast Union) {}
    fn exit_unsafe_block(&mut self, _: &'ast UnsafeBlock) {}
    fn exit_use(&mut self, _: &'ast Use) {}
    fn exit_use_path(&mut self, _: &'ast UsePath) {}
    fn exit_use_tail(&mut self, _: &'ast UseTail) {}
    fn exit_use_tail_glob(&mut self, _: &'ast UseTailGlob) {}
    fn exit_use_tail_ident(&mut self, _: &'ast UseTailIdent) {}
    fn exit_use_tail_multi(&mut self, _: &'ast UseTailMulti) {}
    fn exit_value(&mut self, _: &'ast Value) {}
    fn exit_visibility(&mut self, _: &'ast Visibility) {}
    fn exit_visibility_public(&mut self, _: &'ast VisibilityPublic) {}
    fn exit_where(&mut self, _: &'ast Where) {}
    fn exit_where_kind(&mut self, _: &'ast WhereKind) {}
    fn exit_where_lifetime(&mut self, _: &'ast WhereLifetime) {}
    fn exit_where_type(&mut self, _: &'ast WhereType) {}
    fn exit_while(&mut self, _: &'ast While) {}
    fn exit_while_let(&mut self, _: &'ast WhileLet) {}
    fn exit_whitespace(&mut self, _: &'ast Whitespace) {}
}

/// A visitor of mutable AST nodes
///
/// See [`Visitor`] for general visitor information. Unlike an
/// immutable visitor, a mutable visitor makes no attempt to allow
/// saving references to the AST nodes as aliasing would interfere
/// with the implementation. Since you could use this visitor to add
/// or remove AST nodes, saving a reference is of little use.
pub trait VisitorMut {
    fn visit_argument(&mut self, _: &mut Argument) -> Control { Control::Continue }
    fn visit_array(&mut self, _: &mut Array) -> Control { Control::Continue }
    fn visit_array_explicit(&mut self, _: &mut ArrayExplicit) -> Control { Control::Continue }
    fn visit_array_repeated(&mut self, _: &mut ArrayRepeated) -> Control { Control::Continue }
    fn visit_as_type(&mut self, _: &mut AsType) -> Control { Control::Continue }
    fn visit_ascription(&mut self, _: &mut Ascription) -> Control { Control::Continue }
    fn visit_associated_type(&mut self, _: &mut AssociatedType) -> Control { Control::Continue }
    fn visit_associated_type_value(&mut self, _: &mut AssociatedTypeValue) -> Control { Control::Continue }
    fn visit_associated_type_value_equal(&mut self, _: &mut AssociatedTypeValueEqual) -> Control { Control::Continue }
    fn visit_associated_type_value_bound(&mut self, _: &mut AssociatedTypeValueBound) -> Control { Control::Continue }
    fn visit_async_block(&mut self, _: &mut AsyncBlock) -> Control { Control::Continue }
    fn visit_attribute(&mut self, _: &mut Attribute) -> Control { Control::Continue }
    fn visit_attribute_literal(&mut self, _: &mut AttributeLiteral) -> Control { Control::Continue }
    fn visit_attribute_containing(&mut self, _: &mut AttributeContaining) -> Control { Control::Continue }
    fn visit_attribute_containing_literal(&mut self, _: &mut AttributeContainingLiteral) -> Control { Control::Continue }
    fn visit_attributed_enum_variant(&mut self, _: &mut Attributed<EnumVariant>) -> Control { Control::Continue }
    fn visit_attributed_expression(&mut self, _: &mut Attributed<Expression>) -> Control { Control::Continue }
    fn visit_attributed_extern_block_member(&mut self, _: &mut Attributed<ExternBlockMember>) -> Control { Control::Continue }
    fn visit_attributed_generic_declaration_const(&mut self, _: &mut Attributed<GenericDeclarationConst>) -> Control { Control::Continue }
    fn visit_attributed_generic_declaration_lifetime(&mut self, _: &mut Attributed<GenericDeclarationLifetime>) -> Control { Control::Continue }
    fn visit_attributed_generic_declaration_type(&mut self, _: &mut Attributed<GenericDeclarationType>) -> Control { Control::Continue }
    fn visit_attributed_impl_member(&mut self, _: &mut Attributed<ImplMember>) -> Control { Control::Continue }
    fn visit_attributed_item(&mut self, _: &mut Attributed<Item>) -> Control { Control::Continue }
    fn visit_attributed_pattern_struct_field(&mut self, _: &mut Attributed<PatternStructField>) -> Control { Control::Continue }
    fn visit_attributed_struct_definition_field_named(&mut self, _: &mut Attributed<StructDefinitionFieldNamed>) -> Control { Control::Continue }
    fn visit_attributed_struct_definition_field_unnamed(&mut self, _: &mut Attributed<StructDefinitionFieldUnnamed>) -> Control { Control::Continue }
    fn visit_attributed_struct_literal_field(&mut self, _: &mut Attributed<StructLiteralField>) -> Control { Control::Continue }
    fn visit_attributed_trait_member(&mut self, _: &mut Attributed<TraitMember>) -> Control { Control::Continue }
    fn visit_binary(&mut self, _: &mut Binary) -> Control { Control::Continue }
    fn visit_block(&mut self, _: &mut Block) -> Control { Control::Continue }
    fn visit_break(&mut self, _: &mut Break) -> Control { Control::Continue }
    fn visit_byte(&mut self, _: &mut Byte) -> Control { Control::Continue }
    fn visit_byte_string(&mut self, _: &mut ByteString) -> Control { Control::Continue }
    fn visit_call(&mut self, _: &mut Call) -> Control { Control::Continue }
    fn visit_character(&mut self, _: &mut Character) -> Control { Control::Continue }
    fn visit_closure(&mut self, _: &mut Closure) -> Control { Control::Continue }
    fn visit_closure_arg(&mut self, _: &mut ClosureArg) -> Control { Control::Continue }
    fn visit_comment(&mut self, _: &mut Comment) -> Control { Control::Continue }
    fn visit_const(&mut self, _: &mut Const) -> Control { Control::Continue }
    fn visit_const_block(&mut self, _: &mut ConstBlock) -> Control { Control::Continue }
    fn visit_continue(&mut self, _: &mut Continue) -> Control { Control::Continue }
    fn visit_crate(&mut self, _: &mut Crate) -> Control { Control::Continue }
    fn visit_dereference(&mut self, _: &mut Dereference) -> Control { Control::Continue }
    fn visit_disambiguation(&mut self, _: &mut Disambiguation) -> Control { Control::Continue }
    fn visit_enum(&mut self, _: &mut Enum) -> Control { Control::Continue }
    fn visit_enum_variant(&mut self, _: &mut EnumVariant) -> Control { Control::Continue }
    fn visit_enum_variant_body(&mut self, _: &mut EnumVariantBody) -> Control { Control::Continue }
    fn visit_expression(&mut self, _: &mut Expression) -> Control { Control::Continue }
    fn visit_expression_box(&mut self, _: &mut ExpressionBox) -> Control { Control::Continue }
    fn visit_extern_block(&mut self, _: &mut ExternBlock) -> Control { Control::Continue }
    fn visit_extern_block_member(&mut self, _: &mut ExternBlockMember) -> Control { Control::Continue }
    fn visit_extern_block_member_function(&mut self, _: &mut ExternBlockMemberFunction) -> Control { Control::Continue }
    fn visit_extern_block_member_function_argument(&mut self, _: &mut ExternBlockMemberFunctionArgument) -> Control { Control::Continue }
    fn visit_extern_block_member_function_argument_named(&mut self, _: &mut ExternBlockMemberFunctionArgumentNamed) -> Control { Control::Continue }
    fn visit_extern_block_member_function_argument_variadic(&mut self, _: &mut ExternBlockMemberFunctionArgumentVariadic) -> Control { Control::Continue }
    fn visit_extern_block_member_static(&mut self, _: &mut ExternBlockMemberStatic) -> Control { Control::Continue }
    fn visit_extern_block_member_type(&mut self, _: &mut ExternBlockMemberType) -> Control { Control::Continue }
    fn visit_field_access(&mut self, _: &mut FieldAccess) -> Control { Control::Continue }
    fn visit_field_name(&mut self, _: &mut FieldName) -> Control { Control::Continue }
    fn visit_file(&mut self, _: &mut File) -> Control { Control::Continue }
    fn visit_for_loop(&mut self, _: &mut ForLoop) -> Control { Control::Continue }
    fn visit_function(&mut self, _: &mut Function) -> Control { Control::Continue }
    fn visit_function_header(&mut self, _: &mut FunctionHeader) -> Control { Control::Continue }
    fn visit_function_qualifiers(&mut self, _: &mut FunctionQualifiers) -> Control { Control::Continue }
    fn visit_generic_declaration_const(&mut self, _: &mut GenericDeclarationConst) -> Control { Control::Continue }
    fn visit_generic_declaration_lifetime(&mut self, _: &mut GenericDeclarationLifetime) -> Control { Control::Continue }
    fn visit_generic_declaration_type(&mut self, _: &mut GenericDeclarationType) -> Control { Control::Continue }
    fn visit_generic_declarations(&mut self, _: &mut GenericDeclarations) -> Control { Control::Continue }
    fn visit_ident(&mut self, _: &mut Ident) -> Control { Control::Continue }
    fn visit_if(&mut self, _: &mut If) -> Control { Control::Continue }
    fn visit_if_let(&mut self, _: &mut IfLet) -> Control { Control::Continue }
    fn visit_impl(&mut self, _: &mut Impl) -> Control { Control::Continue }
    fn visit_impl_const(&mut self, _: &mut ImplConst) -> Control { Control::Continue }
    fn visit_impl_function(&mut self, _: &mut ImplFunction) -> Control { Control::Continue }
    fn visit_impl_kind(&mut self, _: &mut ImplKind) -> Control { Control::Continue }
    fn visit_impl_member(&mut self, _: &mut ImplMember) -> Control { Control::Continue }
    fn visit_impl_of_inherent(&mut self, _: &mut ImplOfInherent) -> Control { Control::Continue }
    fn visit_impl_of_trait(&mut self, _: &mut ImplOfTrait) -> Control { Control::Continue }
    fn visit_impl_of_trait_type(&mut self, _: &mut ImplOfTraitType) -> Control { Control::Continue }
    fn visit_impl_type(&mut self, _: &mut ImplType) -> Control { Control::Continue }
    fn visit_item(&mut self, _: &mut Item) -> Control { Control::Continue }
    fn visit_let(&mut self, _: &mut Let) -> Control { Control::Continue }
    fn visit_lifetime(&mut self, _: &mut Lifetime) -> Control { Control::Continue }
    fn visit_loop(&mut self, _: &mut Loop) -> Control { Control::Continue }
    fn visit_macro_call(&mut self, _: &mut MacroCall) -> Control { Control::Continue }
    fn visit_macro_call_args(&mut self, _: &mut MacroCallArgs) -> Control { Control::Continue }
    fn visit_match(&mut self, _: &mut Match) -> Control { Control::Continue }
    fn visit_match_arm(&mut self, _: &mut MatchArm) -> Control { Control::Continue }
    fn visit_match_hand(&mut self, _: &mut MatchHand) -> Control { Control::Continue }
    fn visit_module(&mut self, _: &mut Module) -> Control { Control::Continue }
    fn visit_named_argument(&mut self, _: &mut NamedArgument) -> Control { Control::Continue }
    fn visit_number(&mut self, _: &mut Number) -> Control { Control::Continue }
    fn visit_number_value(&mut self, _: &mut NumberValue) -> Control { Control::Continue }
    fn visit_number_binary(&mut self, _: &mut NumberBinary) -> Control { Control::Continue }
    fn visit_number_decimal(&mut self, _: &mut NumberDecimal) -> Control { Control::Continue }
    fn visit_number_hexadecimal(&mut self, _: &mut NumberHexadecimal) -> Control { Control::Continue }
    fn visit_number_octal(&mut self, _: &mut NumberOctal) -> Control { Control::Continue }
    fn visit_parenthetical(&mut self, _: &mut Parenthetical) -> Control { Control::Continue }
    fn visit_path(&mut self, _: &mut Path) -> Control { Control::Continue }
    fn visit_path_component(&mut self, _: &mut PathComponent) -> Control { Control::Continue }
    fn visit_pathed_ident(&mut self, _: &mut PathedIdent) -> Control { Control::Continue }
    fn visit_pattern(&mut self, _: &mut Pattern) -> Control { Control::Continue }
    fn visit_pattern_box(&mut self, _: &mut PatternBox) -> Control { Control::Continue }
    fn visit_pattern_byte(&mut self, _: &mut PatternByte) -> Control { Control::Continue }
    fn visit_pattern_byte_string(&mut self, _: &mut PatternByteString) -> Control { Control::Continue }
    fn visit_pattern_character(&mut self, _: &mut PatternCharacter) -> Control { Control::Continue }
    fn visit_pattern_ident(&mut self, _: &mut PatternIdent) -> Control { Control::Continue }
    fn visit_pattern_kind(&mut self, _: &mut PatternKind) -> Control { Control::Continue }
    fn visit_pattern_macro_call(&mut self, _: &mut PatternMacroCall) -> Control { Control::Continue }
    fn visit_pattern_name(&mut self, _: &mut PatternName) -> Control { Control::Continue }
    fn visit_pattern_number(&mut self, _: &mut PatternNumber) -> Control { Control::Continue }
    fn visit_pattern_range_component(&mut self, _: &mut PatternRangeComponent) -> Control { Control::Continue }
    fn visit_pattern_range_exclusive(&mut self, _: &mut PatternRangeExclusive) -> Control { Control::Continue }
    fn visit_pattern_range_inclusive(&mut self, _: &mut PatternRangeInclusive) -> Control { Control::Continue }
    fn visit_pattern_reference(&mut self, _: &mut PatternReference) -> Control { Control::Continue }
    fn visit_pattern_slice(&mut self, _: &mut PatternSlice) -> Control { Control::Continue }
    fn visit_pattern_slice_member(&mut self, _: &mut PatternSliceMember) -> Control { Control::Continue }
    fn visit_pattern_slice_subslice(&mut self, _: &mut PatternSliceSubslice) -> Control { Control::Continue }
    fn visit_pattern_string(&mut self, _: &mut PatternString) -> Control { Control::Continue }
    fn visit_pattern_struct(&mut self, _: &mut PatternStruct) -> Control { Control::Continue }
    fn visit_pattern_struct_field(&mut self, _: &mut PatternStructField) -> Control { Control::Continue }
    fn visit_pattern_struct_field_long(&mut self, _: &mut PatternStructFieldLong) -> Control { Control::Continue }
    fn visit_pattern_struct_field_short(&mut self, _: &mut PatternStructFieldShort) -> Control { Control::Continue }
    fn visit_pattern_tuple(&mut self, _: &mut PatternTuple) -> Control { Control::Continue }
    fn visit_pattern_tuple_member(&mut self, _: &mut PatternTupleMember) -> Control { Control::Continue }
    fn visit_range(&mut self, _: &mut Range) -> Control { Control::Continue }
    fn visit_range_inclusive(&mut self, _: &mut RangeInclusive) -> Control { Control::Continue }
    fn visit_reference(&mut self, _: &mut Reference) -> Control { Control::Continue }
    fn visit_return(&mut self, _: &mut Return) -> Control { Control::Continue }
    fn visit_self_argument(&mut self, _: &mut SelfArgument) -> Control { Control::Continue }
    fn visit_self_argument_longhand(&mut self, _: &mut SelfArgumentLonghand) -> Control { Control::Continue }
    fn visit_self_argument_shorthand(&mut self, _: &mut SelfArgumentShorthand) -> Control { Control::Continue }
    fn visit_self_argument_shorthand_qualifier(&mut self, _: &mut SelfArgumentShorthandQualifier) -> Control { Control::Continue }
    fn visit_slice(&mut self, _: &mut Slice) -> Control { Control::Continue }
    fn visit_statement(&mut self, _: &mut Statement) -> Control { Control::Continue }
    fn visit_static(&mut self, _: &mut Static) -> Control { Control::Continue }
    fn visit_string(&mut self, _: &mut String) -> Control { Control::Continue }
    fn visit_struct(&mut self, _: &mut Struct) -> Control { Control::Continue }
    fn visit_struct_definition_body(&mut self, _: &mut StructDefinitionBody) -> Control { Control::Continue }
    fn visit_struct_definition_body_brace(&mut self, _: &mut StructDefinitionBodyBrace) -> Control { Control::Continue }
    fn visit_struct_definition_body_tuple(&mut self, _: &mut StructDefinitionBodyTuple) -> Control { Control::Continue }
    fn visit_struct_definition_field_named(&mut self, _: &mut StructDefinitionFieldNamed) -> Control { Control::Continue }
    fn visit_struct_definition_field_unnamed(&mut self, _: &mut StructDefinitionFieldUnnamed) -> Control { Control::Continue }
    fn visit_struct_literal(&mut self, _: &mut StructLiteral) -> Control { Control::Continue }
    fn visit_struct_literal_field(&mut self, _: &mut StructLiteralField) -> Control { Control::Continue }
    fn visit_trait(&mut self, _: &mut Trait) -> Control { Control::Continue }
    fn visit_trait_bound(&mut self, _: &mut TraitBound) -> Control { Control::Continue }
    fn visit_trait_bound_lifetime(&mut self, _: &mut TraitBoundLifetime) -> Control { Control::Continue }
    fn visit_trait_bound_normal(&mut self, _: &mut TraitBoundNormal) -> Control { Control::Continue }
    fn visit_trait_bound_relaxed(&mut self, _: &mut TraitBoundRelaxed) -> Control { Control::Continue }
    fn visit_trait_bound_type(&mut self, _: &mut TraitBoundType) -> Control { Control::Continue }
    fn visit_trait_bounds(&mut self, _: &mut TraitBounds) -> Control { Control::Continue }
    fn visit_trait_impl_argument(&mut self, _: &mut TraitImplArgument) -> Control { Control::Continue }
    fn visit_trait_impl_argument_named(&mut self, _: &mut TraitImplArgumentNamed) -> Control { Control::Continue }
    fn visit_trait_impl_function_header(&mut self, _: &mut TraitImplFunctionHeader) -> Control { Control::Continue }
    fn visit_trait_member(&mut self, _: &mut TraitMember) -> Control { Control::Continue }
    fn visit_trait_member_const(&mut self, _: &mut TraitMemberConst) -> Control { Control::Continue }
    fn visit_trait_member_function(&mut self, _: &mut TraitMemberFunction) -> Control { Control::Continue }
    fn visit_trait_member_type(&mut self, _: &mut TraitMemberType) -> Control { Control::Continue }
    fn visit_try_operator(&mut self, _: &mut TryOperator) -> Control { Control::Continue }
    fn visit_tuple(&mut self, _: &mut Tuple) -> Control { Control::Continue }
    fn visit_turbofish(&mut self, _: &mut Turbofish) -> Control { Control::Continue }
    fn visit_type(&mut self, _: &mut Type) -> Control { Control::Continue }
    fn visit_type_additional(&mut self, _: &mut TypeAdditional) -> Control { Control::Continue }
    fn visit_type_alias(&mut self, _: &mut TypeAlias) -> Control { Control::Continue }
    fn visit_type_array(&mut self, _: &mut TypeArray) -> Control { Control::Continue }
    fn visit_type_disambiguation(&mut self, _: &mut TypeDisambiguation) -> Control { Control::Continue }
    fn visit_type_dyn_trait(&mut self, _: &mut TypeDynTrait) -> Control { Control::Continue }
    fn visit_type_function(&mut self, _: &mut TypeFunction) -> Control { Control::Continue }
    fn visit_type_function_argument(&mut self, _: &mut TypeFunctionArgument) -> Control { Control::Continue }
    fn visit_type_function_argument_named(&mut self, _: &mut TypeFunctionArgumentNamed) -> Control { Control::Continue }
    fn visit_type_generics(&mut self, _: &mut TypeGenerics) -> Control { Control::Continue }
    fn visit_type_generics_angle(&mut self, _: &mut TypeGenericsAngle) -> Control { Control::Continue }
    fn visit_type_generics_angle_member(&mut self, _: &mut TypeGenericsAngleMember) -> Control { Control::Continue }
    fn visit_type_generics_function(&mut self, _: &mut TypeGenericsFunction) -> Control { Control::Continue }
    fn visit_type_higher_ranked_trait_bounds(&mut self, _: &mut TypeHigherRankedTraitBounds) -> Control { Control::Continue }
    fn visit_type_higher_ranked_trait_bounds_child(&mut self, _: &mut TypeHigherRankedTraitBoundsChild) -> Control { Control::Continue }
    fn visit_type_impl_trait(&mut self, _: &mut TypeImplTrait) -> Control { Control::Continue }
    fn visit_type_kind(&mut self, _: &mut TypeKind) -> Control { Control::Continue }
    fn visit_type_named(&mut self, _: &mut TypeNamed) -> Control { Control::Continue }
    fn visit_type_named_component(&mut self, _: &mut TypeNamedComponent) -> Control { Control::Continue }
    fn visit_type_pointer(&mut self, _: &mut TypePointer) -> Control { Control::Continue }
    fn visit_type_reference(&mut self, _: &mut TypeReference) -> Control { Control::Continue }
    fn visit_type_reference_kind(&mut self, _: &mut TypeReferenceKind) -> Control { Control::Continue }
    fn visit_type_slice(&mut self, _: &mut TypeSlice) -> Control { Control::Continue }
    fn visit_type_tuple(&mut self, _: &mut TypeTuple) -> Control { Control::Continue }
    fn visit_unary(&mut self, _: &mut Unary) -> Control { Control::Continue }
    fn visit_union(&mut self, _: &mut Union) -> Control { Control::Continue }
    fn visit_unsafe_block(&mut self, _: &mut UnsafeBlock) -> Control { Control::Continue }
    fn visit_use(&mut self, _: &mut Use) -> Control { Control::Continue }
    fn visit_use_path(&mut self, _: &mut UsePath) -> Control { Control::Continue }
    fn visit_use_tail(&mut self, _: &mut UseTail) -> Control { Control::Continue }
    fn visit_use_tail_glob(&mut self, _: &mut UseTailGlob) -> Control { Control::Continue }
    fn visit_use_tail_ident(&mut self, _: &mut UseTailIdent) -> Control { Control::Continue }
    fn visit_use_tail_multi(&mut self, _: &mut UseTailMulti) -> Control { Control::Continue }
    fn visit_value(&mut self, _: &mut Value) -> Control { Control::Continue }
    fn visit_visibility(&mut self, _: &mut Visibility) -> Control { Control::Continue }
    fn visit_visibility_public(&mut self, _: &mut VisibilityPublic) -> Control { Control::Continue }
    fn visit_where(&mut self, _: &mut Where) -> Control { Control::Continue }
    fn visit_where_kind(&mut self, _: &mut WhereKind) -> Control { Control::Continue }
    fn visit_where_lifetime(&mut self, _: &mut WhereLifetime) -> Control { Control::Continue }
    fn visit_where_type(&mut self, _: &mut WhereType) -> Control { Control::Continue }
    fn visit_while(&mut self, _: &mut While) -> Control { Control::Continue }
    fn visit_while_let(&mut self, _: &mut WhileLet) -> Control { Control::Continue }
    fn visit_whitespace(&mut self, _: &mut Whitespace) -> Control { Control::Continue }

    fn exit_argument(&mut self, _: &mut Argument) {}
    fn exit_array(&mut self, _: &mut Array) {}
    fn exit_array_explicit(&mut self, _: &mut ArrayExplicit) {}
    fn exit_array_repeated(&mut self, _: &mut ArrayRepeated) {}
    fn exit_as_type(&mut self, _: &mut AsType) {}
    fn exit_ascription(&mut self, _: &mut Ascription) {}
    fn exit_associated_type(&mut self, _: &mut AssociatedType) {}
    fn exit_associated_type_value(&mut self, _: &mut AssociatedTypeValue) {}
    fn exit_associated_type_value_equal(&mut self, _: &mut AssociatedTypeValueEqual) {}
    fn exit_associated_type_value_bound(&mut self, _: &mut AssociatedTypeValueBound) {}
    fn exit_async_block(&mut self, _: &mut AsyncBlock) {}
    fn exit_attribute(&mut self, _: &mut Attribute) {}
    fn exit_attribute_literal(&mut self, _: &mut AttributeLiteral) {}
    fn exit_attribute_containing(&mut self, _: &mut AttributeContaining) {}
    fn exit_attribute_containing_literal(&mut self, _: &mut AttributeContainingLiteral) {}
    fn exit_attributed_enum_variant(&mut self, _: &mut Attributed<EnumVariant>) {}
    fn exit_attributed_expression(&mut self, _: &mut Attributed<Expression>) {}
    fn exit_attributed_extern_block_member(&mut self, _: &mut Attributed<ExternBlockMember>) {}
    fn exit_attributed_generic_declaration_const(&mut self, _: &mut Attributed<GenericDeclarationConst>) {}
    fn exit_attributed_generic_declaration_lifetime(&mut self, _: &mut Attributed<GenericDeclarationLifetime>) {}
    fn exit_attributed_generic_declaration_type(&mut self, _: &mut Attributed<GenericDeclarationType>) {}
    fn exit_attributed_impl_member(&mut self, _: &mut Attributed<ImplMember>) {}
    fn exit_attributed_item(&mut self, _: &mut Attributed<Item>) {}
    fn exit_attributed_pattern_struct_field(&mut self, _: &mut Attributed<PatternStructField>) {}
    fn exit_attributed_struct_definition_field_named(&mut self, _: &mut Attributed<StructDefinitionFieldNamed>) {}
    fn exit_attributed_struct_literal_field(&mut self, _: &mut Attributed<StructLiteralField>) {}
    fn exit_attributed_struct_definition_field_unnamed(&mut self, _: &mut Attributed<StructDefinitionFieldUnnamed>) {}
    fn exit_attributed_trait_member(&mut self, _: &mut Attributed<TraitMember>) {}
    fn exit_binary(&mut self, _: &mut Binary) {}
    fn exit_block(&mut self, _: &mut Block) {}
    fn exit_break(&mut self, _: &mut Break) {}
    fn exit_byte(&mut self, _: &mut Byte) {}
    fn exit_byte_string(&mut self, _: &mut ByteString) {}
    fn exit_call(&mut self, _: &mut Call) {}
    fn exit_character(&mut self, _: &mut Character) {}
    fn exit_closure(&mut self, _: &mut Closure) {}
    fn exit_closure_arg(&mut self, _: &mut ClosureArg) {}
    fn exit_comment(&mut self, _: &mut Comment) {}
    fn exit_const(&mut self, _: &mut Const) {}
    fn exit_const_block(&mut self, _: &mut ConstBlock) {}
    fn exit_continue(&mut self, _: &mut Continue) {}
    fn exit_crate(&mut self, _: &mut Crate) {}
    fn exit_dereference(&mut self, _: &mut Dereference) {}
    fn exit_disambiguation(&mut self, _: &mut Disambiguation) {}
    fn exit_enum(&mut self, _: &mut Enum) {}
    fn exit_enum_variant(&mut self, _: &mut EnumVariant) {}
    fn exit_enum_variant_body(&mut self, _: &mut EnumVariantBody) {}
    fn exit_expression(&mut self, _: &mut Expression) {}
    fn exit_expression_box(&mut self, _: &mut ExpressionBox) {}
    fn exit_extern_block(&mut self, _: &mut ExternBlock) {}
    fn exit_extern_block_member(&mut self, _: &mut ExternBlockMember) {}
    fn exit_extern_block_member_function(&mut self, _: &mut ExternBlockMemberFunction) {}
    fn exit_extern_block_member_function_argument(&mut self, _: &mut ExternBlockMemberFunctionArgument) {}
    fn exit_extern_block_member_function_argument_named(&mut self, _: &mut ExternBlockMemberFunctionArgumentNamed) {}
    fn exit_extern_block_member_function_argument_variadic(&mut self, _: &mut ExternBlockMemberFunctionArgumentVariadic) {}
    fn exit_extern_block_member_static(&mut self, _: &mut ExternBlockMemberStatic) {}
    fn exit_extern_block_member_type(&mut self, _: &mut ExternBlockMemberType) {}
    fn exit_field_access(&mut self, _: &mut FieldAccess) {}
    fn exit_field_name(&mut self, _: &mut FieldName) {}
    fn exit_file(&mut self, _: &mut File) {}
    fn exit_for_loop(&mut self, _: &mut ForLoop) {}
    fn exit_function(&mut self, _: &mut Function) {}
    fn exit_function_header(&mut self, _: &mut FunctionHeader) {}
    fn exit_function_qualifiers(&mut self, _: &mut FunctionQualifiers) {}
    fn exit_generic_declaration_const(&mut self, _: &mut GenericDeclarationConst) {}
    fn exit_generic_declaration_lifetime(&mut self, _: &mut GenericDeclarationLifetime) {}
    fn exit_generic_declaration_type(&mut self, _: &mut GenericDeclarationType) {}
    fn exit_generic_declarations(&mut self, _: &mut GenericDeclarations) {}
    fn exit_ident(&mut self, _: &mut Ident) {}
    fn exit_if(&mut self, _: &mut If) {}
    fn exit_if_let(&mut self, _: &mut IfLet) {}
    fn exit_impl(&mut self, _: &mut Impl) {}
    fn exit_impl_const(&mut self, _: &mut ImplConst) {}
    fn exit_impl_function(&mut self, _: &mut ImplFunction) {}
    fn exit_impl_kind(&mut self, _: &mut ImplKind) {}
    fn exit_impl_member(&mut self, _: &mut ImplMember) {}
    fn exit_impl_of_inherent(&mut self, _: &mut ImplOfInherent) {}
    fn exit_impl_of_trait(&mut self, _: &mut ImplOfTrait) {}
    fn exit_impl_of_trait_type(&mut self, _: &mut ImplOfTraitType) {}
    fn exit_impl_type(&mut self, _: &mut ImplType) {}
    fn exit_item(&mut self, _: &mut Item) {}
    fn exit_let(&mut self, _: &mut Let) {}
    fn exit_lifetime(&mut self, _: &mut Lifetime) {}
    fn exit_loop(&mut self, _: &mut Loop) {}
    fn exit_macro_call(&mut self, _: &mut MacroCall) {}
    fn exit_macro_call_args(&mut self, _: &mut MacroCallArgs) {}
    fn exit_match(&mut self, _: &mut Match) {}
    fn exit_match_arm(&mut self, _: &mut MatchArm) {}
    fn exit_match_hand(&mut self, _: &mut MatchHand) {}
    fn exit_module(&mut self, _: &mut Module) {}
    fn exit_named_argument(&mut self, _: &mut NamedArgument) {}
    fn exit_number(&mut self, _: &mut Number) {}
    fn exit_number_value(&mut self, _: &mut NumberValue) {}
    fn exit_number_binary(&mut self, _: &mut NumberBinary) {}
    fn exit_number_decimal(&mut self, _: &mut NumberDecimal) {}
    fn exit_number_hexadecimal(&mut self, _: &mut NumberHexadecimal) {}
    fn exit_number_octal(&mut self, _: &mut NumberOctal) {}
    fn exit_parenthetical(&mut self, _: &mut Parenthetical) {}
    fn exit_path(&mut self, _: &mut Path) {}
    fn exit_path_component(&mut self, _: &mut PathComponent) {}
    fn exit_pathed_ident(&mut self, _: &mut PathedIdent) {}
    fn exit_pattern(&mut self, _: &mut Pattern) {}
    fn exit_pattern_box(&mut self, _: &mut PatternBox) {}
    fn exit_pattern_byte(&mut self, _: &mut PatternByte) {}
    fn exit_pattern_byte_string(&mut self, _: &mut PatternByteString) {}
    fn exit_pattern_character(&mut self, _: &mut PatternCharacter) {}
    fn exit_pattern_ident(&mut self, _: &mut PatternIdent) {}
    fn exit_pattern_kind(&mut self, _: &mut PatternKind) {}
    fn exit_pattern_macro_call(&mut self, _: &mut PatternMacroCall) {}
    fn exit_pattern_name(&mut self, _: &mut PatternName) {}
    fn exit_pattern_number(&mut self, _: &mut PatternNumber) {}
    fn exit_pattern_range_component(&mut self, _: &mut PatternRangeComponent) {}
    fn exit_pattern_range_exclusive(&mut self, _: &mut PatternRangeExclusive) {}
    fn exit_pattern_range_inclusive(&mut self, _: &mut PatternRangeInclusive) {}
    fn exit_pattern_reference(&mut self, _: &mut PatternReference) {}
    fn exit_pattern_slice(&mut self, _: &mut PatternSlice) {}
    fn exit_pattern_slice_member(&mut self, _: &mut PatternSliceMember) {}
    fn exit_pattern_slice_subslice(&mut self, _: &mut PatternSliceSubslice) {}
    fn exit_pattern_string(&mut self, _: &mut PatternString) {}
    fn exit_pattern_struct(&mut self, _: &mut PatternStruct) {}
    fn exit_pattern_struct_field(&mut self, _: &mut PatternStructField) {}
    fn exit_pattern_struct_field_long(&mut self, _: &mut PatternStructFieldLong) {}
    fn exit_pattern_struct_field_short(&mut self, _: &mut PatternStructFieldShort) {}
    fn exit_pattern_tuple(&mut self, _: &mut PatternTuple) {}
    fn exit_pattern_tuple_member(&mut self, _: &mut PatternTupleMember) {}
    fn exit_range(&mut self, _: &mut Range) {}
    fn exit_range_inclusive(&mut self, _: &mut RangeInclusive) {}
    fn exit_reference(&mut self, _: &mut Reference) {}
    fn exit_return(&mut self, _: &mut Return) {}
    fn exit_self_argument(&mut self, _: &mut SelfArgument) {}
    fn exit_self_argument_longhand(&mut self, _: &mut SelfArgumentLonghand) {}
    fn exit_self_argument_shorthand(&mut self, _: &mut SelfArgumentShorthand) {}
    fn exit_self_argument_shorthand_qualifier(&mut self, _: &mut SelfArgumentShorthandQualifier) {}
    fn exit_slice(&mut self, _: &mut Slice) {}
    fn exit_statement(&mut self, _: &mut Statement) {}
    fn exit_static(&mut self, _: &mut Static) {}
    fn exit_string(&mut self, _: &mut String) {}
    fn exit_struct(&mut self, _: &mut Struct) {}
    fn exit_struct_definition_body(&mut self, _: &mut StructDefinitionBody) {}
    fn exit_struct_definition_body_brace(&mut self, _: &mut StructDefinitionBodyBrace) {}
    fn exit_struct_definition_body_tuple(&mut self, _: &mut StructDefinitionBodyTuple) {}
    fn exit_struct_definition_field_named(&mut self, _: &mut StructDefinitionFieldNamed) {}
    fn exit_struct_definition_field_unnamed(&mut self, _: &mut StructDefinitionFieldUnnamed) {}
    fn exit_struct_literal(&mut self, _: &mut StructLiteral) {}
    fn exit_struct_literal_field(&mut self, _: &mut StructLiteralField) {}
    fn exit_trait(&mut self, _: &mut Trait) {}
    fn exit_trait_bound(&mut self, _: &mut TraitBound) {}
    fn exit_trait_bound_lifetime(&mut self, _: &mut TraitBoundLifetime) {}
    fn exit_trait_bound_normal(&mut self, _: &mut TraitBoundNormal) {}
    fn exit_trait_bound_relaxed(&mut self, _: &mut TraitBoundRelaxed) {}
    fn exit_trait_bound_type(&mut self, _: &mut TraitBoundType) {}
    fn exit_trait_bounds(&mut self, _: &mut TraitBounds) {}
    fn exit_trait_impl_argument(&mut self, _: &mut TraitImplArgument) {}
    fn exit_trait_impl_argument_named(&mut self, _: &mut TraitImplArgumentNamed) {}
    fn exit_trait_impl_function_header(&mut self, _: &mut TraitImplFunctionHeader) {}
    fn exit_trait_member(&mut self, _: &mut TraitMember) {}
    fn exit_trait_member_const(&mut self, _: &mut TraitMemberConst) {}
    fn exit_trait_member_function(&mut self, _: &mut TraitMemberFunction) {}
    fn exit_trait_member_type(&mut self, _: &mut TraitMemberType) {}
    fn exit_try_operator(&mut self, _: &mut TryOperator) {}
    fn exit_tuple(&mut self, _: &mut Tuple) {}
    fn exit_turbofish(&mut self, _: &mut Turbofish) {}
    fn exit_type(&mut self, _: &mut Type) {}
    fn exit_type_additional(&mut self, _: &mut TypeAdditional) {}
    fn exit_type_alias(&mut self, _: &mut TypeAlias) {}
    fn exit_type_array(&mut self, _: &mut TypeArray) {}
    fn exit_type_disambiguation(&mut self, _: &mut TypeDisambiguation) {}
    fn exit_type_dyn_trait(&mut self, _: &mut TypeDynTrait) {}
    fn exit_type_function(&mut self, _: &mut TypeFunction) {}
    fn exit_type_function_argument(&mut self, _: &mut TypeFunctionArgument) {}
    fn exit_type_function_argument_named(&mut self, _: &mut TypeFunctionArgumentNamed) {}
    fn exit_type_generics(&mut self, _: &mut TypeGenerics) {}
    fn exit_type_generics_angle(&mut self, _: &mut TypeGenericsAngle) {}
    fn exit_type_generics_angle_member(&mut self, _: &mut TypeGenericsAngleMember) {}
    fn exit_type_generics_function(&mut self, _: &mut TypeGenericsFunction) {}
    fn exit_type_higher_ranked_trait_bounds(&mut self, _: &mut TypeHigherRankedTraitBounds) {}
    fn exit_type_higher_ranked_trait_bounds_child(&mut self, _: &mut TypeHigherRankedTraitBoundsChild) {}
    fn exit_type_impl_trait(&mut self, _: &mut TypeImplTrait) {}
    fn exit_type_kind(&mut self, _: &mut TypeKind) {}
    fn exit_type_named(&mut self, _: &mut TypeNamed) {}
    fn exit_type_named_component(&mut self, _: &mut TypeNamedComponent) {}
    fn exit_type_pointer(&mut self, _: &mut TypePointer) {}
    fn exit_type_reference(&mut self, _: &mut TypeReference) {}
    fn exit_type_reference_kind(&mut self, _: &mut TypeReferenceKind) {}
    fn exit_type_slice(&mut self, _: &mut TypeSlice) {}
    fn exit_type_tuple(&mut self, _: &mut TypeTuple) {}
    fn exit_unary(&mut self, _: &mut Unary) {}
    fn exit_union(&mut self, _: &mut Union) {}
    fn exit_unsafe_block(&mut self, _: &mut UnsafeBlock) {}
    fn exit_use(&mut self, _: &mut Use) {}
    fn exit_use_path(&mut self, _: &mut UsePath) {}
    fn exit_use_tail(&mut self, _: &mut UseTail) {}
    fn exit_use_tail_glob(&mut self, _: &mut UseTailGlob) {}
    fn exit_use_tail_ident(&mut self, _: &mut UseTailIdent) {}
    fn exit_use_tail_multi(&mut self, _: &mut UseTailMulti) {}
    fn exit_value(&mut self, _: &mut Value) {}
    fn exit_visibility(&mut self, _: &mut Visibility) {}
    fn exit_visibility_public(&mut self, _: &mut VisibilityPublic) {}
    fn exit_where(&mut self, _: &mut Where) {}
    fn exit_where_kind(&mut self, _: &mut WhereKind) {}
    fn exit_where_lifetime(&mut self, _: &mut WhereLifetime) {}
    fn exit_where_type(&mut self, _: &mut WhereType) {}
    fn exit_while(&mut self, _: &mut While) {}
    fn exit_while_let(&mut self, _: &mut WhileLet) {}
    fn exit_whitespace(&mut self, _: &mut Whitespace) {}
}
