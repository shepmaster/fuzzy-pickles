//! Visitors of the AST

use ast::*;
use Extent;

/// An AST node that can be visited
pub trait Visit {
    fn visit<'ast, V>(&'ast self, &mut V)
    where
        V: Visitor<'ast>;
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
}

impl<T> Visit for Option<T>
where
    T: Visit,
{
    fn visit<'ast, V>(&'ast self, v: &mut V)
    where
        V: Visitor<'ast>,
    {
        for i in self {
            i.visit(v)
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
}

// Cheap hack to avoid having to annotate every terminal `Extent`;
// just visit them and don't do anything. An extent without any
// context is pretty useless.
impl Visit for Extent {
    fn visit<'ast, V>(&'ast self, _v: &mut V)
    where
        V: Visitor<'ast>
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
    fn visit_argument(&mut self, &'ast Argument) -> Control { Control::Continue }
    fn visit_array(&mut self, &'ast Array) -> Control { Control::Continue }
    fn visit_array_explicit(&mut self, &'ast ArrayExplicit) -> Control { Control::Continue }
    fn visit_array_repeated(&mut self, &'ast ArrayRepeated) -> Control { Control::Continue }
    fn visit_as_type(&mut self, &'ast AsType) -> Control { Control::Continue }
    fn visit_ascription(&mut self, &'ast Ascription) -> Control { Control::Continue }
    fn visit_associated_type(&mut self, &'ast AssociatedType) -> Control { Control::Continue }
    fn visit_attribute(&mut self, &'ast Attribute) -> Control { Control::Continue }
    fn visit_attribute_literal(&mut self, &'ast AttributeLiteral) -> Control { Control::Continue }
    fn visit_attribute_containing(&mut self, &'ast AttributeContaining) -> Control { Control::Continue }
    fn visit_attribute_containing_literal(&mut self, &'ast AttributeContainingLiteral) -> Control { Control::Continue }
    fn visit_attributed_enum_variant(&mut self, &'ast Attributed<EnumVariant>) -> Control { Control::Continue }
    fn visit_attributed_expression(&mut self, &'ast Attributed<Expression>) -> Control { Control::Continue }
    fn visit_attributed_extern_block_member(&mut self, &'ast Attributed<ExternBlockMember>) -> Control { Control::Continue }
    fn visit_attributed_generic_declaration_lifetime(&mut self, &'ast Attributed<GenericDeclarationLifetime>) -> Control { Control::Continue }
    fn visit_attributed_generic_declaration_type(&mut self, &'ast Attributed<GenericDeclarationType>) -> Control { Control::Continue }
    fn visit_attributed_impl_member(&mut self, &'ast Attributed<ImplMember>) -> Control { Control::Continue }
    fn visit_attributed_item(&mut self, &'ast Attributed<Item>) -> Control { Control::Continue }
    fn visit_attributed_struct_definition_field_named(&mut self, &'ast Attributed<StructDefinitionFieldNamed>) -> Control { Control::Continue }
    fn visit_attributed_struct_definition_field_unnamed(&mut self, &'ast Attributed<StructDefinitionFieldUnnamed>) -> Control { Control::Continue }
    fn visit_attributed_trait_member(&mut self, &'ast Attributed<TraitMember>) -> Control { Control::Continue }
    fn visit_binary(&mut self, &'ast Binary) -> Control { Control::Continue }
    fn visit_block(&mut self, &'ast Block) -> Control { Control::Continue }
    fn visit_break(&mut self, &'ast Break) -> Control { Control::Continue }
    fn visit_byte(&mut self, &'ast Byte) -> Control { Control::Continue }
    fn visit_byte_string(&mut self, &'ast ByteString) -> Control { Control::Continue }
    fn visit_call(&mut self, &'ast Call) -> Control { Control::Continue }
    fn visit_character(&mut self, &'ast Character) -> Control { Control::Continue }
    fn visit_closure(&mut self, &'ast Closure) -> Control { Control::Continue }
    fn visit_closure_arg(&mut self, &'ast ClosureArg) -> Control { Control::Continue }
    fn visit_comment(&mut self, &'ast Comment) -> Control { Control::Continue }
    fn visit_const(&mut self, &'ast Const) -> Control { Control::Continue }
    fn visit_continue(&mut self, &'ast Continue) -> Control { Control::Continue }
    fn visit_crate(&mut self, &'ast Crate) -> Control { Control::Continue }
    fn visit_dereference(&mut self, &'ast Dereference) -> Control { Control::Continue }
    fn visit_disambiguation(&mut self, &'ast Disambiguation) -> Control { Control::Continue }
    fn visit_enum(&mut self, &'ast Enum) -> Control { Control::Continue }
    fn visit_enum_variant(&mut self, &'ast EnumVariant) -> Control { Control::Continue }
    fn visit_enum_variant_body(&mut self, &'ast EnumVariantBody) -> Control { Control::Continue }
    fn visit_expression(&mut self, &'ast Expression) -> Control { Control::Continue }
    fn visit_expression_box(&mut self, &'ast ExpressionBox) -> Control { Control::Continue }
    fn visit_extern_block(&mut self, &'ast ExternBlock) -> Control { Control::Continue }
    fn visit_extern_block_member(&mut self, &'ast ExternBlockMember) -> Control { Control::Continue }
    fn visit_extern_block_member_function(&mut self, &'ast ExternBlockMemberFunction) -> Control { Control::Continue }
    fn visit_extern_block_member_function_argument(&mut self, &'ast ExternBlockMemberFunctionArgument) -> Control { Control::Continue }
    fn visit_extern_block_member_function_argument_named(&mut self, &'ast ExternBlockMemberFunctionArgumentNamed) -> Control { Control::Continue }
    fn visit_extern_block_member_function_argument_variadic(&mut self, &'ast ExternBlockMemberFunctionArgumentVariadic) -> Control { Control::Continue }
    fn visit_extern_block_member_static(&mut self, &'ast ExternBlockMemberStatic) -> Control { Control::Continue }
    fn visit_extern_block_member_type(&mut self, &'ast ExternBlockMemberType) -> Control { Control::Continue }
    fn visit_field_access(&mut self, &'ast FieldAccess) -> Control { Control::Continue }
    fn visit_field_name(&mut self, &'ast FieldName) -> Control { Control::Continue }
    fn visit_file(&mut self, &'ast File) -> Control { Control::Continue }
    fn visit_for_loop(&mut self, &'ast ForLoop) -> Control { Control::Continue }
    fn visit_function(&mut self, &'ast Function) -> Control { Control::Continue }
    fn visit_function_header(&mut self, &'ast FunctionHeader) -> Control { Control::Continue }
    fn visit_function_qualifiers(&mut self, &'ast FunctionQualifiers) -> Control { Control::Continue }
    fn visit_generic_declaration_lifetime(&mut self, &'ast GenericDeclarationLifetime) -> Control { Control::Continue }
    fn visit_generic_declaration_type(&mut self, &'ast GenericDeclarationType) -> Control { Control::Continue }
    fn visit_generic_declarations(&mut self, &'ast GenericDeclarations) -> Control { Control::Continue }
    fn visit_ident(&mut self, &'ast Ident) -> Control { Control::Continue }
    fn visit_if(&mut self, &'ast If) -> Control { Control::Continue }
    fn visit_if_let(&mut self, &'ast IfLet) -> Control { Control::Continue }
    fn visit_impl(&mut self, &'ast Impl) -> Control { Control::Continue }
    fn visit_impl_const(&mut self, &'ast ImplConst) -> Control { Control::Continue }
    fn visit_impl_function(&mut self, &'ast ImplFunction) -> Control { Control::Continue }
    fn visit_impl_kind(&mut self, &'ast ImplKind) -> Control { Control::Continue }
    fn visit_impl_member(&mut self, &'ast ImplMember) -> Control { Control::Continue }
    fn visit_impl_of_inherent(&mut self, &'ast ImplOfInherent) -> Control { Control::Continue }
    fn visit_impl_of_trait(&mut self, &'ast ImplOfTrait) -> Control { Control::Continue }
    fn visit_impl_of_trait_type(&mut self, &'ast ImplOfTraitType) -> Control { Control::Continue }
    fn visit_impl_type(&mut self, &'ast ImplType) -> Control { Control::Continue }
    fn visit_item(&mut self, &'ast Item) -> Control { Control::Continue }
    fn visit_let(&mut self, &'ast Let) -> Control { Control::Continue }
    fn visit_lifetime(&mut self, &'ast Lifetime) -> Control { Control::Continue }
    fn visit_loop(&mut self, &'ast Loop) -> Control { Control::Continue }
    fn visit_macro_call(&mut self, &'ast MacroCall) -> Control { Control::Continue }
    fn visit_macro_call_args(&mut self, &'ast MacroCallArgs) -> Control { Control::Continue }
    fn visit_match(&mut self, &'ast Match) -> Control { Control::Continue }
    fn visit_match_arm(&mut self, &'ast MatchArm) -> Control { Control::Continue }
    fn visit_match_hand(&mut self, &'ast MatchHand) -> Control { Control::Continue }
    fn visit_module(&mut self, &'ast Module) -> Control { Control::Continue }
    fn visit_named_argument(&mut self, &'ast NamedArgument) -> Control { Control::Continue }
    fn visit_number(&mut self, &'ast Number) -> Control { Control::Continue }
    fn visit_number_value(&mut self, &'ast NumberValue) -> Control { Control::Continue }
    fn visit_number_binary(&mut self, &'ast NumberBinary) -> Control { Control::Continue }
    fn visit_number_decimal(&mut self, &'ast NumberDecimal) -> Control { Control::Continue }
    fn visit_number_hexadecimal(&mut self, &'ast NumberHexadecimal) -> Control { Control::Continue }
    fn visit_number_octal(&mut self, &'ast NumberOctal) -> Control { Control::Continue }
    fn visit_parenthetical(&mut self, &'ast Parenthetical) -> Control { Control::Continue }
    fn visit_path(&mut self, &'ast Path) -> Control { Control::Continue }
    fn visit_path_component(&mut self, &'ast PathComponent) -> Control { Control::Continue }
    fn visit_pathed_ident(&mut self, &'ast PathedIdent) -> Control { Control::Continue }
    fn visit_pattern(&mut self, &'ast Pattern) -> Control { Control::Continue }
    fn visit_pattern_box(&mut self, &'ast PatternBox) -> Control { Control::Continue }
    fn visit_pattern_byte(&mut self, &'ast PatternByte) -> Control { Control::Continue }
    fn visit_pattern_byte_string(&mut self, &'ast PatternByteString) -> Control { Control::Continue }
    fn visit_pattern_character(&mut self, &'ast PatternCharacter) -> Control { Control::Continue }
    fn visit_pattern_ident(&mut self, &'ast PatternIdent) -> Control { Control::Continue }
    fn visit_pattern_kind(&mut self, &'ast PatternKind) -> Control { Control::Continue }
    fn visit_pattern_macro_call(&mut self, &'ast PatternMacroCall) -> Control { Control::Continue }
    fn visit_pattern_name(&mut self, &'ast PatternName) -> Control { Control::Continue }
    fn visit_pattern_number(&mut self, &'ast PatternNumber) -> Control { Control::Continue }
    fn visit_pattern_range_component(&mut self, &'ast PatternRangeComponent) -> Control { Control::Continue }
    fn visit_pattern_range_exclusive(&mut self, &'ast PatternRangeExclusive) -> Control { Control::Continue }
    fn visit_pattern_range_inclusive(&mut self, &'ast PatternRangeInclusive) -> Control { Control::Continue }
    fn visit_pattern_reference(&mut self, &'ast PatternReference) -> Control { Control::Continue }
    fn visit_pattern_slice(&mut self, &'ast PatternSlice) -> Control { Control::Continue }
    fn visit_pattern_slice_member(&mut self, &'ast PatternSliceMember) -> Control { Control::Continue }
    fn visit_pattern_slice_subslice(&mut self, &'ast PatternSliceSubslice) -> Control { Control::Continue }
    fn visit_pattern_string(&mut self, &'ast PatternString) -> Control { Control::Continue }
    fn visit_pattern_struct(&mut self, &'ast PatternStruct) -> Control { Control::Continue }
    fn visit_pattern_struct_field(&mut self, &'ast PatternStructField) -> Control { Control::Continue }
    fn visit_pattern_struct_field_long(&mut self, &'ast PatternStructFieldLong) -> Control { Control::Continue }
    fn visit_pattern_struct_field_short(&mut self, &'ast PatternStructFieldShort) -> Control { Control::Continue }
    fn visit_pattern_tuple(&mut self, &'ast PatternTuple) -> Control { Control::Continue }
    fn visit_pattern_tuple_member(&mut self, &'ast PatternTupleMember) -> Control { Control::Continue }
    fn visit_range(&mut self, &'ast Range) -> Control { Control::Continue }
    fn visit_range_inclusive(&mut self, &'ast RangeInclusive) -> Control { Control::Continue }
    fn visit_reference(&mut self, &'ast Reference) -> Control { Control::Continue }
    fn visit_return(&mut self, &'ast Return) -> Control { Control::Continue }
    fn visit_self_argument(&mut self, &'ast SelfArgument) -> Control { Control::Continue }
    fn visit_self_argument_longhand(&mut self, &'ast SelfArgumentLonghand) -> Control { Control::Continue }
    fn visit_self_argument_shorthand(&mut self, &'ast SelfArgumentShorthand) -> Control { Control::Continue }
    fn visit_self_argument_shorthand_qualifier(&mut self, &'ast SelfArgumentShorthandQualifier) -> Control { Control::Continue }
    fn visit_slice(&mut self, &'ast Slice) -> Control { Control::Continue }
    fn visit_statement(&mut self, &'ast Statement) -> Control { Control::Continue }
    fn visit_static(&mut self, &'ast Static) -> Control { Control::Continue }
    fn visit_string(&mut self, &'ast String) -> Control { Control::Continue }
    fn visit_struct(&mut self, &'ast Struct) -> Control { Control::Continue }
    fn visit_struct_definition_body(&mut self, &'ast StructDefinitionBody) -> Control { Control::Continue }
    fn visit_struct_definition_body_brace(&mut self, &'ast StructDefinitionBodyBrace) -> Control { Control::Continue }
    fn visit_struct_definition_body_tuple(&mut self, &'ast StructDefinitionBodyTuple) -> Control { Control::Continue }
    fn visit_struct_definition_field_named(&mut self, &'ast StructDefinitionFieldNamed) -> Control { Control::Continue }
    fn visit_struct_definition_field_unnamed(&mut self, &'ast StructDefinitionFieldUnnamed) -> Control { Control::Continue }
    fn visit_struct_literal(&mut self, &'ast StructLiteral) -> Control { Control::Continue }
    fn visit_struct_literal_field(&mut self, &'ast StructLiteralField) -> Control { Control::Continue }
    fn visit_trait(&mut self, &'ast Trait) -> Control { Control::Continue }
    fn visit_trait_bound(&mut self, &'ast TraitBound) -> Control { Control::Continue }
    fn visit_trait_bound_lifetime(&mut self, &'ast TraitBoundLifetime) -> Control { Control::Continue }
    fn visit_trait_bound_normal(&mut self, &'ast TraitBoundNormal) -> Control { Control::Continue }
    fn visit_trait_bound_relaxed(&mut self, &'ast TraitBoundRelaxed) -> Control { Control::Continue }
    fn visit_trait_bound_type(&mut self, &'ast TraitBoundType) -> Control { Control::Continue }
    fn visit_trait_bounds(&mut self, &'ast TraitBounds) -> Control { Control::Continue }
    fn visit_trait_impl_argument(&mut self, &'ast TraitImplArgument) -> Control { Control::Continue }
    fn visit_trait_impl_argument_named(&mut self, &'ast TraitImplArgumentNamed) -> Control { Control::Continue }
    fn visit_trait_impl_function_header(&mut self, &'ast TraitImplFunctionHeader) -> Control { Control::Continue }
    fn visit_trait_member(&mut self, &'ast TraitMember) -> Control { Control::Continue }
    fn visit_trait_member_const(&mut self, &'ast TraitMemberConst) -> Control { Control::Continue }
    fn visit_trait_member_function(&mut self, &'ast TraitMemberFunction) -> Control { Control::Continue }
    fn visit_trait_member_type(&mut self, &'ast TraitMemberType) -> Control { Control::Continue }
    fn visit_try_operator(&mut self, &'ast TryOperator) -> Control { Control::Continue }
    fn visit_tuple(&mut self, &'ast Tuple) -> Control { Control::Continue }
    fn visit_turbofish(&mut self, &'ast Turbofish) -> Control { Control::Continue }
    fn visit_type(&mut self, &'ast Type) -> Control { Control::Continue }
    fn visit_type_additional(&mut self, &'ast TypeAdditional) -> Control { Control::Continue }
    fn visit_type_alias(&mut self, &'ast TypeAlias) -> Control { Control::Continue }
    fn visit_type_array(&mut self, &'ast TypeArray) -> Control { Control::Continue }
    fn visit_type_disambiguation(&mut self, &'ast TypeDisambiguation) -> Control { Control::Continue }
    fn visit_type_function(&mut self, &'ast TypeFunction) -> Control { Control::Continue }
    fn visit_type_function_argument(&mut self, &'ast TypeFunctionArgument) -> Control { Control::Continue }
    fn visit_type_function_argument_named(&mut self, &'ast TypeFunctionArgumentNamed) -> Control { Control::Continue }
    fn visit_type_generics(&mut self, &'ast TypeGenerics) -> Control { Control::Continue }
    fn visit_type_generics_angle(&mut self, &'ast TypeGenericsAngle) -> Control { Control::Continue }
    fn visit_type_generics_angle_member(&mut self, &'ast TypeGenericsAngleMember) -> Control { Control::Continue }
    fn visit_type_generics_function(&mut self, &'ast TypeGenericsFunction) -> Control { Control::Continue }
    fn visit_type_higher_ranked_trait_bounds(&mut self, &'ast TypeHigherRankedTraitBounds) -> Control { Control::Continue }
    fn visit_type_higher_ranked_trait_bounds_child(&mut self, &'ast TypeHigherRankedTraitBoundsChild) -> Control { Control::Continue }
    fn visit_type_impl_trait(&mut self, &'ast TypeImplTrait) -> Control { Control::Continue }
    fn visit_type_kind(&mut self, &'ast TypeKind) -> Control { Control::Continue }
    fn visit_type_named(&mut self, &'ast TypeNamed) -> Control { Control::Continue }
    fn visit_type_named_component(&mut self, &'ast TypeNamedComponent) -> Control { Control::Continue }
    fn visit_type_pointer(&mut self, &'ast TypePointer) -> Control { Control::Continue }
    fn visit_type_reference(&mut self, &'ast TypeReference) -> Control { Control::Continue }
    fn visit_type_reference_kind(&mut self, &'ast TypeReferenceKind) -> Control { Control::Continue }
    fn visit_type_slice(&mut self, &'ast TypeSlice) -> Control { Control::Continue }
    fn visit_type_tuple(&mut self, &'ast TypeTuple) -> Control { Control::Continue }
    fn visit_unary(&mut self, &'ast Unary) -> Control { Control::Continue }
    fn visit_union(&mut self, &'ast Union) -> Control { Control::Continue }
    fn visit_unsafe_block(&mut self, &'ast UnsafeBlock) -> Control { Control::Continue }
    fn visit_use(&mut self, &'ast Use) -> Control { Control::Continue }
    fn visit_use_path(&mut self, &'ast UsePath) -> Control { Control::Continue }
    fn visit_use_tail(&mut self, &'ast UseTail) -> Control { Control::Continue }
    fn visit_use_tail_glob(&mut self, &'ast UseTailGlob) -> Control { Control::Continue }
    fn visit_use_tail_ident(&mut self, &'ast UseTailIdent) -> Control { Control::Continue }
    fn visit_use_tail_multi(&mut self, &'ast UseTailMulti) -> Control { Control::Continue }
    fn visit_value(&mut self, &'ast Value) -> Control { Control::Continue }
    fn visit_visibility(&mut self, &'ast Visibility) -> Control { Control::Continue }
    fn visit_where(&mut self, &'ast Where) -> Control { Control::Continue }
    fn visit_where_kind(&mut self, &'ast WhereKind) -> Control { Control::Continue }
    fn visit_where_lifetime(&mut self, &'ast WhereLifetime) -> Control { Control::Continue }
    fn visit_where_type(&mut self, &'ast WhereType) -> Control { Control::Continue }
    fn visit_while(&mut self, &'ast While) -> Control { Control::Continue }
    fn visit_while_let(&mut self, &'ast WhileLet) -> Control { Control::Continue }
    fn visit_whitespace(&mut self, &'ast Whitespace) -> Control { Control::Continue }

    fn exit_argument(&mut self, &'ast Argument) {}
    fn exit_array(&mut self, &'ast Array) {}
    fn exit_array_explicit(&mut self, &'ast ArrayExplicit) {}
    fn exit_array_repeated(&mut self, &'ast ArrayRepeated) {}
    fn exit_as_type(&mut self, &'ast AsType) {}
    fn exit_ascription(&mut self, &'ast Ascription) {}
    fn exit_associated_type(&mut self, &'ast AssociatedType) {}
    fn exit_attribute(&mut self, &'ast Attribute) {}
    fn exit_attribute_literal(&mut self, &'ast AttributeLiteral) {}
    fn exit_attribute_containing(&mut self, &'ast AttributeContaining) {}
    fn exit_attribute_containing_literal(&mut self, &'ast AttributeContainingLiteral) {}
    fn exit_attributed_enum_variant(&mut self, &'ast Attributed<EnumVariant>) {}
    fn exit_attributed_expression(&mut self, &'ast Attributed<Expression>) {}
    fn exit_attributed_extern_block_member(&mut self, &'ast Attributed<ExternBlockMember>) {}
    fn exit_attributed_generic_declaration_lifetime(&mut self, &'ast Attributed<GenericDeclarationLifetime>) {}
    fn exit_attributed_generic_declaration_type(&mut self, &'ast Attributed<GenericDeclarationType>) {}
    fn exit_attributed_impl_member(&mut self, &'ast Attributed<ImplMember>) {}
    fn exit_attributed_item(&mut self, &'ast Attributed<Item>) {}
    fn exit_attributed_struct_definition_field_named(&mut self, &'ast Attributed<StructDefinitionFieldNamed>) {}
    fn exit_attributed_struct_definition_field_unnamed(&mut self, &'ast Attributed<StructDefinitionFieldUnnamed>) {}
    fn exit_attributed_trait_member(&mut self, &'ast Attributed<TraitMember>) {}
    fn exit_binary(&mut self, &'ast Binary) {}
    fn exit_block(&mut self, &'ast Block) {}
    fn exit_break(&mut self, &'ast Break) {}
    fn exit_byte(&mut self, &'ast Byte) {}
    fn exit_byte_string(&mut self, &'ast ByteString) {}
    fn exit_call(&mut self, &'ast Call) {}
    fn exit_character(&mut self, &'ast Character) {}
    fn exit_closure(&mut self, &'ast Closure) {}
    fn exit_closure_arg(&mut self, &'ast ClosureArg) {}
    fn exit_comment(&mut self, &'ast Comment) {}
    fn exit_const(&mut self, &'ast Const) {}
    fn exit_continue(&mut self, &'ast Continue) {}
    fn exit_crate(&mut self, &'ast Crate) {}
    fn exit_dereference(&mut self, &'ast Dereference) {}
    fn exit_disambiguation(&mut self, &'ast Disambiguation) {}
    fn exit_enum(&mut self, &'ast Enum) {}
    fn exit_enum_variant(&mut self, &'ast EnumVariant) {}
    fn exit_enum_variant_body(&mut self, &'ast EnumVariantBody) {}
    fn exit_expression(&mut self, &'ast Expression) {}
    fn exit_expression_box(&mut self, &'ast ExpressionBox) {}
    fn exit_extern_block(&mut self, &'ast ExternBlock) {}
    fn exit_extern_block_member(&mut self, &'ast ExternBlockMember) {}
    fn exit_extern_block_member_function(&mut self, &'ast ExternBlockMemberFunction) {}
    fn exit_extern_block_member_function_argument(&mut self, &'ast ExternBlockMemberFunctionArgument) {}
    fn exit_extern_block_member_function_argument_named(&mut self, &'ast ExternBlockMemberFunctionArgumentNamed) {}
    fn exit_extern_block_member_function_argument_variadic(&mut self, &'ast ExternBlockMemberFunctionArgumentVariadic) {}
    fn exit_extern_block_member_static(&mut self, &'ast ExternBlockMemberStatic) {}
    fn exit_extern_block_member_type(&mut self, &'ast ExternBlockMemberType) {}
    fn exit_field_access(&mut self, &'ast FieldAccess) {}
    fn exit_field_name(&mut self, &'ast FieldName) {}
    fn exit_file(&mut self, &'ast File) {}
    fn exit_for_loop(&mut self, &'ast ForLoop) {}
    fn exit_function(&mut self, &'ast Function) {}
    fn exit_function_header(&mut self, &'ast FunctionHeader) {}
    fn exit_function_qualifiers(&mut self, &'ast FunctionQualifiers) {}
    fn exit_generic_declaration_lifetime(&mut self, &'ast GenericDeclarationLifetime) {}
    fn exit_generic_declaration_type(&mut self, &'ast GenericDeclarationType) {}
    fn exit_generic_declarations(&mut self, &'ast GenericDeclarations) {}
    fn exit_ident(&mut self, &'ast Ident) {}
    fn exit_if(&mut self, &'ast If) {}
    fn exit_if_let(&mut self, &'ast IfLet) {}
    fn exit_impl(&mut self, &'ast Impl) {}
    fn exit_impl_const(&mut self, &'ast ImplConst) {}
    fn exit_impl_function(&mut self, &'ast ImplFunction) {}
    fn exit_impl_kind(&mut self, &'ast ImplKind) {}
    fn exit_impl_member(&mut self, &'ast ImplMember) {}
    fn exit_impl_of_inherent(&mut self, &'ast ImplOfInherent) {}
    fn exit_impl_of_trait(&mut self, &'ast ImplOfTrait) {}
    fn exit_impl_of_trait_type(&mut self, &'ast ImplOfTraitType) {}
    fn exit_impl_type(&mut self, &'ast ImplType) {}
    fn exit_item(&mut self, &'ast Item) {}
    fn exit_let(&mut self, &'ast Let) {}
    fn exit_lifetime(&mut self, &'ast Lifetime) {}
    fn exit_loop(&mut self, &'ast Loop) {}
    fn exit_macro_call(&mut self, &'ast MacroCall) {}
    fn exit_macro_call_args(&mut self, &'ast MacroCallArgs) {}
    fn exit_match(&mut self, &'ast Match) {}
    fn exit_match_arm(&mut self, &'ast MatchArm) {}
    fn exit_match_hand(&mut self, &'ast MatchHand) {}
    fn exit_module(&mut self, &'ast Module) {}
    fn exit_named_argument(&mut self, &'ast NamedArgument) {}
    fn exit_number(&mut self, &'ast Number) {}
    fn exit_number_value(&mut self, &'ast NumberValue) {}
    fn exit_number_binary(&mut self, &'ast NumberBinary) {}
    fn exit_number_decimal(&mut self, &'ast NumberDecimal) {}
    fn exit_number_hexadecimal(&mut self, &'ast NumberHexadecimal) {}
    fn exit_number_octal(&mut self, &'ast NumberOctal) {}
    fn exit_parenthetical(&mut self, &'ast Parenthetical) {}
    fn exit_path(&mut self, &'ast Path) {}
    fn exit_path_component(&mut self, &'ast PathComponent) {}
    fn exit_pathed_ident(&mut self, &'ast PathedIdent) {}
    fn exit_pattern(&mut self, &'ast Pattern) {}
    fn exit_pattern_box(&mut self, &'ast PatternBox) {}
    fn exit_pattern_byte(&mut self, &'ast PatternByte) {}
    fn exit_pattern_byte_string(&mut self, &'ast PatternByteString) {}
    fn exit_pattern_character(&mut self, &'ast PatternCharacter) {}
    fn exit_pattern_ident(&mut self, &'ast PatternIdent) {}
    fn exit_pattern_kind(&mut self, &'ast PatternKind) {}
    fn exit_pattern_macro_call(&mut self, &'ast PatternMacroCall) {}
    fn exit_pattern_name(&mut self, &'ast PatternName) {}
    fn exit_pattern_number(&mut self, &'ast PatternNumber) {}
    fn exit_pattern_range_component(&mut self, &'ast PatternRangeComponent) {}
    fn exit_pattern_range_exclusive(&mut self, &'ast PatternRangeExclusive) {}
    fn exit_pattern_range_inclusive(&mut self, &'ast PatternRangeInclusive) {}
    fn exit_pattern_reference(&mut self, &'ast PatternReference) {}
    fn exit_pattern_slice(&mut self, &'ast PatternSlice) {}
    fn exit_pattern_slice_member(&mut self, &'ast PatternSliceMember) {}
    fn exit_pattern_slice_subslice(&mut self, &'ast PatternSliceSubslice) {}
    fn exit_pattern_string(&mut self, &'ast PatternString) {}
    fn exit_pattern_struct(&mut self, &'ast PatternStruct) {}
    fn exit_pattern_struct_field(&mut self, &'ast PatternStructField) {}
    fn exit_pattern_struct_field_long(&mut self, &'ast PatternStructFieldLong) {}
    fn exit_pattern_struct_field_short(&mut self, &'ast PatternStructFieldShort) {}
    fn exit_pattern_tuple(&mut self, &'ast PatternTuple) {}
    fn exit_pattern_tuple_member(&mut self, &'ast PatternTupleMember) {}
    fn exit_range(&mut self, &'ast Range) {}
    fn exit_range_inclusive(&mut self, &'ast RangeInclusive) {}
    fn exit_reference(&mut self, &'ast Reference) {}
    fn exit_return(&mut self, &'ast Return) {}
    fn exit_self_argument(&mut self, &'ast SelfArgument) {}
    fn exit_self_argument_longhand(&mut self, &'ast SelfArgumentLonghand) {}
    fn exit_self_argument_shorthand(&mut self, &'ast SelfArgumentShorthand) {}
    fn exit_self_argument_shorthand_qualifier(&mut self, &'ast SelfArgumentShorthandQualifier) {}
    fn exit_slice(&mut self, &'ast Slice) {}
    fn exit_statement(&mut self, &'ast Statement) {}
    fn exit_static(&mut self, &'ast Static) {}
    fn exit_string(&mut self, &'ast String) {}
    fn exit_struct(&mut self, &'ast Struct) {}
    fn exit_struct_definition_body(&mut self, &'ast StructDefinitionBody) {}
    fn exit_struct_definition_body_brace(&mut self, &'ast StructDefinitionBodyBrace) {}
    fn exit_struct_definition_body_tuple(&mut self, &'ast StructDefinitionBodyTuple) {}
    fn exit_struct_definition_field_named(&mut self, &'ast StructDefinitionFieldNamed) {}
    fn exit_struct_definition_field_unnamed(&mut self, &'ast StructDefinitionFieldUnnamed) {}
    fn exit_struct_literal(&mut self, &'ast StructLiteral) {}
    fn exit_struct_literal_field(&mut self, &'ast StructLiteralField) {}
    fn exit_trait(&mut self, &'ast Trait) {}
    fn exit_trait_bound(&mut self, &'ast TraitBound) {}
    fn exit_trait_bound_lifetime(&mut self, &'ast TraitBoundLifetime) {}
    fn exit_trait_bound_normal(&mut self, &'ast TraitBoundNormal) {}
    fn exit_trait_bound_relaxed(&mut self, &'ast TraitBoundRelaxed) {}
    fn exit_trait_bound_type(&mut self, &'ast TraitBoundType) {}
    fn exit_trait_bounds(&mut self, &'ast TraitBounds) {}
    fn exit_trait_impl_argument(&mut self, &'ast TraitImplArgument) {}
    fn exit_trait_impl_argument_named(&mut self, &'ast TraitImplArgumentNamed) {}
    fn exit_trait_impl_function_header(&mut self, &'ast TraitImplFunctionHeader) {}
    fn exit_trait_member(&mut self, &'ast TraitMember) {}
    fn exit_trait_member_const(&mut self, &'ast TraitMemberConst) {}
    fn exit_trait_member_function(&mut self, &'ast TraitMemberFunction) {}
    fn exit_trait_member_type(&mut self, &'ast TraitMemberType) {}
    fn exit_try_operator(&mut self, &'ast TryOperator) {}
    fn exit_tuple(&mut self, &'ast Tuple) {}
    fn exit_turbofish(&mut self, &'ast Turbofish) {}
    fn exit_type(&mut self, &'ast Type) {}
    fn exit_type_additional(&mut self, &'ast TypeAdditional) {}
    fn exit_type_alias(&mut self, &'ast TypeAlias) {}
    fn exit_type_array(&mut self, &'ast TypeArray) {}
    fn exit_type_disambiguation(&mut self, &'ast TypeDisambiguation) {}
    fn exit_type_function(&mut self, &'ast TypeFunction) {}
    fn exit_type_function_argument(&mut self, &'ast TypeFunctionArgument) {}
    fn exit_type_function_argument_named(&mut self, &'ast TypeFunctionArgumentNamed) {}
    fn exit_type_generics(&mut self, &'ast TypeGenerics) {}
    fn exit_type_generics_angle(&mut self, &'ast TypeGenericsAngle) {}
    fn exit_type_generics_angle_member(&mut self, &'ast TypeGenericsAngleMember) {}
    fn exit_type_generics_function(&mut self, &'ast TypeGenericsFunction) {}
    fn exit_type_higher_ranked_trait_bounds(&mut self, &'ast TypeHigherRankedTraitBounds) {}
    fn exit_type_higher_ranked_trait_bounds_child(&mut self, &'ast TypeHigherRankedTraitBoundsChild) {}
    fn exit_type_impl_trait(&mut self, &'ast TypeImplTrait) {}
    fn exit_type_kind(&mut self, &'ast TypeKind) {}
    fn exit_type_named(&mut self, &'ast TypeNamed) {}
    fn exit_type_named_component(&mut self, &'ast TypeNamedComponent) {}
    fn exit_type_pointer(&mut self, &'ast TypePointer) {}
    fn exit_type_reference(&mut self, &'ast TypeReference) {}
    fn exit_type_reference_kind(&mut self, &'ast TypeReferenceKind) {}
    fn exit_type_slice(&mut self, &'ast TypeSlice) {}
    fn exit_type_tuple(&mut self, &'ast TypeTuple) {}
    fn exit_unary(&mut self, &'ast Unary) {}
    fn exit_union(&mut self, &'ast Union) {}
    fn exit_unsafe_block(&mut self, &'ast UnsafeBlock) {}
    fn exit_use(&mut self, &'ast Use) {}
    fn exit_use_path(&mut self, &'ast UsePath) {}
    fn exit_use_tail(&mut self, &'ast UseTail) {}
    fn exit_use_tail_glob(&mut self, &'ast UseTailGlob) {}
    fn exit_use_tail_ident(&mut self, &'ast UseTailIdent) {}
    fn exit_use_tail_multi(&mut self, &'ast UseTailMulti) {}
    fn exit_value(&mut self, &'ast Value) {}
    fn exit_visibility(&mut self, &'ast Visibility) {}
    fn exit_where(&mut self, &'ast Where) {}
    fn exit_where_kind(&mut self, &'ast WhereKind) {}
    fn exit_where_lifetime(&mut self, &'ast WhereLifetime) {}
    fn exit_where_type(&mut self, &'ast WhereType) {}
    fn exit_while(&mut self, &'ast While) {}
    fn exit_while_let(&mut self, &'ast WhileLet) {}
    fn exit_whitespace(&mut self, &'ast Whitespace) {}
}
