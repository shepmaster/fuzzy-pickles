use ast::*;
use Extent;

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

#[derive(Debug, PartialEq)]
pub enum Control {
    Continue,
    Break
}

pub trait Visitor {
    fn visit_argument(&mut self, &Argument) -> Control { Control::Continue }
    fn visit_array(&mut self, &Array) -> Control { Control::Continue }
    fn visit_array_explicit(&mut self, &ArrayExplicit) -> Control { Control::Continue }
    fn visit_array_repeated(&mut self, &ArrayRepeated) -> Control { Control::Continue }
    fn visit_as_type(&mut self, &AsType) -> Control { Control::Continue }
    fn visit_ascription(&mut self, &Ascription) -> Control { Control::Continue }
    fn visit_associated_type(&mut self, &AssociatedType) -> Control { Control::Continue }
    fn visit_attribute(&mut self, &Attribute) -> Control { Control::Continue }
    fn visit_attribute_containing(&mut self, &AttributeContaining) -> Control { Control::Continue }
    fn visit_attributed_enum_variant(&mut self, &Attributed<EnumVariant>) -> Control { Control::Continue }
    fn visit_attributed_expression(&mut self, &Attributed<Expression>) -> Control { Control::Continue }
    fn visit_attributed_extern_block_member(&mut self, &Attributed<ExternBlockMember>) -> Control { Control::Continue }
    fn visit_attributed_generic_declaration_lifetime(&mut self, &Attributed<GenericDeclarationLifetime>) -> Control { Control::Continue }
    fn visit_attributed_generic_declaration_type(&mut self, &Attributed<GenericDeclarationType>) -> Control { Control::Continue }
    fn visit_attributed_impl_member(&mut self, &Attributed<ImplMember>) -> Control { Control::Continue }
    fn visit_attributed_item(&mut self, &Attributed<Item>) -> Control { Control::Continue }
    fn visit_attributed_struct_definition_field_named(&mut self, &Attributed<StructDefinitionFieldNamed>) -> Control { Control::Continue }
    fn visit_attributed_struct_definition_field_unnamed(&mut self, &Attributed<StructDefinitionFieldUnnamed>) -> Control { Control::Continue }
    fn visit_attributed_trait_member(&mut self, &Attributed<TraitMember>) -> Control { Control::Continue }
    fn visit_binary(&mut self, &Binary) -> Control { Control::Continue }
    fn visit_block(&mut self, &Block) -> Control { Control::Continue }
    fn visit_break(&mut self, &Break) -> Control { Control::Continue }
    fn visit_byte(&mut self, &Byte) -> Control { Control::Continue }
    fn visit_byte_string(&mut self, &ByteString) -> Control { Control::Continue }
    fn visit_call(&mut self, &Call) -> Control { Control::Continue }
    fn visit_character(&mut self, &Character) -> Control { Control::Continue }
    fn visit_closure(&mut self, &Closure) -> Control { Control::Continue }
    fn visit_closure_arg(&mut self, &ClosureArg) -> Control { Control::Continue }
    fn visit_comment(&mut self, &Comment) -> Control { Control::Continue }
    fn visit_const(&mut self, &Const) -> Control { Control::Continue }
    fn visit_continue(&mut self, &Continue) -> Control { Control::Continue }
    fn visit_crate(&mut self, &Crate) -> Control { Control::Continue }
    fn visit_dereference(&mut self, &Dereference) -> Control { Control::Continue }
    fn visit_disambiguation(&mut self, &Disambiguation) -> Control { Control::Continue }
    fn visit_enum(&mut self, &Enum) -> Control { Control::Continue }
    fn visit_enum_variant(&mut self, &EnumVariant) -> Control { Control::Continue }
    fn visit_enum_variant_body(&mut self, &EnumVariantBody) -> Control { Control::Continue }
    fn visit_expression(&mut self, &Expression) -> Control { Control::Continue }
    fn visit_expression_box(&mut self, &ExpressionBox) -> Control { Control::Continue }
    fn visit_extern_block(&mut self, &ExternBlock) -> Control { Control::Continue }
    fn visit_extern_block_member(&mut self, &ExternBlockMember) -> Control { Control::Continue }
    fn visit_extern_block_member_function(&mut self, &ExternBlockMemberFunction) -> Control { Control::Continue }
    fn visit_extern_block_member_function_argument(&mut self, &ExternBlockMemberFunctionArgument) -> Control { Control::Continue }
    fn visit_extern_block_member_function_argument_named(&mut self, &ExternBlockMemberFunctionArgumentNamed) -> Control { Control::Continue }
    fn visit_extern_block_member_function_argument_variadic(&mut self, &ExternBlockMemberFunctionArgumentVariadic) -> Control { Control::Continue }
    fn visit_extern_block_member_static(&mut self, &ExternBlockMemberStatic) -> Control { Control::Continue }
    fn visit_extern_block_member_type(&mut self, &ExternBlockMemberType) -> Control { Control::Continue }
    fn visit_field_access(&mut self, &FieldAccess) -> Control { Control::Continue }
    fn visit_file(&mut self, &File) -> Control { Control::Continue }
    fn visit_for_loop(&mut self, &ForLoop) -> Control { Control::Continue }
    fn visit_function(&mut self, &Function) -> Control { Control::Continue }
    fn visit_function_header(&mut self, &FunctionHeader) -> Control { Control::Continue }
    fn visit_function_qualifiers(&mut self, &FunctionQualifiers) -> Control { Control::Continue }
    fn visit_generic_declaration_lifetime(&mut self, &GenericDeclarationLifetime) -> Control { Control::Continue }
    fn visit_generic_declaration_type(&mut self, &GenericDeclarationType) -> Control { Control::Continue }
    fn visit_generic_declarations(&mut self, &GenericDeclarations) -> Control { Control::Continue }
    fn visit_ident(&mut self, &Ident) -> Control { Control::Continue }
    fn visit_if(&mut self, &If) -> Control { Control::Continue }
    fn visit_if_let(&mut self, &IfLet) -> Control { Control::Continue }
    fn visit_impl(&mut self, &Impl) -> Control { Control::Continue }
    fn visit_impl_const(&mut self, &ImplConst) -> Control { Control::Continue }
    fn visit_impl_function(&mut self, &ImplFunction) -> Control { Control::Continue }
    fn visit_impl_kind(&mut self, &ImplKind) -> Control { Control::Continue }
    fn visit_impl_member(&mut self, &ImplMember) -> Control { Control::Continue }
    fn visit_impl_of_inherent(&mut self, &ImplOfInherent) -> Control { Control::Continue }
    fn visit_impl_of_trait(&mut self, &ImplOfTrait) -> Control { Control::Continue }
    fn visit_impl_of_trait_type(&mut self, &ImplOfTraitType) -> Control { Control::Continue }
    fn visit_impl_type(&mut self, &ImplType) -> Control { Control::Continue }
    fn visit_item(&mut self, &Item) -> Control { Control::Continue }
    fn visit_let(&mut self, &Let) -> Control { Control::Continue }
    fn visit_lifetime(&mut self, &Lifetime) -> Control { Control::Continue }
    fn visit_loop(&mut self, &Loop) -> Control { Control::Continue }
    fn visit_macro_call(&mut self, &MacroCall) -> Control { Control::Continue }
    fn visit_macro_call_args(&mut self, &MacroCallArgs) -> Control { Control::Continue }
    fn visit_match(&mut self, &Match) -> Control { Control::Continue }
    fn visit_match_arm(&mut self, &MatchArm) -> Control { Control::Continue }
    fn visit_match_hand(&mut self, &MatchHand) -> Control { Control::Continue }
    fn visit_module(&mut self, &Module) -> Control { Control::Continue }
    fn visit_named_argument(&mut self, &NamedArgument) -> Control { Control::Continue }
    fn visit_number(&mut self, &Number) -> Control { Control::Continue }
    fn visit_number_value(&mut self, &NumberValue) -> Control { Control::Continue }
    fn visit_number_binary(&mut self, &NumberBinary) -> Control { Control::Continue }
    fn visit_number_decimal(&mut self, &NumberDecimal) -> Control { Control::Continue }
    fn visit_number_hexadecimal(&mut self, &NumberHexadecimal) -> Control { Control::Continue }
    fn visit_number_octal(&mut self, &NumberOctal) -> Control { Control::Continue }
    fn visit_parenthetical(&mut self, &Parenthetical) -> Control { Control::Continue }
    fn visit_path(&mut self, &Path) -> Control { Control::Continue }
    fn visit_path_component(&mut self, &PathComponent) -> Control { Control::Continue }
    fn visit_pathed_ident(&mut self, &PathedIdent) -> Control { Control::Continue }
    fn visit_pattern(&mut self, &Pattern) -> Control { Control::Continue }
    fn visit_pattern_name(&mut self, &PatternName) -> Control { Control::Continue }
    fn visit_pattern_bundle_member(&mut self, &PatternBundleMember) -> Control { Control::Continue }
    fn visit_pattern_byte(&mut self, &PatternByte) -> Control { Control::Continue }
    fn visit_pattern_byte_string(&mut self, &PatternByteString) -> Control { Control::Continue }
    fn visit_pattern_character(&mut self, &PatternCharacter) -> Control { Control::Continue }
    fn visit_pattern_ident(&mut self, &PatternIdent) -> Control { Control::Continue }
    fn visit_pattern_kind(&mut self, &PatternKind) -> Control { Control::Continue }
    fn visit_pattern_macro_call(&mut self, &PatternMacroCall) -> Control { Control::Continue }
    fn visit_pattern_number(&mut self, &PatternNumber) -> Control { Control::Continue }
    fn visit_pattern_range_exclusive(&mut self, &PatternRangeExclusive) -> Control { Control::Continue }
    fn visit_pattern_range_inclusive(&mut self, &PatternRangeInclusive) -> Control { Control::Continue }
    fn visit_pattern_reference(&mut self, &PatternReference) -> Control { Control::Continue }
    fn visit_pattern_slice(&mut self, &PatternSlice) -> Control { Control::Continue }
    fn visit_pattern_string(&mut self, &PatternString) -> Control { Control::Continue }
    fn visit_pattern_struct(&mut self, &PatternStruct) -> Control { Control::Continue }
    fn visit_pattern_struct_field(&mut self, &PatternStructField) -> Control { Control::Continue }
    fn visit_pattern_struct_field_long(&mut self, &PatternStructFieldLong) -> Control { Control::Continue }
    fn visit_pattern_struct_field_short(&mut self, &PatternStructFieldShort) -> Control { Control::Continue }
    fn visit_pattern_tuple(&mut self, &PatternTuple) -> Control { Control::Continue }
    fn visit_pattern_wildcard(&mut self, &PatternWildcard) -> Control { Control::Continue }
    fn visit_pattern_box(&mut self, &PatternBox) -> Control { Control::Continue }
    fn visit_range(&mut self, &Range) -> Control { Control::Continue }
    fn visit_range_inclusive(&mut self, &RangeInclusive) -> Control { Control::Continue }
    fn visit_reference(&mut self, &Reference) -> Control { Control::Continue }
    fn visit_return(&mut self, &Return) -> Control { Control::Continue }
    fn visit_self_argument(&mut self, &SelfArgument) -> Control { Control::Continue }
    fn visit_self_argument_longhand(&mut self, &SelfArgumentLonghand) -> Control { Control::Continue }
    fn visit_self_argument_shorthand(&mut self, &SelfArgumentShorthand) -> Control { Control::Continue }
    fn visit_self_argument_shorthand_qualifier(&mut self, &SelfArgumentShorthandQualifier) -> Control { Control::Continue }
    fn visit_slice(&mut self, &Slice) -> Control { Control::Continue }
    fn visit_statement(&mut self, &Statement) -> Control { Control::Continue }
    fn visit_static(&mut self, &Static) -> Control { Control::Continue }
    fn visit_string(&mut self, &String) -> Control { Control::Continue }
    fn visit_struct(&mut self, &Struct) -> Control { Control::Continue }
    fn visit_struct_definition_body(&mut self, &StructDefinitionBody) -> Control { Control::Continue }
    fn visit_struct_definition_body_brace(&mut self, &StructDefinitionBodyBrace) -> Control { Control::Continue }
    fn visit_struct_definition_body_tuple(&mut self, &StructDefinitionBodyTuple) -> Control { Control::Continue }
    fn visit_struct_definition_field_named(&mut self, &StructDefinitionFieldNamed) -> Control { Control::Continue }
    fn visit_struct_definition_field_unnamed(&mut self, &StructDefinitionFieldUnnamed) -> Control { Control::Continue }
    fn visit_struct_literal(&mut self, &StructLiteral) -> Control { Control::Continue }
    fn visit_struct_literal_field(&mut self, &StructLiteralField) -> Control { Control::Continue }
    fn visit_trait(&mut self, &Trait) -> Control { Control::Continue }
    fn visit_trait_bound(&mut self, &TraitBound) -> Control { Control::Continue }
    fn visit_trait_bound_lifetime(&mut self, &TraitBoundLifetime) -> Control { Control::Continue }
    fn visit_trait_bound_normal(&mut self, &TraitBoundNormal) -> Control { Control::Continue }
    fn visit_trait_bound_relaxed(&mut self, &TraitBoundRelaxed) -> Control { Control::Continue }
    fn visit_trait_bound_type(&mut self, &TraitBoundType) -> Control { Control::Continue }
    fn visit_trait_bounds(&mut self, &TraitBounds) -> Control { Control::Continue }
    fn visit_trait_impl_argument(&mut self, &TraitImplArgument) -> Control { Control::Continue }
    fn visit_trait_impl_argument_named(&mut self, &TraitImplArgumentNamed) -> Control { Control::Continue }
    fn visit_trait_impl_function_header(&mut self, &TraitImplFunctionHeader) -> Control { Control::Continue }
    fn visit_trait_member(&mut self, &TraitMember) -> Control { Control::Continue }
    fn visit_trait_member_const(&mut self, &TraitMemberConst) -> Control { Control::Continue }
    fn visit_trait_member_function(&mut self, &TraitMemberFunction) -> Control { Control::Continue }
    fn visit_trait_member_type(&mut self, &TraitMemberType) -> Control { Control::Continue }
    fn visit_try_operator(&mut self, &TryOperator) -> Control { Control::Continue }
    fn visit_tuple(&mut self, &Tuple) -> Control { Control::Continue }
    fn visit_turbofish(&mut self, &Turbofish) -> Control { Control::Continue }
    fn visit_type(&mut self, &Type) -> Control { Control::Continue }
    fn visit_type_additional(&mut self, &TypeAdditional) -> Control { Control::Continue }
    fn visit_type_alias(&mut self, &TypeAlias) -> Control { Control::Continue }
    fn visit_type_array(&mut self, &TypeArray) -> Control { Control::Continue }
    fn visit_type_disambiguation(&mut self, &TypeDisambiguation) -> Control { Control::Continue }
    fn visit_type_function(&mut self, &TypeFunction) -> Control { Control::Continue }
    fn visit_type_function_argument(&mut self, &TypeFunctionArgument) -> Control { Control::Continue }
    fn visit_type_function_argument_named(&mut self, &TypeFunctionArgumentNamed) -> Control { Control::Continue }
    fn visit_type_generics(&mut self, &TypeGenerics) -> Control { Control::Continue }
    fn visit_type_generics_angle(&mut self, &TypeGenericsAngle) -> Control { Control::Continue }
    fn visit_type_generics_angle_member(&mut self, &TypeGenericsAngleMember) -> Control { Control::Continue }
    fn visit_type_generics_function(&mut self, &TypeGenericsFunction) -> Control { Control::Continue }
    fn visit_type_higher_ranked_trait_bounds(&mut self, &TypeHigherRankedTraitBounds) -> Control { Control::Continue }
    fn visit_type_higher_ranked_trait_bounds_child(&mut self, &TypeHigherRankedTraitBoundsChild) -> Control { Control::Continue }
    fn visit_type_impl_trait(&mut self, &TypeImplTrait) -> Control { Control::Continue }
    fn visit_type_kind(&mut self, &TypeKind) -> Control { Control::Continue }
    fn visit_type_named(&mut self, &TypeNamed) -> Control { Control::Continue }
    fn visit_type_named_component(&mut self, &TypeNamedComponent) -> Control { Control::Continue }
    fn visit_type_pointer(&mut self, &TypePointer) -> Control { Control::Continue }
    fn visit_type_reference(&mut self, &TypeReference) -> Control { Control::Continue }
    fn visit_type_reference_kind(&mut self, &TypeReferenceKind) -> Control { Control::Continue }
    fn visit_type_slice(&mut self, &TypeSlice) -> Control { Control::Continue }
    fn visit_type_tuple(&mut self, &TypeTuple) -> Control { Control::Continue }
    fn visit_unary(&mut self, &Unary) -> Control { Control::Continue }
    fn visit_union(&mut self, &Union) -> Control { Control::Continue }
    fn visit_unsafe_block(&mut self, &UnsafeBlock) -> Control { Control::Continue }
    fn visit_use(&mut self, &Use) -> Control { Control::Continue }
    fn visit_use_path(&mut self, &UsePath) -> Control { Control::Continue }
    fn visit_use_tail(&mut self, &UseTail) -> Control { Control::Continue }
    fn visit_use_tail_glob(&mut self, &UseTailGlob) -> Control { Control::Continue }
    fn visit_use_tail_ident(&mut self, &UseTailIdent) -> Control { Control::Continue }
    fn visit_use_tail_multi(&mut self, &UseTailMulti) -> Control { Control::Continue }
    fn visit_value(&mut self, &Value) -> Control { Control::Continue }
    fn visit_visibility(&mut self, &Visibility) -> Control { Control::Continue }
    fn visit_where(&mut self, &Where) -> Control { Control::Continue }
    fn visit_where_kind(&mut self, &WhereKind) -> Control { Control::Continue }
    fn visit_where_lifetime(&mut self, &WhereLifetime) -> Control { Control::Continue }
    fn visit_where_type(&mut self, &WhereType) -> Control { Control::Continue }
    fn visit_while(&mut self, &While) -> Control { Control::Continue }
    fn visit_while_let(&mut self, &WhileLet) -> Control { Control::Continue }
    fn visit_whitespace(&mut self, &Whitespace) -> Control { Control::Continue }

    fn exit_argument(&mut self, &Argument) {}
    fn exit_array(&mut self, &Array) {}
    fn exit_array_explicit(&mut self, &ArrayExplicit) {}
    fn exit_array_repeated(&mut self, &ArrayRepeated) {}
    fn exit_as_type(&mut self, &AsType) {}
    fn exit_ascription(&mut self, &Ascription) {}
    fn exit_associated_type(&mut self, &AssociatedType) {}
    fn exit_attribute(&mut self, &Attribute) {}
    fn exit_attribute_containing(&mut self, &AttributeContaining) {}
    fn exit_attributed_enum_variant(&mut self, &Attributed<EnumVariant>) {}
    fn exit_attributed_expression(&mut self, &Attributed<Expression>) {}
    fn exit_attributed_extern_block_member(&mut self, &Attributed<ExternBlockMember>) {}
    fn exit_attributed_generic_declaration_lifetime(&mut self, &Attributed<GenericDeclarationLifetime>) {}
    fn exit_attributed_generic_declaration_type(&mut self, &Attributed<GenericDeclarationType>) {}
    fn exit_attributed_impl_member(&mut self, &Attributed<ImplMember>) {}
    fn exit_attributed_item(&mut self, &Attributed<Item>) {}
    fn exit_attributed_struct_definition_field_named(&mut self, &Attributed<StructDefinitionFieldNamed>) {}
    fn exit_attributed_struct_definition_field_unnamed(&mut self, &Attributed<StructDefinitionFieldUnnamed>) {}
    fn exit_attributed_trait_member(&mut self, &Attributed<TraitMember>) {}
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
    fn exit_extern_block_member_type(&mut self, &ExternBlockMemberType) {}
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
    fn exit_pattern_box(&mut self, &PatternBox) {}
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
    fn exit_type_additional(&mut self, &TypeAdditional) {}
    fn exit_type_alias(&mut self, &TypeAlias) {}
    fn exit_type_array(&mut self, &TypeArray) {}
    fn exit_type_disambiguation(&mut self, &TypeDisambiguation) {}
    fn exit_type_function(&mut self, &TypeFunction) {}
    fn exit_type_function_argument(&mut self, &TypeFunctionArgument) {}
    fn exit_type_function_argument_named(&mut self, &TypeFunctionArgumentNamed) {}
    fn exit_type_generics(&mut self, &TypeGenerics) {}
    fn exit_type_generics_angle(&mut self, &TypeGenericsAngle) {}
    fn exit_type_generics_angle_member(&mut self, &TypeGenericsAngleMember) {}
    fn exit_type_generics_function(&mut self, &TypeGenericsFunction) {}
    fn exit_type_higher_ranked_trait_bounds(&mut self, &TypeHigherRankedTraitBounds) {}
    fn exit_type_higher_ranked_trait_bounds_child(&mut self, &TypeHigherRankedTraitBoundsChild) {}
    fn exit_type_impl_trait(&mut self, &TypeImplTrait) {}
    fn exit_type_kind(&mut self, &TypeKind) {}
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
    fn exit_use_path(&mut self, &UsePath) {}
    fn exit_use_tail(&mut self, &UseTail) {}
    fn exit_use_tail_glob(&mut self, &UseTailGlob) {}
    fn exit_use_tail_ident(&mut self, &UseTailIdent) {}
    fn exit_use_tail_multi(&mut self, &UseTailMulti) {}
    fn exit_value(&mut self, &Value) {}
    fn exit_visibility(&mut self, &Visibility) {}
    fn exit_where(&mut self, &Where) {}
    fn exit_where_kind(&mut self, &WhereKind) {}
    fn exit_where_lifetime(&mut self, &WhereLifetime) {}
    fn exit_where_type(&mut self, &WhereType) {}
    fn exit_while(&mut self, &While) {}
    fn exit_while_let(&mut self, &WhileLet) {}
    fn exit_whitespace(&mut self, &Whitespace) {}
}
