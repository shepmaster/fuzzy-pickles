use std::{
    cmp,
    collections::BTreeMap,
};

use ::{
    ast::*,
    HasExtent,
    visit::VisitorMut,
};

#[derive(Debug, PartialEq, Eq)]
struct FirstIsMax(Whitespace);

impl PartialOrd for FirstIsMax {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FirstIsMax {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.0.extent().cmp(&other.0.extent())
    }
}

#[derive(Debug, Default)]
pub struct WhitespaceApportioner(BTreeMap<usize, Whitespace>);

impl WhitespaceApportioner {
    pub fn push(&mut self, ws: Whitespace) {
        self.0.insert(ws.extent().0, ws);
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn divvy_up(&mut self, node_extent: ::Extent, whitespace: &mut Vec<Whitespace>) {
        // This could be optimized with the right data structure
        let contained_whitespace_keys: Vec<_> = self.0.range(node_extent.0..node_extent.1)
            .map(|(k, _)| k)
            .cloned()
            .collect();

        let contained_whitespace = contained_whitespace_keys
            .into_iter()
            .map(|k| self.0.remove(&k).unwrap());

        whitespace.extend(contained_whitespace);
    }
}

macro_rules! apply {
    ($(fn $name:ident(&mut self, &mut $kind:ty) {})+) => {
        $(
            fn $name(&mut self, node: &mut $kind) {
                self.divvy_up(node.extent(), &mut node.whitespace);
            }
        )+
    }
}

impl VisitorMut for WhitespaceApportioner {
    apply! {
        fn exit_array_explicit(&mut self, &mut ArrayExplicit) {}
        fn exit_array_repeated(&mut self, &mut ArrayRepeated) {}
        fn exit_as_type(&mut self, &mut AsType) {}
        fn exit_ascription(&mut self, &mut Ascription) {}
        fn exit_associated_type(&mut self, &mut AssociatedType) {}
        fn exit_attribute_literal(&mut self, &mut AttributeLiteral) {}
        fn exit_attribute_containing_literal(&mut self, &mut AttributeContainingLiteral) {}
        fn exit_attributed_enum_variant(&mut self, &mut Attributed<EnumVariant>) {}
        fn exit_attributed_expression(&mut self, &mut Attributed<Expression>) {}
        fn exit_attributed_extern_block_member(&mut self, &mut Attributed<ExternBlockMember>) {}
        fn exit_attributed_generic_declaration_lifetime(&mut self, &mut Attributed<GenericDeclarationLifetime>) {}
        fn exit_attributed_generic_declaration_type(&mut self, &mut Attributed<GenericDeclarationType>) {}
        fn exit_attributed_impl_member(&mut self, &mut Attributed<ImplMember>) {}
        fn exit_attributed_item(&mut self, &mut Attributed<Item>) {}
        fn exit_attributed_struct_definition_field_named(&mut self, &mut Attributed<StructDefinitionFieldNamed>) {}
        fn exit_attributed_struct_definition_field_unnamed(&mut self, &mut Attributed<StructDefinitionFieldUnnamed>) {}
        fn exit_attributed_trait_member(&mut self, &mut Attributed<TraitMember>) {}
        fn exit_binary(&mut self, &mut Binary) {}
        fn exit_block(&mut self, &mut Block) {}
        fn exit_break(&mut self, &mut Break) {}
        fn exit_call(&mut self, &mut Call) {}
        fn exit_closure(&mut self, &mut Closure) {}
        fn exit_closure_arg(&mut self, &mut ClosureArg) {}
        fn exit_const(&mut self, &mut Const) {}
        fn exit_continue(&mut self, &mut Continue) {}
        fn exit_crate(&mut self, &mut Crate) {}
        fn exit_dereference(&mut self, &mut Dereference) {}
        fn exit_disambiguation(&mut self, &mut Disambiguation) {}
        fn exit_enum(&mut self, &mut Enum) {}
        fn exit_enum_variant(&mut self, &mut EnumVariant) {}
        fn exit_expression_box(&mut self, &mut ExpressionBox) {}
        fn exit_extern_block(&mut self, &mut ExternBlock) {}
        fn exit_extern_block_member_function(&mut self, &mut ExternBlockMemberFunction) {}
        fn exit_extern_block_member_function_argument_named(&mut self, &mut ExternBlockMemberFunctionArgumentNamed) {}
        fn exit_extern_block_member_static(&mut self, &mut ExternBlockMemberStatic) {}
        fn exit_extern_block_member_type(&mut self, &mut ExternBlockMemberType) {}
        fn exit_field_access(&mut self, &mut FieldAccess) {}
        fn exit_for_loop(&mut self, &mut ForLoop) {}
        fn exit_function(&mut self, &mut Function) {}
        fn exit_function_header(&mut self, &mut FunctionHeader) {}
        fn exit_function_qualifiers(&mut self, &mut FunctionQualifiers) {}
        fn exit_generic_declaration_lifetime(&mut self, &mut GenericDeclarationLifetime) {}
        fn exit_generic_declaration_type(&mut self, &mut GenericDeclarationType) {}
        fn exit_generic_declarations(&mut self, &mut GenericDeclarations) {}
        fn exit_if(&mut self, &mut If) {}
        fn exit_if_let(&mut self, &mut IfLet) {}
        fn exit_impl(&mut self, &mut Impl) {}
        fn exit_impl_const(&mut self, &mut ImplConst) {}
        fn exit_impl_function(&mut self, &mut ImplFunction) {}
        fn exit_impl_of_inherent(&mut self, &mut ImplOfInherent) {}
        fn exit_impl_of_trait(&mut self, &mut ImplOfTrait) {}
        fn exit_impl_type(&mut self, &mut ImplType) {}
        fn exit_let(&mut self, &mut Let) {}
        fn exit_loop(&mut self, &mut Loop) {}
        fn exit_macro_call(&mut self, &mut MacroCall) {}
        fn exit_match(&mut self, &mut Match) {}
        fn exit_match_arm(&mut self, &mut MatchArm) {}
        fn exit_module(&mut self, &mut Module) {}
        fn exit_named_argument(&mut self, &mut NamedArgument) {}
        fn exit_number(&mut self, &mut Number) {}
        fn exit_parenthetical(&mut self, &mut Parenthetical) {}
        fn exit_path(&mut self, &mut Path) {}
        fn exit_path_component(&mut self, &mut PathComponent) {}
        fn exit_pathed_ident(&mut self, &mut PathedIdent) {}
        fn exit_pattern(&mut self, &mut Pattern) {}
        fn exit_pattern_box(&mut self, &mut PatternBox) {}
        fn exit_pattern_ident(&mut self, &mut PatternIdent) {}
        fn exit_pattern_macro_call(&mut self, &mut PatternMacroCall) {}
        fn exit_pattern_name(&mut self, &mut PatternName) {}
        fn exit_pattern_number(&mut self, &mut PatternNumber) {}
        fn exit_pattern_range_exclusive(&mut self, &mut PatternRangeExclusive) {}
        fn exit_pattern_range_inclusive(&mut self, &mut PatternRangeInclusive) {}
        fn exit_pattern_reference(&mut self, &mut PatternReference) {}
        fn exit_pattern_slice(&mut self, &mut PatternSlice) {}
        fn exit_pattern_slice_subslice(&mut self, &mut PatternSliceSubslice) {}
        fn exit_pattern_struct(&mut self, &mut PatternStruct) {}
        fn exit_pattern_struct_field_long(&mut self, &mut PatternStructFieldLong) {}
        fn exit_pattern_struct_field_short(&mut self, &mut PatternStructFieldShort) {}
        fn exit_pattern_tuple(&mut self, &mut PatternTuple) {}
        fn exit_range(&mut self, &mut Range) {}
        fn exit_range_inclusive(&mut self, &mut RangeInclusive) {}
        fn exit_reference(&mut self, &mut Reference) {}
        fn exit_return(&mut self, &mut Return) {}
        fn exit_self_argument_longhand(&mut self, &mut SelfArgumentLonghand) {}
        fn exit_self_argument_shorthand(&mut self, &mut SelfArgumentShorthand) {}
        fn exit_slice(&mut self, &mut Slice) {}
        fn exit_static(&mut self, &mut Static) {}
        fn exit_struct(&mut self, &mut Struct) {}
        fn exit_struct_definition_body_brace(&mut self, &mut StructDefinitionBodyBrace) {}
        fn exit_struct_definition_body_tuple(&mut self, &mut StructDefinitionBodyTuple) {}
        fn exit_struct_definition_field_named(&mut self, &mut StructDefinitionFieldNamed) {}
        fn exit_struct_definition_field_unnamed(&mut self, &mut StructDefinitionFieldUnnamed) {}
        fn exit_struct_literal(&mut self, &mut StructLiteral) {}
        fn exit_struct_literal_field(&mut self, &mut StructLiteralField) {}
        fn exit_trait(&mut self, &mut Trait) {}
        fn exit_trait_bound_lifetime(&mut self, &mut TraitBoundLifetime) {}
        fn exit_trait_bound_normal(&mut self, &mut TraitBoundNormal) {}
        fn exit_trait_bound_relaxed(&mut self, &mut TraitBoundRelaxed) {}
        fn exit_trait_bounds(&mut self, &mut TraitBounds) {}
        fn exit_trait_impl_argument_named(&mut self, &mut TraitImplArgumentNamed) {}
        fn exit_trait_impl_function_header(&mut self, &mut TraitImplFunctionHeader) {}
        fn exit_trait_member_const(&mut self, &mut TraitMemberConst) {}
        fn exit_trait_member_function(&mut self, &mut TraitMemberFunction) {}
        fn exit_trait_member_type(&mut self, &mut TraitMemberType) {}
        fn exit_try_operator(&mut self, &mut TryOperator) {}
        fn exit_tuple(&mut self, &mut Tuple) {}
        fn exit_turbofish(&mut self, &mut Turbofish) {}
        fn exit_type(&mut self, &mut Type) {}
        fn exit_type_alias(&mut self, &mut TypeAlias) {}
        fn exit_type_array(&mut self, &mut TypeArray) {}
        fn exit_type_disambiguation(&mut self, &mut TypeDisambiguation) {}
        fn exit_type_function(&mut self, &mut TypeFunction) {}
        fn exit_type_function_argument_named(&mut self, &mut TypeFunctionArgumentNamed) {}
        fn exit_type_generics_angle(&mut self, &mut TypeGenericsAngle) {}
        fn exit_type_generics_function(&mut self, &mut TypeGenericsFunction) {}
        fn exit_type_higher_ranked_trait_bounds(&mut self, &mut TypeHigherRankedTraitBounds) {}
        fn exit_type_impl_trait(&mut self, &mut TypeImplTrait) {}
        fn exit_type_named(&mut self, &mut TypeNamed) {}
        fn exit_type_named_component(&mut self, &mut TypeNamedComponent) {}
        fn exit_type_pointer(&mut self, &mut TypePointer) {}
        fn exit_type_reference(&mut self, &mut TypeReference) {}
        fn exit_type_reference_kind(&mut self, &mut TypeReferenceKind) {}
        fn exit_type_slice(&mut self, &mut TypeSlice) {}
        fn exit_type_tuple(&mut self, &mut TypeTuple) {}
        fn exit_unary(&mut self, &mut Unary) {}
        fn exit_union(&mut self, &mut Union) {}
        fn exit_unsafe_block(&mut self, &mut UnsafeBlock) {}
        fn exit_use(&mut self, &mut Use) {}
        fn exit_use_path(&mut self, &mut UsePath) {}
        fn exit_use_tail_ident(&mut self, &mut UseTailIdent) {}
        fn exit_use_tail_multi(&mut self, &mut UseTailMulti) {}
        fn exit_value(&mut self, &mut Value) {}
        fn exit_visibility(&mut self, &mut Visibility) {}
        fn exit_where(&mut self, &mut Where) {}
        fn exit_where_lifetime(&mut self, &mut WhereLifetime) {}
        fn exit_where_type(&mut self, &mut WhereType) {}
        fn exit_while(&mut self, &mut While) {}
        fn exit_while_let(&mut self, &mut WhileLet) {}
    }

    // Everything else should fall into here
    fn exit_file(&mut self, node: &mut File) {
        let remaining_whitespace = ::std::mem::replace(&mut self.0, Default::default())
            .into_iter()
            .map(|(_, ws)| ws);

        node.whitespace.extend(remaining_whitespace)
    }
}

#[cfg(test)]
mod test {
    use ::{parse_rust_file, Extent};

    macro_rules! assert_whitespace {
        ($a:expr, $b:expr) => {
            let a = $a.iter().map(::HasExtent::extent).collect::<Vec<_>>();
            let b = $b.iter().map(::HasExtent::extent).collect::<Vec<_>>();
            assert_eq!(a, b);
        }
    }

    #[test]
    fn whitespace_is_assigned() {
        let r = parse_rust_file(" fn main() { /* comment */ } ")
            .expect("Unable to parse file");
        println!("{:#?}", r);
        assert_whitespace!(r.whitespace, [Extent(0, 1), Extent(28, 29)]);

        let f = r.items[0].value.as_function().unwrap();

        assert_whitespace!(f.header.whitespace, [Extent(3, 4)]);
        assert_whitespace!(f.body.whitespace, [Extent(12, 13), Extent(13, 26), Extent(26, 27)]);
        assert_whitespace!(f.whitespace, [Extent(10, 11)]);
    }
}
