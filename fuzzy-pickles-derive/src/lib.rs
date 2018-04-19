extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use std::iter;

use proc_macro::TokenStream;

#[proc_macro_derive(Visit, attributes(visit))]
pub fn visit_derive(input: TokenStream) -> TokenStream {
    // Construct a string representation of the type definition
    let s = input.to_string();

    // Parse the string representation
    let ast = syn::parse_macro_input(&s).expect("Unable to parse input");

    // Build the impl
    let gen = impl_visit(&ast);

    // Return the generated impl
    gen.parse().expect("Unable to generate")
}

#[proc_macro_derive(HasExtent)]
pub fn has_extent_derive(input: TokenStream) -> TokenStream {
    // Construct a string representation of the type definition
    let s = input.to_string();

    // Parse the string representation
    let ast = syn::parse_macro_input(&s).expect("Unable to parse input");

    // Build the impl
    let gen = impl_has_extent(&ast);

    // Return the generated impl
    gen.parse().expect("Unable to generate")
}

#[proc_macro_derive(ExtentIndex)]
pub fn extent_index_derive(input: TokenStream) -> TokenStream {
    // Construct a string representation of the type definition
    let s = input.to_string();

    // Parse the string representation
    let ast = syn::parse_macro_input(&s).expect("Unable to parse input");

    // Build the impl
    let gen = impl_extent_index(&ast);

    // Return the generated impl
    gen.parse().expect("Unable to generate")
}

fn camelcase_to_snake_case(camelcase: &str) -> String {
    let mut s = String::new();

    for c in camelcase.chars() {
        if c.is_lowercase() {
            s.push(c);
        } else {
            s.push('_');
            s.extend(c.to_lowercase());
        }
    }

    s
}

fn impl_visit(ast: &syn::MacroInput) -> quote::Tokens {
    let name = &ast.ident;
    let method_name_base = camelcase_to_snake_case(&name.to_string());
    let method_name: quote::Ident = format!("visit{}", method_name_base).into();
    let exit_method_name: quote::Ident = format!("exit{}", method_name_base).into();

    let visit_fields = impl_visit_fields(ast);

    quote! {
        impl ::visit::Visit for #name {
            fn visit<'ast, V>(&'ast self, v: &mut V)
            where
                V: ::visit::Visitor<'ast>,
            {
                if ::visit::Control::Continue == v.#method_name(self) {
                    #visit_fields;
                }
                v.#exit_method_name(self);
            }
        }
    }
}

fn impl_visit_fields(ast: &syn::MacroInput) -> quote::Tokens {
    use syn::{Body, VariantData};

    match ast.body {
        Body::Enum(ref e) => {
            let enum_name = iter::repeat(&ast.ident);
            let variant_names = e.iter().map(|variant| &variant.ident);

            quote! {
                match *self {
                    #(#enum_name::#variant_names(ref x) => ::visit::Visit::visit(x, v),)*
                }
            }
        }
        Body::Struct(VariantData::Struct(ref fields)) |
        Body::Struct(VariantData::Tuple(ref fields)) => {
            let field_names = fields
                .iter()
                .enumerate()
                .filter(|&(_, ref f)| !is_ignore_field(f))
                .map(|(i, f)| f.ident.clone().unwrap_or_else(|| i.into()));

            quote! {
                #(::visit::Visit::visit(&self.#field_names, v);)*
            }
        }
        Body::Struct(VariantData::Unit) => quote! {},
    }
}

fn is_ignore_field(field: &syn::Field) -> bool {
    use syn::MetaItem;

    let attr_name: syn::Ident = "visit".into();

    field.attrs.iter().any(|attr| {
        match attr.value {
            MetaItem::List(ref name, ref children) => {
                name == &attr_name && children.iter().any(ignore_field_inner)
            },
            _ => false,
        }
    })
}

fn ignore_field_inner(item: &syn::NestedMetaItem) -> bool {
    use syn::{NestedMetaItem, MetaItem};

    let ignore_value: syn::Ident = "ignore".into();

    match *item {
        NestedMetaItem::MetaItem(MetaItem::Word(ref i)) => i == &ignore_value,
        _ => false
    }
}

#[proc_macro_derive(Decompose)]
pub fn decompose_derive(input: TokenStream) -> TokenStream {
    // Construct a string representation of the type definition
    let s = input.to_string();

    // Parse the string representation
    let ast = syn::parse_macro_input(&s).expect("Unable to parse input");

    // Build the impl
    let gen = impl_decompose(&ast);

    // Return the generated impl
    gen.parse().expect("Unable to generate")
}

fn impl_decompose(ast: &syn::MacroInput) -> quote::Tokens {
    use syn::{Ident, Body, VariantData, Ty};

    let name = &ast.ident;

    let e = match ast.body {
        Body::Enum(ref e) => e,
        _ => panic!("Can only decompose enums"),
    };

    let enum_name = &ast.ident;

    struct Info<'a> {
        variant_name: &'a Ident,
        variant_snake_name: String,
        variant_type: &'a Ty,
    }

    let enum_info: Vec<_> = e.iter().map(|variant| {
        let fields = match variant.data {
            VariantData::Tuple(ref fields) => fields,
            _ => panic!("Can only decompose tuple variants"),
        };

        let field = match fields.len() {
            1 => &fields[0],
            _ => panic!("can only decompose exactly one field"),
        };

        Info {
            variant_name: &variant.ident,
            variant_snake_name: camelcase_to_snake_case(&variant.ident.to_string()),
            variant_type: &field.ty,
        }
    }).collect();

    let into_fns = enum_info.iter().map(|info| {
        let Info { variant_name, ref variant_snake_name, variant_type } = *info;
        let method_name: Ident = format!("into{}", variant_snake_name).into();

        quote! {
            pub fn #method_name(self) -> Option<#variant_type> {
                match self {
                    #enum_name::#variant_name(x) => Some(x),
                    _ => None,
                }
            }
        }
    });

    let as_fns = enum_info.iter().map(|info| {
        let Info { variant_name, ref variant_snake_name, variant_type } = *info;
        let method_name: Ident = format!("as{}", variant_snake_name).into();

        quote! {
            pub fn #method_name(&self) -> Option<&#variant_type> {
                match *self {
                    #enum_name::#variant_name(ref x) => Some(x),
                    _ => None,
                }
            }
        }
    });

    let is_fns = enum_info.iter().map(|info| {
        let Info { variant_name, ref variant_snake_name, .. } = *info;
        let method_name: Ident = format!("is{}", variant_snake_name).into();

        quote! {
            pub fn #method_name(&self) -> bool {
                match *self {
                    #enum_name::#variant_name(..) => true,
                    _ => false,
                }
            }
        }
    });

    quote! {
        impl #name {
            #(#into_fns)*
            #(#as_fns)*
            #(#is_fns)*
        }
    }
}

fn impl_has_extent(ast: &syn::MacroInput) -> quote::Tokens {
    let name = &ast.ident;

    use syn::{Body, VariantData};

    let body = match ast.body {
        Body::Enum(ref e) => {
            let enum_name = iter::repeat(&ast.ident);
            let variant_names = e.iter().map(|variant| &variant.ident);

            quote! {
                match *self {
                    #(#enum_name::#variant_names(ref x) => ::HasExtent::extent(x),)*
                }
            }
        }
        Body::Struct(VariantData::Struct(..)) => {
            quote! {
                self.extent
            }
        }
        Body::Struct(VariantData::Tuple(..)) |
        Body::Struct(VariantData::Unit) => {
            panic!("Don't know how to implement HasExtent for tuple or unit structs");
        }
    };

    quote! {
        impl ::HasExtent for #name {
            fn extent(&self) -> Extent {
                #body
            }
        }
    }
}

fn impl_extent_index(ast: &syn::MacroInput) -> quote::Tokens {
    let name = &ast.ident;

    quote! {
        impl ::std::ops::Index<#name> for str {
            type Output = str;

            fn index(&self, i: #name) -> &Self::Output {
                let Extent(s, e) = i.extent();
                &self[s..e]
            }
        }

        impl<'a> ::std::ops::Index<&'a #name> for str {
            type Output = str;

            fn index(&self, i: &'a #name) -> &Self::Output {
                let Extent(s, e) = i.extent();
                &self[s..e]
            }
        }
    }
}
