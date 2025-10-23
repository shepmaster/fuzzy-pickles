#![recursion_limit="128"]

extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
#[macro_use]
extern crate quote;

use std::iter;

use proc_macro2::{TokenStream, Span};

#[proc_macro_derive(Visit, attributes(visit))]
pub fn visit_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    let gen = impl_visit(&ast);
    gen.into()
}

#[proc_macro_derive(HasExtent)]
pub fn has_extent_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    let gen = impl_has_extent(&ast);
    gen.into()
}

#[proc_macro_derive(ExtentIndex)]
pub fn extent_index_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    let gen = impl_extent_index(&ast);
    gen.into()
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

fn impl_visit(ast: &syn::DeriveInput) -> TokenStream {
    use syn::Ident;

    let name = &ast.ident;
    let method_name_base = camelcase_to_snake_case(&name.to_string());
    let method_name = Ident::new(&format!("visit{}", method_name_base), Span::call_site());
    let exit_method_name = Ident::new(&format!("exit{}", method_name_base), Span::call_site());

    let visit_fields = impl_visit_fields(ast, IsMut(false));
    let visit_fields_mut = impl_visit_fields(ast, IsMut(true));

    quote! {
        impl crate::visit::Visit for #name {
            fn visit<'ast, V>(&'ast self, v: &mut V)
            where
                V: crate::visit::Visitor<'ast>,
            {
                if crate::visit::Control::Continue == v.#method_name(self) {
                    #visit_fields;
                }
                v.#exit_method_name(self);
            }

            fn visit_mut<V>(&mut self, v: &mut V)
            where
                V: crate::visit::VisitorMut,
            {
                if crate::visit::Control::Continue == v.#method_name(self) {
                    #visit_fields_mut;
                }
                v.#exit_method_name(self);
            }
        }
    }
}

struct IsMut(bool);

fn impl_visit_fields(ast: &syn::DeriveInput, IsMut(is_mut): IsMut) -> TokenStream {
    use syn::{Ident, Data, Fields};

    let method = if is_mut { "visit_mut" } else { "visit" };
    let method = Ident::new(method, Span::call_site());
    let method = iter::repeat(quote! { crate::visit::Visit::#method });

    fn field_names<'a>(fields: impl IntoIterator<Item = &'a syn::Field>) -> Vec<Ident> {
        fields
            .into_iter()
            .enumerate()
            .filter(|(_, f)| !is_ignore_field(f))
            .map(|(i, f)| f.ident.clone().unwrap_or_else(|| Ident::new(&i.to_string(), Span::call_site())))
            .collect()
    }

    match ast.data {
        Data::Enum(ref e) => {
            let enum_name = iter::repeat(&ast.ident);
            let variant_names = e.variants.iter().map(|variant| &variant.ident);

            if is_mut {
                quote! {
                    match *self {
                        #(#enum_name::#variant_names(ref mut x) => #method(x, v),)*
                    }
                }
            } else {
                quote! {
                    match *self {
                        #(#enum_name::#variant_names(ref x) => #method(x, v),)*
                    }
                }
            }
        }
        Data::Struct(ref s) => {
            let field_names: Vec<_> = match s.fields {
                Fields::Named(ref fields) => field_names(&fields.named),
                Fields::Unnamed(ref fields) => field_names(&fields.unnamed),
                Fields::Unit => vec![],
            };

            if is_mut {
                quote! {
                    #(#method(&mut self.#field_names, v);)*
                }
            } else {
                quote! {
                    #(#method(&self.#field_names, v);)*
                }
            }
        }
        Data::Union(..) => panic!("Unions are not supported"),
    }
}

fn is_ignore_field(field: &syn::Field) -> bool {
    field.attrs.iter().any(|attr| {
        if !attr.path().is_ident("visit") {
            return false;
        }

        let mut is_ignore = false;

        attr.parse_nested_meta(|meta| {
            is_ignore = meta.path.is_ident("ignore");

            Ok(())
        }).expect("Unknown attribute structure");

        is_ignore
    })
}

#[proc_macro_derive(Decompose)]
pub fn decompose_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    let gen = impl_decompose(&ast);
    gen.into()
}

fn impl_decompose(ast: &syn::DeriveInput) -> TokenStream {
    use syn::{Ident, Data, Fields, Type};

    let name = &ast.ident;

    let e = match ast.data {
        Data::Enum(ref e) => e,
        _ => panic!("Can only decompose enums"),
    };

    let enum_name = &ast.ident;

    struct Info<'a> {
        variant_name: &'a Ident,
        variant_snake_name: String,
        variant_type: &'a Type,
    }

    let enum_info: Vec<_> = e.variants.iter().map(|variant| {
        let fields = match variant.fields {
            Fields::Unnamed(ref fields) => fields,
            _ => panic!("Can only decompose tuple variants"),
        };

        let field = match fields.unnamed.len() {
            1 => &fields.unnamed[0],
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
        let method_name = Ident::new(&format!("into{}", variant_snake_name), Span::call_site());

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
        let method_name = Ident::new(&format!("as{}", variant_snake_name), Span::call_site());

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
        let method_name = Ident::new(&format!("is{}", variant_snake_name), Span::call_site());

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

fn impl_has_extent(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;

    use syn::{Data, Fields};

    let body = match ast.data {
        Data::Enum(ref e) => {
            let enum_name = iter::repeat(&ast.ident);
            let variant_names = e.variants.iter().map(|variant| &variant.ident);

            quote! {
                match *self {
                    #(#enum_name::#variant_names(ref x) => crate::HasExtent::extent(x),)*
                }
            }
        }
        Data::Struct(ref s) => {
            match s.fields {
                Fields::Named(..) => {
                    quote! {
                        self.extent
                    }
                }
                Fields::Unnamed(..) | Fields::Unit => {
                    panic!("Don't know how to implement HasExtent for tuple or unit structs");
                }
            }
        }
        Data::Union(..) => panic!("Don't know how to implement HasExtent for unions"),
    };

    quote! {
        impl crate::HasExtent for #name {
            fn extent(&self) -> Extent {
                #body
            }
        }
    }
}

fn impl_extent_index(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;

    quote! {
        impl std::ops::Index<#name> for str {
            type Output = str;

            fn index(&self, i: #name) -> &Self::Output {
                let Extent(s, e) = i.extent();
                &self[s..e]
            }
        }

        impl<'a> std::ops::Index<&'a #name> for str {
            type Output = str;

            fn index(&self, i: &'a #name) -> &Self::Output {
                let Extent(s, e) = i.extent();
                &self[s..e]
            }
        }
    }
}
