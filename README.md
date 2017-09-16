# Fuzzy Pickles

While there are many high-quality Rust parsers available, a slightly
different kind of parser was needed to provide the detail needed for
[Strata Rust][]. To that end, Fuzzy Pickles was built using the
[Peresil][] parsing toolkit. The parser aims to:

- be a low-level parser of Rust source code, providing unshackled
  access to the direct parsing structures including items like
  whitespace.
- provide an easier-to-use visitor interface that allows quickly
  gathering information about Rust code.
- provide reasonable quality errors about the parsing.
- parse all syntactically-valid Rust code.

It has anti-goals as well! The parser does not:

- attempt to perform *semantic* analysis of Rust code.
- guarantee to reject all syntactically-invalid code.

While Peresil works with stable Rust, our usage of it requires some
**nightly-only** features.

[Strata Rust]: https://github.com/shepmaster/strata-rust
[Peresil]: https://github.com/shepmaster/peresil

## Contribution opportunities

A project always has need for help from interested people!

### Introductory opportunities 🌟

These are things that anyone should be able to help with!

- Run the parser against your own Rust file, or a particularly
  interesting file you are aware of.
- Narrow down a file that fails to parse to construct a test case.

### Intermediate opportunities 🌟🌟🌟

These might require diving into the code a bit and adding new code or
changing existing code.

- Enhance the parser to recognize code that is currently not recognizable.
- Verify that a piece of code is parsed correctly.

Please open up an issue to begin a dialog before starting if a feature
seems like it will require more than just a straight-forward addition!

### Advanced opportunities 🌟🌟🌟🌟🌟

These are intense feature requests that probably require a good amount
of effort and *definitely* should be discussed in an issue before
beginning.

- Perform macro expansion

## License

fuzzy-pickles is distributed under the terms of both the MIT license and
the Apache License (Version 2.0).

## Authors

This crate was created by Jake Goulding of [Integer 32][].

[Integer 32]: http://www.integer32.com/
