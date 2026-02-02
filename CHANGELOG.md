# Changelog

All notable changes to this project will be documented in this file.

## [unreleased]

- Add macro visibility syntax: `#[bob(pub(crate))]` and `pub(crate) => fn foo(...)`.
- Add `#[bobtail::macro_attrs(...)]` for custom macro attributes.
- Add `pub(self)` to create private macros for public functions.
- Add `#[macro_export]` for `pub` macros.
- Make types optional in `define!`: `fn foo(a, #[tail] b)`.
- Error when visibility is placed after `=>` in `define!`.

## [0.2.0] - 2026-01-11
- Remove default `#[tail]` placement.
- Warn when no `#[tail]` is present.

## [0.1.0] - 2026-01-10

- Initial release.

