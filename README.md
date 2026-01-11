# bobtail

Generate macro proxies of functions whose tails can be "bobbed" as in cut off.

<p align="center">
  <img height="300" alt="A bobtail machine" src="https://github.com/user-attachments/assets/68f58156-1308-4bb8-9700-546733d6d18b"><br>
  <em>Figure 1. A bobtail machine.</em>
</p>


This crate produces macro proxies of functions whose trailing arguments may be
omitted or provided with less boilerplate.

## Prototypes

The `define!` macro generates macro proxies for functions and method prototypes.

### Free Functions

```rust
fn f(a: u8, b: Option<u8>) -> u8 {
  b.map(|x| x + a).unwrap_or(a)
}

bobtail::define! {
    fn f(a: u8, #[tail] b: Option<u8>);
}
assert_eq!(f(1, Some(2)), 3);    // Call function.
assert_eq!(f!(1), 1);            // Call macro with omission.
assert_eq!(f!(1, _), 1);         // Call macro with explicit omission.
assert_eq!(f!(1, 2), 3);         // Pass unwrapped second argument.
# assert_eq!(f!(1, Some(2)), 3); // Pass wrapped second argument.
```

### Generated Macro Rules
The `bobtail::define!` produces macro rules that might have looked like this code.
```rust,ignore
macro_rules! f {
    ($a:expr) => {
        f($a, None)
    };
    ($a:expr, $b:expr) => {
        f($a, Some($b))
    };
}
```

But `bobtail::define!` can handle the following case, which the above macro can
not.

``` rust,ignore
assert_eq!(f!(1, Some(2)), 3);   // Pass wrapped second argument.
```

How? Because instead of being restricted to `Option`, an ommitable parameter can
be any type that implements `Default` and `From<T>`. What `bobtail::define!`
actually produces is this:

```rust,ignore
macro_rules! f {
    ($a:expr) => {
        f($a, Default::default())
    };
    ($a:expr, $b:expr) => {
        f($a, From::from($b))
    };
}
```

`From<T>` is not only more flexible, but it permits one to use `Some(2)` above
because there is a blanket implementation for `From<T>` for all `T`, which is an
identity function.

### Methods

Methods with a `&self`, `&mut self`, or `self` expect the receiver as the first
argument to the macro.

```rust
struct A;

impl A {
    fn b(&self, a: u8, b: Option<u8>) -> u8 {
        b.map(|x| x + a).unwrap_or(a)
    }
    fn c(self, a: u8) -> u8 {
        a
    }
}

bobtail::define! {
    fn b(&self, a: u8, #[bobtail::tail] b: Option<u8>) -> u8;
    // Name the macro explicitly.
    c_macro => fn c(self, #[tail] a: u8); // Return type can be omitted.
}
let a = A;

assert_eq!(a.b(1, Some(2)), 3);   // Call function.

assert_eq!(b!(a, 1, Some(2)), 3); // Call macro.
assert_eq!(b!(a, 1, 2), 3);       // Omit `Some`.
assert_eq!(b!(a, 1), 1);          // Omit second argument.
assert_eq!(b!(a, 1, _), 1);       // Explicitly omit second argument.
assert_eq!(c_macro!(a, 4), 4);    // Consume self.

let a = A;
assert_eq!(c_macro!(a), 0);       // Any `Default` will do.
```

## Attributes

One can also generate macro proxies with attributes.

### Free Functions

```rust
#[bobtail::bob]
fn f(a: u8, #[tail] b: Option<u8>) -> u8 {
  b.map(|x| x + a).unwrap_or(a)
}

assert_eq!(f(1, Some(2)), 3);    // Call function.
assert_eq!(f!(1), 1);            // Call macro with omission.
assert_eq!(f!(1, _), 1);         // Call macro with explicit omission.
assert_eq!(f!(1, 2), 3);         // Pass unwrapped second argument.
# assert_eq!(f!(1, Some(2)), 3); // Pass wrapped second argument.
```

### Methods

Methods with a `&self`, `&mut self`, or `self` expect the receiver as the first
argument to the macro proxy.

```rust
struct A;

#[bobtail::block]
impl A {
    #[bobtail::bob]
        fn b(&self, a: u8, #[bobtail::tail] b: Option<u8>) -> u8 {
        b.map(|x| x + a).unwrap_or(a)
    }
    #[bob(c_macro)] // Name the macro explicitly.
    fn c(self, #[tail] a: u8) -> u8 {
        a
    }
}

let a = A;
assert_eq!(a.b(1, Some(2)), 3);   // Call function.

assert_eq!(b!(a, 1, Some(2)), 3); // Call macro.
assert_eq!(b!(a, 1, 2), 3);       // Omit `Some`.
assert_eq!(b!(a, 1), 1);          // Omit second argument.
assert_eq!(b!(a, 1, _), 1);       // Explicitly omit second argument.
assert_eq!(c_macro!(a, 4), 4);    // Consume self.

let a = A;
assert_eq!(c_macro!(a), 0);       // Any `Default` will do.
```
## Motivation and Justification

This crate was inspired by my work on
[Nano-9](https://github.com/shanecelis/nano-9), a Pico-8 compatibility layer for
Bevy. Pico-8's Lua API has many arguments that are often omitted. Consider
Pico-8's text drawing function `print`.

``` lua
-- print(str, [x,] [y,] [color])
print("hello world")
-- No x? No y? No problem.
```

Nano-9 provides the Lua API as-is, but it also provides a Pico-8-like API in
Rust for which the above looks like this:

``` rust,ignore
// print(str, vec2, color, /* Nano-9 extensions: */ font_size, font_index)
pico8.print("hello world", None, None, None, None).unwrap();
```

The aim of this crate is to offer an API on the Rust side that is not so
verbose.

``` rust,ignore
print!(pico8, "hello world").unwrap();
```

### A Caution

This is my reason for creating this crate, but that does not mean I
wholeheartly endorse this kind of positional, omittable, API design. If I were
not constrained by Pico-8's initial design and wanting to bear a strong
resemblance to it, I would consider using structs expressively as named and
omittable arguments potentially using other crates like
[typed-builder](https://crates.io/crates/typed-builder) and
[derive_builder](https://crates.io/crates/derive_builder).

## Install

Add bobtail to a project with the following command:

``` sh
cargo add bobtail
```

## License

This crate is licensed under the MIT License or the Apache License 2.0.
