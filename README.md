# bobtail

`macro_rules!` generator for methods with trailing `Option<T>` parameters.

## Tests

- Unit tests: `cargo test`
- Compile-fail tests (trybuild): `cargo test` (runs automatically)

## Example

```bash
cargo run --example demo
```

## Attributes (proc-macro)

This crate includes optional proc-macro attributes (via a workspace member) to generate the `macro_rules!` wrappers for you.

- `#[bobtail::block]`: attach to an `impl` block
- `#[bobtail::bob]`: attach to each method you want a macro for (optional rename: `#[bobtail::bob(my_macro)]`)
- `#[bobtail::tail]`: attach to the first tail-omittable parameter
- `#[bobtail::map(path::to::conv)]`: optional conversion hook for a parameter (only meaningful on tail params)

Example:

```rust,ignore
use bobtail;

#[bobtail::block]
impl Pico8 {
    // default: first non-receiver arg is required; remaining args are tail-omittable
    #[bobtail::bob]
    fn sset(&mut self, pos: (u32,u32), #[bobtail::tail] #[bobtail::map(PColor::from)] color: Option<PColor>, sheet_index: Option<usize>) -> Result<(), ()> {
        Ok(())
    }
}
```

### Note
These `#[bobtail::tail]` / `#[bobtail::map(...)]` parameter markers are *consumed and stripped* by `#[bobtail::block]` during macro expansion.
