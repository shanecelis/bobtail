# tail_optional_macros

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

- `#[tail_omittable::block]`: attach to an `impl` block
- `#[tail_omittable(...)]`: attach to each method you want a macro for

Example:

```rust,ignore
use tail_optional_macros as tail_omittable; // enables #[tail_omittable::block]
use tail_optional_macros::tail_omittable;  // enables #[tail_omittable]

#[tail_omittable::block]
impl Pico8 {
    // default: first non-receiver arg is required; remaining args are tail-omittable
    #[tail_omittable(conv(color(PColor::from)))]
    fn sset(&mut self, pos: (u32,u32), color: Option<PColor>, sheet_index: Option<usize>) -> Result<(), ()> {
        Ok(())
    }
}
```

### Why `#[tail_omittable::conv(...)]` on the parameter isn't supported

Custom namespaced helper attributes on parameters (like `#[tail_omittable::conv(...)]`) are **not stable** in Rust, so the conversion mapping is provided on the method attribute instead via `conv(...)`.
