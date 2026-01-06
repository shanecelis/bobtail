# bobtail

`macro_rules!` generator for methods with trailing `Option<T>` parameters.

## Tests

- Unit tests: `cargo test`
- Compile-fail tests (trybuild): `cargo test` (runs automatically)

## Example

```bash
cargo run --example demo
```

## Prototypes

You can provide method and free-function prototypes that will generate a macro for each using `tail_define!`.

``` rust,ignore
 /// This expression defines three macros: sset_macro!, sspr!, and prnt!
tail_define!(
    sset_macro/* macro name */ => /* function prototype */ fn sset(
        &mut self,
        pos: (u32, u32),
        #[tail]
        color: Option<PColor>,
        sheet_index: Option<usize>,
    ) -> Result<(), ()>;
    // macro name can be omitted, in which case it is the same as the function name.
    /* function prototype */ fn sspr(
        &mut self,
        pos: (u32, u32),
        #[tail]
        color: Option<PColor>,
    ) -> Result<(), ()>;

    /* free-function function prototype */ fn prnt(
        pos: (u32, u32),
        #[tail]
        color: Option<PColor>,
    ) /* missing an explicit return */;
)
```

## Attributes (proc-macro)

This crate includes optional proc-macro attributes (via a workspace member) to generate the `macro_rules!` wrappers for you.

- `#[bobtail::block]`: attach to an `impl` block
- `#[bobtail::bob]`: attach to each method you want a macro for (optional rename: `#[bobtail::bob(my_macro)]`)
- `#[bobtail::tail]`: attach to the first tail-omittable parameter

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
    
    #[bob]
    fn hi(#[tail] x: Option<u8>) {
       // Associated functions also work.
    }
}
#[bobtail::bob]
fn free(x: u8, #[tail] y: Option<u8>) {
  // Free functions also work.
}
```

### Note
These `#[bobtail::tail]` parameter markers are *consumed and stripped* by `#[bobtail::block]` during macro expansion.
