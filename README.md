tracing-record-hierarchical
=======

[![crates.io](https://img.shields.io/crates/v/tracing-record-hierarchical.svg "crates.io")](https://crates.io/crates/tracing-record-hierarchical)
[![Rust 1.56+](https://img.shields.io/badge/rustc-1.56+-lightgray.svg "Rust 1.56+")](https://blog.rust-lang.org/2021/10/21/Rust-1.56.0.html)
[![CI](https://github.com/instrumentisto/tracing-record-hierarchical/workflows/CI/badge.svg?branch=main "CI")](https://github.com/instrumentisto/tracing-record-hierarchical/actions?query=workflow%3ACI+branch%3Amain)
[![Rust docs](https://docs.rs/tracing-record-hierarchical/badge.svg "Rust docs")](https://docs.rs/tracing-record-hierarchical)

[API Docs](https://docs.rs/tracing-record-hierarchical) |
[Changelog](https://github.com/instrumentisto/tracing-record-hierarchical/blob/main/CHANGELOG.md)

Record parent's [`tracing`] span fields from inside child span's context.




## Motivation

Dealing with the complex relationship in the hierarchy of nested [`Span`]s in
`tracing` might become cumbersome. When faced with the need to record new value
for a field of a [`Span`] _higher in the tree_, users have no choice but to
refactor their code in some way to allow that. This includes:

1. Extracting [`record`][`Span::record`] out of the child [`Span`]:
   ```rust
   fn called_from_withing_a_span() {
       let id = 42;
       tracing::Span::current().record("id", id);

       tracing::info_span!("child").in_scope(|| {
           // ...
       })
   }
   ```

   This will not work when:
   - There is another "layer" of spans between them;
   - The value that needs to be recorded is computed in the function (you may
     still be able to work around by returning from `in_scope` closure);
   - The parent [`Span`] in question is in another crate you have no control of.

2. Bringing the parent [`Span`] to the child:
   ```rust
   fn parent() {
       let parent_span = tracing::info_span!(
            "parent",
            id = tracing::field::Empty,
       );
       let _entered = parent_span.enter();
       child(parent_span.clone());
   }

   #[tracing::instrument(skip_all)]
   fn child(parent_span: tracing::Span) {
       let id = 42;
       parent_span.record("id", id);
   }
   ```

   We had to construct `parent` [`Span`] using `*_span!` macro and pass it to
   the child.

Those workarounds are not ergonomic, if can be used at all in other cases.



## Overview

This crate adds a [`HierarchicalRecord`] [`Layer`] and a [`SpanExt`] trait with
the [`record_hierarchical`] method that can be used as a drop-in replacement for
[`Span::record`].


## Usage

Add the [`HierarchicalRecord`] layer to your [subscriber]:

```rust
# use tracing_subscriber::prelude::*;
use tracing_record_hierarchical::HierarchicalRecord;

fn init_tracing() {
    tracing_subscriber::registry()
        .with(HierarchicalRecord::new())
        .init();
}
```

When you were to use [`Span::record`] to record a value to a parent span's
field, do [`record_hierarchical`], or a panicky version,
[`must_record_hierarchical`], instead:

```rust
use tracing_record_hierarchical::SpanExt as _;

#[tracing::instrument(fields(my_field = tracing::field::Empty))]
fn foo() {
    bar();
}

#[tracing::instrument]
fn bar() {
    tracing::Span::current()
        // This will walk the chain of spans from the span the method was called
        // on (`current` in this example) to the "root" span. If some span in
        // the chain has the field `my_field`, the value would be recorded 
        // there. If none of the spans from firs to "root" have this field,
        // panic will occur.
        .must_record_hierarchical("my_field", 42);
}

# fn main() {
#     use tracing_subscriber::prelude::*;
#     use tracing_record_hierarchical::HierarchicalRecord;
# 
#     tracing_subscriber::registry().with(HierarchicalRecord::new()).init();
# 
#     foo();
# }
```




## License

Copyright © 2022-2023 Instrumentisto Team, <https://github.com/instrumentisto>

Licensed under either of [Apache License, Version 2.0][APACHE] or [MIT license][MIT] at your option.

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in this crate by you, as defined in the [Apache-2.0 license][APACHE], shall be dual licensed as above, without any additional terms or conditions.




[`HierarchicalRecord`]: https://docs.rs/tracing-record-hierarchical/latest/tracing_record_hierarchical/struct.HierarchicalRecord.html
[`Layer`]: https://docs.rs/tracing-subscriber/latest/tracing_subscriber/layer/trait.Layer.html
[`must_record_hierarchical`]: https://docs.rs/tracing-record-hierarchical/latest/tracing_record_hierarchical/trait.SpanExt.html#tymethod.must_record_hierarchical
[`record_hierarchical`]: https://docs.rs/tracing-record-hierarchical/latest/tracing_record_hierarchical/trait.SpanExt.html#tymethod.record_hierarchical
[`Span::record`]: https://docs.rs/tracing/latest/tracing/struct.Span.html#method.record
[`Span`]: https://docs.rs/tracing/latest/tracing/struct.Span.html
[`SpanExt`]: https://docs.rs/tracing-record-hierarchical/latest/tracing_record_hierarchical/trait.SpanExt.html
[`tracing`]: https://docs.rs/tracing
[subscriber]: https://docs.rs/tracing/latest/tracing/#subscribers

[APACHE]: https://github.com/instrumentisto/tracing-record-hierarchical/blob/main/LICENSE-APACHE
[MIT]: https://github.com/instrumentisto/tracing-record-hierarchical/blob/main/LICENSE-MIT
