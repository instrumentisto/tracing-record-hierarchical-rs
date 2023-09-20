// These links overwrite the ones in `README.md`
// to become proper intra-doc links in Rust docs.
//! [`HierarchicalRecord`]: HierarchicalRecord
//! [`Layer`]: Layer
//! [`must_record_hierarchical()`]: SpanExt::must_record_hierarchical
//! [`record_hierarchical()`]: SpanExt::record_hierarchical
//! [`Span::record`]: Span::record()
//! [`Span`]: tracing::Span
//! [`SpanExt`]: SpanExt
//! [`tracing`]: tracing
//! [`tracing::Span`]: tracing::Span
//! [subscriber]: tracing#subscribers
#![doc = include_str!("../README.md")]
#![deny(
    macro_use_extern_crate,
    nonstandard_style,
    rust_2018_idioms,
    rustdoc::all,
    trivial_casts,
    trivial_numeric_casts,
    unsafe_code
)]
#![forbid(non_ascii_idents)]
#![warn(
    clippy::as_conversions,
    clippy::as_ptr_cast_mut,
    clippy::assertions_on_result_states,
    clippy::branches_sharing_code,
    clippy::clear_with_drain,
    clippy::clone_on_ref_ptr,
    clippy::collection_is_never_read,
    clippy::create_dir,
    clippy::dbg_macro,
    clippy::debug_assert_with_mut_call,
    clippy::decimal_literal_representation,
    clippy::default_union_representation,
    clippy::derive_partial_eq_without_eq,
    clippy::else_if_without_else,
    clippy::empty_drop,
    clippy::empty_line_after_outer_attr,
    clippy::empty_structs_with_brackets,
    clippy::equatable_if_let,
    clippy::exit,
    clippy::fallible_impl_from,
    clippy::filetype_is_file,
    clippy::float_cmp_const,
    clippy::fn_to_numeric_cast,
    clippy::fn_to_numeric_cast_any,
    clippy::format_push_string,
    clippy::get_unwrap,
    clippy::if_then_some_else_none,
    clippy::imprecise_flops,
    clippy::index_refutable_slice,
    clippy::iter_on_empty_collections,
    clippy::iter_on_single_items,
    clippy::iter_with_drain,
    clippy::large_include_file,
    clippy::large_stack_frames,
    clippy::let_underscore_untyped,
    clippy::lossy_float_literal,
    clippy::manual_clamp,
    clippy::map_err_ignore,
    clippy::mem_forget,
    clippy::missing_assert_message,
    clippy::missing_const_for_fn,
    clippy::missing_docs_in_private_items,
    clippy::multiple_inherent_impl,
    clippy::multiple_unsafe_ops_per_block,
    clippy::mutex_atomic,
    clippy::mutex_integer,
    clippy::needless_collect,
    clippy::needless_raw_strings,
    clippy::nonstandard_macro_braces,
    clippy::option_if_let_else,
    clippy::or_fun_call,
    clippy::panic_in_result_fn,
    clippy::partial_pub_fields,
    clippy::pedantic,
    clippy::print_stderr,
    clippy::print_stdout,
    clippy::pub_without_shorthand,
    clippy::rc_buffer,
    clippy::rc_mutex,
    clippy::redundant_clone,
    clippy::redundant_type_annotations,
    clippy::ref_patterns,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::same_name_method,
    clippy::semicolon_inside_block,
    clippy::significant_drop_in_scrutinee,
    clippy::significant_drop_tightening,
    clippy::str_to_string,
    clippy::string_add,
    clippy::string_lit_as_bytes,
    clippy::string_slice,
    clippy::string_to_string,
    clippy::suboptimal_flops,
    clippy::suspicious_operation_groupings,
    clippy::suspicious_xor_used_as_pow,
    clippy::tests_outside_test_module,
    clippy::todo,
    clippy::trailing_empty_array,
    clippy::transmute_undefined_repr,
    clippy::trivial_regex,
    clippy::try_err,
    clippy::tuple_array_conversions,
    clippy::undocumented_unsafe_blocks,
    clippy::unimplemented,
    clippy::unnecessary_safety_comment,
    clippy::unnecessary_safety_doc,
    clippy::unnecessary_self_imports,
    clippy::unnecessary_struct_initialization,
    clippy::unneeded_field_pattern,
    clippy::unused_peekable,
    clippy::unwrap_in_result,
    clippy::unwrap_used,
    clippy::use_debug,
    clippy::use_self,
    clippy::useless_let_if_seq,
    clippy::verbose_file_reads,
    clippy::wildcard_enum_match_arm,
    future_incompatible,
    invalid_reference_casting,
    let_underscore_drop,
    meta_variable_misuse,
    missing_copy_implementations,
    missing_debug_implementations,
    missing_docs,
    noop_method_call,
    semicolon_in_expressions_from_macros,
    unreachable_pub,
    unused_crate_dependencies,
    unused_extern_crates,
    unused_import_braces,
    unused_labels,
    unused_lifetimes,
    unused_qualifications,
    unused_results,
    unused_tuple_struct_fields,
    variant_size_differences
)]

use std::{any::TypeId, fmt::Display, marker::PhantomData, ptr};

use sealed::sealed;
use tracing::{self as log, field, span, Dispatch, Metadata, Span, Subscriber};
use tracing_subscriber::{registry::LookupSpan, Layer, Registry};

/// Extension of a [`tracing::Span`] providing more ergonomic handling of
/// [`tracing::Span::record`]s.
#[sealed]
pub trait SpanExt {
    /// Same as [`tracing::Span::record()`], but ensures that the provided
    /// `field`'s `value` will be written into the first [`tracing::Span`] with
    /// this `field` up by hierarchy.
    fn record_hierarchical<Q, V>(&self, field: &Q, value: V) -> &Self
    where
        Q: field::AsField + Display + ?Sized,
        V: field::Value;

    /// Same as [`SpanExt::record_hierarchical()`], but panics.
    ///
    /// # Panics
    ///
    /// In case none of [`tracing::Span`]s in the hierarchy has the provided
    /// `field`.
    fn must_record_hierarchical<Q, V>(&self, field: &Q, value: V) -> &Self
    where
        Q: field::AsField + Display + ?Sized,
        V: field::Value;
}

#[sealed]
impl SpanExt for Span {
    fn record_hierarchical<Q, V>(&self, field: &Q, value: V) -> &Self
    where
        Q: field::AsField + Display + ?Sized,
        V: field::Value,
    {
        record(self, field, value, false);
        self
    }

    fn must_record_hierarchical<Q, V>(&self, field: &Q, value: V) -> &Self
    where
        Q: field::AsField + Display + ?Sized,
        V: field::Value,
    {
        record(self, field, value, true);
        self
    }
}

/// Records the provided `value` to the `field` of the provided [`Span`] if it
/// has the one, otherwise tries to record it to its parent [`Span`].
fn record<Q, V>(span: &Span, field: &Q, value: V, panic: bool)
where
    Q: field::AsField + Display + ?Sized,
    V: field::Value,
{
    if span.has_field(field) {
        _ = span.record(field, value);
    } else {
        record_parent(span, field, value, panic);
    }
}

/// Walks up the parents' hierarchy of the provided [`Span`] and tries to record
/// the provided `value` into the `field` of the first [`Span`] having it, or
/// the "root" [`Span`] is reached.
fn record_parent<Q, V>(span: &Span, field: &Q, value: V, panic: bool)
where
    Q: field::AsField + Display + ?Sized,
    V: field::Value,
{
    _ = span.with_subscriber(|(id, dispatch)| {
        let reg = dispatch
            .downcast_ref::<Registry>()
            .expect("`tracing::Subscriber` not found");
        let ctx = dispatch.downcast_ref::<WithContext>().expect(
            "add `HierarchicalRecord` `Layer` to your `tracing::Subscriber`",
        );

        if let Some(span) = reg.span(id) {
            let parent = span.scope().find_map(|parent| {
                ctx.record_hierarchical(
                    dispatch,
                    &parent.id(),
                    &|parent_id: span::Id, meta: Meta| {
                        field
                            .as_field(meta)
                            .map(|field| (parent_id, meta, field))
                    },
                    &|| field.to_string(),
                    panic,
                )
            });

            if let Some((id, meta, field)) = parent {
                let value: &dyn field::Value = &value;
                dispatch.record(
                    &id,
                    &span::Record::new(
                        &meta.fields().value_set(&[(&field, Some(value))]),
                    ),
                );
            }
        }
    });
}

/// Shortcut for a [`tracing::Span`]'s `'static` [`Metadata`].
type Meta = &'static Metadata<'static>;

/// Shortcut for [`HierarchicalRecord::with_context`]'s callback function
/// signature.
type WithContextCallback<'f> =
    &'f dyn Fn(span::Id, Meta) -> Option<(span::Id, Meta, field::Field)>;

/// Shortcut for a [`HierarchicalRecord::with_context`] method signature.
type WithContextFn = fn(
    dispatch: &Dispatch,
    id: &span::Id,
    f: WithContextCallback<'_>,
    field: &dyn Fn() -> String,
    panic: bool,
) -> Option<(span::Id, Meta, field::Field)>;

/// Type-erased [`Layer`]'s context.
///
/// This function "remembers" the type of the subscriber, so that we can
/// downcast to something aware of them without knowing those types at the
/// call-site.
#[derive(Debug)]
struct WithContext(WithContextFn);

impl WithContext {
    /// Allows a function to be called in the context of the "remembered"
    /// subscriber.
    fn record_hierarchical(
        &self,
        dispatch: &Dispatch,
        id: &span::Id,
        f: WithContextCallback<'_>,
        field: &dyn Fn() -> String,
        panic: bool,
    ) -> Option<(span::Id, Meta, field::Field)> {
        (self.0)(dispatch, id, f, field, panic)
    }
}

/// [`Layer`] that helps [`field`]s find their corresponding [`Span`] in the
/// hierarchy of [`Span`]s.
#[derive(Debug)]
pub struct HierarchicalRecord<S> {
    /// Type-erased [`Layer`]'s context.
    ctx: WithContext,

    /// [`Subscriber`]'s type.
    _subscriber: PhantomData<fn(S)>,
}

impl<S> Default for HierarchicalRecord<S>
where
    S: for<'span> LookupSpan<'span> + Subscriber,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<S> HierarchicalRecord<S>
where
    S: for<'span> LookupSpan<'span> + Subscriber,
{
    /// Creates a new [`HierarchicalRecord`].
    #[must_use]
    pub fn new() -> Self {
        Self { ctx: WithContext(Self::with_context), _subscriber: PhantomData }
    }

    /// Function saved and called by a [`WithContext`] at the call-site.
    #[allow(clippy::unwrap_in_result)]
    fn with_context(
        dispatch: &Dispatch,
        id: &span::Id,
        f: WithContextCallback<'_>,
        field: &dyn Fn() -> String,
        do_panic: bool,
    ) -> Option<(span::Id, Meta, field::Field)> {
        let subscriber = dispatch
            .downcast_ref::<S>()
            .expect("`tracing::Subscriber` not found");
        let span = subscriber.span(id).expect("unknown `tracing::Span`");

        #[allow(clippy::option_if_let_else)]
        if let Some(parent) = span.parent() {
            f(parent.id(), parent.metadata())
        } else {
            // `Span` wants to record a field but has no parents. This means
            // that we walked the entire hierarchy of `Span`s to the root, yet
            // this field did not find it's corresponding `Span`. We know that,
            // because otherwise the iteration in `record_parent()` would end,
            // and this function would not be called again anymore, yet it is.
            // Nothing to do, but report the error.

            let meta = span.metadata();
            let field = field();

            // We log and then panic to avoid situation, when we get double
            // panic without any info.
            log::error!(
                "`Span(id={id:?}, meta={meta:?})` doesn't have `{field}` field"
            );
            do_panic.then(|| {
                panic!(
                    "`Span(id={id:?}, meta={meta:?})` doesn't have `{field}` \
                     field",
                )
            })
        }
    }
}

impl<S> Layer<S> for HierarchicalRecord<S>
where
    S: for<'span> LookupSpan<'span> + Subscriber,
{
    // SAFETY: This is safe because the `WithContext` function pointer is valid
    //         for the lifetime of `&self`.
    #[allow(unsafe_code)]
    unsafe fn downcast_raw(&self, id: TypeId) -> Option<*const ()> {
        match id {
            id if id == TypeId::of::<Self>() => {
                Some(ptr::addr_of!(self).cast::<()>())
            }
            id if id == TypeId::of::<WithContext>() => {
                Some(ptr::addr_of!(self.ctx).cast::<()>())
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod spec {
    use super::*;

    fn with_subscriber(f: impl FnOnce()) {
        use tracing_subscriber::layer::SubscriberExt as _;

        tracing::subscriber::with_default(
            tracing_subscriber::registry().with(HierarchicalRecord::new()),
            f,
        );
    }

    #[test]
    fn does_nothing_if_not_in_span() {
        with_subscriber(|| {
            _ = Span::current().must_record_hierarchical("field", "value");
        });
    }

    #[test]
    fn records_into_parent_span() {
        with_subscriber(|| {
            tracing::info_span!("parent", field = field::Empty).in_scope(
                || {
                    tracing::info_span!("child").in_scope(|| {
                        _ = Span::current()
                            .record_hierarchical("field", "value");
                    });
                },
            );
        });
    }

    #[test]
    fn must_records_into_parent_span() {
        with_subscriber(|| {
            tracing::info_span!("parent", field = field::Empty).in_scope(
                || {
                    tracing::info_span!("child").in_scope(|| {
                        _ = Span::current()
                            .must_record_hierarchical("field", "value");
                    });
                },
            );
        });
    }

    #[test]
    fn must_records_into_toplevel_parent_span() {
        with_subscriber(|| {
            tracing::info_span!("grand-grandparent", field = field::Empty)
                .in_scope(|| {
                    tracing::info_span!("grandparent").in_scope(|| {
                        tracing::info_span!("parent").in_scope(|| {
                            tracing::info_span!("child").in_scope(|| {
                                _ = Span::current()
                                    .must_record_hierarchical("field", "value");
                            });
                        });
                    });
                });
        });
    }

    #[test]
    fn must_records_into_intermediate_parent_span() {
        with_subscriber(|| {
            tracing::info_span!("grand-grandparent").in_scope(|| {
                tracing::info_span!("grandparent", field = field::Empty)
                    .in_scope(|| {
                        tracing::info_span!("parent").in_scope(|| {
                            tracing::info_span!("child").in_scope(|| {
                                _ = Span::current()
                                    .must_record_hierarchical("field", "value");
                            });
                        });
                    });
            });
        });
    }

    #[test]
    fn no_panic_on_missing_field() {
        with_subscriber(|| {
            tracing::info_span!("parent", abc = field::Empty).in_scope(|| {
                tracing::info_span!("child").in_scope(|| {
                    _ = Span::current().record_hierarchical("field", "value");
                });
            });
        });
    }

    #[test]
    #[should_panic = "doesn't have `field` field"]
    fn must_panics_on_missing_field() {
        with_subscriber(|| {
            tracing::info_span!("parent", abc = field::Empty).in_scope(|| {
                tracing::info_span!("child").in_scope(|| {
                    _ = Span::current()
                        .must_record_hierarchical("field", "value");
                });
            });
        });
    }

    #[test]
    #[should_panic = "add `HierarchicalRecord` `Layer` to your \
                      `tracing::Subscriber`"]
    fn panics_when_no_layer() {
        let subscriber = tracing_subscriber::registry();

        tracing::subscriber::with_default(subscriber, || {
            tracing::info_span!("parent", abc = field::Empty).in_scope(|| {
                _ = Span::current().must_record_hierarchical("field", "value");
            });
        });
    }
}
