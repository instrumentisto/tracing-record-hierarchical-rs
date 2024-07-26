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
    trivial_numeric_casts
)]
#![forbid(non_ascii_idents, unsafe_code)]
#![warn(
    clippy::absolute_paths,
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
    clippy::empty_enum_variants_with_brackets,
    clippy::exit,
    clippy::expect_used,
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
    clippy::infinite_loop,
    clippy::iter_on_empty_collections,
    clippy::iter_on_single_items,
    clippy::iter_over_hash_type,
    clippy::iter_with_drain,
    clippy::large_include_file,
    clippy::large_stack_frames,
    clippy::let_underscore_untyped,
    clippy::lossy_float_literal,
    clippy::manual_c_str_literals,
    clippy::map_err_ignore,
    clippy::mem_forget,
    clippy::missing_assert_message,
    clippy::missing_asserts_for_indexing,
    clippy::missing_const_for_fn,
    clippy::missing_docs_in_private_items,
    clippy::multiple_inherent_impl,
    clippy::multiple_unsafe_ops_per_block,
    clippy::mutex_atomic,
    clippy::mutex_integer,
    clippy::needless_collect,
    clippy::needless_pass_by_ref_mut,
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
    clippy::ref_as_ptr,
    clippy::rc_buffer,
    clippy::rc_mutex,
    clippy::read_zero_byte_vec,
    clippy::redundant_clone,
    clippy::redundant_type_annotations,
    clippy::renamed_function_params,
    clippy::ref_patterns,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::same_name_method,
    clippy::semicolon_inside_block,
    clippy::shadow_unrelated,
    clippy::significant_drop_in_scrutinee,
    clippy::significant_drop_tightening,
    clippy::str_to_string,
    clippy::string_add,
    clippy::string_lit_as_bytes,
    clippy::string_lit_chars_any,
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
    clippy::undocumented_unsafe_blocks,
    clippy::unimplemented,
    clippy::uninhabited_references,
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
    clippy::while_float,
    clippy::wildcard_enum_match_arm,
    explicit_outlives_requirements,
    future_incompatible,
    let_underscore_drop,
    meta_variable_misuse,
    missing_abi,
    missing_copy_implementations,
    missing_debug_implementations,
    missing_docs,
    redundant_lifetimes,
    semicolon_in_expressions_from_macros,
    single_use_lifetimes,
    unit_bindings,
    unnameable_types,
    unreachable_pub,
    unsafe_op_in_unsafe_fn,
    unstable_features,
    unused_crate_dependencies,
    unused_extern_crates,
    unused_import_braces,
    unused_lifetimes,
    unused_macro_rules,
    unused_qualifications,
    unused_results,
    variant_size_differences
)]

/// For surviving MSRV check only.
mod unused_deps {
    use lazy_static as _;
}

use std::fmt::Display;

use sealed::sealed;
use tracing::{self as log, field, span, Dispatch, Metadata, Span, Subscriber};
use tracing_subscriber::{registry::LookupSpan, Layer};

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
fn record<Q, V>(span: &Span, field: &Q, value: V, do_panic: bool)
where
    Q: field::AsField + Display + ?Sized,
    V: field::Value,
{
    if span.has_field(field) {
        _ = span.record(field, value);
    } else {
        record_parent(span, field, value, do_panic);
    }
}

/// Walks up the parents' hierarchy of the provided [`Span`] and tries to record
/// the provided `value` into the `field` of the first [`Span`] having it, or
/// the "root" [`Span`] is reached.
fn record_parent<Q, V>(span: &Span, field: &Q, value: V, do_panic: bool)
where
    Q: field::AsField + Display + ?Sized,
    V: field::Value,
{
    _ = span.with_subscriber(|(id, dispatch)| {
        #[allow(clippy::expect_used)] // intentional
        let ctx = dispatch.downcast_ref::<HierarchicalRecord>().expect(
            "add `HierarchicalRecord` `Layer` to your `tracing::Subscriber`",
        );

        if let Some((id, meta)) = ctx.with_context(
            dispatch,
            id,
            &|meta: Meta| field.as_field(meta),
            &|span_id, meta, field_name| {
                let value: &dyn field::Value = &value;
                dispatch.record(
                    span_id,
                    &span::Record::new(
                        &meta.fields().value_set(&[(&field_name, Some(value))]),
                    ),
                );
            },
        ) {
            // `Span` wants to record a field that has no corresponding parent.
            // This means that we walked the entire hierarchy of `Span`s to the
            // root, yet this field did not find it's corresponding `Span`. We
            // know that, because otherwise the iteration in `record_parent()`
            // would end, and this function would not be called again anymore,
            // yet it is. Nothing to do, but report the error.

            log::error!(
                "`Span(id={id:?}, meta={meta:?})` doesn't have `{field}` field"
            );
            assert!(
                !do_panic,
                "`Span(id={id:?}, meta={meta:?})` doesn't have `{field}` field"
            );
        };
    });
}

/// Shortcut for a [`tracing::Span`]'s `'static` [`Metadata`].
type Meta = &'static Metadata<'static>;

/// Shortcut for a [`HierarchicalRecord::with_context`] method signature.
type WithContextFn = fn(
    dispatch: &Dispatch,
    id: &span::Id,
    find_field: &dyn Fn(Meta) -> Option<field::Field>,
    record: &dyn Fn(&span::Id, Meta, field::Field),
) -> Option<(span::Id, Meta)>;

/// [`Layer`] helping [`field`]s to find their corresponding [`Span`] in the
/// hierarchy of [`Span`]s.
#[derive(Clone, Copy, Debug, Default)]
pub struct HierarchicalRecord {
    /// This function "remembers" the type of the subscriber, so that we can do
    /// something aware of them without knowing those types at the call-site.
    with_context: Option<WithContextFn>,
}

impl HierarchicalRecord {
    /// Allows a function to be called in the context of the "remembered"
    /// subscriber.
    fn with_context(
        self,
        dispatch: &Dispatch,
        id: &span::Id,
        find_field: &dyn Fn(Meta) -> Option<field::Field>,
        record: &dyn Fn(&span::Id, Meta, field::Field),
    ) -> Option<(span::Id, Meta)> {
        (self.with_context?)(dispatch, id, find_field, record)
    }
}

impl<S> Layer<S> for HierarchicalRecord
where
    S: for<'span> LookupSpan<'span> + Subscriber,
{
    fn on_layer(&mut self, _: &mut S) {
        self.with_context = Some(|dispatch, id, find_field, record| {
            let subscriber = dispatch.downcast_ref::<S>()?;
            let span = subscriber.span(id)?;

            let parent = span.parent().and_then(|parent| {
                parent.scope().find_map(|s| {
                    let meta = s.metadata();
                    let field = find_field(meta)?;
                    Some((s.id(), meta, field))
                })
            });

            parent.map_or_else(
                || Some((span.id(), span.metadata())),
                |(parent_id, parent_meta, parent_field)| {
                    record(&parent_id, parent_meta, parent_field);
                    None
                },
            )
        });
    }
}

#[cfg(test)]
mod spec {
    use std::{collections::HashMap, fmt::Debug};

    use tracing::{
        field::{self, Visit},
        span, Dispatch, Span, Subscriber,
    };
    use tracing_subscriber::{layer, registry::LookupSpan, Layer};

    use super::{HierarchicalRecord, SpanExt as _};

    #[test]
    fn does_nothing_if_not_in_span() {
        with_subscriber(|| {
            _ = Span::current().must_record_hierarchical("field", "value");

            assert_eq!(try_current("field").as_deref(), None);
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

                        assert_eq!(
                            try_current("field").as_deref(),
                            Some("value"),
                        );
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

                        assert_eq!(
                            try_current("field").as_deref(),
                            Some("value"),
                        );
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

                                assert_eq!(
                                    try_current("field").as_deref(),
                                    Some("value"),
                                );
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

                                assert_eq!(
                                    try_current("field").as_deref(),
                                    Some("value"),
                                );
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

                    assert_eq!(try_current("field").as_deref(), None);
                    assert_eq!(try_current("abc").as_deref(), None);
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
    #[should_panic = "doesn't have `field` field"]
    fn must_panics_on_missing_field_and_no_parents() {
        with_subscriber(|| {
            tracing::info_span!("child").in_scope(|| {
                _ = Span::current().must_record_hierarchical("field", "value");
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

    /// Wraps the provided function into a [`Subscriber`] for tests.
    fn with_subscriber(f: impl FnOnce()) {
        use tracing_subscriber::layer::SubscriberExt as _;

        tracing::subscriber::with_default(
            tracing_subscriber::registry()
                .with(HierarchicalRecord::default())
                .with(FieldValueRecorder {
                    current_field_value: None,
                    lookup: &["field"],
                }),
            f,
        );
    }

    /// Tries to extract the specified field's value from the current [`Span`].
    fn try_current(name: &'static str) -> Option<String> {
        Span::current()
            .with_subscriber(|(id, dispatch)| {
                dispatch
                    .downcast_ref::<FieldValueRecorder>()?
                    .current_field_value(dispatch, id, name)
            })
            .flatten()
    }

    /// Shortcut for a [`FieldValueRecorder::current_field_value()`] method
    /// signature.
    type CurrentFieldValueFn = fn(
        dispatch: &Dispatch,
        id: &span::Id,
        key: &'static str,
    ) -> Option<String>;

    /// Shortcut for a list of field names to lookup from a [`Span`].
    type Lookup = &'static [&'static str];

    /// Helper [`Layer`] for field recording, which records [`Span`] fields in
    /// [`Extensions`] to be retrieved back using the [`try_current()`]
    /// function.
    ///
    /// [`Extensions`]: tracing_subscriber::registry::Extensions
    #[derive(Clone, Copy, Debug)]
    struct FieldValueRecorder {
        /// Function remembering the type of the subscriber, allowing us to do
        /// something aware of them without knowing those types at the
        /// call-site.
        current_field_value: Option<CurrentFieldValueFn>,

        /// Field names to extract from a [`Span`].
        lookup: Lookup,
    }

    impl FieldValueRecorder {
        /// Allows a function to be called in the context of the "remembered"
        /// subscriber.
        fn current_field_value(
            self,
            dispatch: &Dispatch,
            id: &span::Id,
            key: &'static str,
        ) -> Option<String> {
            (self.current_field_value?)(dispatch, id, key)
        }
    }

    impl<S> Layer<S> for FieldValueRecorder
    where
        S: for<'span> LookupSpan<'span> + Subscriber,
    {
        fn on_layer(&mut self, _: &mut S) {
            self.current_field_value = Some(|dispatch, id, key| {
                let sub = dispatch.downcast_ref::<S>()?;
                let span = sub.span(id)?;

                span.scope().find_map(|span| {
                    let ext = span.extensions();
                    let Fields(field) = ext.get::<Fields>()?;
                    Some(field.get(key)?.clone().into())
                })
            });
        }

        fn on_new_span(
            &self,
            attrs: &span::Attributes<'_>,
            id: &span::Id,
            ctx: layer::Context<'_, S>,
        ) {
            if let Some(span) = ctx.span(id) {
                let fields = Fields::from_record(
                    &span::Record::new(attrs.values()),
                    self.lookup,
                );
                span.extensions_mut().insert(fields);
            }
        }

        fn on_record(
            &self,
            id: &span::Id,
            values: &span::Record<'_>,
            ctx: layer::Context<'_, S>,
        ) {
            if let Some(span) = ctx.span(id) {
                let new_fields = Fields::from_record(values, self.lookup);
                if let Some(fields) = span.extensions_mut().get_mut::<Fields>()
                {
                    for (k, v) in &new_fields.0 {
                        drop(fields.0.insert(k, v.clone()));
                    }
                }
            }
        }
    }

    /// Values of [`Span`] fields along with their names.
    #[derive(Clone, Debug, Default)]
    struct Fields(HashMap<&'static str, String>);

    impl Fields {
        /// Extracts [`Fields`] from the provided [`span::Record`].
        fn from_record(record: &span::Record<'_>, lookup: Lookup) -> Self {
            #[derive(Debug)]
            struct Visitor {
                fields: Fields,
                lookup: Lookup,
            }

            impl Visit for Visitor {
                fn record_debug(&mut self, _: &field::Field, _: &dyn Debug) {}

                fn record_str(&mut self, field: &field::Field, value: &str) {
                    let key = field.name();
                    if self.lookup.contains(&key) {
                        drop(self.fields.0.insert(key, value.into()));
                    }
                }
            }

            let mut visitor = Visitor { fields: Fields::default(), lookup };
            record.record(&mut visitor);
            visitor.fields
        }
    }
}
