`tracing-record-hierarchical` changelog
=======================================

All user visible changes to this project will be documented in this file. This project uses [Semantic Versioning 2.0.0].




## [0.1.1] · 2023-12-04
[0.1.1]: /../../tree/v0.1.1

[Diff](/../../compare/v0.1.0...v0.1.1)

### Fixed

- Recording of fields to parent `tracing::Span`s. ([#4])

[#4]: /../../pull/4




## [0.1.0] · 2023-10-10
[0.1.0]: /../../tree/v0.1.0

### Initially implemented

- `HierarchicalRecord` implementation of `tracing_subscriber::Layer`. ([#1])
- `SpanExt` trait extension for `tracing::Span`s. ([#1])

[#1]: /../../pull/1




[Semantic Versioning 2.0.0]: https://semver.org
