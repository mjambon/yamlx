YAMLx - a pure-OCaml YAML library
==

We want a strict YAML 1.2 parser that's lossless, comment-preserving,
and possibly template-friendly.

It must be written in OCaml (no C).

In addition to being able to parse strict YAML 1.2, we also want the
following features:

- node locations (for error messages)
- preserve comments
- keep track of the original formatting (especially for text nodes)
- pretty-printer that preserves the style
- pretty-print into a simple subset of YAML that most people are
  familiar with
- pretty-print into JSON

Recommended architecture:

Reader → Scanner → Parser → Composer → Resolver

Scanner does:
- indentation
- flow context
- scalar extraction
- token classification

Parser does:
- grammar structure

Composer builds:
- node graph
- anchors/aliases

Resolver does:
- schema typing
