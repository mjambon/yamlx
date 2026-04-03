# YAMLx

A pure-OCaml YAML 1.2 library with a lossless, comment-preserving AST.

## Features

- **Full YAML 1.2 compliance** — passes all 371 tests from the
  [yaml-test-suite](https://github.com/yaml/yaml-test-suite).
- **Pure OCaml** — no C bindings, no external runtime dependencies.
- **Lossless AST** — the parsed node tree preserves scalar styles
  (`plain`, `'single-quoted'`, `"double-quoted"`, `|` literal, `>`
  folded), flow vs. block collection style, tags, anchors, and source
  positions.
- **Best-effort comment preservation** — standalone (head) comments
  before a node, inline (line) comments after a value, and trailing
  (foot) comments after the last item of a block collection are
  attached to the nearest node and re-emitted by the printer.
- **Pretty-printer** — `to_yaml` serializes a node list back to a YAML
  string, preserving all of the above.
- **Plain-YAML printer** — `to_plain_yaml` produces a restricted subset
  with no anchors, no aliases (expanded inline), no tags, no flow
  collections, and no complex mapping keys — the fragment of YAML that
  most people recognize on sight.
- **Typed-value resolver** — `of_string` applies the YAML 1.2 JSON
  schema and returns `value list` with `Null | Bool | Int | Float |
  String | Seq | Map` constructors.
- **Multi-document streams** — both the AST and value APIs handle
  streams containing more than one document.
- **Structured errors** — `Scan_error` and `Parse_error` carry a `pos`
  record with line, column, and byte offset.
- **Command-line tool** — the `yamlx` binary reads YAML from a file or
  stdin and re-emits it in `yaml` (default), `plain`, or `events`
  format.

## Quick start

```ocaml
(* Resolve to typed values *)
let values = YAMLx.of_string "answer: 42\nflag: true"
(* → [Map [(String "answer", Int 42L); (String "flag", Bool true)]] *)

(* Non-raising variant *)
match YAMLx.of_string_result input with
| Ok values -> ...
| Error msg -> ...

(* Round-trip through the lossless AST *)
let yaml = YAMLx.to_yaml (YAMLx.parse_nodes input)

(* Strip YAML-specific features *)
let plain = YAMLx.to_plain_yaml (YAMLx.parse_nodes input)
```

## Command-line tool

```
yamlx [--format FORMAT] [FILE]

  --format yaml    Re-emit YAML preserving styles and comments (default)
  --format plain   Plain YAML: aliases expanded, tags stripped, block-only
  --format events  yaml-test-suite event-tree notation (for debugging)
  --strict         With --format plain: error on tags instead of stripping
```

## Comment preservation

Comments are captured on a best-effort basis during scanning and
attached to the AST after parsing:

```yaml
# head comment (attached to the node that follows)
key: value  # line comment (attached to the scalar)
list:
  - a
  - b
  # foot comment (attached to the sequence, after the last item)
```

Comments inside flow collections (`[...]`, `{...}`) and on block scalar
header lines (`key: |  # this`) are not captured.

## Pipeline

```
Reader → Scanner → Parser → Composer → Resolver
```

- **Reader** — UTF-8 / UTF-16 / UTF-32 input normalization.
- **Scanner** — tokenisation, indentation, flow-level tracking, comment
  capture.
- **Parser** — token stream → event stream (YAML grammar).
- **Composer** — events → node graph (anchor/alias resolution).
- **Resolver** — nodes → typed `value` tree (YAML 1.2 JSON schema).

## References

- [YAML 1.2 specification (1.2.2)](https://yaml.org/spec/1.2.2/)
- Anil Madhavapeddy's [AoAH Day 6: Getting a YAML 1.2 implementation
  in pure OCaml](https://anil.recoil.org/notes/aoah-2025-6)
