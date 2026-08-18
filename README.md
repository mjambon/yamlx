# YAMLx

A pure-OCaml YAML 1.2 and 1.1 library with a lossless, comment-preserving AST.

This is AI-assisted software, fully owned and maintained by Martin Jambon.

👉 **[API Documentation](https://mjambon.github.io/yamlx/yamlx/YAMLx/)**

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
- **Pretty-printer** — `Nodes.to_yaml` serializes a node list back to a
  YAML string, preserving all of the above.
- **Plain-YAML printer** — `Nodes.to_plain_yaml_exn` produces a
  restricted subset with no anchors, no aliases (expanded inline), no
  tags, no flow collections, and no complex mapping keys — the fragment
  of YAML that most people recognize on sight.
- **Typed-value resolver** — `Values.of_yaml` applies the YAML 1.2 JSON
  schema and returns `value list` with `Null | Bool | Int | Float |
  String | Seq | Map` constructors.
- **Programmatic value construction** — `Value.Build` provides convenience
  constructors (`null`, `bool`, `int`, `float`, `string`, `seq`, `map`)
  that fill in `zero_loc` automatically, for building value trees without
  a source file.
- **Duplicate-key detection on export** — `Values.to_yaml` and friends
  raise `Duplicate_key_error` by default when a map contains repeated
  keys, catching bugs in programmatically-constructed values before they
  reach the output. Pass `~strict_keys:false` to allow duplicates through.
- **Multi-document streams** — both the node and value APIs handle
  streams containing more than one `---`-separated document.
- **Correct anchor scoping** — anchors are document-local; an alias in
  document N cannot refer to an anchor defined in document N−1.
- **Structured errors** — `Scan_error` and `Parse_error` carry a `pos`
  record with line, column, and byte offset. `catch_errors` wraps any
  of these into a `(_, string) result` with a human-readable message,
  optionally prefixed with a file name.
- **Command-line tool** — the `yamlx` binary reads YAML from a file or
  stdin and prints it in one of several formats (see below).

## Quick start

```ocaml
(* Single-document config file — most common pattern *)
match YAMLx.Value.of_yaml_file "config.yaml" with
| Ok value  -> ...
| Error msg -> prerr_endline msg  (* "file config.yaml, line 3, col 5: ..." *)

(* Parse a YAML string into a single typed value *)
match YAMLx.Value.of_yaml "answer: 42\nflag: true" with
| Ok (Map (_, [(_, String (_, "answer"), Int (_, 42L));
               (_, String (_, "flag"),   Bool (_, true))])) -> ...
| _ -> ...

(* Multi-document stream *)
match YAMLx.Values.of_yaml input with
| Ok values -> ...
| Error msg -> ...

(* Build a value tree programmatically and serialize it *)
let v =
  YAMLx.Value.Build.(
    map [ "name", string "Alice"; "scores", seq [ int 95; int 87 ] ])
in
print_string (YAMLx.Value.to_yaml v)

(* Round-trip through the lossless AST *)
match YAMLx.Nodes.of_yaml input with
| Ok nodes -> print_string (YAMLx.Nodes.to_yaml nodes)
| Error msg -> ...

(* Strip YAML-specific features *)
match YAMLx.Nodes.of_yaml input with
| Ok nodes ->
    (match YAMLx.catch_errors (fun () ->
         YAMLx.Nodes.to_plain_yaml_exn nodes) with
    | Ok plain -> print_string plain
    | Error msg -> ...)
| Error msg -> ...
```

## Command-line tool

```
yamlx [-f FORMAT] [--schema VERSION] [FILE]

Output formats (-f FORMAT):
  yaml         Pretty-printed YAML — scalar styles and block/flow mode
               preserved (default)
  plain        Simplified YAML — aliases expanded, tags stripped, flow
               collections converted to block; merge keys expanded in
               YAML 1.1 mode
  reformat     Normalized YAML — reads input as typed values, then
               re-serializes. Drops comments, anchors, and tags. Converts
               flow collections to block. Long strings use literal (|) or
               folded (>) block style as appropriate.
  value        Typed-value tree: Null / Bool / Int / Float / String / Seq /
               Map. Useful for checking how scalars are resolved.
  value-loc    Same as value but with source locations
  node         Full AST without source locations or heights
  node-loc     Same as node but with source locations and heights
  events       yaml-test-suite event-tree notation (mainly for parser testing)

YAML schema (--schema VERSION):
  1.2  YAML 1.2 JSON schema — default. Booleans: true/false only.
  1.1  YAML 1.1 schema — extended booleans (yes/no/on/off), 0755-style
       octal, sexagesimal, merge keys (<<).

Options:
  --strict          With -f plain: error on tags instead of stripping them
  --strict-schema   Error if the document's %YAML directive disagrees with
                    --schema
  --reject-ambiguous
                    With --schema 1.2: error on plain scalars that would
                    resolve differently under YAML 1.1 (e.g. yes, 0755, <<)
  --plain           With -f value or value-loc: error on anchors, aliases,
                    explicit tags, or (with --schema 1.1) merge keys
  --strict-keys     With -f value or value-loc: error on duplicate mapping
                    keys instead of silently keeping the last occurrence
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
- **Scanner** — tokenization, indentation, flow-level tracking, comment
  capture.
- **Parser** — token stream → event stream (YAML grammar).
- **Composer** — events → node graph (anchor/alias resolution).
- **Resolver** — nodes → typed `value` tree (YAML 1.2 JSON schema).

## License

YAMLx is currently released under the **AGPL**. There is an ongoing
fundraiser: once a funding goal is reached, the license will switch to
the permissive **ISC** license for everyone. Donors above a certain
threshold receive an immediate **commercial license**. See
[FUNDING.md](FUNDING.md) for details.

## References

- [YAML 1.2 specification (1.2.2)](https://yaml.org/spec/1.2.2/)
- [YAML 1.1 specification](https://yaml.org/spec/1.1/)
- Anil Madhavapeddy's [AoAH Day 6: Getting a YAML 1.2 implementation
  in pure OCaml](https://anil.recoil.org/notes/aoah-2025-6)
