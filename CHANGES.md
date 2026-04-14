## Unreleased

### Comment attachment improvements

- Comments between a mapping key and its value (when the value starts on the
  next line) are now correctly attached as `head_comments` of the value node.
  Previously they were silently discarded.
- Trailing comments after the last item of a block collection are now attached
  as `foot_comments` of the collection and printed at the correct indentation
  level.  Previously they were attached to the last item's `foot_comments` and
  printed at column 0.
- Block scalar header comments (`| # note`, `> # note`) are now captured and
  round-trip correctly.  Previously the comment was silently dropped.
- Standalone comments that appear between two documents (before `---`) are now
  attached as `foot_comments` of the preceding document rather than leaking
  into the `head_comments` of the following document's root node.
- Added `foot_comments : string list` field to `Scalar_node` and `Alias_node`
  (previously only collection nodes had this field).

## 0.1.0 (2026-04-08)

Initial release.

### Parser

- Full YAML 1.2 parser written in pure OCaml with no C bindings or
  external runtime dependencies.
- Passes all 371 tests from the
  [yaml-test-suite](https://github.com/yaml/yaml-test-suite).
- UTF-8 input with BOM stripping and line-ending normalisation (CR+LF,
  bare CR, NEL, LS, PS → LF).
- Multi-document streams.
- Anchors and aliases, including cycle detection: cyclic structures raise
  `Cycle_error` rather than looping forever.
- Anchors are scoped to the document in which they are defined.
- Tags (both shorthand and verbatim forms).
- All scalar styles: plain, single-quoted, double-quoted, literal block,
  folded block.
- Flow and block sequences and mappings.
- Directives (`%YAML`, `%TAG`).

### YAML 1.1 support

- Optional YAML 1.1 schema (selectable per-parse or per-document via the
  `%YAML 1.1` directive).
- Extended booleans: `yes/no`, `on/off`, `y/n` and their case variants.
- Legacy octal: `0755` in addition to `0o755`.
- Sexagesimal integers and floats: `3:25:45` = 12345, `20:30.15` = 1230.15.
- Merge keys: plain `<<` mapping key merges the associated mapping(s) into
  the current mapping; explicit keys win over merged keys.

### Lossless AST (`Nodes` module)

- The `node` type preserves scalar style (plain / single-quoted /
  double-quoted / literal / folded), flow vs. block collection style, tags,
  anchors, and source positions (line, column, byte offset).
- Best-effort comment preservation: head comments (standalone lines before a
  node), line comments (end-of-line comments on the same line as a node), and
  foot comments (trailing lines after the last item of a block collection) are
  attached to the nearest node and faithfully re-emitted by the printer.
- `Nodes.of_yaml` / `of_yaml_exn` / `of_yaml_file`: parse YAML to a
  `node list` (one entry per document).
- `Nodes.to_yaml`: serialise back to YAML, round-tripping styles and
  comments.
- `Nodes.to_plain_yaml` / `to_plain_yaml_exn`: serialise to simplified
  YAML (aliases expanded, tags stripped, block collections only).

### Typed values (`Values` module)

- `value` type: `Null | Bool of bool | Int of int | Float of float |
  String of string | Seq of value list | Map of (value * value) list`.
- Applies the YAML 1.2 JSON schema (or YAML 1.1 schema when requested) to
  resolve plain scalars to typed values.
- `Values.of_yaml` / `of_yaml_exn` / `of_yaml_file`: parse and resolve in
  one step.
- `Values.one_of_yaml` / `one_of_yaml_exn` / `one_of_yaml_file`: parse a
  single-document stream and return the value directly (error on zero or
  multiple documents).
- Optional parameters on all resolution functions:
  - `?schema`: choose `Yaml_1_2` (default) or `Yaml_1_1`.
  - `?strict_keys`: raise `Duplicate_key_error` on duplicate mapping keys
    (default: keep the last occurrence silently).
  - `?plain`: raise `Simplicity_error` if the input uses anchors, aliases,
    or explicit tags — useful when only simple, unambiguous YAML is
    acceptable.
  - `?strict_schema`: error when a document's `%YAML` directive conflicts
    with the `?schema` setting.
  - `?reject_ambiguous`: with YAML 1.2, error on plain scalars that would
    resolve differently under YAML 1.1.

### Safety limits

- **Expansion limit** (default 1 000 000 nodes): alias expansion is counted
  and raises `Expansion_limit_exceeded` before the node tree can grow
  unboundedly. Protects against YAML bombs.
- **Depth limit** (default 512 levels): deeply nested inputs raise
  `Depth_limit_exceeded` rather than overflowing the stack.
- Both limits are configurable via optional parameters on the parse
  functions.

### Error handling

- All exceptions share a single `Error of error` wrapper, making it
  straightforward to catch every YAMLx error in one place.
- `error` variants: `Scan_error`, `Parse_error`, `Compose_error`,
  `Resolve_error`, `Schema_error`, `Duplicate_key_error`, `Simplicity_error`,
  `Cycle_error`, `Expansion_limit_exceeded`, `Depth_limit_exceeded`,
  `Printer_error`.
- Every error carries a `loc` value with the start and end position
  (line, column, byte offset) of the offending input range.
- `show_yaml_error`: formats an error as a human-readable string. Accepts
  an optional `?format_loc` parameter for custom location formatting (e.g.
  for LSP servers or structured logging).
- `catch_errors`: wraps a thunk, returning `Ok value` or `Error message`.
  Accepts an optional `?file` argument to prefix messages with the file path.
- `register_exception_printers`: registers `Printexc` printers so YAMLx
  exceptions print legibly in uncaught-exception output.

### Command-line tool (`yamlx`)

Seven output formats selectable with `-f FORMAT`:

| Format | Description |
|---|---|
| `yaml` | Re-emit YAML preserving styles and comments (default) |
| `plain` | Simplified YAML: aliases expanded, tags stripped, block-only |
| `value` | Typed-value tree without source locations |
| `value-loc` | Typed-value tree with source locations |
| `node` | AST without source locations or heights |
| `node-loc` | Full AST with source locations, anchors, tags, and comments |
| `events` | yaml-test-suite event-tree notation (for debugging) |

Options:

- `--schema 1.1 / 1.2`: choose the default YAML schema for the stream.
- `--strict-schema`: error when a document's `%YAML` directive contradicts
  `--schema`.
- `--reject-ambiguous`: with `--schema 1.2`, error on scalars that are
  ambiguous between YAML 1.1 and 1.2.
- `--strict-keys`: error on duplicate mapping keys (with `-f value` /
  `value-loc`).
- `--plain`: reject anchors, aliases, and tags in the input (with `-f value`
  / `value-loc`).
- `--strict`: with `-f plain`, error on tags rather than stripping them.
- `--depth-limit N` / `--expansion-limit N`: override the default safety
  limits.
