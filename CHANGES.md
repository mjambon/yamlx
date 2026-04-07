## Unreleased

### API

- Initial release of the `YAMLx` library.
- Full YAML 1.2 parser that passes all 371 tests from the
  [yaml-test-suite](https://github.com/yaml/yaml-test-suite).
- Lossless AST (`node` type) preserving scalar styles, flow/block
  collection style, tags, anchors, source positions, and comments.
- Best-effort comment preservation: head comments (standalone lines
  before a node), line comments (same line as a node), and foot comments
  (trailing lines after the last item of a block collection) are attached
  to the nearest node and re-emitted by the printer.
- `Nodes` submodule: parse YAML to a node list and serialize it back
  with `of_yaml`, `of_yaml_file`, `of_yaml_exn`, `to_yaml`, and
  `to_plain_yaml_exn`.
- `Values` submodule: apply the YAML 1.2 JSON schema to obtain a typed
  `value` tree (`Null | Bool | Int | Float | String | Seq | Map`) with
  `of_yaml`, `of_yaml_file`, `of_yaml_exn`, `one_of_yaml`,
  `one_of_yaml_file`, and `one_of_yaml_exn`.
- `catch_errors`: wraps any parse/scan/plain error into a
  `(_, string) result` with a human-readable message. Accepts an
  optional `?file` argument to prefix messages with the file name.
- `register_exception_printers`: registers `Printexc` printers for all
  YAMLx exceptions so they print legibly in uncaught-exception output.
- Anchors are correctly scoped to the document in which they are defined;
  an alias in document N cannot resolve an anchor from document N−1.
- Expansion limit (default: 1 000 000 nodes) and nesting depth limit
  (default: 512) protect against YAML bombs and deeply nested inputs.

### Command-line tool

- `yamlx` binary with seven output formats selectable via `-f FORMAT`:
  - `yaml` — re-emit YAML preserving styles and comments (default)
  - `plain` — simplified YAML: aliases expanded, tags stripped, block-only
  - `value` — typed-value tree without source locations
  - `value-loc` — typed-value tree with source locations
  - `node` — AST without source locations or heights
  - `node-loc` — full AST with source locations, anchors, tags, and comments
  - `events` — yaml-test-suite event-tree notation (for debugging)
- `--strict` flag: with `-f plain`, raise an error on tags instead of
  stripping them silently.
- `--schema 1.1` / `--schema 1.2` (default): choose the YAML schema for
  the whole stream. A `%YAML` directive in a document overrides the
  default per document unless `--strict-schema` is set.
- `--strict-schema`: error when a document's `%YAML` directive conflicts
  with the `--schema` setting.
- `--reject-ambiguous`: with `--schema 1.2`, error on plain scalars that
  would resolve differently under YAML 1.1 (e.g. `yes`, `0755`, `3:25:45`,
  mapping key `<<`).
- With `-f plain --schema 1.1`, merge keys are expanded before
  serialisation.

### YAML 1.1 support

- Added `schema` type (`Yaml_1_2` | `Yaml_1_1`) to the public API.
- Added `Schema_error of yaml_error` error variant; raised on
  schema-directive conflicts (`strict_schema = true`) or ambiguous
  scalars (`reject_ambiguous = true`). Carries a source location.
- `Values` functions (`of_yaml`, `of_yaml_exn`, `of_yaml_file`,
  `of_nodes`, `one_of_yaml`, …) now accept `?schema`, `?strict_schema`,
  and `?reject_ambiguous` optional parameters.
- YAML 1.1 schema differences versus 1.2:
  - Extended booleans: `y/Y/yes/Yes/YES/n/N/no/No/NO/on/On/ON/off/Off/OFF`
  - Legacy octal: `0755` (leading zero) in addition to `0o755`
  - Sexagesimal integers: `3:25:45` = 12345
  - Sexagesimal floats: `20:30.15` = 1230.15
  - Merge keys: plain `<<` mapping key expands the associated mapping(s)
    into the current mapping (explicit keys win over merged keys)
- YAML 1.2 JSON schema integer rule tightened to disallow leading zeros
  (`0755` is now a string, not integer 755).
- Scanner fix: `<` is no longer erroneously rejected as a plain-scalar
  start character (the YAML 1.2 spec does not list it as an indicator).
  This allows `<<` to appear as a mapping key.

### Error locations and formatting

- `yaml_error` now carries `loc : loc` (a source range with start and
  end positions) instead of `pos : pos` (a single point). All three
  positional error variants (`Scan_error`, `Parse_error`, `Schema_error`)
  benefit from this.
- New `default_format_loc : ?file:string -> loc -> string` formats a
  source range for human consumption:
  - zero-width: `"line 3, column 8"`
  - single-line range: `"line 3, columns 8-11"`
  - multi-line range: `"lines 3-12, columns 8-4"`
  - with `~file`: prefixed with `"file foo.yaml, "`
- `string_of_error` now shows the full source range via `default_format_loc`.
- `catch_errors` and `register_exception_printers` accept an optional
  `?format_loc` parameter to plug in a custom location formatter (e.g.
  for LSP servers or structured logging).

### Simple mode

- New `?simple:bool` option on all `Values` functions (`of_yaml`,
  `of_yaml_exn`, `of_yaml_file`, `of_nodes`, `of_nodes_exn`, `one_of_yaml`,
  `one_of_yaml_file`, `one_of_yaml_exn`). When `~simple:true`, the resolver
  raises `Simplicity_error` if it encounters any anchor (`&name`), alias
  (`*name`), or explicit tag on any node. In YAML 1.1 mode, merge keys (`<<`)
  also raise `Simplicity_error`. Plain YAML inputs without those features are
  accepted unchanged.
- New `Simplicity_error of yaml_error` error variant (carries source location).
  Handled by `catch_errors` (renders as `"simplicity error: …"`) and
  `register_exception_printers`.
- New `--simple` flag for the `yamlx` command, applicable with `-f value` and
  `-f value-loc`. Corresponds to the parsing-side counterpart of the `plain`
  output format.

### Duplicate key handling

- The resolver now deduplicates mapping keys, keeping the last occurrence
  of each key and discarding earlier ones. The surviving entry appears at
  the position of the last occurrence. This applies to both YAML 1.1 and
  1.2 and is consistent with the uniqueness requirement in both specs.
  In YAML 1.1 mode, explicit keys always win over merged keys regardless
  of order.
