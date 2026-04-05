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
  - `value` — typed-value tree with source locations
  - `value-noloc` — typed-value tree without source locations
  - `node` — full AST with source locations, anchors, tags, and comments
  - `node-noloc` — full AST without source locations or heights
  - `events` — yaml-test-suite event-tree notation (for debugging)
- `--strict` flag: with `-f plain`, raise an error on tags instead of
  stripping them silently.
