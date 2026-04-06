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
- Added `Schema_error of string` error variant; raised on schema-directive
  conflicts (`strict_schema = true`) or ambiguous scalars
  (`reject_ambiguous = true`).
- `Values` functions (`of_yaml`, `of_yaml_exn`, `of_yaml_file`,
  `of_nodes`, `one_of_yaml`, …) now accept `?schema`, `?strict_schema`,
  and `?reject_ambiguous` optional parameters.
- YAML 1.1 schema differences versus 1.2:
  - Extended booleans: `y/Y/yes/Yes/YES/n/N/no/No/NO/on/On/ON/off/Off/OFF`
  - Legacy octal: `0755` (leading zero) in addition to `0o755`
  - Sexagesimal integers: `3:25:45` = 12345
  - Sexagesimal floats: `20:30.15` = 1230.15
  - Merge keys: plain `<<` mapping key expands the associated mapping(s)
    into the current mapping (earlier explicit keys win)
- YAML 1.2 JSON schema integer rule tightened to disallow leading zeros
  (`0755` is now a string, not an integer 755).
- Scanner fix: `<` is no longer erroneously rejected as a plain-scalar
  start character (the YAML 1.2 spec does not list it as an indicator).
  This allows `<<` to appear as a mapping key.
