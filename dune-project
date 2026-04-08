(lang dune 3.20)

(name yamlx)

(generate_opam_files true)

(source
 (github mjambon/yamlx))

(authors "Martin Jambon <martin@mjambon.com>")

(maintainers "Martin Jambon <martin@mjambon.com>")

(license AGPL-3.0-only)

(documentation https://mjambon.github.io/yamlx)

(package
 (name yamlx)
 (synopsis "Pure-OCaml YAML 1.2 parser with a lossless, comment-preserving AST")
 (description "YAMLx is a pure-OCaml YAML 1.2 library. It passes all 371 tests from the yaml-test-suite and has no C bindings or external runtime dependencies. The parsed node tree preserves scalar styles, flow vs. block collection style, tags, anchors, source positions, and comments. A pretty-printer can round-trip the AST back to YAML. A typed-value resolver applies the YAML 1.2 JSON schema and returns Null / Bool / Int / Float / String / Seq / Map values. Structured errors carry line, column, and byte-offset information.\n\nYAMLx is currently released under the AGPL. A commercial license and a path to a fully permissive ISC license are available — see FUNDING.md.")
 (depends
  ocaml
  ppx_deriving
  (testo :with-test)
  (afl-persistent :with-dev)
 )
 (tags
  (yaml parser serialization)
 )
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
