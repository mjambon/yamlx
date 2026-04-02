(** YAML 1.2 tag resolver (JSON schema).
    Resolves untagged plain scalars to typed values according to the
    YAML 1.2 JSON schema (the recommended default schema).

    Rules (applied only to plain scalars without an explicit tag):
      null   →  matches /^(null|Null|NULL|~|)$/
      bool   →  matches /^(true|True|TRUE|false|False|FALSE)$/
      int    →  decimal:     /^[-+]?[0-9]+$/
                hex:         /^0x[0-9a-fA-F]+$/
                octal:       /^0o[0-7]+$/
      float  →  decimal or scientific notation; also .inf/.nan variants

    Non-plain scalars (quoted, block) always resolve to String.

    Explicit [!!str], [!!int], etc. tags override the schema resolution. *)

(** Resolve a list of document nodes (as returned by [Composer.compose_stream])
    into a list of typed [Types.value] trees. *)
val resolve_documents : Types.node list -> Types.value list
