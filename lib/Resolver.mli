(** YAML 1.2 tag resolver (JSON schema). Resolves untagged plain scalars to
    typed values according to the YAML 1.2 JSON schema (the recommended default
    schema).

    Rules (applied only to plain scalars without an explicit tag): null →
    matches /^(null|Null|NULL|~|)$/ bool → matches
    /^(true|True|TRUE|false|False|FALSE)$/ int → decimal: /^[-+]?[0-9]+$/ hex:
    /^0x[0-9a-fA-F]+$/ octal: /^0o[0-7]+$/ float → decimal or scientific
    notation; also .inf/.nan variants

    Non-plain scalars (quoted, block) always resolve to String.

    Explicit [!!str], [!!int], etc. tags override the schema resolution. *)

val resolve_documents :
  ?expansion_limit:int -> Types.node list -> Types.value list
(** Resolve a list of document nodes (as returned by [Composer.compose_stream])
    into a list of typed [Types.value] trees.

    [expansion_limit] caps the total number of nodes visited during alias
    expansion. Raises {!Types.Expansion_limit_exceeded} if the limit is
    exceeded. Defaults to {!Types.default_expansion_limit}. *)
