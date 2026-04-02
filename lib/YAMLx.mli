(** YAMLx — pure-OCaml YAML 1.2 parser.
    This is the main public module.  All other modules in the library are
    implementation details; this module exposes the high-level API.

    Typical usage:
    {[
      (* Parse into typed values (JSON schema) *)
      let values = YAMLx.of_string "answer: 42\nflag: true"
      (* → [Map [(String "answer", Int 42L); (String "flag", Bool true)]] *)

      (* Parse into AST nodes (preserves style, anchors, tags) *)
      let nodes = YAMLx.parse_nodes "- foo\n- bar"

      (* Parse into a raw event list (for testing / streaming) *)
      let events = YAMLx.parse_events "key: val"
    ]}

    All functions raise [Types.Scan_error] or [Types.Parse_error] on
    malformed input unless the [_result] variant is used. *)

(** {1 Events} *)

(** Parse [input] and return the flat event list.
    Includes the [Stream_start] and [Stream_end] boundary events.
    Raises [Types.Scan_error] or [Types.Parse_error] on malformed input. *)
val parse_events : string -> Types.event list

(** {1 AST nodes} *)

(** Parse [input] and return one AST node per YAML document.
    Anchor references are resolved; aliases carry the resolved node.
    Raises [Types.Scan_error] or [Types.Parse_error] on malformed input. *)
val parse_nodes : string -> Types.node list

(** {1 Typed values} *)

(** Parse [input] and resolve each document's node to a [Types.value] using
    the YAML 1.2 JSON schema.
    Raises [Types.Scan_error] or [Types.Parse_error] on malformed input. *)
val of_string : string -> Types.value list

(** Parse [input] and return the first document's value.
    Raises [Not_found] if the stream is empty.
    Raises [Types.Scan_error] or [Types.Parse_error] on malformed input. *)
val one_of_string : string -> Types.value

(** {1 Error handling} *)

(** Format a [Types.yaml_error] as a human-readable string. *)
val string_of_error : Types.yaml_error -> string

(** Parse [input] and return [Ok values] or [Error msg] instead of raising
    exceptions. *)
val of_string_result : string -> (Types.value list, string) result
