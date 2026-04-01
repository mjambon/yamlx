(** YAMLx — pure-OCaml YAML 1.2 parser.
    This is the main public module.  All other modules in the library are
    implementation details; this module exposes the high-level API.

    Typical usage
    ~~~~~~~~~~~~~
    {[
      (* Parse into typed values (JSON schema) *)
      let values = YAMLx.of_string 'answer: 42\nflag: true'
      (* → [Map [(String 'answer', Int 42L); (String 'flag', Bool true)]] *)

      (* Parse into AST nodes (preserves style, anchors, tags) *)
      let nodes = YAMLx.parse_nodes '- foo\n- bar'

      (* Parse into a raw event list (for testing / streaming) *)
      let events = YAMLx.parse_events 'key: val'
    ]}
*)

open Types

(* ------------------------------------------------------------------ *)
(* Internal pipeline wiring                                              *)
(* ------------------------------------------------------------------ *)

let make_pipeline (input : string) =
  let reader  = Reader.of_string input in
  let scanner = Scanner.create reader  in
  let parser_ = Parser.create scanner  in
  parser_

(* ------------------------------------------------------------------ *)
(* Public API — Events                                                   *)
(* ------------------------------------------------------------------ *)

(** Parse [input] and return the flat event list.
    Includes the STREAM_START and STREAM_END boundary events.
    Raises [Scan_error] or [Parse_error] on malformed input. *)
let parse_events (input : string) : event list =
  let parser_ = make_pipeline input in
  Parser.to_event_list parser_

(* ------------------------------------------------------------------ *)
(* Public API — Nodes                                                    *)
(* ------------------------------------------------------------------ *)

(** Parse [input] and return one AST node per YAML document.
    Anchor references are resolved; aliases carry the resolved node.
    Raises [Scan_error] or [Parse_error] on malformed input. *)
let parse_nodes (input : string) : node list =
  let parser_  = make_pipeline input  in
  let composer = Composer.create parser_ in
  Composer.compose_stream composer

(* ------------------------------------------------------------------ *)
(* Public API — Typed values                                             *)
(* ------------------------------------------------------------------ *)

(** Parse [input] and resolve each document's node to a [value] using the
    YAML 1.2 JSON schema.
    Raises [Scan_error] or [Parse_error] on malformed input. *)
let of_string (input : string) : value list =
  let nodes = parse_nodes input in
  Resolver.resolve_documents nodes

(** Parse [input] and return the first document's value.
    Raises [Not_found] if the stream is empty.
    Raises [Scan_error] or [Parse_error] on malformed input. *)
let one_of_string (input : string) : value =
  match of_string input with
  | []    -> raise Not_found
  | v :: _ -> v

(* ------------------------------------------------------------------ *)
(* Error formatting                                                      *)
(* ------------------------------------------------------------------ *)

(** Format a [yaml_error] as a human-readable string. *)
let string_of_error (e : yaml_error) : string =
  Printf.sprintf "line %d, column %d: %s" e.pos.line e.pos.column e.msg

(** Return [Ok v] or [Error msg] instead of raising exceptions. *)
let of_string_result (input : string) : (value list, string) result =
  try Ok (of_string input)
  with
  | Scan_error e  -> Error ("scan error: "  ^ string_of_error e)
  | Parse_error e -> Error ("parse error: " ^ string_of_error e)
