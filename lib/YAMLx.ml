(** YAMLx — pure-OCaml YAML 1.2 parser. *)

(* ------------------------------------------------------------------ *)
(* Type sharing with Types                                               *)
(* ------------------------------------------------------------------ *)

(* These declarations make the types defined here identical to the ones
   in Types, so values flow through the pipeline without any conversion.
   The sharing syntax 'type t = Types.t = ...' satisfies both the
   compiler (same physical type) and the mli (no mention of Types). *)

type pos = Types.pos = {
  line : int;
  column : int;
  column_bytes : int;
  offset : int;
  offset_bytes : int;
}
[@@deriving show { with_path = false }]

type loc = Types.loc = { start_pos : pos; end_pos : pos }
[@@deriving show { with_path = false }]

type yaml_error = Types.yaml_error = { msg : string; pos : pos }

exception Scan_error = Types.Scan_error
exception Parse_error = Types.Parse_error

type scalar_style = Types.scalar_style =
  | Plain
  | Single_quoted
  | Double_quoted
  | Literal
  | Folded
[@@deriving show { with_path = false }]

type event_kind = Types.event_kind =
  | Stream_start
  | Stream_end
  | Document_start of {
      explicit : bool;
      version : (int * int) option;
      tag_directives : (string * string) list;
    }
  | Document_end of { explicit : bool }
  | Mapping_start of {
      anchor : string option;
      tag : string option;
      implicit : bool;
      flow : bool;
    }
  | Mapping_end
  | Sequence_start of {
      anchor : string option;
      tag : string option;
      implicit : bool;
      flow : bool;
    }
  | Sequence_end
  | Scalar of {
      anchor : string option;
      tag : string option;
      value : string;
      style : scalar_style;
    }
  | Alias of string

type event = Types.event = { kind : event_kind; start_pos : pos; end_pos : pos }

type node = Types.node =
  | Scalar_node of {
      anchor : string option;
      tag : string option;
      value : string;
      style : scalar_style;
      loc : loc;
      head_comments : string list;
      line_comment : string option;
    }
  | Sequence_node of {
      anchor : string option;
      tag : string option;
      items : node list;
      flow : bool;
      loc : loc;
      head_comments : string list;
      line_comment : string option;
      foot_comments : string list;
    }
  | Mapping_node of {
      anchor : string option;
      tag : string option;
      pairs : (node * node) list;
      flow : bool;
      loc : loc;
      head_comments : string list;
      line_comment : string option;
      foot_comments : string list;
    }
  | Alias_node of {
      name : string;
      resolved : node;
      loc : loc;
      head_comments : string list;
      line_comment : string option;
    }
[@@deriving show { with_path = false }]

type value = Types.value =
  | Null of loc
  | Bool of loc * bool
  | Int of loc * int64
  | Float of loc * float
  | String of loc * string
  | Seq of loc * value list
  | Map of loc * (loc * value * value) list
[@@deriving show { with_path = false }]

(** Structural equality that ignores source locations. *)
let rec equal_value a b =
  match (a, b) with
  | Null _, Null _ -> true
  | Bool (_, x), Bool (_, y) -> x = y
  | Int (_, x), Int (_, y) -> Int64.equal x y
  | Float (_, x), Float (_, y) -> x = y
  | String (_, x), String (_, y) -> x = y
  | Seq (_, xs), Seq (_, ys) -> List.equal equal_value xs ys
  | Map (_, ps), Map (_, qs) ->
      List.equal
        (fun (_, k1, v1) (_, k2, v2) -> equal_value k1 k2 && equal_value v1 v2)
        ps qs
  | _ -> false

(* ------------------------------------------------------------------ *)
(* Internal pipeline wiring                                              *)
(* ------------------------------------------------------------------ *)

let make_pipeline (input : string) =
  let reader = Reader.of_string input in
  let scanner = Scanner.create reader in
  let parser_ = Parser.create scanner in
  parser_

(* ------------------------------------------------------------------ *)
(* Public API — Events                                                   *)
(* ------------------------------------------------------------------ *)

let parse_events (input : string) : event list =
  let parser_ = make_pipeline input in
  Parser.to_event_list parser_

(* ------------------------------------------------------------------ *)
(* Public API — Nodes                                                    *)
(* ------------------------------------------------------------------ *)

let parse_nodes (input : string) : node list =
  let parser_ = make_pipeline input in
  let composer = Composer.create parser_ in
  let nodes = Composer.compose_stream composer in
  let raw_comments = Scanner.drain_comments (Parser.get_scanner parser_) in
  Comment_attacher.attach nodes raw_comments

(* ------------------------------------------------------------------ *)
(* Public API — Pretty-printing                                          *)
(* ------------------------------------------------------------------ *)

let to_yaml = Printer.to_yaml

exception Plain_error = Printer.Plain_error

let to_plain_yaml ?strict ?expansion_limit =
  Printer.to_plain_yaml ?strict ?expansion_limit

(* ------------------------------------------------------------------ *)
(* Public API — Typed values                                             *)
(* ------------------------------------------------------------------ *)

exception Expansion_limit_exceeded = Types.Expansion_limit_exceeded

let default_expansion_limit = Types.default_expansion_limit

let of_string ?(expansion_limit = Types.default_expansion_limit)
    (input : string) : value list =
  let nodes = parse_nodes input in
  Resolver.resolve_documents ~expansion_limit nodes

let one_of_string ?(expansion_limit = Types.default_expansion_limit)
    (input : string) : value =
  match of_string ~expansion_limit input with
  | [] -> raise Not_found
  | v :: _ -> v

(* ------------------------------------------------------------------ *)
(* Error formatting                                                      *)
(* ------------------------------------------------------------------ *)

let string_of_error (e : yaml_error) : string =
  Printf.sprintf "line %d, column %d: %s" e.pos.line e.pos.column e.msg

let of_string_result ?(expansion_limit = Types.default_expansion_limit)
    (input : string) : (value list, string) result =
  try Ok (of_string ~expansion_limit input) with
  | Scan_error e -> Error ("scan error: " ^ string_of_error e)
  | Parse_error e -> Error ("parse error: " ^ string_of_error e)
  | Expansion_limit_exceeded n ->
      Error (Printf.sprintf "expansion limit exceeded (%d nodes)" n)

(* ------------------------------------------------------------------ *)
(* Event printing — internal helpers for tests and the CLI tool         *)
(* ------------------------------------------------------------------ *)

let events_to_tree = Event_printer.to_tree
let diff_event_trees = Event_printer.diff_trees
