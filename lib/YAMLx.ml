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
      height : int;
      head_comments : string list;
      line_comment : string option;
    }
  | Sequence_node of {
      anchor : string option;
      tag : string option;
      items : node list;
      flow : bool;
      loc : loc;
      height : int;
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
      height : int;
      head_comments : string list;
      line_comment : string option;
      foot_comments : string list;
    }
  | Alias_node of {
      name : string;
      resolved : node;
      loc : loc;
      height : int;
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

(** Extract the precomputed height from any node variant. *)
let node_height : node -> int = function
  | Scalar_node r -> r.height
  | Sequence_node r -> r.height
  | Mapping_node r -> r.height
  | Alias_node r -> r.height

(** Compute the height of a value tree (maximum depth from root to leaf). Leaf
    values (Null, Bool, Int, Float, String) have height 1. *)
let rec value_height : value -> int = function
  | Null _
  | Bool _
  | Int _
  | Float _
  | String _ ->
      1
  | Seq (_, items) ->
      1 + List.fold_left (fun acc v -> max acc (value_height v)) 0 items
  | Map (_, pairs) ->
      1
      + List.fold_left
          (fun acc (_, k, v) -> max acc (max (value_height k) (value_height v)))
          0 pairs

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

let parse_nodes ?(max_depth = Types.default_max_depth) (input : string) :
    node list =
  let parser_ = make_pipeline input in
  let composer = Composer.create ~max_depth parser_ in
  let nodes = Composer.compose_stream composer in
  let raw_comments = Scanner.drain_comments (Parser.get_scanner parser_) in
  Comment_attacher.attach nodes raw_comments

(* ------------------------------------------------------------------ *)
(* Public API — exceptions and defaults                                  *)
(* ------------------------------------------------------------------ *)

exception Expansion_limit_exceeded = Types.Expansion_limit_exceeded
exception Depth_limit_exceeded = Types.Depth_limit_exceeded
exception Plain_error = Printer.Plain_error

let default_expansion_limit = Types.default_expansion_limit
let default_max_depth = Types.default_max_depth

(* ------------------------------------------------------------------ *)
(* Error formatting                                                      *)
(* ------------------------------------------------------------------ *)

let string_of_error (e : yaml_error) : string =
  Printf.sprintf "line %d, column %d: %s" e.pos.line e.pos.column e.msg

let catch_errors f =
  try Ok (f ()) with
  | Scan_error e -> Error ("scan error: " ^ string_of_error e)
  | Parse_error e -> Error ("parse error: " ^ string_of_error e)
  | Expansion_limit_exceeded n ->
      Error (Printf.sprintf "expansion limit exceeded (%d nodes)" n)
  | Depth_limit_exceeded n ->
      Error (Printf.sprintf "depth limit exceeded (%d levels)" n)
  | Plain_error msg -> Error ("plain error: " ^ msg)

let register_exception_printers () =
  Printexc.register_printer (function
    | Scan_error e -> Some ("YAMLx.Scan_error: " ^ string_of_error e)
    | Parse_error e -> Some ("YAMLx.Parse_error: " ^ string_of_error e)
    | Expansion_limit_exceeded n ->
        Some (Printf.sprintf "YAMLx.Expansion_limit_exceeded (%d)" n)
    | Depth_limit_exceeded n ->
        Some (Printf.sprintf "YAMLx.Depth_limit_exceeded (%d)" n)
    | Plain_error msg -> Some ("YAMLx.Plain_error: " ^ msg)
    | _ -> None)

(* ------------------------------------------------------------------ *)
(* Public submodules                                                     *)
(* ------------------------------------------------------------------ *)

module Nodes = struct
  type t = node list

  let of_yaml_exn = parse_nodes

  let of_yaml ?max_depth input =
    catch_errors (fun () -> of_yaml_exn ?max_depth input)

  let to_yaml = Printer.to_yaml

  let to_plain_yaml_exn ?strict ?expansion_limit docs =
    Printer.to_plain_yaml ?strict ?expansion_limit docs

  let height = node_height
end

module Values = struct
  type t = value list

  let of_yaml_exn ?(max_depth = Types.default_max_depth)
      ?(expansion_limit = Types.default_expansion_limit) (input : string) :
      value list =
    let nodes = parse_nodes ~max_depth input in
    Resolver.resolve_documents ~expansion_limit nodes

  let of_yaml ?max_depth ?expansion_limit input =
    catch_errors (fun () -> of_yaml_exn ?max_depth ?expansion_limit input)

  let one_of_yaml_exn ?max_depth ?expansion_limit input =
    match of_yaml_exn ?max_depth ?expansion_limit input with
    | [] -> invalid_arg "YAMLx.Values.one_of_yaml_exn: no document in input"
    | [ v ] -> v
    | _ :: _ :: _ ->
        invalid_arg "YAMLx.Values.one_of_yaml_exn: multiple documents in input"

  let equal = equal_value
  let height = value_height
end

(* ------------------------------------------------------------------ *)
(* Event printing — internal helpers for tests and the CLI tool         *)
(* ------------------------------------------------------------------ *)

let events_to_tree = Event_printer.to_tree
let diff_event_trees = Event_printer.diff_trees
