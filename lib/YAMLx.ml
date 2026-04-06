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

type yaml_error = Types.yaml_error = { msg : string; loc : loc }
[@@deriving show { with_path = false }]

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
  | Float (_, x), Float (_, y) -> Float.equal x y
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

let parse_nodes_versioned ?(max_depth = Types.default_max_depth)
    (input : string) : ((int * int) option * node) list =
  let parser_ = make_pipeline input in
  let composer = Composer.create ~max_depth parser_ in
  let versioned = Composer.compose_stream composer in
  let raw_comments = Scanner.drain_comments (Parser.get_scanner parser_) in
  let plain_nodes = List.map snd versioned in
  let attached = Comment_attacher.attach plain_nodes raw_comments in
  (* Re-pair version info with the comment-attached nodes (same order). *)
  List.map2 (fun (ver, _) node -> (ver, node)) versioned attached

let parse_nodes ?(max_depth = Types.default_max_depth) (input : string) :
    node list =
  List.map snd (parse_nodes_versioned ~max_depth input)

(* ------------------------------------------------------------------ *)
(* Public API — exceptions and defaults                                  *)
(* ------------------------------------------------------------------ *)

type schema = Types.schema = Yaml_1_2 | Yaml_1_1
[@@deriving show { with_path = false }]

type error = Types.error =
  | Scan_error of yaml_error
  | Parse_error of yaml_error
  | Expansion_limit_exceeded of int
  | Depth_limit_exceeded of int
  | Plain_error of string
  | Document_count_error of string
  | Schema_error of yaml_error
[@@deriving show { with_path = false }]

exception Error = Types.Error

let default_expansion_limit = Types.default_expansion_limit
let default_max_depth = Types.default_max_depth

(* ------------------------------------------------------------------ *)
(* Error formatting                                                      *)
(* ------------------------------------------------------------------ *)

let string_of_error (e : yaml_error) : string =
  Printf.sprintf "line %d, column %d: %s" e.loc.start_pos.line
    e.loc.start_pos.column e.msg

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let n = in_channel_length ic in
      let s = Bytes.create n in
      really_input ic s 0 n;
      Bytes.to_string s)

let catch_errors ?file f =
  let pos_error prefix e =
    match file with
    | None -> prefix ^ string_of_error e
    | Some path -> Printf.sprintf "file %s, %s" path (string_of_error e)
  in
  let other_error msg =
    match file with
    | None -> msg
    | Some path -> "file " ^ path ^ ": " ^ msg
  in
  try Ok (f ()) with
  | Error (Scan_error e) -> Result.Error (pos_error "scan error: " e)
  | Error (Parse_error e) -> Result.Error (pos_error "parse error: " e)
  | Error (Expansion_limit_exceeded n) ->
      Result.Error
        (other_error (Printf.sprintf "expansion limit exceeded (%d nodes)" n))
  | Error (Depth_limit_exceeded n) ->
      Result.Error
        (other_error (Printf.sprintf "depth limit exceeded (%d levels)" n))
  | Error (Plain_error msg) ->
      Result.Error (other_error ("plain error: " ^ msg))
  | Error (Document_count_error msg) ->
      Result.Error (other_error ("document count error: " ^ msg))
  | Error (Schema_error e) -> Result.Error (pos_error "schema error: " e)

let register_exception_printers () =
  Printexc.register_printer (function
    | Error (Scan_error e) ->
        Some ("YAMLx.Error (Scan_error): " ^ string_of_error e)
    | Error (Parse_error e) ->
        Some ("YAMLx.Error (Parse_error): " ^ string_of_error e)
    | Error (Expansion_limit_exceeded n) ->
        Some (Printf.sprintf "YAMLx.Error (Expansion_limit_exceeded %d)" n)
    | Error (Depth_limit_exceeded n) ->
        Some (Printf.sprintf "YAMLx.Error (Depth_limit_exceeded %d)" n)
    | Error (Plain_error msg) -> Some ("YAMLx.Error (Plain_error): " ^ msg)
    | Error (Document_count_error msg) ->
        Some ("YAMLx.Error (Document_count_error): " ^ msg)
    | Error (Schema_error e) ->
        Some ("YAMLx.Error (Schema_error): " ^ string_of_error e)
    | _ -> None)

(* ------------------------------------------------------------------ *)
(* Public submodules                                                     *)
(* ------------------------------------------------------------------ *)

let write_file path content =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc content)

module Nodes = struct
  type t = node list

  let of_yaml_exn = parse_nodes

  let of_yaml ?file ?max_depth input =
    catch_errors ?file (fun () -> of_yaml_exn ?max_depth input)

  let of_yaml_file ?max_depth path =
    match
      try Ok (read_file path) with
      | Sys_error msg -> Result.Error msg
    with
    | Result.Error msg -> Result.Error ("file " ^ path ^ ": " ^ msg)
    | Ok input -> of_yaml ~file:path ?max_depth input

  let to_yaml = Printer.to_yaml
  let to_yaml_file nodes path = write_file path (to_yaml nodes)

  let to_plain_yaml_exn ?strict ?expansion_limit docs =
    Printer.to_plain_yaml ?strict ?expansion_limit docs

  let to_plain_yaml_file ?strict ?expansion_limit nodes path =
    match
      catch_errors (fun () -> to_plain_yaml_exn ?strict ?expansion_limit nodes)
    with
    | Error _ as e -> e
    | Ok yaml ->
        write_file path yaml;
        Ok ()
end

(* ------------------------------------------------------------------ *)
(* Value ↔ Node conversion                                              *)
(* ------------------------------------------------------------------ *)

(** Decide the scalar style for a string value so that it round-trips correctly
    through a YAML parser using the JSON schema. Double-quoted style is used
    whenever the plain representation would be misread as a non-string type or
    is otherwise unsafe as a plain scalar. *)
let string_scalar_style (s : string) : scalar_style =
  let n = String.length s in
  if n = 0 then Double_quoted
  else
    (* Patterns resolved to non-String types by the JSON schema *)
    let is_null = s = "null" || s = "Null" || s = "NULL" || s = "~" in
    let is_bool =
      s = "true" || s = "True" || s = "TRUE" || s = "false" || s = "False"
      || s = "FALSE"
    in
    let looks_numeric =
      (* Decimal / hex / octal integers *)
      (let start = if s.[0] = '+' || s.[0] = '-' then 1 else 0 in
       start < n
       && String.to_seq (String.sub s start (n - start))
          |> Seq.for_all (fun c -> c >= '0' && c <= '9'))
      || (n > 2 && s.[0] = '0' && (s.[1] = 'x' || s.[1] = 'X'))
      || (n > 2 && s.[0] = '0' && (s.[1] = 'o' || s.[1] = 'O'))
      (* Floats *)
      || s = ".inf"
      || s = ".Inf" || s = ".INF" || s = "+.inf" || s = "+.Inf" || s = "+.INF"
      || s = "-.inf" || s = "-.Inf" || s = "-.INF" || s = ".nan" || s = ".NaN"
      || s = ".NAN"
      ||
      match float_of_string_opt s with
      | Some _ ->
          String.contains s '.' || String.contains s 'e'
          || String.contains s 'E'
      | None -> false
    in
    if is_null || is_bool || looks_numeric then Double_quoted
    else if not (Char_class.can_start_plain_block (Char.code s.[0])) then
      Double_quoted
    else if s.[n - 1] = ' ' then Double_quoted
    else if String.contains s '\n' || String.contains s '\r' then Double_quoted
    else
      (* ': ' or ':\n' → mapping-value indicator inside the scalar *)
      let colon_unsafe =
        try
          let i = String.index s ':' in
          i + 1 < n && (s.[i + 1] = ' ' || s.[i + 1] = '\n')
        with
        | Not_found -> false
      in
      (* ' #' → comment indicator inside the scalar *)
      let hash_unsafe =
        try
          let i = String.index s '#' in
          i > 0 && s.[i - 1] = ' '
        with
        | Not_found -> false
      in
      if colon_unsafe || hash_unsafe then Double_quoted else Plain

(** Format a float as a YAML-safe plain scalar. *)
let format_float (f : float) : string =
  if Float.is_nan f then ".nan"
  else if Float.is_infinite f then if f > 0.0 then ".inf" else "-.inf"
  else
    let s = Printf.sprintf "%.17g" f in
    (* Ensure the string won't be re-read as an integer by requiring a
       decimal point or exponent marker. *)
    if String.contains s '.' || String.contains s 'e' || String.contains s 'E'
    then s
    else s ^ ".0"

let zero_pos = Types.zero_pos
let zero_loc = { Types.start_pos = zero_pos; end_pos = zero_pos }
let no_loc = zero_loc

let make_scalar ?(style = Types.Plain) value : node =
  Scalar_node
    {
      anchor = None;
      tag = None;
      value;
      style;
      loc = no_loc;
      height = 1;
      head_comments = [];
      line_comment = None;
    }

let rec value_to_node : value -> node = function
  | Null _ -> make_scalar "null"
  | Bool (_, b) -> make_scalar (if b then "true" else "false")
  | Int (_, n) -> make_scalar (Int64.to_string n)
  | Float (_, f) -> make_scalar (format_float f)
  | String (_, s) -> make_scalar ~style:(string_scalar_style s) s
  | Seq (_, items) ->
      let ns = List.map value_to_node items in
      let h =
        1 + List.fold_left (fun acc nd -> max acc (node_height nd)) 0 ns
      in
      Sequence_node
        {
          anchor = None;
          tag = None;
          items = ns;
          flow = false;
          loc = no_loc;
          height = h;
          head_comments = [];
          line_comment = None;
          foot_comments = [];
        }
  | Map (_, pairs) ->
      let ns =
        List.map (fun (_, k, v) -> (value_to_node k, value_to_node v)) pairs
      in
      let h =
        1
        + List.fold_left
            (fun acc (k, v) -> max acc (max (node_height k) (node_height v)))
            0 ns
      in
      Mapping_node
        {
          anchor = None;
          tag = None;
          pairs = ns;
          flow = false;
          loc = no_loc;
          height = h;
          head_comments = [];
          line_comment = None;
          foot_comments = [];
        }

module Values = struct
  type t = value list

  let of_yaml_exn ?(max_depth = Types.default_max_depth)
      ?(expansion_limit = Types.default_expansion_limit) ?schema ?strict_schema
      ?reject_ambiguous (input : string) : value list =
    let versioned = parse_nodes_versioned ~max_depth input in
    Resolver.resolve_documents ~expansion_limit ?schema ?strict_schema
      ?reject_ambiguous versioned

  let of_yaml ?file ?max_depth ?expansion_limit ?schema ?strict_schema
      ?reject_ambiguous input =
    catch_errors ?file (fun () ->
        of_yaml_exn ?max_depth ?expansion_limit ?schema ?strict_schema
          ?reject_ambiguous input)

  let of_yaml_file ?max_depth ?expansion_limit ?schema ?strict_schema
      ?reject_ambiguous path =
    match
      try Ok (read_file path) with
      | Sys_error msg -> Result.Error msg
    with
    | Result.Error msg -> Result.Error ("file " ^ path ^ ": " ^ msg)
    | Ok input ->
        of_yaml ~file:path ?max_depth ?expansion_limit ?schema ?strict_schema
          ?reject_ambiguous input

  let of_nodes_exn ?(expansion_limit = Types.default_expansion_limit) ?schema
      ?strict_schema ?reject_ambiguous nodes =
    (* Nodes without version info: pass None for each document. *)
    let versioned = List.map (fun n -> (None, n)) nodes in
    Resolver.resolve_documents ~expansion_limit ?schema ?strict_schema
      ?reject_ambiguous versioned

  let of_nodes ?expansion_limit ?schema ?strict_schema ?reject_ambiguous nodes =
    catch_errors (fun () ->
        of_nodes_exn ?expansion_limit ?schema ?strict_schema ?reject_ambiguous
          nodes)

  let one_of_yaml_exn ?max_depth ?expansion_limit ?schema ?strict_schema
      ?reject_ambiguous input =
    match
      of_yaml_exn ?max_depth ?expansion_limit ?schema ?strict_schema
        ?reject_ambiguous input
    with
    | [] -> raise (Error (Document_count_error "no document in input"))
    | [ v ] -> v
    | _ :: _ :: _ ->
        raise (Error (Document_count_error "multiple documents in input"))

  let one_of_yaml ?file ?max_depth ?expansion_limit ?schema ?strict_schema
      ?reject_ambiguous input =
    catch_errors ?file (fun () ->
        one_of_yaml_exn ?max_depth ?expansion_limit ?schema ?strict_schema
          ?reject_ambiguous input)

  let one_of_yaml_file ?max_depth ?expansion_limit ?schema ?strict_schema
      ?reject_ambiguous path =
    match
      try Ok (read_file path) with
      | Sys_error msg -> Result.Error msg
    with
    | Result.Error msg -> Result.Error ("file " ^ path ^ ": " ^ msg)
    | Ok input ->
        one_of_yaml ~file:path ?max_depth ?expansion_limit ?schema
          ?strict_schema ?reject_ambiguous input

  let to_nodes values = List.map value_to_node values
  let to_yaml values = Nodes.to_yaml (to_nodes values)
  let to_yaml_file values path = write_file path (to_yaml values)
end

(* ------------------------------------------------------------------ *)
(* Event printing — internal helpers for tests and the CLI tool         *)
(* ------------------------------------------------------------------ *)

let events_to_tree = Event_printer.to_tree
let diff_event_trees = Event_printer.diff_trees
