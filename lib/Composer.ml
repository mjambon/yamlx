(** YAML Composer.
    Builds an in-memory AST (Types.node) from the Parser's event stream.
    The Composer resolves anchor/alias references: when an alias is encountered
    the already-composed node for that anchor is substituted in place.

    Note that the YAML specification allows forward aliases (an alias to a node
    declared later), but this is extremely rare and hard to support without
    breaking the single-pass model.  We require anchors to be declared before
    they are used, which is what all common YAML documents do. *)

open Types

(* ------------------------------------------------------------------ *)
(* Composer state                                                        *)
(* ------------------------------------------------------------------ *)

type t = {
  parser_  : Parser.t;
  anchors  : (string, node) Hashtbl.t;
}

let create (parser_ : Parser.t) : t =
  { parser_; anchors = Hashtbl.create 16 }

(* ------------------------------------------------------------------ *)
(* Helpers                                                               *)
(* ------------------------------------------------------------------ *)

let get_ev c = Parser.get_event c.parser_

let register_anchor c name node =
  Hashtbl.replace c.anchors name node

(* ------------------------------------------------------------------ *)
(* Node composition                                                      *)
(* ------------------------------------------------------------------ *)

(** Compose a single node from the next event(s) in the stream.
    Precondition: the next event is the *start* of a node (i.e. not a
    *_end or document event). *)
let rec compose_node (c : t) : node =
  let ev = get_ev c in
  match ev.kind with

  | Alias name ->
    (match Hashtbl.find_opt c.anchors name with
    | Some target ->
      Alias_node { name; resolved = target; pos = ev.start_pos;
                   head_comments = []; line_comment = None }
    | None ->
      Types.parse_error ev.start_pos "undefined alias '*%s'" name)

  | Scalar { anchor; tag; value; style } ->
    let node = Scalar_node { anchor; tag; value; style; pos = ev.start_pos;
                              head_comments = []; line_comment = None } in
    Option.iter (fun n -> register_anchor c n node) anchor;
    node

  | Sequence_start { anchor; tag; flow; _ } ->
    let start = ev.start_pos in
    let items = ref [] in
    let stop = ref false in
    while not !stop do
      let next = Parser.peek_event c.parser_ in
      (match next.kind with
      | Sequence_end ->
        ignore (get_ev c);
        stop := true
      | _ ->
        items := compose_node c :: !items)
    done;
    let node =
      Sequence_node { anchor; tag; items = List.rev !items; flow; pos = start;
                      head_comments = []; line_comment = None; foot_comments = [] }
    in
    Option.iter (fun n -> register_anchor c n node) anchor;
    node

  | Mapping_start { anchor; tag; flow; _ } ->
    let start = ev.start_pos in
    let pairs = ref [] in
    let stop = ref false in
    while not !stop do
      let next = Parser.peek_event c.parser_ in
      (match next.kind with
      | Mapping_end ->
        ignore (get_ev c);
        stop := true
      | _ ->
        let key   = compose_node c in
        let value = compose_node c in
        pairs := (key, value) :: !pairs)
    done;
    let node =
      Mapping_node { anchor; tag; pairs = List.rev !pairs; flow; pos = start;
                     head_comments = []; line_comment = None; foot_comments = [] }
    in
    Option.iter (fun n -> register_anchor c n node) anchor;
    node

  | _ ->
    Types.parse_error ev.start_pos
      "compose_node: unexpected event kind at this position"

(** Compose a complete YAML document.
    Expects: DOCUMENT_START … DOCUMENT_END. *)
let compose_document (c : t) : node =
  (* Consume DOCUMENT_START *)
  let start_ev = get_ev c in
  (match start_ev.kind with
  | Document_start _ -> ()
  | _ -> Types.parse_error start_ev.start_pos "expected DOCUMENT_START");
  let node = compose_node c in
  (* Consume DOCUMENT_END *)
  let end_ev = get_ev c in
  (match end_ev.kind with
  | Document_end _ -> ()
  | _ -> Types.parse_error end_ev.start_pos "expected DOCUMENT_END");
  node

(** Compose all documents in a YAML stream.
    Returns one node per document. *)
let compose_stream (c : t) : node list =
  (* Consume STREAM_START *)
  let start_ev = get_ev c in
  (match start_ev.kind with
  | Stream_start -> ()
  | _ -> Types.parse_error start_ev.start_pos "expected STREAM_START");
  let docs = ref [] in
  let stop = ref false in
  while not !stop do
    let next = Parser.peek_event c.parser_ in
    (match next.kind with
    | Stream_end ->
      ignore (get_ev c);
      stop := true
    | Document_start _ ->
      docs := compose_document c :: !docs
    | _ ->
      Types.parse_error next.start_pos "expected DOCUMENT_START or STREAM_END")
  done;
  List.rev !docs
