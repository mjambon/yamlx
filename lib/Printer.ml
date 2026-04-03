(** YAML pretty-printer. Converts a [Types.node list] (one node per document)
    back into a YAML string. Scalar styles ([Plain], [Single_quoted],
    [Double_quoted], [Literal], [Folded]) and collection flow/block style are
    preserved. *)

open Types

(* ------------------------------------------------------------------ *)
(* Indentation                                                           *)
(* ------------------------------------------------------------------ *)

let spaces_per_level = 2
let indent level = String.make (level * spaces_per_level) ' '

(* ------------------------------------------------------------------ *)
(* Anchor and tag helpers                                                *)
(* ------------------------------------------------------------------ *)

let pp_anchor = function
  | None -> ""
  | Some a -> "&" ^ a ^ " "

let pp_tag = function
  | None -> ""
  | Some t ->
      let prefix = "tag:yaml.org,2002:" in
      let plen = String.length prefix in
      if String.length t > plen && String.sub t 0 plen = prefix then
        "!!" ^ String.sub t plen (String.length t - plen) ^ " "
      else "!<" ^ t ^ "> "

(* ------------------------------------------------------------------ *)
(* Scalar encoding                                                       *)
(* ------------------------------------------------------------------ *)

(** Encode a value for a single-quoted scalar (content between the quotes).
    Embedded single quotes are doubled. *)
let single_quoted_body s =
  let b = Buffer.create (String.length s + 4) in
  String.iter
    (fun c ->
      if c = '\'' then Buffer.add_string b "''" else Buffer.add_char b c)
    s;
  "'" ^ Buffer.contents b ^ "'"

(** Encode a value for a double-quoted scalar (content between the quotes).
    Special characters are replaced with backslash escapes. *)
let double_quoted_body s =
  let b = Buffer.create (String.length s + 4) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | '\x00' -> Buffer.add_string b "\\0"
      | '\x07' -> Buffer.add_string b "\\a"
      | '\x08' -> Buffer.add_string b "\\b"
      | '\x0B' -> Buffer.add_string b "\\v"
      | '\x0C' -> Buffer.add_string b "\\f"
      | '\x1B' -> Buffer.add_string b "\\e"
      | c when Char.code c < 0x20 -> Printf.bprintf b "\\x%02X" (Char.code c)
      | c -> Buffer.add_char b c)
    s;
  "\"" ^ Buffer.contents b ^ "\""

(* ------------------------------------------------------------------ *)
(* Block scalar emission                                                 *)
(* ------------------------------------------------------------------ *)

(** Count trailing ['\n'] characters in [s]. *)
let count_trailing_newlines s =
  let n = String.length s in
  let i = ref (n - 1) in
  while !i >= 0 && s.[!i] = '\n' do
    decr i
  done;
  n - 1 - !i

(* ------------------------------------------------------------------ *)
(* Comment emission helpers                                              *)
(* ------------------------------------------------------------------ *)

(** Return a string of head-comment lines, each indented to [level] and
    terminated by ['\n']. Returns [""] when [comments] is empty. *)
let emit_heads ~level comments =
  String.concat "" (List.map (fun h -> indent level ^ "# " ^ h ^ "\n") comments)

(** Return the inline suffix for a line comment, e.g. ["  # text"]. Returns [""]
    when [lc] is [None]. *)
let emit_lc = function
  | None -> ""
  | Some t -> "  # " ^ t

(* ------------------------------------------------------------------ *)
(* Block scalar emission                                                 *)
(* ------------------------------------------------------------------ *)

(** Return the block scalar header + indented body as a string. [marker] is
    ["|"] or [">"]. Content is indented to [(level + 1) * spaces_per_level]
    columns. [lc] is an optional line comment appended to the header line. The
    returned string includes the header line and all body lines, each terminated
    by ['\n']. *)
let block_scalar ?(lc = None) marker s level =
  let n = String.length s in
  let trail = count_trailing_newlines s in
  let main = String.sub s 0 (n - trail) in
  let chomp =
    if trail = 0 then "-"
    else if trail = 1 then "" (* clip — the default *)
    else "+" (* keep *)
  in
  let ind_n = (level + 1) * spaces_per_level in
  let ind = String.make ind_n ' ' in
  (* Explicit indentation indicator: required when first content char is
     a space (otherwise the parser would mistake it for indentation). *)
  let ind_ind =
    if String.length main > 0 && main.[0] = ' ' then string_of_int ind_n else ""
  in
  let b = Buffer.create (n + 16) in
  Buffer.add_string b (marker ^ ind_ind ^ chomp ^ emit_lc lc ^ "\n");
  let lines = String.split_on_char '\n' main in
  List.iter
    (fun line ->
      if line = "" then Buffer.add_char b '\n'
      else (
        Buffer.add_string b ind;
        Buffer.add_string b line;
        Buffer.add_char b '\n'))
    lines;
  (* Extra blank lines for "keep" chomp *)
  for _ = 2 to trail do
    Buffer.add_char b '\n'
  done;
  Buffer.contents b

(* ------------------------------------------------------------------ *)
(* Flow serialization                                                    *)
(* ------------------------------------------------------------------ *)

(** Serialize [node] in flow (inline) context. [Literal] and [Folded] scalars
    fall back to double-quoted since block styles are not allowed inside flow
    collections. *)
let rec flow node =
  match node with
  | Scalar_node { anchor; tag; value; style; _ } ->
      let pre = pp_anchor anchor ^ pp_tag tag in
      let body =
        match style with
        | Plain -> value
        | Single_quoted -> single_quoted_body value
        | Double_quoted -> double_quoted_body value
        | Literal
        | Folded ->
            double_quoted_body value
      in
      pre ^ body
  | Sequence_node { anchor; tag; items; _ } ->
      pp_anchor anchor ^ pp_tag tag ^ "["
      ^ String.concat ", " (List.map flow items)
      ^ "]"
  | Mapping_node { anchor; tag; pairs; _ } ->
      pp_anchor anchor ^ pp_tag tag ^ "{"
      ^ String.concat ", "
          (List.map (fun (k, v) -> flow k ^ ": " ^ flow v) pairs)
      ^ "}"
  | Alias_node { name; _ } -> "*" ^ name

(* ------------------------------------------------------------------ *)
(* Node field helpers                                                    *)
(* ------------------------------------------------------------------ *)

let node_head_comments = function
  | Scalar_node r -> r.head_comments
  | Sequence_node r -> r.head_comments
  | Mapping_node r -> r.head_comments
  | Alias_node r -> r.head_comments

(* ------------------------------------------------------------------ *)
(* Block serialization                                                   *)
(* ------------------------------------------------------------------ *)

(** True if [node] is a non-empty block (non-flow) sequence or mapping. Such
    nodes require their own lines and cannot be written inline. *)
let is_block_collection = function
  | Sequence_node { flow = false; items = _ :: _; _ } -> true
  | Mapping_node { flow = false; pairs = _ :: _; _ } -> true
  | _ -> false

(** Serialize [node] as a block-context value at the given indentation [level].

    Returns [(needs_own_line, content)] where:
    - [needs_own_line = false]: the caller should write ["key: " ^ content] or
      ["- " ^ content]; [content] ends with ['\n'].
    - [needs_own_line = true]: the caller should write ["key:\n" ^ content] or
      ["-\n" ^ content]; [content] starts with the first item line at [level]
      and ends with ['\n']. *)
let rec block_value ~level node =
  match node with
  | Alias_node { name; line_comment; _ } ->
      (false, "*" ^ name ^ emit_lc line_comment ^ "\n")
  | Scalar_node { anchor; tag; value; style; line_comment; _ } ->
      let pre = pp_anchor anchor ^ pp_tag tag in
      let body =
        match style with
        | Plain -> value ^ emit_lc line_comment ^ "\n"
        | Single_quoted ->
            single_quoted_body value ^ emit_lc line_comment ^ "\n"
        | Double_quoted ->
            double_quoted_body value ^ emit_lc line_comment ^ "\n"
        | Literal -> block_scalar ~lc:line_comment "|" value level
        | Folded -> block_scalar ~lc:line_comment ">" value level
      in
      (false, pre ^ body)
  | Sequence_node
      { anchor; tag; items; flow = is_flow; line_comment; foot_comments; _ } ->
      let pre = pp_anchor anchor ^ pp_tag tag in
      if is_flow || items = [] then
        ( false,
          pre ^ "["
          ^ String.concat ", " (List.map flow items)
          ^ "]" ^ emit_lc line_comment ^ "\n" )
      else begin
        let b = Buffer.create 64 in
        (* If there is an anchor or tag, emit it on its own line first *)
        if pre <> "" then Buffer.add_string b (String.trim pre ^ "\n");
        List.iter (fun item -> Buffer.add_string b (seq_item ~level item)) items;
        List.iter
          (fun fc -> Buffer.add_string b (indent level ^ "# " ^ fc ^ "\n"))
          foot_comments;
        (true, Buffer.contents b)
      end
  | Mapping_node
      { anchor; tag; pairs; flow = is_flow; line_comment; foot_comments; _ } ->
      let pre = pp_anchor anchor ^ pp_tag tag in
      if is_flow || pairs = [] then
        ( false,
          pre ^ "{"
          ^ String.concat ", "
              (List.map (fun (k, v) -> flow k ^ ": " ^ flow v) pairs)
          ^ "}" ^ emit_lc line_comment ^ "\n" )
      else begin
        let b = Buffer.create 64 in
        if pre <> "" then Buffer.add_string b (String.trim pre ^ "\n");
        List.iter
          (fun (k, v) -> Buffer.add_string b (map_pair ~level k v))
          pairs;
        List.iter
          (fun fc -> Buffer.add_string b (indent level ^ "# " ^ fc ^ "\n"))
          foot_comments;
        (true, Buffer.contents b)
      end

(** Emit one block sequence item at indentation [level]. The returned string
    includes the item's indentation, the ["-"] or ["- "] prefix, the value, and
    a trailing ['\n']. *)
and seq_item ~level item =
  let ind = indent level in
  let heads = emit_heads ~level (node_head_comments item) in
  if is_block_collection item then begin
    (* Nested block collection: write "-\n" then items one level deeper *)
    let _, content = block_value ~level:(level + 1) item in
    heads ^ ind ^ "-\n" ^ content
  end
  else begin
    let _, content = block_value ~level:(level + 1) item in
    heads ^ ind ^ "- " ^ content
  end

(** Emit one block mapping pair at indentation [level]. *)
and map_pair ~level key value =
  let ind = indent level in
  let key_heads = emit_heads ~level (node_head_comments key) in
  (* Mapping keys are always written in inline form; block styles fall
     back to double-quoted since they cannot span multiple lines in a key
     position.  Complex (non-scalar) keys are serialized as flow nodes. *)
  let key_str =
    match key with
    | Scalar_node { anchor; tag; value = v; style; _ } -> (
        pp_anchor anchor ^ pp_tag tag
        ^
        match style with
        | Plain -> v
        | Single_quoted -> single_quoted_body v
        | Double_quoted -> double_quoted_body v
        | Literal
        | Folded ->
            double_quoted_body v)
    | Alias_node { name; _ } -> "*" ^ name
    | _ -> flow key (* complex key as flow node *)
  in
  let nl, val_str = block_value ~level:(level + 1) value in
  if nl then begin
    (* Non-empty block collection value: key on its own line.
       Emit any head comments of the value between "key:\n" and the content. *)
    let val_heads = emit_heads ~level:(level + 1) (node_head_comments value) in
    key_heads ^ ind ^ key_str ^ ":\n" ^ val_heads ^ val_str
  end
  else key_heads ^ ind ^ key_str ^ ": " ^ val_str

(* ------------------------------------------------------------------ *)
(* Plain normalization                                                   *)
(* ------------------------------------------------------------------ *)

exception Plain_error of string
(** Raised by {!to_plain_yaml} when the input contains a feature that plain YAML
    does not allow: an explicit tag or a complex mapping key. *)

(** Normalize a node for plain-YAML output:
    - Expand aliases (substitute the resolved node recursively).
    - Strip all anchor declarations.
    - If [strict], raise [Plain_error] on any explicit tag; otherwise strip the
      tag silently.
    - Raise [Plain_error] on any complex (non-scalar) mapping key.
    - Convert all flow collections to block style. *)
let tick ~limit ~counter =
  incr counter;
  if !counter > limit then raise (Types.Expansion_limit_exceeded limit)

let rec normalize_plain ~strict ~limit ~counter node =
  tick ~limit ~counter;
  match node with
  | Alias_node { resolved; _ } ->
      normalize_plain ~strict ~limit ~counter resolved
  | Scalar_node { tag = Some t; _ } when strict ->
      raise
        (Plain_error
           (Printf.sprintf "tags are not allowed in plain YAML (tag: %s)" t))
  | Scalar_node r -> Scalar_node { r with anchor = None; tag = None }
  | Sequence_node { tag = Some t; _ } when strict ->
      raise
        (Plain_error
           (Printf.sprintf "tags are not allowed in plain YAML (tag: %s)" t))
  | Sequence_node r ->
      Sequence_node
        {
          r with
          anchor = None;
          tag = None;
          flow = false;
          items = List.map (normalize_plain ~strict ~limit ~counter) r.items;
        }
  | Mapping_node { tag = Some t; _ } when strict ->
      raise
        (Plain_error
           (Printf.sprintf "tags are not allowed in plain YAML (tag: %s)" t))
  | Mapping_node r ->
      let pairs =
        List.map
          (fun (k, v) ->
            let k' = normalize_plain ~strict ~limit ~counter k in
            (match k' with
            | Sequence_node _
            | Mapping_node _ ->
                raise
                  (Plain_error
                     "complex mapping keys are not allowed in plain YAML")
            | _ -> ());
            (k', normalize_plain ~strict ~limit ~counter v))
          r.pairs
      in
      Mapping_node { r with anchor = None; tag = None; flow = false; pairs }

(* ------------------------------------------------------------------ *)
(* Document / stream                                                     *)
(* ------------------------------------------------------------------ *)

(** Serialize a list of YAML documents into a single string. The first document
    is emitted without a [---] marker unless it is preceded by a tag-directive
    or version-directive (not tracked here). Every subsequent document is
    preceded by [---]. *)
let to_yaml (docs : node list) : string =
  let b = Buffer.create 256 in
  List.iteri
    (fun i doc ->
      let heads = emit_heads ~level:0 (node_head_comments doc) in
      let nl, content = block_value ~level:0 doc in
      if i = 0 then begin
        (* First document: no separator marker needed *)
        Buffer.add_string b heads;
        Buffer.add_string b content
      end
      else if nl then begin
        (* Subsequent document, block collection *)
        Buffer.add_string b heads;
        Buffer.add_string b "---\n";
        Buffer.add_string b content
      end
      else begin
        (* Subsequent document, inline value *)
        Buffer.add_string b heads;
        Buffer.add_string b "--- ";
        Buffer.add_string b content
      end)
    docs;
  Buffer.contents b

(** Like {!to_yaml} but restricted to plain YAML: aliases are expanded, anchor
    declarations are stripped, tags are stripped (or raise {!Plain_error} when
    [strict = true]), complex mapping keys raise {!Plain_error}, and all flow
    collections are converted to block style. *)
let to_plain_yaml ?(strict = false)
    ?(expansion_limit = Types.default_expansion_limit) (docs : node list) :
    string =
  let counter = ref 0 in
  to_yaml
    (List.map (normalize_plain ~strict ~limit:expansion_limit ~counter) docs)
