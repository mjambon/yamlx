(** YAML pretty-printer.
    Converts a [Types.node list] (one node per document) back into a YAML
    string.  Scalar styles ([Plain], [Single_quoted], [Double_quoted],
    [Literal], [Folded]) and collection flow/block style are preserved. *)

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
  | None   -> ""
  | Some a -> "&" ^ a ^ " "

let pp_tag = function
  | None   -> ""
  | Some t ->
    let prefix = "tag:yaml.org,2002:" in
    let plen   = String.length prefix in
    if String.length t > plen && String.sub t 0 plen = prefix then
      "!!" ^ String.sub t plen (String.length t - plen) ^ " "
    else
      "!<" ^ t ^ "> "

(* ------------------------------------------------------------------ *)
(* Scalar encoding                                                       *)
(* ------------------------------------------------------------------ *)

(** Encode a value for a single-quoted scalar (content between the
    quotes).  Embedded single quotes are doubled. *)
let single_quoted_body s =
  let b = Buffer.create (String.length s + 4) in
  String.iter (fun c ->
    if c = '\'' then Buffer.add_string b "''"
    else Buffer.add_char b c) s;
  "'" ^ Buffer.contents b ^ "'"

(** Encode a value for a double-quoted scalar (content between the
    quotes).  Special characters are replaced with backslash escapes. *)
let double_quoted_body s =
  let b = Buffer.create (String.length s + 4) in
  String.iter (fun c ->
    match c with
    | '"'  -> Buffer.add_string b "\\\""
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
    | c when Char.code c < 0x20 ->
      Printf.bprintf b "\\x%02X" (Char.code c)
    | c -> Buffer.add_char b c) s;
  "\"" ^ Buffer.contents b ^ "\""

(* ------------------------------------------------------------------ *)
(* Block scalar emission                                                 *)
(* ------------------------------------------------------------------ *)

(** Count trailing ['\n'] characters in [s]. *)
let count_trailing_newlines s =
  let n = String.length s in
  let i = ref (n - 1) in
  while !i >= 0 && s.[!i] = '\n' do decr i done;
  n - 1 - !i

(** Return the block scalar header + indented body as a string.
    [marker] is ["|"] or [">"].
    Content is indented to [(level + 1) * spaces_per_level] columns.
    The returned string includes the header line and all body lines,
    each terminated by ['\n']. *)
let block_scalar marker s level =
  let n     = String.length s in
  let trail = count_trailing_newlines s in
  let main  = String.sub s 0 (n - trail) in
  let chomp =
    if trail = 0 then "-"
    else if trail = 1 then ""    (* clip — the default *)
    else "+"                     (* keep *)
  in
  let ind_n   = (level + 1) * spaces_per_level in
  let ind     = String.make ind_n ' ' in
  (* Explicit indentation indicator: required when first content char is
     a space (otherwise the parser would mistake it for indentation). *)
  let ind_ind =
    if String.length main > 0 && main.[0] = ' ' then string_of_int ind_n
    else ""
  in
  let b = Buffer.create (n + 16) in
  Buffer.add_string b (marker ^ ind_ind ^ chomp ^ "\n");
  let lines = String.split_on_char '\n' main in
  List.iter (fun line ->
    if line = "" then Buffer.add_char b '\n'
    else (Buffer.add_string b ind; Buffer.add_string b line;
          Buffer.add_char b '\n')
  ) lines;
  (* Extra blank lines for "keep" chomp *)
  for _ = 2 to trail do Buffer.add_char b '\n' done;
  Buffer.contents b

(* ------------------------------------------------------------------ *)
(* Flow serialisation                                                    *)
(* ------------------------------------------------------------------ *)

(** Serialise [node] in flow (inline) context.
    [Literal] and [Folded] scalars fall back to double-quoted since block
    styles are not allowed inside flow collections. *)
let rec flow node =
  match node with
  | Scalar_node { anchor; tag; value; style; _ } ->
    let pre  = pp_anchor anchor ^ pp_tag tag in
    let body = match style with
      | Plain         -> value
      | Single_quoted -> single_quoted_body value
      | Double_quoted -> double_quoted_body  value
      | Literal | Folded -> double_quoted_body value
    in
    pre ^ body
  | Sequence_node { anchor; tag; items; _ } ->
    pp_anchor anchor ^ pp_tag tag ^
    "[" ^ String.concat ", " (List.map flow items) ^ "]"
  | Mapping_node { anchor; tag; pairs; _ } ->
    pp_anchor anchor ^ pp_tag tag ^
    "{" ^ String.concat ", "
            (List.map (fun (k, v) -> flow k ^ ": " ^ flow v) pairs)
    ^ "}"
  | Alias_node { name; _ } -> "*" ^ name

(* ------------------------------------------------------------------ *)
(* Block serialisation                                                   *)
(* ------------------------------------------------------------------ *)

(** True if [node] is a non-empty block (non-flow) sequence or mapping.
    Such nodes require their own lines and cannot be written inline. *)
let is_block_collection = function
  | Sequence_node { flow = false; items = _ :: _; _ } -> true
  | Mapping_node  { flow = false; pairs = _ :: _; _ } -> true
  | _ -> false

(** Serialise [node] as a block-context value at the given indentation
    [level].

    Returns [(needs_own_line, content)] where:
    - [needs_own_line = false]: the caller should write ["key: " ^ content]
      or ["- " ^ content]; [content] ends with ['\n'].
    - [needs_own_line = true]: the caller should write ["key:\n" ^ content]
      or ["-\n" ^ content]; [content] starts with the first item line at
      [level] and ends with ['\n']. *)
let rec block_value ~level node =
  match node with
  | Alias_node { name; _ } ->
    (false, "*" ^ name ^ "\n")

  | Scalar_node { anchor; tag; value; style; _ } ->
    let pre  = pp_anchor anchor ^ pp_tag tag in
    let body = match style with
      | Plain         -> value ^ "\n"
      | Single_quoted -> single_quoted_body value ^ "\n"
      | Double_quoted -> double_quoted_body  value ^ "\n"
      | Literal       -> block_scalar "|" value level
      | Folded        -> block_scalar ">" value level
    in
    (false, pre ^ body)

  | Sequence_node { anchor; tag; items; flow = is_flow; _ } ->
    let pre = pp_anchor anchor ^ pp_tag tag in
    if is_flow || items = [] then
      (false,
       pre ^ "[" ^ String.concat ", " (List.map flow items) ^ "]\n")
    else begin
      let b = Buffer.create 64 in
      (* If there is an anchor or tag, emit it on its own line first *)
      if pre <> "" then
        Buffer.add_string b (String.trim pre ^ "\n");
      List.iter (fun item ->
        Buffer.add_string b (seq_item ~level item)) items;
      (true, Buffer.contents b)
    end

  | Mapping_node { anchor; tag; pairs; flow = is_flow; _ } ->
    let pre = pp_anchor anchor ^ pp_tag tag in
    if is_flow || pairs = [] then
      (false,
       pre ^ "{" ^
       String.concat ", "
         (List.map (fun (k, v) -> flow k ^ ": " ^ flow v) pairs)
       ^ "}\n")
    else begin
      let b = Buffer.create 64 in
      if pre <> "" then
        Buffer.add_string b (String.trim pre ^ "\n");
      List.iter (fun (k, v) ->
        Buffer.add_string b (map_pair ~level k v)) pairs;
      (true, Buffer.contents b)
    end

(** Emit one block sequence item at indentation [level].
    The returned string includes the item's indentation, the ["-"] or
    ["- "] prefix, the value, and a trailing ['\n']. *)
and seq_item ~level item =
  let ind = indent level in
  if is_block_collection item then begin
    (* Nested block collection: write "-\n" then items one level deeper *)
    let (_, content) = block_value ~level:(level + 1) item in
    ind ^ "-\n" ^ content
  end else begin
    let (_, content) = block_value ~level:(level + 1) item in
    ind ^ "- " ^ content
  end

(** Emit one block mapping pair at indentation [level]. *)
and map_pair ~level key value =
  let ind = indent level in
  (* Mapping keys are always written in inline form; block styles fall
     back to double-quoted since they cannot span multiple lines in a key
     position.  Complex (non-scalar) keys are serialised as flow nodes. *)
  let key_str = match key with
    | Scalar_node { anchor; tag; value = v; style; _ } ->
      pp_anchor anchor ^ pp_tag tag ^
      (match style with
       | Plain         -> v
       | Single_quoted -> single_quoted_body v
       | Double_quoted -> double_quoted_body  v
       | Literal | Folded -> double_quoted_body v)
    | Alias_node { name; _ } -> "*" ^ name
    | _ -> flow key  (* complex key as flow node *)
  in
  let (nl, val_str) = block_value ~level:(level + 1) value in
  if nl then
    (* Non-empty block collection value: key on its own line *)
    ind ^ key_str ^ ":\n" ^ val_str
  else
    ind ^ key_str ^ ": " ^ val_str

(* ------------------------------------------------------------------ *)
(* Plain normalisation                                                   *)
(* ------------------------------------------------------------------ *)

(** Raised by {!to_plain_yaml} when the input contains a feature that
    plain YAML does not allow: an explicit tag or a complex mapping key. *)
exception Plain_error of string

(** Normalise a node for plain-YAML output:
    - Expand aliases (substitute the resolved node recursively).
    - Strip all anchor declarations.
    - Raise [Plain_error] on any explicit tag.
    - Raise [Plain_error] on any complex (non-scalar) mapping key.
    - Convert all flow collections to block style. *)
let rec normalise_plain node =
  match node with
  | Alias_node { resolved; _ } ->
    normalise_plain resolved

  | Scalar_node { tag = Some t; _ } ->
    raise (Plain_error (Printf.sprintf "tags are not allowed in plain YAML (tag: %s)" t))

  | Scalar_node r ->
    Scalar_node { r with anchor = None }

  | Sequence_node { tag = Some t; _ } ->
    raise (Plain_error (Printf.sprintf "tags are not allowed in plain YAML (tag: %s)" t))

  | Sequence_node r ->
    Sequence_node { r with anchor = None;
                            flow   = false;
                            items  = List.map normalise_plain r.items }

  | Mapping_node { tag = Some t; _ } ->
    raise (Plain_error (Printf.sprintf "tags are not allowed in plain YAML (tag: %s)" t))

  | Mapping_node r ->
    let pairs = List.map (fun (k, v) ->
      let k' = normalise_plain k in
      (match k' with
      | Sequence_node _ | Mapping_node _ ->
        raise (Plain_error "complex mapping keys are not allowed in plain YAML")
      | _ -> ());
      (k', normalise_plain v)
    ) r.pairs in
    Mapping_node { r with anchor = None; flow = false; pairs }

(* ------------------------------------------------------------------ *)
(* Document / stream                                                     *)
(* ------------------------------------------------------------------ *)

(** Serialise a list of YAML documents into a single string.
    The first document is emitted without a [---] marker unless it is
    preceded by a tag-directive or version-directive (not tracked here).
    Every subsequent document is preceded by [---]. *)
let to_yaml (docs : node list) : string =
  let b = Buffer.create 256 in
  List.iteri (fun i doc ->
    let (nl, content) = block_value ~level:0 doc in
    if i = 0 then begin
      (* First document: no separator marker needed *)
      Buffer.add_string b content
    end else if nl then begin
      (* Subsequent document, block collection *)
      Buffer.add_string b "---\n";
      Buffer.add_string b content
    end else begin
      (* Subsequent document, inline value *)
      Buffer.add_string b "--- ";
      Buffer.add_string b content
    end
  ) docs;
  Buffer.contents b

(** Like {!to_yaml} but restricted to plain YAML:
    aliases are expanded, anchor declarations are stripped, explicit tags
    raise {!Plain_error}, complex mapping keys raise {!Plain_error}, and
    all flow collections are converted to block style. *)
let to_plain_yaml (docs : node list) : string =
  to_yaml (List.map normalise_plain docs)
