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

    The full URI for the standard types is [tag:yaml.org,2002:XXX].
    Explicit [!!str], [!!int], etc. tags override the schema resolution. *)

open Types

(* ------------------------------------------------------------------ *)
(* Pattern matching helpers                                             *)
(* ------------------------------------------------------------------ *)

let is_null_str s =
  s = "null" || s = "Null" || s = "NULL" || s = "~" || s = ""

let is_true_str s =
  s = "true" || s = "True" || s = "TRUE"

let is_false_str s =
  s = "false" || s = "False" || s = "FALSE"

(** True if [s] is a valid decimal integer (possibly with sign). *)
let is_decimal_int s =
  let n = String.length s in
  if n = 0 then false
  else begin
    let start = if s.[0] = '+' || s.[0] = '-' then 1 else 0 in
    if start >= n then false
    else String.sub s start (n - start)
         |> String.to_seq
         |> Seq.for_all (fun c -> c >= '0' && c <= '9')
  end

(** True if [s] is a hex integer (0x…). *)
let is_hex_int s =
  let n = String.length s in
  n > 2 && s.[0] = '0' && (s.[1] = 'x' || s.[1] = 'X')
  && String.sub s 2 (n - 2)
     |> String.to_seq
     |> Seq.for_all Char_class.(fun c -> is_hex_digit (Char.code c))

(** True if [s] is an octal integer (0o…). *)
let is_octal_int s =
  let n = String.length s in
  n > 2 && s.[0] = '0' && (s.[1] = 'o' || s.[1] = 'O')
  && String.sub s 2 (n - 2)
     |> String.to_seq
     |> Seq.for_all (fun c -> c >= '0' && c <= '7')

(** Try to parse a float.  Returns [None] for non-numeric strings. *)
let try_float s =
  if s = ".inf" || s = ".Inf" || s = ".INF" then Some Float.infinity
  else if s = "+.inf" || s = "+.Inf" || s = "+.INF" then Some Float.infinity
  else if s = "-.inf" || s = "-.Inf" || s = "-.INF" then Some Float.neg_infinity
  else if s = ".nan" || s = ".NaN" || s = ".NAN" then Some Float.nan
  else
    match float_of_string_opt s with
    | Some f when String.contains s '.' || String.contains s 'e'
               || String.contains s 'E' -> Some f
    | _ -> None

(* ------------------------------------------------------------------ *)
(* Tag resolution                                                        *)
(* ------------------------------------------------------------------ *)

(** Canonical tag URI prefix for YAML standard types. *)
let yaml_prefix = "tag:yaml.org,2002:"

(** Resolve a plain scalar's tag according to the JSON schema.
    Returns the canonical tag URI. *)
let resolve_plain_tag s =
  if is_null_str s then yaml_prefix ^ "null"
  else if is_true_str s || is_false_str s then yaml_prefix ^ "bool"
  else if is_decimal_int s || is_hex_int s || is_octal_int s then
    yaml_prefix ^ "int"
  else
    match try_float s with
    | Some _ -> yaml_prefix ^ "float"
    | None   -> yaml_prefix ^ "str"

(** Determine the effective tag for a scalar.
    [explicit_tag] is [Some uri] if the YAML source had an explicit [!!tag].
    For non-plain scalars without an explicit tag, the tag is always [str]. *)
let effective_tag ~explicit_tag ~style ~value =
  match explicit_tag with
  | Some t -> t   (* explicit tag always wins *)
  | None ->
    (match style with
    | Plain          -> resolve_plain_tag value
    | Single_quoted  -> yaml_prefix ^ "str"
    | Double_quoted  -> yaml_prefix ^ "str"
    | Literal        -> yaml_prefix ^ "str"
    | Folded         -> yaml_prefix ^ "str")

(* ------------------------------------------------------------------ *)
(* Value construction                                                    *)
(* ------------------------------------------------------------------ *)

(** Parse an integer from a YAML scalar value.
    Supports decimal, hexadecimal (0x), and octal (0o) notation. *)
let parse_int s =
  if is_hex_int s then
    Int64.of_string ("0x" ^ String.sub s 2 (String.length s - 2))
  else if is_octal_int s then
    Int64.of_string ("0o" ^ String.sub s 2 (String.length s - 2))
  else
    Int64.of_string s

(** Resolve a scalar node's tag and construct the corresponding [value]. *)
let resolve_scalar ~(explicit_tag : string option) ~(style : scalar_style)
    ~(value : string) : Types.value =
  let tag = effective_tag ~explicit_tag ~style ~value in
  let str_tag   = yaml_prefix ^ "str"   in
  let int_tag   = yaml_prefix ^ "int"   in
  let float_tag = yaml_prefix ^ "float" in
  let bool_tag  = yaml_prefix ^ "bool"  in
  let null_tag  = yaml_prefix ^ "null"  in
  if tag = null_tag then
    Null
  else if tag = bool_tag then
    Bool (is_true_str value)
  else if tag = int_tag then
    (try Int (parse_int value)
     with _ -> String value)
  else if tag = float_tag then
    (match try_float value with
    | Some f -> Float f
    | None   -> String value)
  else if tag = str_tag then
    String value
  else
    (* Unknown tag: pass through as string *)
    String value

(* ------------------------------------------------------------------ *)
(* Full node resolution                                                  *)
(* ------------------------------------------------------------------ *)

(** Resolve an entire node tree into a [value] tree. *)
let rec resolve_node (node : Types.node) : Types.value =
  match node with
  | Scalar_node { tag; value; style; _ } ->
    resolve_scalar ~explicit_tag:tag ~style ~value

  | Sequence_node { items; _ } ->
    Seq (List.map resolve_node items)

  | Mapping_node { pairs; _ } ->
    Map (List.map (fun (k, v) -> (resolve_node k, resolve_node v)) pairs)

  | Alias_node { resolved; _ } ->
    resolve_node resolved

let resolve_documents (nodes : Types.node list) : Types.value list =
  List.map resolve_node nodes
