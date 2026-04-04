(** YAMLx — pure-OCaml YAML 1.2 parser.

    Typical usage:
    {[
      (* Parse into typed values — safe variant *)
      match YAMLx.Values.of_yaml "answer: 42\nflag: true" with
      | Ok docs  -> (* process docs *)
      | Error msg -> (* handle error *)

      (* Parse into typed values — raising variant *)
      let docs = YAMLx.Values.of_yaml_exn "answer: 42\nflag: true"

      (* Parse preserving the full AST (tags, anchors, positions) *)
      let nodes = YAMLx.Nodes.of_yaml_exn "- foo\n- bar"

      (* Serialize back to YAML *)
      let yaml = YAMLx.Nodes.to_yaml nodes
    ]}

    Scan and parse errors are reported by raising {!Scan_error} or
    {!Parse_error}. Depth and expansion limits raise {!Depth_limit_exceeded} or
    {!Expansion_limit_exceeded}. Use {!Values.of_yaml} to get a [result] instead
    of raising. *)

(** {1 Source positions} *)

type pos = {
  line : int;
  column : int;  (** 0-based Unicode codepoint column *)
  column_bytes : int;  (** 0-based UTF-8 byte column *)
  offset : int;  (** codepoint index from the start of the input *)
  offset_bytes : int;  (** UTF-8 byte offset from the start of the input *)
}
[@@deriving show]
(** A location in the YAML source text. [line] is 1-based. [column] and
    [column_bytes] are 0-based distances from the start of the line, in
    codepoints and UTF-8 bytes respectively. [offset] and [offset_bytes] are
    absolute distances from the start of the input, measured the same way. The
    byte fields make it easy to slice the original [string] without re-encoding.
*)

type loc = { start_pos : pos; end_pos : pos } [@@deriving show]
(** A source range. [start_pos] is the first character of the node; [end_pos] is
    the position immediately after the last character. *)

(** {1 Errors} *)

type yaml_error = { msg : string; pos : pos }

exception Scan_error of yaml_error
(** Raised when the input contains invalid YAML syntax at the scanning
    (tokenization) stage. *)

exception Parse_error of yaml_error
(** Raised when the token stream does not conform to the YAML grammar. *)

exception Expansion_limit_exceeded of int
(** Raised when alias expansion visits more nodes than allowed by
    [~expansion_limit]. The payload is the limit that was exceeded. See
    {!default_expansion_limit}. *)

exception Depth_limit_exceeded of int
(** Raised when the YAML nesting depth exceeds [~max_depth] during parsing. The
    payload is the limit that was exceeded. See {!default_max_depth}. *)

exception Plain_error of string
(** Raised by {!Nodes.to_plain_yaml_exn} when the input uses a feature that
    plain YAML does not support: an explicit tag or a complex mapping key. *)

val catch_errors : (unit -> 'a) -> ('a, string) result
(** Catch the exceptions exposed by this module and turn them into nice error
    messages. *)

val register_exception_printers : unit -> unit
(** Register nice exception printers for the exceptions exposed by this module.
*)

val default_expansion_limit : int
(** Default node-visit budget for alias expansion (1,000,000). *)

val default_max_depth : int
(** Default maximum nesting depth (512). *)

val string_of_error : yaml_error -> string
(** Format a {!yaml_error} as ["line L, column C: message"]. *)

(** {1 Scalar styles} *)

(** How a scalar value was written in the source. Preserved in AST nodes so
    callers can distinguish, for example, a quoted empty string from an unquoted
    null. *)
type scalar_style =
  | Plain  (** unquoted, e.g. [foo] *)
  | Single_quoted  (** e.g. ['foo'] *)
  | Double_quoted  (** e.g. ["foo"] *)
  | Literal  (** block scalar [|]: newlines preserved *)
  | Folded  (** block scalar [>]: newlines folded to spaces *)
[@@deriving show]

(** {1 AST nodes} *)

(** An in-memory representation of a parsed YAML document that preserves all
    source-level detail: tags, anchors, scalar styles, source positions, and —
    on a best-effort basis — comments.

    {b Comment preservation} is best-effort. Comments inside flow collections
    are dropped. The attachment rules may change in future versions.

    - [head_comments]: standalone comment lines immediately before the node.
      Each string is one comment line's text, without the leading ['#'].
    - [line_comment]: a comment on the same source line as the node, after its
      content. Text does not include the leading ['#'].
    - [foot_comments] (collections only): standalone comment lines after the
      last child of the collection, before the next sibling. *)
type node =
  | Scalar_node of {
      anchor : string option;  (** [&name] if present *)
      tag : string option;  (** resolved tag URI if present *)
      value : string;
      style : scalar_style;
      loc : loc;
      height : int;  (** always 1 for scalars *)
      head_comments : string list;
      line_comment : string option;
    }
  | Sequence_node of {
      anchor : string option;
      tag : string option;
      items : node list;
      flow : bool;  (** true for [[a, b]] style, false for block *)
      loc : loc;
      height : int;  (** 1 + max height of items, or 1 if empty *)
      head_comments : string list;
      line_comment : string option;
      foot_comments : string list;
    }
  | Mapping_node of {
      anchor : string option;
      tag : string option;
      pairs : (node * node) list;
      flow : bool;  (** true for [{a: b}] style, false for block *)
      loc : loc;
      height : int;  (** 1 + max height of keys and values, or 1 if empty *)
      head_comments : string list;
      line_comment : string option;
      foot_comments : string list;
    }
  | Alias_node of {
      name : string;  (** the anchor name, without the [*] *)
      resolved : node;
      loc : loc;
      height : int;  (** 1 + height of the resolved node *)
      head_comments : string list;
      line_comment : string option;
    }
[@@deriving show]

(** {1 Typed values} *)

(** A YAML value resolved according to the YAML 1.2 JSON schema.

    Plain (unquoted) scalars are matched against the following patterns:
    - [null], [Null], [NULL], [~], or empty string → {!Null}
    - [true]/[True]/[TRUE]/[false]/[False]/[FALSE] → {!Bool}
    - Decimal, [0x…] hex, or [0o…] octal integers → {!Int}
    - Decimal or scientific floats; [.inf], [.nan] variants → {!Float}
    - Everything else, and all quoted or block scalars → {!String}

    Each constructor carries a {!loc} giving the source range of the
    corresponding YAML node. Use {!Values.equal} for location-independent
    structural equality. *)
type value =
  | Null of loc
  | Bool of loc * bool
  | Int of loc * int64
  | Float of loc * float
  | String of loc * string
  | Seq of loc * value list
  | Map of loc * (loc * value * value) list
[@@deriving show]

(** {1 Node operations} *)

module Nodes : sig
  type t = node list
  (** One {!node} per YAML document in the input stream. *)

  val of_yaml : ?max_depth:int -> string -> (t, string) result
  (** Like {!of_yaml_exn} but returns [Ok nodes] on success or [Error msg] on
      any failure. Does not raise. *)

  val of_yaml_exn : ?max_depth:int -> string -> t
  (** Parse a YAML string and return one node per document. Use this when you
      need tags, anchors, scalar styles, source positions, or comments. For
      plain data extraction prefer {!Values.of_yaml_exn}.

      Raises {!Scan_error} on invalid YAML syntax. Raises {!Parse_error} on a
      malformed token stream. Raises {!Depth_limit_exceeded} when nesting
      exceeds [max_depth] (default: {!default_max_depth}). *)

  val to_yaml : t -> string
  (** Serialize nodes back to a YAML string. Scalar styles and flow/block mode
      are preserved. The output round-trips through {!of_yaml_exn} to equivalent
      nodes. Does not raise. *)

  val to_plain_yaml_exn : ?strict:bool -> ?expansion_limit:int -> t -> string
  (** Like {!to_yaml} but produces a plain subset of YAML:
      - Aliases are expanded; anchor declarations are stripped.
      - Tags are stripped unless [~strict:true], in which case they raise
        {!Plain_error}.
      - Complex (non-scalar) mapping keys raise {!Plain_error}.
      - Flow collections are converted to block style.

      Raises {!Plain_error} on unsupported features (see above). Raises
      {!Expansion_limit_exceeded} when alias expansion exceeds [expansion_limit]
      (default: {!default_expansion_limit}). *)

  (**/**)

  val height : node -> int
  (** Precomputed subtree height (O(1)). Internal — subject to change. *)

  (**/**)
end

(** {1 Value operations} *)

module Values : sig
  type t = value list
  (** One {!value} per YAML document in the input stream. *)

  val of_yaml :
    ?max_depth:int -> ?expansion_limit:int -> string -> (t, string) result
  (** Parse a YAML string and resolve each document to a typed value. Returns
      [Ok docs] on success or [Error msg] on any failure, including scan errors,
      parse errors, and limit violations. Does not raise. *)

  val of_yaml_exn : ?max_depth:int -> ?expansion_limit:int -> string -> t
  (** Like {!of_yaml} but raises instead of returning a result.

      Raises {!Scan_error} on invalid YAML syntax. Raises {!Parse_error} on a
      malformed token stream. Raises {!Depth_limit_exceeded} when nesting
      exceeds [max_depth] (default: {!default_max_depth}). Raises
      {!Expansion_limit_exceeded} when alias expansion exceeds [expansion_limit]
      (default: {!default_expansion_limit}). *)

  val one_of_yaml_exn :
    ?max_depth:int -> ?expansion_limit:int -> string -> value
  (** Parse a YAML string expecting exactly one document and return its value.
      Raises [Invalid_argument] if the input contains zero or more than one
      document.

      Raises {!Scan_error}, {!Parse_error}, {!Depth_limit_exceeded}, or
      {!Expansion_limit_exceeded} on errors (same conditions as {!of_yaml_exn}).
  *)

  val equal : value -> value -> bool
  (** Structural equality that ignores source locations. Two values are equal
      when they represent the same YAML data regardless of where they appear in
      the source. *)

  (**/**)

  val height : value -> int
  (** Compute subtree height (O(n)). Internal — subject to change. *)

  (**/**)
end

(**/**)

(** {1 Event stream — internal / subject to change} *)

type event_kind =
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

type event = { kind : event_kind; start_pos : pos; end_pos : pos }

val parse_events : string -> event list
val events_to_tree : event list -> string
val diff_event_trees : expected:string -> actual:string -> string option

(**/**)
