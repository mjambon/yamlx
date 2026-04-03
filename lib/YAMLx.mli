(** YAMLx — pure-OCaml YAML 1.2 parser.

    Typical usage:
    {[
      (* Parse a multi-document YAML string into typed values *)
      let values = YAMLx.of_string "answer: 42\nflag: true"
      (* → [Map [(String "answer", Int 42L); (String "flag", Bool true)]] *)

      (* Parse preserving the full AST (tags, anchors, source positions) *)
      let nodes = YAMLx.parse_nodes "- foo\n- bar"

      (* Non-raising variant *)
      match YAMLx.of_string_result input with
      | Ok values -> ...
      | Error msg -> ...
    ]}

    Errors are reported by raising {!Scan_error} or {!Parse_error}. Use
    {!of_string_result} to get a [result] instead. *)

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
    (tokenisation) stage. *)

exception Parse_error of yaml_error
(** Raised when the token stream does not conform to the YAML grammar. *)

exception Expansion_limit_exceeded of int
(** Raised when alias expansion visits more than the configured number of nodes.
    The payload is the limit that was exceeded. Use [~expansion_limit] on
    {!of_string}, {!one_of_string}, {!of_string_result}, or {!to_plain_yaml} to
    adjust the threshold. See also {!default_expansion_limit}. *)

val default_expansion_limit : int
(** Default node-visit budget used by all alias-expanding functions (1,000,000).
*)

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

    {b Comment preservation} is a best-effort heuristic. Comments inside flow
    collections are dropped. The attachment rules and field semantics may change
    in future versions.

    - [head_comments]: standalone comment lines that appear immediately before
      the node in the source. Each string is one comment line's text, without
      the leading ['#'] character.
    - [line_comment]: a comment appearing on the same source line as the node,
      after its content. Text does not include the leading ['#'].
    - [foot_comments] (collections only): standalone comment lines appearing
      after the last child of the collection, before the next sibling. *)
type node =
  | Scalar_node of {
      anchor : string option;  (** [&name] if present *)
      tag : string option;  (** resolved tag URI if present *)
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
      flow : bool;  (** true for [[a, b]] style, false for block *)
      loc : loc;
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
      head_comments : string list;
      line_comment : string option;
      foot_comments : string list;
    }
  | Alias_node of {
      name : string;  (** the anchor name, without the [*] *)
      resolved : node;
      loc : loc;
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
    - Everything else, and all quoted or block scalars → {!String} *)
type value =
  | Null
  | Bool of bool
  | Int of int64
  | Float of float
  | String of string
  | Seq of value list
  | Map of (value * value) list
[@@deriving eq, show]

(** {1 Parsing} *)

val parse_nodes : string -> node list
(** Parse [input] and return one {!node} per YAML document. Use this when you
    need tags, anchors, scalar styles, or source positions. For simple data
    extraction, prefer {!of_string}. Raises {!Scan_error} or {!Parse_error} on
    malformed input. *)

val to_yaml : node list -> string
(** Serialize [docs] back into a YAML string. Scalar styles ([Plain],
    [Single_quoted], [Double_quoted], [Literal], [Folded]) and collection
    flow/block style are preserved. The output is valid YAML 1.2 that
    round-trips through {!parse_nodes} to equivalent nodes. *)

exception Plain_error of string
(** Raised by {!to_plain_yaml} when the input uses a feature that plain YAML
    does not allow: an explicit tag or a complex (non-scalar) mapping key. *)

val to_plain_yaml : ?strict:bool -> ?expansion_limit:int -> node list -> string
(** Like {!to_yaml} but restricted to a plain subset of YAML:
    - Aliases are expanded (the resolved node is substituted in place).
    - Anchor declarations are stripped.
    - Tags are stripped unless [~strict:true], in which case they raise
      {!Plain_error}.
    - Complex (non-scalar) mapping keys always raise {!Plain_error}.
    - Flow collections are converted to block style.

    [expansion_limit] caps the total number of nodes visited during alias
    expansion; raises {!Expansion_limit_exceeded} if exceeded. Defaults to
    {!default_expansion_limit}.

    The result contains only scalars, block sequences, and block mappings with
    scalar keys — no YAML-specific features. *)

val of_string : ?expansion_limit:int -> string -> value list
(** Parse [input] and resolve each document to a typed {!value} using the YAML
    1.2 JSON schema. Raises {!Scan_error} or {!Parse_error} on malformed input.
    Raises {!Expansion_limit_exceeded} if alias expansion exceeds
    [expansion_limit] (default: {!default_expansion_limit}). *)

val one_of_string : ?expansion_limit:int -> string -> value
(** Parse [input] and return the first document's value. Raises [Not_found] if
    the stream contains no documents. Raises {!Scan_error} or {!Parse_error} on
    malformed input. Raises {!Expansion_limit_exceeded} if alias expansion
    exceeds [expansion_limit] (default: {!default_expansion_limit}). *)

(** {1 Error handling} *)

val string_of_error : yaml_error -> string
(** Format a {!yaml_error} as ["line L, column C: message"]. *)

val of_string_result :
  ?expansion_limit:int -> string -> (value list, string) result
(** Like {!of_string} but returns [Ok values] or [Error msg] instead of raising
    exceptions. {!Expansion_limit_exceeded} is also converted to [Error]. *)

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
(** Parse [input] and return the raw event list. Used internally by the test
    suite to compare against yaml-test-suite expected output. Not part of the
    stable public API. *)

val events_to_tree : event list -> string
(** Render an event list as a yaml-test-suite tree string. Each event becomes
    one line; the result ends with a newline. *)

val diff_event_trees : expected:string -> actual:string -> string option
(** Compare two tree strings and return a human-readable description of the
    first difference, or [None] if they are equal. *)

(**/**)
