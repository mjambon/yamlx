(** Core types shared by all YAMLx modules. This module defines the data
    structures that flow through the parsing pipeline: positions and errors,
    then tokens (Scanner → Parser), events (Parser → Composer), and AST nodes
    (Composer → user).

    Pipeline overview: string/channel └─▶ Reader (UTF-8 decoding, line tracking)
    └─▶ Scanner (tokenisation) → token list └─▶ Parser (grammar) → event list
    └─▶ Composer (node building) → node list └─▶ Resolver (type resolution) →
    value list *)

(** {1 Source positions} *)

type pos = {
  line : int;
  column : int;
  offset : int;  (** codepoint index from the start of the input *)
}
(** A location in the source input. [line] is 1-based; [column] is 0-based
    (Unicode codepoints from line start, matching the YAML specification's
    column numbering). *)

val pos_zero : pos

(** {1 Errors} *)

type yaml_error = { msg : string; pos : pos }

exception Scan_error of yaml_error
exception Parse_error of yaml_error

val scan_error : pos -> ('a, unit, string, 'b) format4 -> 'a
val parse_error : pos -> ('a, unit, string, 'b) format4 -> 'a

(** {1 Scalar styles} *)

(** How a scalar was written in the YAML source. The style is preserved through
    the parsing pipeline because it affects tag resolution and round-trip
    fidelity. *)
type scalar_style =
  | Plain  (** unquoted, e.g. [foo] *)
  | Single_quoted  (** e.g. ['foo'] *)
  | Double_quoted  (** e.g. ["foo"] *)
  | Literal  (** block scalar [|]: newlines preserved *)
  | Folded  (** block scalar [>]: newlines folded to spaces *)

(** {1 Tokens — Scanner output} *)

(** A token represents one lexical element. The YAML grammar requires the
    scanner to emit synthetic structural tokens (BLOCK_SEQUENCE_START,
    BLOCK_MAPPING_START, BLOCK_END) in addition to the characters that appear in
    the source. *)
type token_kind =
  | Stream_start
  | Stream_end
  | Directive of string * string
      (** [Directive (name, value)]: e.g. [%YAML 1.2] or [%TAG ! !foo/] *)
  | Document_start  (** [---] *)
  | Document_end  (** [...] *)
  | Block_sequence_start
      (** Synthetic: emitted when a block sequence begins (indentation increases
          to a new level). *)
  | Block_mapping_start  (** Synthetic: emitted when a block mapping begins. *)
  | Block_end
      (** Synthetic: emitted when indentation decreases, closing one or more
          block collections. *)
  | Flow_sequence_start  (** [ *)
  | Flow_sequence_end  (** ] *)
  | Flow_mapping_start  (** { *)
  | Flow_mapping_end  (** } *)
  | Block_entry  (** [-] followed by whitespace or newline *)
  | Flow_entry  (** [,] *)
  | Key  (** [?] (explicit) or synthetic (implicit key) *)
  | Value  (** [:] followed by whitespace or end-of-line *)
  | Alias of string  (** [*name] *)
  | Anchor of string  (** [&name] *)
  | Tag of string * string  (** (handle, suffix) *)
  | Scalar of string * scalar_style

type token = { tok_kind : token_kind; tok_start_pos : pos; tok_end_pos : pos }

(** {1 Events — Parser output} *)

(** Events are the output of the Parser. They form a flat, sequential stream
    that can be processed without building a full tree in memory. The format
    mirrors the yaml-test-suite event notation. *)
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

(** {1 AST nodes — Composer output} *)

(** Nodes are the in-memory representation of a parsed YAML document. Anchors
    are resolved at compose time so [Alias_node] carries the actual target node.
*)
type node =
  | Scalar_node of {
      anchor : string option;
      tag : string option;
      value : string;
      style : scalar_style;
      pos : pos;
      head_comments : string list;
      line_comment : string option;
    }
  | Sequence_node of {
      anchor : string option;
      tag : string option;
      items : node list;
      flow : bool;
      pos : pos;
      head_comments : string list;
      line_comment : string option;
      foot_comments : string list;
    }
  | Mapping_node of {
      anchor : string option;
      tag : string option;
      pairs : (node * node) list;
      flow : bool;
      pos : pos;
      head_comments : string list;
      line_comment : string option;
      foot_comments : string list;
    }
  | Alias_node of {
      name : string;
      resolved : node;
      pos : pos;
      head_comments : string list;
      line_comment : string option;
    }

(** {1 Resolved values — Resolver output} *)

(** Typed values produced by applying the YAML 1.2 JSON schema to a composed
    node tree. *)
type value =
  | Null
  | Bool of bool
  | Int of int64
  | Float of float
  | String of string
  | Seq of value list
  | Map of (value * value) list
