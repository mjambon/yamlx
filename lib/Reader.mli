(** UTF-8 input reader for the YAML scanner.
    Decodes the input string into an array of Unicode codepoints (int values),
    skips an optional byte-order mark (BOM, U+FEFF) at the start, and
    normalises all line endings to LF (U+000A):
      * CR+LF  (\r\n)   → LF
      * CR     (\r)     → LF
      * NEL    (\x85)   → LF
      * LS     (\u2028) → LF
      * PS     (\u2029) → LF
    Tracks the current position (codepoint index, line, column) and exposes a
    small lookahead interface used by the Scanner. *)

(** End-of-input sentinel.  Returned by [peek] when past the end. *)
val eof : int

type t

(** Create a Reader from a UTF-8 string. *)
val of_string : string -> t

(** Total number of codepoints in the input. *)
val length : t -> int

(** True when there are no more characters to read. *)
val at_end : t -> bool

(** The current position as a [Types.pos]. *)
val pos : t -> Types.pos

(** Look ahead without consuming.  [peek r 0] is the current character.
    Returns [eof] past the end of input. *)
val peek : t -> int -> int

(** Advance by [n] codepoints, updating line and column. *)
val advance : t -> int -> unit

(** Consume and return the current codepoint.  Returns [eof] at end. *)
val read : t -> int

(** Return the next [n] codepoints as a UTF-8 string without consuming them. *)
val peek_string : t -> int -> string

(** Consume [n] codepoints and return them as a UTF-8 string. *)
val read_string : t -> int -> string

(** True if the next codepoints match the given ASCII string exactly. *)
val prefix_is : t -> string -> bool

(** Encode a Unicode codepoint as UTF-8 bytes appended to [buf]. *)
val encode_utf8_to : Buffer.t -> int -> unit
