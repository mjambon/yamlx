(** YAML 1.2 character classification.
    Functions that answer questions about Unicode codepoints used throughout
    the scanner.  All codepoint values come from YAML 1.2.2 §5.

    The Reader uses [-1] as the end-of-input sentinel; many predicates treat
    it as false (not in any character class). *)

(** End-of-input sentinel returned by the Reader at end of stream. *)
val eof : int

val is_null      : int -> bool
val is_linebreak : int -> bool
val is_space     : int -> bool
val is_blank     : int -> bool
val is_white     : int -> bool
val is_anchor_char : int -> bool
val is_tag_char    : int -> bool
val is_directive_char : int -> bool
val is_letter    : int -> bool
val is_digit     : int -> bool
val is_hex_digit : int -> bool

(** Decode a hex digit to its integer value (0–15). *)
val hex_value : int -> int

val is_flow_indicator       : int -> bool
val can_start_plain_block   : int -> bool
val can_start_plain_flow    : int -> bool
val can_continue_plain_block : int -> bool
val can_continue_plain_flow  : int -> bool

(** Return the printable representation of a codepoint for error messages. *)
val show : int -> string
