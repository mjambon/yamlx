(** YAML 1.2 scanner (tokeniser). Transforms a character stream (Reader) into a
    token stream consumed by the Parser.

    The scanner maintains a token queue of buffered tokens not yet delivered to
    the parser. Tokens are produced lazily: [get_token] calls internal fetch
    routines as needed. *)

type state
(** Opaque scanner state. *)

val create : Reader.t -> state
(** Create a scanner from a Reader. *)

val peek_token : state -> Types.token
(** Return (without consuming) the next token. Blocks until at least one token
    is available. *)

val get_token : state -> Types.token
(** Consume and return the next token. *)

val check_token : state -> Types.token_kind list -> bool
(** True if the next token's kind is structurally equal to one of [kinds].
    Fields inside the constructor are ignored; only the constructor tag is
    checked. *)

val peek_kind : state -> Types.token_kind option
(** Return the constructor of the next token's kind, or [None] at end of input.
*)

val drain_comments : state -> (int * bool * string) list
(** Return all accumulated comments in source order as
    [(line, is_line_comment, text)] triples. [is_line_comment = true] means the
    comment appeared on the same line as the preceding token (trailing comment);
    [false] means it was on its own line (head/standalone comment). Text does
    not include the leading ['#'] character. Comments inside flow collections
    are not recorded. Typically called after the full token stream has been
    consumed. *)
