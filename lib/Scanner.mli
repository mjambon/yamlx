(** YAML 1.2 scanner (tokeniser).
    Transforms a character stream (Reader) into a token stream consumed by
    the Parser.

    The scanner maintains a token queue of buffered tokens not yet delivered
    to the parser.  Tokens are produced lazily: [get_token] calls internal
    fetch routines as needed. *)

(** Opaque scanner state. *)
type state

(** Create a scanner from a Reader. *)
val create : Reader.t -> state

(** Return (without consuming) the next token.
    Blocks until at least one token is available. *)
val peek_token : state -> Types.token

(** Consume and return the next token. *)
val get_token : state -> Types.token

(** True if the next token's kind is structurally equal to one of [kinds].
    Fields inside the constructor are ignored; only the constructor tag is
    checked. *)
val check_token : state -> Types.token_kind list -> bool

(** Return the constructor of the next token's kind, or [None] at end of
    input. *)
val peek_kind : state -> Types.token_kind option

(** Return all accumulated comments in source order as
    [(line, is_line_comment, text)] triples.
    [is_line_comment = true] means the comment appeared on the same line
    as the preceding token (trailing comment); [false] means it was on its
    own line (head/standalone comment).  Text does not include the leading
    ['#'] character.  Comments inside flow collections are not recorded.
    Typically called after the full token stream has been consumed. *)
val drain_comments : state -> (int * bool * string) list
