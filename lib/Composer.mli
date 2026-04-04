(** YAML Composer. Builds an in-memory AST ([Types.node]) from the Parser's
    event stream.

    The Composer resolves anchor/alias references: when an alias is encountered
    the already-composed node for that anchor is substituted in place. Anchors
    must be declared before they are used. *)

type t
(** Opaque composer state. *)

val create : ?max_depth:int -> Parser.t -> t
(** Create a composer from a Parser. [max_depth] sets the maximum nesting depth
    (default: {!Types.default_max_depth}); inputs exceeding it raise
    {!Types.Depth_limit_exceeded}. *)

val compose_stream : t -> Types.node list
(** Compose all documents in a YAML stream. Reads from [STREAM_START] to
    [STREAM_END] and returns one [Types.node] per document. *)
