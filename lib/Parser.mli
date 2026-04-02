(** YAML 1.2 parser.
    Transforms the token stream from the Scanner into an event stream.

    The parser implements a recursive-descent grammar matching the YAML
    1.2.2 specification.  It is driven as a pull-style state machine:
    [get_event] advances it one step at a time. *)

(** Opaque parser state. *)
type t

(** Create a parser from a Scanner. *)
val create : Scanner.state -> t

(** Return (without consuming) the next event.
    Produces a new event if the buffer is empty. *)
val peek_event : t -> Types.event

(** Consume and return the next event. *)
val get_event : t -> Types.event

(** True if the next event's kind matches one of [kinds].
    Fields inside the constructor are ignored; only the constructor tag is
    checked. *)
val check_event : t -> Types.event_kind list -> bool

(** Collect all events into a list.  Reads until [Stream_end]. *)
val to_event_list : t -> Types.event list
