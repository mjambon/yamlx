(** Comment attachment pass.

    After the Composer has built a node tree (with all comment fields empty),
    this module takes the raw comment list collected by the Scanner and
    attaches each comment to the most appropriate node.

    Best-effort semantics
    ~~~~~~~~~~~~~~~~~~~~~
    Comment attachment is a heuristic process and cannot be perfect in all
    cases.  The rules applied are:

    - {b Head comments}: standalone comment lines (lines that contain nothing
      but a comment) that appear immediately before a node are attached as
      [head_comments] of that node.

    - {b Line comments}: a comment on the same line as a node's start
      position is attached as [line_comment].  For mapping pairs where key
      and value start on the same line, the line comment is attached to the
      value (the last node on that line), not the key.

    - {b Foot comments}: standalone comment lines that appear after the last
      child of a collection, before the next sibling, are attached as
      [foot_comments] of the collection.

    Comments inside flow collections are not recorded by the Scanner.
    Comments that cannot be attributed to any node are silently discarded.
    The behaviour is subject to change as the heuristics are refined. *)

open Types

(* ------------------------------------------------------------------ *)
(* Comment cursor                                                        *)
(* ------------------------------------------------------------------ *)

(** A mutable cursor through the sorted comment list. *)
type cursor = (int * bool * string) list ref

let make_cursor comments =
  ref (List.sort (fun (a, _, _) (b, _, _) -> compare a b) comments)

(** Consume and return comments satisfying [pred]. *)
let take_while pred (cur : cursor) =
  let taken = ref [] in
  while !cur <> [] && pred (List.hd !cur) do
    taken := List.hd !cur :: !taken;
    cur   := List.tl !cur
  done;
  List.rev !taken

(** Consume and return the texts of non-line-comment entries at lines
    strictly before [line]. *)
let take_head_before line cur =
  take_while (fun (l, is_line, _) -> (not is_line) && l < line) cur
  |> List.map (fun (_, _, t) -> t)

(** Consume and return the text of a line-comment entry exactly at [line],
    if present. *)
let take_line_comment line (cur : cursor) =
  match !cur with
  | (l, true, t) :: rest when l = line -> cur := rest; Some t
  | _ -> None

(* ------------------------------------------------------------------ *)
(* Node helpers                                                          *)
(* ------------------------------------------------------------------ *)

let node_line = function
  | Scalar_node   r -> r.pos.line
  | Sequence_node r -> r.pos.line
  | Mapping_node  r -> r.pos.line
  | Alias_node    r -> r.pos.line

(** Return the [head_comments] field of a node. *)
let get_heads = function
  | Scalar_node   r -> r.head_comments
  | Sequence_node r -> r.head_comments
  | Mapping_node  r -> r.head_comments
  | Alias_node    r -> r.head_comments

(** Return the [line_comment] field of a node. *)
let get_lc = function
  | Scalar_node   r -> r.line_comment
  | Sequence_node r -> r.line_comment
  | Mapping_node  r -> r.line_comment
  | Alias_node    r -> r.line_comment

(** Set [head_comments] on a node. *)
let set_heads heads = function
  | Scalar_node   r -> Scalar_node   { r with head_comments = heads }
  | Sequence_node r -> Sequence_node { r with head_comments = heads }
  | Mapping_node  r -> Mapping_node  { r with head_comments = heads }
  | Alias_node    r -> Alias_node    { r with head_comments = heads }

(** Set [line_comment] on a node. *)
let set_lc lc = function
  | Scalar_node   r -> Scalar_node   { r with line_comment = lc }
  | Sequence_node r -> Sequence_node { r with line_comment = lc }
  | Mapping_node  r -> Mapping_node  { r with line_comment = lc }
  | Alias_node    r -> Alias_node    { r with line_comment = lc }

(* ------------------------------------------------------------------ *)
(* Attachment traversal                                                  *)
(* ------------------------------------------------------------------ *)

(** Attach comments to a single node.
    [next_line] is the source line where the next sibling begins (or
    [max_int] for the last sibling), used to bound foot-comment collection. *)
let rec attach_node cur ~next_line node =
  let nl = node_line node in

  (* Head comments: standalone lines before this node *)
  let heads = take_head_before nl cur in
  let node  = set_heads (get_heads node @ heads) node in

  (* Descend into children and then collect foot comments *)
  match node with
  | Scalar_node _ | Alias_node _ ->
    let lc   = take_line_comment nl cur in
    let node = set_lc lc node in
    node

  | Sequence_node r ->
    (* For a flow sequence or an empty sequence, a same-line comment belongs to
       the sequence node itself.  For a non-empty block sequence the comment on
       its start line belongs to the first item, so we skip take_line_comment
       here and let attach_siblings pick it up. *)
    let lc =
      if r.flow || r.items = [] then take_line_comment nl cur
      else None
    in
    let items = attach_siblings cur r.items ~parent_next_line:next_line in
    let feet  = take_head_before next_line cur in
    Sequence_node { r with head_comments = get_heads node;
                            line_comment  = lc;
                            items;
                            foot_comments = feet }

  | Mapping_node r ->
    (* Same reasoning as Sequence_node above. *)
    let lc =
      if r.flow || r.pairs = [] then take_line_comment nl cur
      else None
    in
    let pairs = attach_pairs cur r.pairs ~parent_next_line:next_line in
    let feet  = take_head_before next_line cur in
    Mapping_node { r with head_comments = get_heads node;
                           line_comment  = lc;
                           pairs;
                           foot_comments = feet }

(** Attach comments to a list of sibling nodes.
    [parent_next_line] bounds the foot-comment zone of the last sibling. *)
and attach_siblings cur nodes ~parent_next_line =
  let arr = Array.of_list nodes in
  let n   = Array.length arr in
  for i = 0 to n - 1 do
    let next = if i + 1 < n then node_line arr.(i + 1) else parent_next_line in
    arr.(i) <- attach_node cur ~next_line:next arr.(i)
  done;
  Array.to_list arr

(** Attach comments to a list of mapping pairs.
    [parent_next_line] bounds the foot-comment zone of the last pair. *)
and attach_pairs cur pairs ~parent_next_line =
  let arr = Array.of_list pairs in
  let n   = Array.length arr in
  for i = 0 to n - 1 do
    let (k, v) = arr.(i) in
    let next_pair_line =
      if i + 1 < n then node_line (fst arr.(i + 1)) else parent_next_line
    in
    let key_line   = node_line k in
    let value_line = node_line v in

    (* Attach to the key, but hold back the line comment if key and value
       share the same line — it belongs to the value (the last node there). *)
    let k' = attach_node cur ~next_line:value_line k in
    let (k', transferred_lc) =
      if key_line = value_line then
        (* Strip the line comment we just put on the key and defer it *)
        (set_lc None k', get_lc k')
      else
        (k', None)
    in

    let v' = attach_node cur ~next_line:next_pair_line v in
    (* Apply the transferred line comment to the value if it has none *)
    let v' = match transferred_lc with
      | Some _ when get_lc v' = None -> set_lc transferred_lc v'
      | _ -> v'
    in

    arr.(i) <- (k', v')
  done;
  Array.to_list arr

(* ------------------------------------------------------------------ *)
(* Entry point                                                           *)
(* ------------------------------------------------------------------ *)

(** Attach [raw_comments] to [docs] and return the annotated node list.
    [raw_comments] is the [(line, is_line_comment, text)] list returned by
    {!Scanner.drain_comments}.  Documents are treated as top-level siblings;
    any comments after the last document are discarded. *)
let attach (docs : node list) (raw_comments : (int * bool * string) list)
    : node list =
  if raw_comments = [] then docs
  else begin
    let cur = make_cursor raw_comments in
    let arr = Array.of_list docs in
    let n   = Array.length arr in
    for i = 0 to n - 1 do
      let next = if i + 1 < n then node_line arr.(i + 1) else max_int in
      arr.(i) <- attach_node cur ~next_line:next arr.(i)
    done;
    Array.to_list arr
  end
