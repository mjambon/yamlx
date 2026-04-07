(** yamlx command-line tool. Reads YAML from a file (first positional argument)
    or from standard input and prints the parsed result in the chosen format.

    Usage: yamlx [--format FORMAT] [FILE]

    Formats: yaml pretty-printed YAML via YAMLx.to_yaml (default) plain plain
    YAML: aliases expanded, tags stripped, no flow collections events
    yaml-test-suite event-tree notation

    --strict (plain only): error on tags instead of stripping them *)

(* ------------------------------------------------------------------ *)
(* Output format                                                         *)
(* ------------------------------------------------------------------ *)

type format = Events | Yaml | Plain | Value | Value_loc | Node | Node_loc

let format = ref Yaml
let strict = ref false
let schema : YAMLx.schema ref = ref YAMLx.Yaml_1_2
let strict_schema = ref false
let reject_ambiguous = ref false
let plain_input = ref false
let strict_keys = ref false

let set_schema s =
  match s with
  | "1.1" -> schema := Yaml_1_1
  | "1.2" -> schema := Yaml_1_2
  | other ->
      raise
        (Arg.Bad (Printf.sprintf "unknown schema %S (choose: 1.1, 1.2)" other))

let set_format s =
  match s with
  | "events" -> format := Events
  | "yaml" -> format := Yaml
  | "plain" -> format := Plain
  | "value" -> format := Value
  | "value-loc" -> format := Value_loc
  | "node" -> format := Node
  | "node-loc" -> format := Node_loc
  | other ->
      raise
        (Arg.Bad
           (Printf.sprintf
              "unknown format %S (choose: yaml, plain, events, value, \
               value-loc, node, node-loc)"
              other))

let usage_msg =
  {|Usage: yamlx [-f FORMAT] [--schema VERSION] [FILE]

  Parse YAML from FILE (or stdin) and write the result to stdout.
  Reads from standard input when no FILE is given.
  Multi-document streams, anchors, tags, and Unicode are fully supported.

  Output formats (-f FORMAT):
    yaml    Pretty-printed YAML — scalar styles and block/flow mode preserved
            (default)
    plain   Simplified YAML — aliases expanded, tags stripped, flow collections
            converted to block; merge keys expanded in YAML 1.1 mode
    value   Typed-value tree: Null / Bool / Int / Float / String / Seq / Map
            Useful for checking how scalars are resolved (e.g. is "1e2" a Float?)
    value-loc  Same as value but with source locations
    node    Full AST without source locations or heights
    node-loc  Same as node but with source locations and heights
    events  yaml-test-suite event-tree notation (mainly for parser testing)

  YAML schema (--schema VERSION):
    1.2  YAML 1.2 JSON schema — default. Booleans: true/false only.
         Octal: 0o755. No sexagesimal.
    1.1  YAML 1.1 schema — for legacy files. Extended booleans (yes/no/on/off),
         0755-style octal, sexagesimal integers and floats, merge keys (<<).
    A %YAML directive in the document overrides this setting per document
    unless --strict-schema is given.

  Options:|}

let spec =
  [
    ( "--format",
      Arg.String set_format,
      "FORMAT  Select output format (see above)" );
    ("-f", Arg.String set_format, "FORMAT  Short alias for --format");
    ( "--strict",
      Arg.Set strict,
      "  With -f plain: raise an error on tags instead of silently stripping \
       them" );
    ( "--schema",
      Arg.String set_schema,
      "VERSION  YAML schema: 1.1 or 1.2 (default: 1.2)" );
    ( "--strict-schema",
      Arg.Set strict_schema,
      "  Error if the document's %YAML directive disagrees with --schema" );
    ( "--reject-ambiguous",
      Arg.Set reject_ambiguous,
      "  With --schema 1.2: error on plain scalars that would resolve \
       differently under YAML 1.1 (e.g. yes, 0755, <<)" );
    ( "--plain",
      Arg.Set plain_input,
      "  With -f value or value-loc: error on anchors, aliases, explicit tags, \
       or (with --schema 1.1) merge keys" );
    ( "--strict-keys",
      Arg.Set strict_keys,
      "  With -f value or value-loc: error on duplicate mapping keys instead \
       of silently keeping the last occurrence" );
  ]

(* ------------------------------------------------------------------ *)
(* node-noloc type: node AST without source locations or heights         *)
(* ------------------------------------------------------------------ *)

type scalar_style = Plain | Single_quoted | Double_quoted | Literal | Folded
[@@deriving show { with_path = false }]

type noloc_node =
  | Scalar of {
      anchor : string option;
      tag : string option;
      value : string;
      style : scalar_style;
      head_comments : string list;
      line_comment : string option;
    }
  | Sequence of {
      anchor : string option;
      tag : string option;
      items : noloc_node list;
      flow : bool;
      head_comments : string list;
      line_comment : string option;
      foot_comments : string list;
    }
  | Mapping of {
      anchor : string option;
      tag : string option;
      pairs : (noloc_node * noloc_node) list;
      flow : bool;
      head_comments : string list;
      line_comment : string option;
      foot_comments : string list;
    }
  | Alias of {
      name : string;
      resolved : noloc_node;
      head_comments : string list;
      line_comment : string option;
    }
[@@deriving show { with_path = false }]

let noloc_style = function
  | YAMLx.Plain -> Plain
  | YAMLx.Single_quoted -> Single_quoted
  | YAMLx.Double_quoted -> Double_quoted
  | YAMLx.Literal -> Literal
  | YAMLx.Folded -> Folded

let rec noloc_node = function
  | YAMLx.Scalar_node r ->
      Scalar
        {
          anchor = r.anchor;
          tag = r.tag;
          value = r.value;
          style = noloc_style r.style;
          head_comments = r.head_comments;
          line_comment = r.line_comment;
        }
  | YAMLx.Sequence_node r ->
      Sequence
        {
          anchor = r.anchor;
          tag = r.tag;
          items = List.map noloc_node r.items;
          flow = r.flow;
          head_comments = r.head_comments;
          line_comment = r.line_comment;
          foot_comments = r.foot_comments;
        }
  | YAMLx.Mapping_node r ->
      Mapping
        {
          anchor = r.anchor;
          tag = r.tag;
          pairs = List.map (fun (k, v) -> (noloc_node k, noloc_node v)) r.pairs;
          flow = r.flow;
          head_comments = r.head_comments;
          line_comment = r.line_comment;
          foot_comments = r.foot_comments;
        }
  | YAMLx.Alias_node r ->
      Alias
        {
          name = r.name;
          resolved = noloc_node r.resolved;
          head_comments = r.head_comments;
          line_comment = r.line_comment;
        }

(* ------------------------------------------------------------------ *)
(* value-noloc type: value tree without source locations                 *)
(* ------------------------------------------------------------------ *)

type noloc_value =
  | Null
  | Bool of bool
  | Int of int64
  | Float of float
  | String of string
  | Seq of noloc_value list
  | Map of (noloc_value * noloc_value) list
[@@deriving show { with_path = false }]

let rec noloc_value = function
  | YAMLx.Null _ -> Null
  | YAMLx.Bool (_, b) -> Bool b
  | YAMLx.Int (_, i) -> Int i
  | YAMLx.Float (_, f) -> Float f
  | YAMLx.String (_, s) -> String s
  | YAMLx.Seq (_, vs) -> Seq (List.map noloc_value vs)
  | YAMLx.Map (_, pairs) ->
      Map (List.map (fun (_, k, v) -> (noloc_value k, noloc_value v)) pairs)

(* ------------------------------------------------------------------ *)
(* Input reading                                                         *)
(* ------------------------------------------------------------------ *)

let read_stdin () =
  let buf = Buffer.create 1024 in
  (try
     while true do
       Buffer.add_channel buf stdin 4096
     done
   with
  | End_of_file -> ());
  Buffer.contents buf

(* Needed only for the Events format, which requires a raw string as input
   to the internal parse_events function. All other formats use the
   YAMLx result-returning functions that handle file I/O themselves. *)
let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let n = in_channel_length ic in
      let s = Bytes.create n in
      really_input ic s 0 n;
      Bytes.to_string s)

(* ------------------------------------------------------------------ *)
(* Entry point                                                           *)
(* ------------------------------------------------------------------ *)

let or_die = function
  | Ok x -> x
  | Error msg ->
      Printf.eprintf "%s\n" msg;
      exit 1

let () =
  let files = ref [] in
  Arg.parse spec (fun s -> files := s :: !files) usage_msg;
  let source =
    match List.rev !files with
    | [] -> `Stdin
    | path :: _ -> `File path
  in
  let file =
    match source with
    | `File p -> Some p
    | `Stdin -> None
  in
  let get_nodes () =
    match source with
    | `Stdin -> YAMLx.Nodes.of_yaml (read_stdin ())
    | `File path -> YAMLx.Nodes.of_yaml_file path
  in
  let output =
    match !format with
    | Events ->
        let input =
          match source with
          | `Stdin -> read_stdin ()
          | `File path ->
              YAMLx.catch_errors ~file:path (fun () -> read_file path) |> or_die
        in
        YAMLx.catch_errors ?file (fun () ->
            YAMLx.events_to_tree (YAMLx.parse_events input))
        |> or_die
    | Yaml -> get_nodes () |> Result.map YAMLx.Nodes.to_yaml |> or_die
    | Plain -> (
        (* In YAML 1.1 mode, go through Values so that merge keys are expanded
           before serialisation.  In 1.2 mode take the faster nodes path. *)
        match !schema with
        | YAMLx.Yaml_1_1 ->
            let values =
              (match source with
                | `Stdin ->
                    YAMLx.Values.of_yaml ~schema:Yaml_1_1
                      ~strict_schema:!strict_schema (read_stdin ())
                | `File path ->
                    YAMLx.Values.of_yaml_file ~schema:Yaml_1_1
                      ~strict_schema:!strict_schema path)
              |> or_die
            in
            let nodes = YAMLx.Values.to_nodes values in
            YAMLx.catch_errors ?file (fun () ->
                YAMLx.Nodes.to_plain_yaml_exn ~strict:!strict nodes)
            |> or_die
        | YAMLx.Yaml_1_2 ->
            let nodes = get_nodes () |> or_die in
            YAMLx.catch_errors ?file (fun () ->
                YAMLx.Nodes.to_plain_yaml_exn ~strict:!strict nodes)
            |> or_die)
    | Value ->
        (match source with
          | `Stdin ->
              YAMLx.Values.of_yaml ~schema:!schema ~strict_schema:!strict_schema
                ~reject_ambiguous:!reject_ambiguous ~plain:!plain_input
                ~strict_keys:!strict_keys (read_stdin ())
          | `File path ->
              YAMLx.Values.of_yaml_file ~schema:!schema
                ~strict_schema:!strict_schema
                ~reject_ambiguous:!reject_ambiguous ~plain:!plain_input
                ~strict_keys:!strict_keys path)
        |> Result.map (fun values ->
            let buf = Buffer.create 256 in
            List.iter
              (fun v ->
                Buffer.add_string buf (show_noloc_value (noloc_value v));
                Buffer.add_char buf '\n')
              values;
            Buffer.contents buf)
        |> or_die
    | Value_loc ->
        (match source with
          | `Stdin ->
              YAMLx.Values.of_yaml ~schema:!schema ~strict_schema:!strict_schema
                ~reject_ambiguous:!reject_ambiguous ~plain:!plain_input
                ~strict_keys:!strict_keys (read_stdin ())
          | `File path ->
              YAMLx.Values.of_yaml_file ~schema:!schema
                ~strict_schema:!strict_schema
                ~reject_ambiguous:!reject_ambiguous ~plain:!plain_input
                ~strict_keys:!strict_keys path)
        |> Result.map (fun values ->
            let buf = Buffer.create 256 in
            List.iter
              (fun v ->
                Buffer.add_string buf (YAMLx.show_value v);
                Buffer.add_char buf '\n')
              values;
            Buffer.contents buf)
        |> or_die
    | Node ->
        get_nodes ()
        |> Result.map (fun nodes ->
            let buf = Buffer.create 256 in
            List.iter
              (fun n ->
                Buffer.add_string buf (show_noloc_node (noloc_node n));
                Buffer.add_char buf '\n')
              nodes;
            Buffer.contents buf)
        |> or_die
    | Node_loc ->
        get_nodes ()
        |> Result.map (fun nodes ->
            let buf = Buffer.create 256 in
            List.iter
              (fun n ->
                Buffer.add_string buf (YAMLx.show_node n);
                Buffer.add_char buf '\n')
              nodes;
            Buffer.contents buf)
        |> or_die
  in
  print_string output
