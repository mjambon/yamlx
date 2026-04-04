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

type format = Events | Yaml | Plain | Value | Node

let format = ref Yaml
let strict = ref false

let set_format s =
  match s with
  | "events" -> format := Events
  | "yaml" -> format := Yaml
  | "plain" -> format := Plain
  | "value" -> format := Value
  | "node" -> format := Node
  | other ->
      raise
        (Arg.Bad
           (Printf.sprintf
              "unknown format %S (choose: yaml, plain, events, value, node)"
              other))

let usage_msg =
  {|Usage: yamlx [-f FORMAT] [FILE]

  Parse YAML 1.2 from FILE (or stdin) and write the result to stdout.
  Reads from standard input when no FILE is given.
  Multi-document streams, anchors, tags, and Unicode are fully supported.

  Output formats (-f FORMAT):
    yaml    Pretty-printed YAML — scalar styles and block/flow mode preserved
            (default)
    plain   Simplified YAML — aliases expanded, tags stripped, flow collections
            converted to block; raises an error on complex mapping keys
    value   Typed-value tree: Null / Bool / Int / Float / String / Seq / Map
            Useful for checking how scalars are resolved (e.g. is "1e2" a Float?)
    node    Full AST with source locations, anchors, tags, scalar styles, and
            best-effort comment preservation
    events  yaml-test-suite event-tree notation (mainly for parser testing)

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
  ]

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
    | Plain ->
        let nodes = get_nodes () |> or_die in
        YAMLx.catch_errors ?file (fun () ->
            YAMLx.Nodes.to_plain_yaml_exn ~strict:!strict nodes)
        |> or_die
    | Value ->
        (match source with
          | `Stdin -> YAMLx.Values.of_yaml (read_stdin ())
          | `File path -> YAMLx.Values.of_yaml_file path)
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
                Buffer.add_string buf (YAMLx.show_node n);
                Buffer.add_char buf '\n')
              nodes;
            Buffer.contents buf)
        |> or_die
  in
  print_string output
