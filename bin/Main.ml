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

let spec =
  [
    ( "--format",
      Arg.String set_format,
      "FORMAT  Output format: yaml (default), plain, events, value, or node" );
    ("-f", Arg.String set_format, "FORMAT  Short alias for --format");
    ( "--strict",
      Arg.Set strict,
      "  With --format plain: error on tags instead of stripping them" );
  ]

(* ------------------------------------------------------------------ *)
(* Input reading                                                         *)
(* ------------------------------------------------------------------ *)

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let read_stdin () =
  let buf = Buffer.create 1024 in
  (try
     while true do
       Buffer.add_channel buf stdin 4096
     done
   with
  | End_of_file -> ());
  Buffer.contents buf

(* ------------------------------------------------------------------ *)
(* Entry point                                                           *)
(* ------------------------------------------------------------------ *)

let () =
  let files = ref [] in
  Arg.parse spec
    (fun s -> files := s :: !files)
    "Usage: yamlx [--format FORMAT] [FILE]";
  let input =
    match List.rev !files with
    | [] -> read_stdin ()
    | path :: _ -> read_file path
  in
  let nodes_or_exit () =
    match YAMLx.parse_nodes input with
    | exception YAMLx.Scan_error e ->
        Printf.eprintf "Scan error at line %d col %d: %s\n" e.pos.line
          e.pos.column e.msg;
        exit 1
    | exception YAMLx.Parse_error e ->
        Printf.eprintf "Parse error at line %d col %d: %s\n" e.pos.line
          e.pos.column e.msg;
        exit 1
    | nodes -> nodes
  in
  let output =
    match !format with
    | Events -> (
        match YAMLx.parse_events input with
        | exception YAMLx.Scan_error e ->
            Printf.eprintf "Scan error at line %d col %d: %s\n" e.pos.line
              e.pos.column e.msg;
            exit 1
        | exception YAMLx.Parse_error e ->
            Printf.eprintf "Parse error at line %d col %d: %s\n" e.pos.line
              e.pos.column e.msg;
            exit 1
        | events -> YAMLx.events_to_tree events)
    | Yaml -> YAMLx.to_yaml (nodes_or_exit ())
    | Plain -> (
        match YAMLx.to_plain_yaml ~strict:!strict (nodes_or_exit ()) with
        | exception YAMLx.Plain_error msg ->
            Printf.eprintf "Plain error: %s\n" msg;
            exit 1
        | s -> s)
    | Value ->
        let values =
          match YAMLx.of_string input with
          | exception YAMLx.Scan_error e ->
              Printf.eprintf "Scan error at line %d col %d: %s\n" e.pos.line
                e.pos.column e.msg;
              exit 1
          | exception YAMLx.Parse_error e ->
              Printf.eprintf "Parse error at line %d col %d: %s\n" e.pos.line
                e.pos.column e.msg;
              exit 1
          | vs -> vs
        in
        let buf = Buffer.create 256 in
        List.iter
          (fun v ->
            Buffer.add_string buf (YAMLx.show_value v);
            Buffer.add_char buf '\n')
          values;
        Buffer.contents buf
    | Node ->
        let nodes = nodes_or_exit () in
        let buf = Buffer.create 256 in
        List.iter
          (fun n ->
            Buffer.add_string buf (YAMLx.show_node n);
            Buffer.add_char buf '\n')
          nodes;
        Buffer.contents buf
  in
  print_string output
