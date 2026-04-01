(** yamlx command-line tool.
    Reads YAML from standard input (or from a file if one is given as the
    first argument) and prints the parsed event stream in yaml-test-suite
    tree notation, then exits with code 0.  Exits with code 1 on error. *)

let () =
  let input =
    if Array.length Sys.argv > 1 then begin
      let ic = open_in Sys.argv.(1) in
      let n  = in_channel_length ic in
      let s  = Bytes.create n in
      really_input ic s 0 n;
      close_in ic;
      Bytes.to_string s
    end else
      (* Read all of stdin *)
      let buf = Buffer.create 1024 in
      (try
        while true do
          Buffer.add_channel buf stdin 4096
        done
      with End_of_file -> ());
      Buffer.contents buf
  in
  match YAMLx.parse_events input with
  | exception (Types.Scan_error e) ->
    Printf.eprintf "Scan error at line %d col %d: %s\n"
      e.pos.line e.pos.column e.msg;
    exit 1
  | exception (Types.Parse_error e) ->
    Printf.eprintf "Parse error at line %d col %d: %s\n"
      e.pos.line e.pos.column e.msg;
    exit 1
  | events ->
    let tree = Event_printer.to_tree events in
    print_string tree
