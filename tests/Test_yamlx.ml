(** Main test suite for YAMLx.
    Runs the standard yaml-test-suite cases plus a handful of hand-written
    unit tests.

    Test strategy
    ~~~~~~~~~~~~~
    Each yaml-test-suite entry specifies either:
      1. A [yaml:] input that should parse successfully and produce a known
         event stream ([tree:] field).
      2. A [yaml:] input that should fail to parse ([fail: true]).

    For case 1 we compare the normalised event tree produced by YAMLx
    against the expected tree from the test file.

    For case 2 we check that YAMLx raises [Scan_error] or [Parse_error].

    The test suite is loaded from the [reference/yaml-test-suite/src/]
    directory relative to the repository root.  The path is hard-coded here
    for simplicity; adjust if the layout changes. *)

(* ------------------------------------------------------------------ *)
(* Locate the test-suite source directory                                *)
(* ------------------------------------------------------------------ *)

(** Absolute path to the yaml-test-suite src directory.
    We derive it from the location of this file's compilation artifact so
    the tests can be run from any working directory. *)
let suite_dir =
  (* __FILE__ expands to the source path at compile time. *)
  let here = Filename.dirname __FILE__ in
  (* Navigate up one level from tests/ to reach the repo root. *)
  let root = Filename.concat here ".." in
  Filename.concat root "reference/yaml-test-suite/src"

(* ------------------------------------------------------------------ *)
(* Run a single test case                                                *)
(* ------------------------------------------------------------------ *)

(** Run one yaml-test-suite test case.
    For success cases: parse the YAML and compare the event tree.
    For failure cases: assert that an error is raised. *)
let run_test_case (tc : Suite_loader.test_case) () =
  if tc.fail then begin
    (* Expect a scan or parse error *)
    (match YAMLx.parse_events tc.yaml with
    | exception (Types.Scan_error _)  -> ()  (* expected *)
    | exception (Types.Parse_error _) -> ()  (* expected *)
    | _ ->
      failwith (Printf.sprintf
        "[%s] %s: expected a parse failure but parsing succeeded"
        tc.id tc.name))
  end else begin
    (* Parse must succeed *)
    let events =
      match YAMLx.parse_events tc.yaml with
      | exception (Types.Scan_error e) ->
        failwith (Printf.sprintf
          "[%s] %s: unexpected scan error at line %d col %d: %s"
          tc.id tc.name e.pos.line e.pos.column e.msg)
      | exception (Types.Parse_error e) ->
        failwith (Printf.sprintf
          "[%s] %s: unexpected parse error at line %d col %d: %s"
          tc.id tc.name e.pos.line e.pos.column e.msg)
      | evs -> evs
    in
    (* Only compare against the expected tree if one is given *)
    (match tc.tree with
    | None -> ()
    | Some expected_tree ->
      let actual_tree = Event_printer.to_tree events in
      (match Event_printer.diff_trees ~expected:expected_tree ~actual:actual_tree with
      | None -> ()  (* trees match *)
      | Some diff ->
        failwith (Printf.sprintf
          "[%s] %s: event tree mismatch\n  %s\nExpected:\n%s\nGot:\n%s"
          tc.id tc.name diff
          expected_tree actual_tree)))
  end

(* ------------------------------------------------------------------ *)
(* Hand-written unit tests                                               *)
(* ------------------------------------------------------------------ *)

(** Basic sanity tests that don't depend on the yaml-test-suite. *)
let unit_tests () =
  let check_parse label yaml expected_events () =
    let events = YAMLx.parse_events yaml in
    let actual = Event_printer.to_tree events in
    (match Event_printer.diff_trees ~expected:expected_events ~actual with
    | None      -> ()
    | Some diff ->
      failwith (Printf.sprintf "%s: %s\nExpected:\n%s\nGot:\n%s"
        label diff expected_events actual))
  in
  [ Testo.create "empty stream"
      (check_parse "empty stream" "" "+STR\n-STR\n")

  ; Testo.create "plain scalar"
      (check_parse "plain scalar" "hello"
         "+STR\n+DOC\n=VAL :hello\n-DOC\n-STR\n")

  ; Testo.create "simple mapping"
      (check_parse "simple mapping" "a: 1\nb: 2"
         "+STR\n+DOC\n+MAP\n=VAL :a\n=VAL :1\n=VAL :b\n=VAL :2\n-MAP\n-DOC\n-STR\n")

  ; Testo.create "simple sequence"
      (check_parse "simple sequence" "- a\n- b"
         "+STR\n+DOC\n+SEQ\n=VAL :a\n=VAL :b\n-SEQ\n-DOC\n-STR\n")

  ; Testo.create "double-quoted scalar"
      (check_parse "double-quoted" {|"hello\nworld"|}
         "+STR\n+DOC\n=VAL \"hello\\nworld\n-DOC\n-STR\n")

  ; Testo.create "single-quoted scalar"
      (check_parse "single-quoted" {|'it''s'|}
         "+STR\n+DOC\n=VAL 'it's\n-DOC\n-STR\n")

  ; Testo.create "anchor and alias"
      (check_parse "anchor and alias" "a: &x foo\nb: *x"
         "+STR\n+DOC\n+MAP\n=VAL :a\n=VAL &x :foo\n=VAL :b\n=ALI *x\n-MAP\n-DOC\n-STR\n")

  ; Testo.create "explicit document"
      (check_parse "explicit doc" "---\nfoo"
         "+STR\n+DOC ---\n=VAL :foo\n-DOC\n-STR\n")

  ; Testo.create "resolver null"
      (fun () ->
        let vs = YAMLx.of_string "~" in
        match vs with
        | [Types.Null] -> ()
        | _ -> failwith "expected Null")

  ; Testo.create "resolver bool true"
      (fun () ->
        let vs = YAMLx.of_string "true" in
        match vs with
        | [Types.Bool true] -> ()
        | _ -> failwith "expected Bool true")

  ; Testo.create "resolver int"
      (fun () ->
        let vs = YAMLx.of_string "42" in
        match vs with
        | [Types.Int 42L] -> ()
        | _ -> failwith "expected Int 42")

  ; Testo.create "resolver float"
      (fun () ->
        let vs = YAMLx.of_string "3.14" in
        match vs with
        | [Types.Float f] when Float.abs (f -. 3.14) < 1e-10 -> ()
        | _ -> failwith "expected Float ~3.14")

  ; Testo.create "resolver string (quoted)"
      (fun () ->
        let vs = YAMLx.of_string {|"42"|} in
        match vs with
        | [Types.String "42"] -> ()
        | _ -> failwith "expected String \"42\"")
  ]

(* ------------------------------------------------------------------ *)
(* Build test list from yaml-test-suite                                  *)
(* ------------------------------------------------------------------ *)

let suite_tests () =
  if not (Sys.file_exists suite_dir) then begin
    Printf.eprintf "Warning: yaml-test-suite not found at %s\n%!" suite_dir;
    []
  end else begin
    let cases = Suite_loader.load_dir suite_dir in
    (* Assign a per-id sequence number so that multiple test cases from the
       same file (same id) get unique names: 'DK95 #2: name'. *)
    let counts : (string, int ref) Hashtbl.t = Hashtbl.create 256 in
    List.map (fun (tc : Suite_loader.test_case) ->
      let seq =
        match Hashtbl.find_opt counts tc.id with
        | None   -> Hashtbl.add counts tc.id (ref 2); 1
        | Some r -> let n = !r in r := n + 1; n
      in
      let name =
        if tc.name = "" then
          if seq = 1 then tc.id
          else Printf.sprintf "%s #%d" tc.id seq
        else
          if seq = 1 then Printf.sprintf "%s: %s" tc.id tc.name
          else Printf.sprintf "%s #%d: %s" tc.id seq tc.name
      in
      Testo.create ~category:["yaml-test-suite"] ~max_duration:5.0 name (run_test_case tc)
    ) cases
  end

(* ------------------------------------------------------------------ *)
(* Entry point                                                           *)
(* ------------------------------------------------------------------ *)

let () =
  Testo.interpret_argv
    ~project_name:"yamlx"
    (fun _tags -> unit_tests () @ suite_tests ())
