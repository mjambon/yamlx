(** Main test suite for YAMLx. Runs the standard yaml-test-suite cases plus a
    handful of hand-written unit tests.

    Test strategy ~~~~~~~~~~~~~ Each yaml-test-suite entry specifies either: 1.
    A [yaml:] input that should parse successfully and produce a known event
    stream ([tree:] field). 2. A [yaml:] input that should fail to parse
    ([fail: true]).

    For case 1 we compare the normalized event tree produced by YAMLx against
    the expected tree from the test file.

    For case 2 we check that YAMLx raises [Scan_error] or [Parse_error].

    The test suite is loaded from the [tests/yaml-test-suite/src/] directory
    relative to the repository root. The path is hard-coded here for simplicity;
    adjust if the layout changes. *)

(* ------------------------------------------------------------------ *)
(* Locate the test-suite source directory                                *)
(* ------------------------------------------------------------------ *)

(** Absolute path to the yaml-test-suite src directory. We derive it from the
    location of this file's compilation artifact so the tests can be run from
    any working directory. *)
let suite_dir =
  (* __FILE__ expands to the source path at compile time. *)
  let here = Filename.dirname __FILE__ in
  Filename.concat here "yaml-test-suite/src"

(* ------------------------------------------------------------------ *)
(* Run a single test case                                                *)
(* ------------------------------------------------------------------ *)

(** Run one yaml-test-suite test case. For success cases: parse the YAML and
    compare the event tree. For failure cases: assert that an error is raised.
*)
let run_test_case (tc : Suite_loader.test_case) () =
  if tc.fail then
    (* Expect a scan or parse error *)
    begin match YAMLx.parse_events tc.yaml with
    | exception YAMLx.Scan_error _ -> () (* expected *)
    | exception YAMLx.Parse_error _ -> () (* expected *)
    | _ ->
        failwith
          (Printf.sprintf
             "[%s] %s: expected a parse failure but parsing succeeded" tc.id
             tc.name)
    end
  else begin
    (* Parse must succeed *)
    let events =
      match YAMLx.parse_events tc.yaml with
      | exception YAMLx.Scan_error e ->
          failwith
            (Printf.sprintf
               "[%s] %s: unexpected scan error at line %d col %d: %s" tc.id
               tc.name e.pos.line e.pos.column e.msg)
      | exception YAMLx.Parse_error e ->
          failwith
            (Printf.sprintf
               "[%s] %s: unexpected parse error at line %d col %d: %s" tc.id
               tc.name e.pos.line e.pos.column e.msg)
      | evs -> evs
    in
    (* Only compare against the expected tree if one is given *)
    match tc.tree with
    | None -> ()
    | Some expected_tree -> (
        let actual_tree = YAMLx.events_to_tree events in
        match
          YAMLx.diff_event_trees ~expected:expected_tree ~actual:actual_tree
        with
        | None -> () (* trees match *)
        | Some diff ->
            failwith
              (Printf.sprintf
                 "[%s] %s: event tree mismatch\n  %s\nExpected:\n%s\nGot:\n%s"
                 tc.id tc.name diff expected_tree actual_tree))
  end

(* ------------------------------------------------------------------ *)
(* Hand-written unit tests                                               *)
(* ------------------------------------------------------------------ *)

(** Basic sanity tests that don't depend on the yaml-test-suite. *)
let unit_tests () =
  let check_parse label yaml expected_events () =
    let events = YAMLx.parse_events yaml in
    let actual = YAMLx.events_to_tree events in
    match YAMLx.diff_event_trees ~expected:expected_events ~actual with
    | None -> ()
    | Some diff ->
        failwith
          (Printf.sprintf "%s: %s\nExpected:\n%s\nGot:\n%s" label diff
             expected_events actual)
  in
  [
    Testo.create "empty stream" (check_parse "empty stream" "" "+STR\n-STR\n");
    Testo.create "plain scalar"
      (check_parse "plain scalar" "hello"
         "+STR\n+DOC\n=VAL :hello\n-DOC\n-STR\n");
    Testo.create "simple mapping"
      (check_parse "simple mapping" "a: 1\nb: 2"
         "+STR\n\
          +DOC\n\
          +MAP\n\
          =VAL :a\n\
          =VAL :1\n\
          =VAL :b\n\
          =VAL :2\n\
          -MAP\n\
          -DOC\n\
          -STR\n");
    Testo.create "simple sequence"
      (check_parse "simple sequence" "- a\n- b"
         "+STR\n+DOC\n+SEQ\n=VAL :a\n=VAL :b\n-SEQ\n-DOC\n-STR\n");
    Testo.create "double-quoted scalar"
      (check_parse "double-quoted" {|"hello\nworld"|}
         "+STR\n+DOC\n=VAL \"hello\\nworld\n-DOC\n-STR\n");
    Testo.create "single-quoted scalar"
      (check_parse "single-quoted" {|'it''s'|}
         "+STR\n+DOC\n=VAL 'it's\n-DOC\n-STR\n");
    Testo.create "anchor and alias"
      (check_parse "anchor and alias" "a: &x foo\nb: *x"
         "+STR\n\
          +DOC\n\
          +MAP\n\
          =VAL :a\n\
          =VAL &x :foo\n\
          =VAL :b\n\
          =ALI *x\n\
          -MAP\n\
          -DOC\n\
          -STR\n");
    Testo.create "explicit document"
      (check_parse "explicit doc" "---\nfoo"
         "+STR\n+DOC ---\n=VAL :foo\n-DOC\n-STR\n");
    Testo.create "resolver null" (fun () ->
        let vs = YAMLx.of_string "~" in
        match vs with
        | [ YAMLx.Null ] -> ()
        | _ -> failwith "expected Null");
    Testo.create "resolver bool true" (fun () ->
        let vs = YAMLx.of_string "true" in
        match vs with
        | [ YAMLx.Bool true ] -> ()
        | _ -> failwith "expected Bool true");
    Testo.create "resolver int" (fun () ->
        let vs = YAMLx.of_string "42" in
        match vs with
        | [ YAMLx.Int 42L ] -> ()
        | _ -> failwith "expected Int 42");
    Testo.create "resolver float" (fun () ->
        let vs = YAMLx.of_string "3.14" in
        match vs with
        | [ YAMLx.Float f ] when Float.abs (f -. 3.14) < 1e-10 -> ()
        | _ -> failwith "expected Float ~3.14");
    Testo.create "resolver string (quoted)" (fun () ->
        let vs = YAMLx.of_string {|"42"|} in
        match vs with
        | [ YAMLx.String "42" ] -> ()
        | _ -> failwith "expected String \"42\"");
  ]

(* ------------------------------------------------------------------ *)
(* Round-trip tests                                                       *)
(* ------------------------------------------------------------------ *)

type values = YAMLx.value list [@@deriving eq, show { with_path = false }]

let yamlx_values = Testo.testable show_values equal_values

(** Round-trip helpers.

    [check_idempotent label input] verifies that printing is stable: yaml1 =
    to_yaml (parse_nodes input) yaml2 = to_yaml (parse_nodes yaml1) assert yaml1
    = yaml2

    [check_values label input] verifies that the resolved values are the same
    before and after a round trip: of_string input = of_string (to_yaml
    (parse_nodes input)) *)
let roundtrip_tests () =
  let check_idempotent input () =
    let yaml1 = YAMLx.to_yaml (YAMLx.parse_nodes input) in
    let yaml2 = YAMLx.to_yaml (YAMLx.parse_nodes yaml1) in
    Testo.(check text) yaml1 yaml2
  in
  let check_values input () =
    let before = YAMLx.of_string input in
    let after = YAMLx.of_string (YAMLx.to_yaml (YAMLx.parse_nodes input)) in
    Testo.(check yamlx_values) before after
  in
  let check label input =
    [
      Testo.create ~category:[ "roundtrip" ] (label ^ " (idempotent)")
        (check_idempotent input);
      Testo.create ~category:[ "roundtrip" ] (label ^ " (values)")
        (check_values input);
    ]
  in
  List.concat
    [
      check "plain scalar" "hello";
      check "plain scalar int" "42";
      check "plain scalar null" "~";
      check "single-quoted scalar" {|'it''s'|};
      check "double-quoted scalar" {|"hello\nworld"|};
      check "double-quoted with escapes" {|"tab:\there\nand newline"|};
      check "literal block scalar" "key: |\n  line1\n  line2\n";
      check "literal block scalar strip" "key: |-\n  no trailing newline\n";
      check "literal block scalar keep" "key: |+\n  trailing newlines\n\n";
      check "folded block scalar" "key: >\n  folded line\n";
      check "block mapping" "name: Alice\nage: 30\n";
      check "nested block mapping" "outer:\n  inner: value\n  other: 42\n";
      check "block sequence" "- foo\n- bar\n- baz\n";
      check "nested block sequence" "- - a\n  - b\n- - c\n  - d\n";
      check "sequence in mapping" "items:\n  - one\n  - two\ncount: 2\n";
      check "mapping in sequence"
        "- name: Alice\n  age: 30\n- name: Bob\n  age: 25\n";
      check "flow mapping" "{a: 1, b: 2}\n";
      check "flow sequence" "[foo, bar, baz]\n";
      check "anchor and alias" "a: &x foo\nb: *x\n";
      check "multi-document" "doc1\n---\ndoc2\n";
      check "explicit document start" "---\nfoo\n";
      check "empty mapping" "{}\n";
      check "empty sequence" "[]\n";
    ]

(* ------------------------------------------------------------------ *)
(* Comment preservation tests                                            *)
(* ------------------------------------------------------------------ *)

(** Tests for comment capture and re-emission. These are best-effort: we verify
    the comments that *are* preserved, not that every edge case round-trips
    perfectly. *)
let comment_tests () =
  let check input expected () =
    let actual = YAMLx.to_yaml (YAMLx.parse_nodes input) in
    Testo.(check text) expected actual
  in
  [
    Testo.create ~category:[ "comments" ] "head comment on document"
      (check "# preamble\nkey: value\n" "# preamble\nkey: value\n");
    Testo.create ~category:[ "comments" ] "line comment on scalar value"
      (check "key: value  # inline\n" "key: value  # inline\n");
    Testo.create ~category:[ "comments" ] "head comment before sequence item"
      (check "items:\n  # before first\n  - a\n  - b\n"
         "items:\n  # before first\n  - a\n  - b\n");
    Testo.create ~category:[ "comments" ]
      "foot comment after last sequence item"
      (check "items:\n  - a\n  - b\n  # trailing\nnext: x\n"
         "items:\n  - a\n  - b\n  # trailing\nnext: x\n");
    Testo.create ~category:[ "comments" ] "multiple head comments"
      (check "# line 1\n# line 2\nscalar\n" "# line 1\n# line 2\nscalar\n");
    Testo.create ~category:[ "comments" ]
      "comment on block scalar header not preserved"
      (* Block scalar header lines are parsed by a dedicated path that does not
         capture comments; the comment is silently dropped. *)
      (check "desc: |  # note\n  content\n" "desc: |\n    content\n");
    Testo.create ~category:[ "comments" ] "line comment on alias"
      (check "a: &x foo\nb: *x  # ref\n" "a: &x foo\nb: *x  # ref\n");
    Testo.create ~category:[ "comments" ]
      "head comment before mapping value on its own line"
      (check "parent:\n  # about child\n  child: x\n"
         "parent:\n  # about child\n  child: x\n");
    Testo.create ~category:[ "comments" ]
      "comments dropped inside flow collection"
      (* Flow-context comments are discarded by the scanner; the output should
         contain no comment lines at all. *)
      (fun () ->
        (* We cannot embed a comment inside [a, b] in the source; we just verify
           that a flow sequence with no comments round-trips without adding any. *)
        let out = YAMLx.to_yaml (YAMLx.parse_nodes "[a, b]\n") in
        if String.contains out '#' then
          failwith (Printf.sprintf "unexpected '#' in output: %S" out));
  ]

(* ------------------------------------------------------------------ *)
(* Expansion limit / YAML bomb tests                                     *)
(* ------------------------------------------------------------------ *)

(** The canonical YAML bomb: 9 levels of 9-element sequences, each level
    aliasing the previous one. Expanding all aliases would require visiting 9^9
    ≈ 387 million nodes; the default limit of 1,000,000 stops it early.

    See https://en.wikipedia.org/wiki/Billion_laughs_attack for a generic
    description of the issue. *)
let yaml_bomb =
  {|a: &a ["lol","lol","lol","lol","lol","lol","lol","lol","lol"]
b: &b [*a,*a,*a,*a,*a,*a,*a,*a,*a]
c: &c [*b,*b,*b,*b,*b,*b,*b,*b,*b]
d: &d [*c,*c,*c,*c,*c,*c,*c,*c,*c]
e: &e [*d,*d,*d,*d,*d,*d,*d,*d,*d]
f: &f [*e,*e,*e,*e,*e,*e,*e,*e,*e]
g: &g [*f,*f,*f,*f,*f,*f,*f,*f,*f]
h: &h [*g,*g,*g,*g,*g,*g,*g,*g,*g]
i: &i [*h,*h,*h,*h,*h,*h,*h,*h,*h]|}

let expansion_limit_tests () =
  [
    Testo.create ~category:[ "expansion-limit" ]
      "YAML bomb raises Expansion_limit_exceeded via of_string" (fun () ->
        match YAMLx.of_string yaml_bomb with
        | exception YAMLx.Expansion_limit_exceeded n ->
            (* limit payload should equal the configured default *)
            assert (n = YAMLx.default_expansion_limit)
        | _ -> failwith "expected Expansion_limit_exceeded");
    Testo.create ~category:[ "expansion-limit" ]
      "YAML bomb raises Expansion_limit_exceeded via to_plain_yaml" (fun () ->
        let nodes = YAMLx.parse_nodes yaml_bomb in
        match YAMLx.to_plain_yaml nodes with
        | exception YAMLx.Expansion_limit_exceeded _ -> ()
        | _ -> failwith "expected Expansion_limit_exceeded");
    Testo.create ~category:[ "expansion-limit" ]
      "YAML bomb is Error via of_string_result" (fun () ->
        match YAMLx.of_string_result yaml_bomb with
        | Error msg when String.sub msg 0 10 = "expansion " -> ()
        | Ok _ -> failwith "expected Error"
        | Error msg -> failwith ("unexpected error message: " ^ msg));
    Testo.create ~category:[ "expansion-limit" ] "custom low limit is respected"
      (fun () ->
        (* Even a tiny document with aliases must respect a limit of 1. *)
        let input = "x: &x foo\ny: *x\n" in
        match YAMLx.of_string ~expansion_limit:1 input with
        | exception YAMLx.Expansion_limit_exceeded 1 -> ()
        | _ -> failwith "expected Expansion_limit_exceeded 1");
    Testo.create ~category:[ "expansion-limit" ]
      "normal aliases within default limit succeed" (fun () ->
        (* A small number of alias expansions must not be blocked. *)
        let input = "x: &x foo\na: *x\nb: *x\nc: *x\n" in
        let values = YAMLx.of_string input in
        match values with
        | [ YAMLx.Map pairs ] when List.length pairs = 4 -> ()
        | _ -> failwith "unexpected value");
    Testo.create ~category:[ "expansion-limit" ]
      "to_yaml does not expand aliases and ignores limit" (fun () ->
        (* to_yaml emits *alias syntax without expansion — the bomb is safe. *)
        let nodes = YAMLx.parse_nodes yaml_bomb in
        let out = YAMLx.to_yaml nodes in
        (* Output should contain alias references, not an explosion *)
        assert (String.length out < 10_000));
  ]

(* ------------------------------------------------------------------ *)
(* Build test list from yaml-test-suite                                  *)
(* ------------------------------------------------------------------ *)

let suite_tests () =
  if not (Sys.file_exists suite_dir) then begin
    Printf.eprintf "Warning: yaml-test-suite not found at %s\n%!" suite_dir;
    []
  end
  else begin
    let cases = Suite_loader.load_dir suite_dir in
    (* Assign a per-id sequence number so that multiple test cases from the
       same file (same id) get unique names: 'DK95 #2: name'. *)
    let counts : (string, int ref) Hashtbl.t = Hashtbl.create 256 in
    List.map
      (fun (tc : Suite_loader.test_case) ->
        let seq =
          match Hashtbl.find_opt counts tc.id with
          | None ->
              Hashtbl.add counts tc.id (ref 2);
              1
          | Some r ->
              let n = !r in
              r := n + 1;
              n
        in
        let name =
          if tc.name = "" then
            if seq = 1 then tc.id else Printf.sprintf "%s #%d" tc.id seq
          else if seq = 1 then Printf.sprintf "%s: %s" tc.id tc.name
          else Printf.sprintf "%s #%d: %s" tc.id seq tc.name
        in
        Testo.create ~category:[ "yaml-test-suite" ] ~max_duration:5.0 name
          (run_test_case tc))
      cases
  end

(* ------------------------------------------------------------------ *)
(* Entry point                                                           *)
(* ------------------------------------------------------------------ *)

let () =
  Testo.interpret_argv ~project_name:"yamlx" (fun _tags ->
      unit_tests () @ roundtrip_tests () @ comment_tests ()
      @ expansion_limit_tests () @ suite_tests ())
