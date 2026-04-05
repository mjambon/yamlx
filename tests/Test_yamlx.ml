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

(** Path to the yaml-test-suite src directory, relative to the dune test
    working directory ({i _build/default/tests/}). The directory is copied
    there by dune via the [(source_tree yaml-test-suite)] dep in [tests/dune]. *)
let suite_dir = "yaml-test-suite/src"

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
    | exception YAMLx.Error (YAMLx.Scan_error _) -> () (* expected *)
    | exception YAMLx.Error (YAMLx.Parse_error _) -> () (* expected *)
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
      | exception YAMLx.Error (YAMLx.Scan_error e) ->
          failwith
            (Printf.sprintf
               "[%s] %s: unexpected scan error at line %d col %d: %s" tc.id
               tc.name e.pos.line e.pos.column e.msg)
      | exception YAMLx.Error (YAMLx.Parse_error e) ->
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
        let vs = YAMLx.Values.of_yaml_exn "~" in
        match vs with
        | [ YAMLx.Null _ ] -> ()
        | _ -> failwith "expected Null");
    Testo.create "resolver bool true" (fun () ->
        let vs = YAMLx.Values.of_yaml_exn "true" in
        match vs with
        | [ YAMLx.Bool (_, true) ] -> ()
        | _ -> failwith "expected Bool true");
    Testo.create "resolver int" (fun () ->
        let vs = YAMLx.Values.of_yaml_exn "42" in
        match vs with
        | [ YAMLx.Int (_, 42L) ] -> ()
        | _ -> failwith "expected Int 42");
    Testo.create "resolver float" (fun () ->
        let vs = YAMLx.Values.of_yaml_exn "3.14" in
        match vs with
        | [ YAMLx.Float (_, f) ] when Float.abs (f -. 3.14) < 1e-10 -> ()
        | _ -> failwith "expected Float ~3.14");
    Testo.create "resolver string (quoted)" (fun () ->
        let vs = YAMLx.Values.of_yaml_exn {|"42"|} in
        match vs with
        | [ YAMLx.String (_, "42") ] -> ()
        | _ -> failwith "expected String \"42\"");
  ]

(* ------------------------------------------------------------------ *)
(* Encoding detection tests                                              *)
(* ------------------------------------------------------------------ *)

let encoding_tests () =
  let check_bom_error label input () =
    match YAMLx.Nodes.of_yaml_exn input with
    | exception YAMLx.Error (YAMLx.Scan_error { msg; _ })
      when String.length msg > 6 && String.sub msg 0 6 = "input " ->
        ()
    | exception YAMLx.Error (YAMLx.Scan_error { msg; _ }) ->
        failwith (label ^ ": unexpected Scan_error message: " ^ msg)
    | _ -> failwith (label ^ ": expected Scan_error for non-UTF-8 BOM")
  in
  [
    Testo.create ~category:[ "encoding" ] "UTF-32 BE BOM rejected"
      (check_bom_error "UTF-32 BE" "\x00\x00\xFE\xFF");
    Testo.create ~category:[ "encoding" ] "UTF-32 LE BOM rejected"
      (check_bom_error "UTF-32 LE" "\xFF\xFE\x00\x00");
    Testo.create ~category:[ "encoding" ] "UTF-16 BE BOM rejected"
      (check_bom_error "UTF-16 BE" "\xFE\xFF");
    Testo.create ~category:[ "encoding" ] "UTF-16 LE BOM rejected"
      (check_bom_error "UTF-16 LE" "\xFF\xFE");
    Testo.create ~category:[ "encoding" ] "UTF-8 BOM accepted" (fun () ->
        (* U+FEFF encoded as UTF-8: EF BB BF — should parse fine, BOM is stripped *)
        let nodes = YAMLx.Nodes.of_yaml_exn "\xEF\xBB\xBFhello" in
        match nodes with
        | [ YAMLx.Scalar_node { value = "hello"; _ } ] -> ()
        | _ -> failwith "expected scalar 'hello' after UTF-8 BOM");
  ]

(* ------------------------------------------------------------------ *)
(* Round-trip tests                                                       *)
(* ------------------------------------------------------------------ *)

type values = YAMLx.value list [@@deriving show { with_path = false }]

let equal_values a b = List.equal YAMLx.equal_value a b
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
    let yaml1 = YAMLx.Nodes.to_yaml (YAMLx.Nodes.of_yaml_exn input) in
    let yaml2 = YAMLx.Nodes.to_yaml (YAMLx.Nodes.of_yaml_exn yaml1) in
    Testo.(check text) yaml1 yaml2
  in
  let check_values input () =
    let before = YAMLx.Values.of_yaml_exn input in
    let after =
      YAMLx.Values.of_yaml_exn
        (YAMLx.Nodes.to_yaml (YAMLx.Nodes.of_yaml_exn input))
    in
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
    let actual = YAMLx.Nodes.to_yaml (YAMLx.Nodes.of_yaml_exn input) in
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
      "comment between sequence items becomes head of next item, not foot of \
       previous"
      (* A standalone comment at column 0 between two top-level sequence items
         must attach as a head comment of the following item, not as a foot
         comment of the preceding nested collection. *)
      (check "- a:\n    - b\n\n# something about c\n- c\n"
         "- a:\n    - b\n# something about c\n- c\n");
    Testo.create ~category:[ "comments" ]
      "comments dropped inside flow collection"
      (* Flow-context comments are discarded by the scanner; the output should
         contain no comment lines at all. *)
      (fun () ->
        (* We cannot embed a comment inside [a, b] in the source; we just verify
           that a flow sequence with no comments round-trips without adding any. *)
        let out = YAMLx.Nodes.to_yaml (YAMLx.Nodes.of_yaml_exn "[a, b]\n") in
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
        match YAMLx.Values.of_yaml_exn yaml_bomb with
        | exception YAMLx.Error (YAMLx.Expansion_limit_exceeded n) ->
            (* limit payload should equal the configured default *)
            assert (n = YAMLx.default_expansion_limit)
        | _ -> failwith "expected Expansion_limit_exceeded");
    Testo.create ~category:[ "expansion-limit" ]
      "YAML bomb raises Expansion_limit_exceeded via to_plain_yaml" (fun () ->
        let nodes = YAMLx.Nodes.of_yaml_exn yaml_bomb in
        match YAMLx.Nodes.to_plain_yaml_exn nodes with
        | exception YAMLx.Error (YAMLx.Expansion_limit_exceeded _) -> ()
        | _ -> failwith "expected Expansion_limit_exceeded");
    Testo.create ~category:[ "expansion-limit" ]
      "YAML bomb is Error via of_string_result" (fun () ->
        match YAMLx.Values.of_yaml yaml_bomb with
        | Error msg when String.sub msg 0 10 = "expansion " -> ()
        | Ok _ -> failwith "expected Error"
        | Error msg -> failwith ("unexpected error message: " ^ msg));
    Testo.create ~category:[ "expansion-limit" ] "custom low limit is respected"
      (fun () ->
        (* Even a tiny document with aliases must respect a limit of 1. *)
        let input = "x: &x foo\ny: *x\n" in
        match YAMLx.Values.of_yaml_exn ~expansion_limit:1 input with
        | exception YAMLx.Error (YAMLx.Expansion_limit_exceeded 1) -> ()
        | _ -> failwith "expected Expansion_limit_exceeded 1");
    Testo.create ~category:[ "expansion-limit" ]
      "normal aliases within default limit succeed" (fun () ->
        (* A small number of alias expansions must not be blocked. *)
        let input = "x: &x foo\na: *x\nb: *x\nc: *x\n" in
        let values = YAMLx.Values.of_yaml_exn input in
        match values with
        | [ YAMLx.Map (_, pairs) ] when List.length pairs = 4 -> ()
        | _ -> failwith "unexpected value");
    Testo.create ~category:[ "expansion-limit" ]
      "to_yaml does not expand aliases and ignores limit" (fun () ->
        (* to_yaml emits *alias syntax without expansion — the bomb is safe. *)
        let nodes = YAMLx.Nodes.of_yaml_exn yaml_bomb in
        let out = YAMLx.Nodes.to_yaml nodes in
        (* Output should contain alias references, not an explosion *)
        assert (String.length out < 10_000));
  ]

(* ------------------------------------------------------------------ *)
(* Depth limit tests                                                     *)
(* ------------------------------------------------------------------ *)

let depth_limit_tests () =
  [
    Testo.create ~category:[ "depth-limit" ]
      "deeply nested input raises Depth_limit_exceeded via parse_nodes"
      (fun () ->
        let n = YAMLx.default_max_depth + 1 in
        let input = String.make n '[' ^ String.make n ']' in
        match YAMLx.Nodes.of_yaml_exn input with
        | exception YAMLx.Error (YAMLx.Depth_limit_exceeded lim) ->
            assert (lim = YAMLx.default_max_depth)
        | _ -> failwith "expected Depth_limit_exceeded");
    Testo.create ~category:[ "depth-limit" ]
      "deeply nested input raises Depth_limit_exceeded via of_string" (fun () ->
        let n = YAMLx.default_max_depth + 1 in
        let input = String.make n '[' ^ String.make n ']' in
        match YAMLx.Values.of_yaml_exn input with
        | exception YAMLx.Error (YAMLx.Depth_limit_exceeded _) -> ()
        | _ -> failwith "expected Depth_limit_exceeded");
    Testo.create ~category:[ "depth-limit" ]
      "depth limit is Error via of_string_result" (fun () ->
        let n = YAMLx.default_max_depth + 1 in
        let input = String.make n '[' ^ String.make n ']' in
        match YAMLx.Values.of_yaml input with
        | Error msg when String.length msg >= 5 && String.sub msg 0 5 = "depth"
          ->
            ()
        | Ok _ -> failwith "expected Error"
        | Error msg -> failwith ("unexpected error message: " ^ msg));
    Testo.create ~category:[ "depth-limit" ] "custom low limit is respected"
      (fun () ->
        match YAMLx.Nodes.of_yaml_exn ~max_depth:2 "[[a]]" with
        | exception YAMLx.Error (YAMLx.Depth_limit_exceeded 2) -> ()
        | _ -> failwith "expected Depth_limit_exceeded 2");
    Testo.create ~category:[ "depth-limit" ]
      "input exactly at the limit is accepted" (fun () ->
        let n = YAMLx.default_max_depth in
        let input = String.make n '[' ^ String.make n ']' in
        ignore (YAMLx.Nodes.of_yaml_exn input));
    Testo.create ~category:[ "depth-limit" ]
      "node_height reflects correct subtree height" (fun () ->
        match YAMLx.Nodes.of_yaml_exn "- - a\n  - b\n" with
        | [ node ] ->
            (* outer sequence has height 3: seq → seq → scalar *)
            assert (YAMLx.node_height node = 3)
        | _ -> failwith "expected one document");
    Testo.create ~category:[ "depth-limit" ]
      "value_height reflects correct subtree height" (fun () ->
        match YAMLx.Values.of_yaml_exn "- - 1\n  - 2\n" with
        | [ v ] -> assert (YAMLx.value_height v = 3)
        | _ -> failwith "expected one document");
  ]

(* ------------------------------------------------------------------ *)
(* Performance / scalability tests                                       *)
(* ------------------------------------------------------------------ *)

let performance_tests () =
  [
    Testo.create ~category:[ "performance" ]
      "deeply nested flow sequences parse in linear time" (fun () ->
        (* Verify that O(n) nesting does not trigger O(n²) behaviour in the
           scanner.  We time two depths and check that doubling the depth does
           not more than quadruple the elapsed time (a 4× headroom accommodates
           noise while still catching quadratic regression). *)
        let time n =
          let input = String.make n '[' ^ String.make n ']' in
          let t0 = Unix.gettimeofday () in
          (try ignore (YAMLx.Nodes.of_yaml_exn ~max_depth:n input) with
          | _ -> ());
          Unix.gettimeofday () -. t0
        in
        let t1 = time 4000 in
        let t2 = time 8000 in
        (* t2 / t1 should be close to 2 for O(n); allow up to 8× for noise *)
        if t1 > 0.0 && t2 /. t1 > 8.0 then
          failwith
            (Printf.sprintf
               "quadratic behaviour detected: depth 4000 = %.4fs, depth 8000 = \
                %.4fs (ratio %.1f, expected ~2)"
               t1 t2 (t2 /. t1)));
  ]

(* ------------------------------------------------------------------ *)
(* Build test list from yaml-test-suite                                  *)
(* ------------------------------------------------------------------ *)

let suite_tests () =
  if not (Sys.file_exists suite_dir) then
    failwith (Printf.sprintf "yaml-test-suite not found at %s" suite_dir)
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
(* Anchor scoping tests                                                  *)
(* ------------------------------------------------------------------ *)

let anchor_tests () =
  [
    Testo.create ~category:[ "anchors" ]
      "anchor defined in one document is not visible in the next"
      (* Per the YAML 1.2 spec, anchors are document-local: an alias in
         document N must not resolve to an anchor defined in document N-1. *)
      (fun () ->
        match YAMLx.Nodes.of_yaml "---\na: &A 1\n---\nb: *A\n" with
        | Error _ -> () (* expected: undefined alias *)
        | Ok _ -> failwith "expected an error for cross-document alias *A");
    Testo.create ~category:[ "anchors" ]
      "same anchor name may be reused across documents"
      (* Each document starts with a clean anchor table, so reusing &A in
         a later document is valid and independent of the earlier one. *)
      (fun () ->
        match YAMLx.Nodes.of_yaml "---\na: &A 1\n---\nb: &A 2\nc: *A\n" with
        | Error msg -> failwith ("unexpected error: " ^ msg)
        | Ok _ -> ());
  ]

(* ------------------------------------------------------------------ *)
(* Pretty-printer output tests                                           *)
(* ------------------------------------------------------------------ *)

(** These tests pin down the exact string produced by [Nodes.to_yaml]. Unlike
    the round-trip tests they do not merely check structural equivalence — they
    verify that specific formatting choices are made. *)
let printer_tests () =
  let check input expected () =
    let actual = YAMLx.Nodes.to_yaml (YAMLx.Nodes.of_yaml_exn input) in
    Testo.(check text) expected actual
  in
  [
    (* Compact mapping notation: first key on the same line as "-" *)
    Testo.create ~category:[ "printer" ] "compact mapping — single key"
      (check "- a: 1\n" "- a: 1\n");
    Testo.create ~category:[ "printer" ] "compact mapping — multiple keys"
      (check "- a: 1\n  b: 2\n" "- a: 1\n  b: 2\n");
    Testo.create ~category:[ "printer" ]
      "compact mapping — nested block mapping value"
      (check "- a:\n    x: 1\n" "- a:\n    x: 1\n");
    Testo.create ~category:[ "printer" ]
      "compact mapping — nested block mapping value with sibling key"
      (check "- a:\n    x: 1\n  b: 2\n" "- a:\n    x: 1\n  b: 2\n");
    Testo.create ~category:[ "printer" ]
      "compact mapping — nested block sequence value"
      (check "- a:\n  - 1\n  - 2\n" "- a:\n    - 1\n    - 2\n");
    Testo.create ~category:[ "printer" ]
      "compact mapping — flow sequence value preserved"
      (check "- a: [1, 2]\n" "- a: [1, 2]\n");
    Testo.create ~category:[ "printer" ] "compact mapping — list of mappings"
      (check "- a: 1\n- b: 2\n" "- a: 1\n- b: 2\n");
    (* Non-compact fallback cases *)
    Testo.create ~category:[ "printer" ]
      "non-compact: nested block sequence uses dash-newline"
      (check "- - a\n  - b\n" "-\n  - a\n  - b\n");
    (* Scalar styles *)
    Testo.create ~category:[ "printer" ] "single-quoted scalar preserved"
      (check "'hello world'\n" "'hello world'\n");
    Testo.create ~category:[ "printer" ] "double-quoted scalar preserved"
      (check "\"hello\\nworld\"\n" "\"hello\\nworld\"\n");
    Testo.create ~category:[ "printer" ] "literal block scalar preserved"
      (check "key: |\n  line1\n  line2\n" "key: |\n    line1\n    line2\n");
    Testo.create ~category:[ "printer" ] "folded block scalar preserved"
      (check "key: >\n  folded line\n" "key: >\n    folded line\n");
    (* Collections *)
    Testo.create ~category:[ "printer" ] "flow mapping preserved"
      (check "{a: 1, b: 2}\n" "{a: 1, b: 2}\n");
    Testo.create ~category:[ "printer" ] "flow sequence preserved"
      (check "[foo, bar]\n" "[foo, bar]\n");
    Testo.create ~category:[ "printer" ] "empty block mapping"
      (check "{}\n" "{}\n");
    Testo.create ~category:[ "printer" ] "empty block sequence"
      (check "[]\n" "[]\n");
    Testo.create ~category:[ "printer" ] "sequence in mapping"
      (check "items:\n  - one\n  - two\n" "items:\n  - one\n  - two\n");
    Testo.create ~category:[ "printer" ] "mapping in mapping"
      (check "outer:\n  inner: value\n" "outer:\n  inner: value\n");
    (* Anchors and aliases *)
    Testo.create ~category:[ "printer" ] "anchor and alias"
      (check "a: &x foo\nb: *x\n" "a: &x foo\nb: *x\n");
    (* Multi-document *)
    Testo.create ~category:[ "printer" ]
      "multi-document: second gets --- marker"
      (check "foo\n---\nbar\n" "foo\n--- bar\n");
    Testo.create ~category:[ "printer" ]
      "multi-document: block collection gets own line after ---"
      (check "foo\n---\na: 1\n" "foo\n---\na: 1\n");
  ]

(* ------------------------------------------------------------------ *)
(* Entry point                                                           *)
(* ------------------------------------------------------------------ *)

let () =
  Testo.interpret_argv ~project_name:"yamlx" (fun _tags ->
      unit_tests () @ encoding_tests () @ roundtrip_tests () @ comment_tests ()
      @ anchor_tests () @ printer_tests () @ expansion_limit_tests ()
      @ depth_limit_tests () @ performance_tests () @ suite_tests ())
