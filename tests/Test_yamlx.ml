(** Main test suite for YAMLx. Runs the standard yaml-test-suite cases plus a
    handful of hand-written unit tests. *)

(*
    Test strategy
    ~~~~~~~~~~~~~
    Each yaml-test-suite entry specifies either:
      1. A [yaml:] input that should parse successfully and produce a known
         event stream ([tree:] field).
      2. A [yaml:] input that should fail to parse ([fail: true]).

    For case 1 we compare the normalized event tree produced by YAMLx against
    the expected tree from the test file.

    For case 2 we check that YAMLx raises [Scan_error] or [Parse_error].

    The test suite is loaded from the [tests/yaml-test-suite/src/] directory
    relative to the repository root.  The path is hard-coded here for
    simplicity; adjust if the layout changes.
*)

(* ------------------------------------------------------------------ *)
(* Locate the test-suite source directory                                *)
(* ------------------------------------------------------------------ *)

(** Path to the yaml-test-suite src directory, relative to the dune test working
    directory ({i _build/default/tests/}). The directory is copied there by dune
    via the [(source_tree yaml-test-suite)] dep in [tests/dune]. *)
let suite_dir = "yaml-test-suite/src"

(* ------------------------------------------------------------------ *)
(* Run a single test case                                                *)
(* ------------------------------------------------------------------ *)

(** Normalize an event tree string for comparison: strip leading/trailing
    whitespace from each line, drop empty lines, and re-join with ['\n']. This
    removes the visual indentation used in the yaml-test-suite tree format and
    makes comparisons platform-independent. *)
let canonical_tree s =
  String.split_on_char '\n' s
  |> List.map String.trim
  |> List.filter (fun l -> l <> "")
  |> String.concat "\n"
  |> fun t -> if t = "" then t else t ^ "\n"

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
    let events = YAMLx.parse_events tc.yaml in
    (* Only compare against the expected tree if one is given *)
    match tc.tree with
    | None -> ()
    | Some expected_tree ->
        let actual_tree = YAMLx.events_to_tree events in
        Testo.(check text)
          (canonical_tree expected_tree)
          (canonical_tree actual_tree)
  end

(* ------------------------------------------------------------------ *)
(* Hand-written unit tests                                               *)
(* ------------------------------------------------------------------ *)

(** Basic sanity tests that don't depend on the yaml-test-suite. *)
let unit_tests () =
  let check_parse _label yaml expected_events () =
    let events = YAMLx.parse_events yaml in
    let actual = YAMLx.events_to_tree events in
    Testo.(check text) (canonical_tree expected_events) (canonical_tree actual)
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
(* Source location tests                                                 *)
(* ------------------------------------------------------------------ *)

(** Check that node locs exclude trailing whitespace and line terminators.

    Each test parses a small YAML snippet, extracts the scalar node of interest,
    and asserts that its [loc.end_pos] points right after the last content
    character — not after trailing spaces, line breaks, or the start of the next
    line. *)
let loc_tests () =
  let open YAMLx in
  (* Extract the [loc] of the first (and only) document root. *)
  let root_loc yaml =
    match Nodes.of_yaml_exn yaml with
    | [ node ] -> (
        match node with
        | Scalar_node { loc; _ } -> loc
        | Mapping_node { loc; _ } -> loc
        | Sequence_node { loc; _ } -> loc
        | Alias_node { loc; _ } -> loc)
    | _ -> failwith "expected exactly one document"
  in
  (* Extract the loc of the value scalar in a single-pair mapping. *)
  let value_loc yaml =
    match Nodes.of_yaml_exn yaml with
    | [ Mapping_node { pairs = [ (_, v) ]; _ } ] -> (
        match v with
        | Scalar_node { loc; _ } -> loc
        | _ -> failwith "expected scalar value")
    | _ -> failwith "expected single-pair mapping"
  in
  let check_end name yaml get_loc expected_line expected_col expected_offset =
    Testo.create ~category:[ "loc" ] name (fun () ->
        let (loc : YAMLx.loc) = get_loc yaml in
        let e = loc.end_pos in
        if
          e.line <> expected_line || e.column <> expected_col
          || e.offset <> expected_offset
        then
          failwith
            (Printf.sprintf
               "expected end_pos {line=%d col=%d offset=%d}, got {line=%d \
                col=%d offset=%d}"
               expected_line expected_col expected_offset e.line e.column
               e.offset))
  in
  [
    (* Plain scalar at top level: trailing newline must not be in loc *)
    check_end "plain scalar: end_pos excludes trailing newline" "a\n" root_loc 1
      1 1;
    (* Plain scalar in flow sequence: trailing space must not be in loc *)
    check_end "plain scalar in flow: end_pos excludes trailing space" "[ a ]\n"
      (fun yaml ->
        match Nodes.of_yaml_exn yaml with
        | [ Sequence_node { items = [ Scalar_node { loc; _ } ]; _ } ] -> loc
        | _ -> failwith "expected flow sequence with one scalar")
      1 3 3;
    (* Multiline plain scalar: end_pos is right after the last content char *)
    check_end "multiline plain scalar: end_pos on last content line"
      "foo\nbar\n" root_loc 2 3 7;
    (* Block literal scalar: end_pos is right at the newline ending the last
       content line, not at the start of the next line *)
    check_end "block literal scalar: end_pos excludes trailing newline"
      "desc: |\n  content\n" value_loc 2 9 17;
    (* Block scalar with strip chomp *)
    check_end "block scalar strip: end_pos excludes trailing newline"
      "desc: |-\n  content\n" value_loc 2 9 18;
    (* Plain scalar in mapping key: end right after key *)
    check_end "mapping key: end_pos excludes trailing colon/space"
      "key: value\n"
      (fun yaml ->
        match Nodes.of_yaml_exn yaml with
        | [ Mapping_node { pairs = [ (Scalar_node { loc; _ }, _) ]; _ } ] -> loc
        | _ -> failwith "expected mapping key")
      1 3 3;
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
  (* Check that [input] produces the same event tree as the LF reference. *)
  let check_line_ending input () =
    let reference = "a: 1\nb: 2\n" in
    let expected = YAMLx.events_to_tree (YAMLx.parse_events reference) in
    let actual = YAMLx.events_to_tree (YAMLx.parse_events input) in
    Testo.(check text) (canonical_tree expected) (canonical_tree actual)
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
    (* YAML 1.2 §5.4: CR, CRLF, NEL, LS, PS are all line breaks *)
    Testo.create ~category:[ "encoding" ] "CRLF line endings normalized to LF"
      (check_line_ending "a: 1\r\nb: 2\r\n");
    Testo.create ~category:[ "encoding" ]
      "bare CR line endings normalized to LF"
      (check_line_ending "a: 1\rb: 2\r");
    Testo.create ~category:[ "encoding" ]
      "NEL (U+0085) line endings normalized to LF"
      (* NEL encoded as UTF-8: C2 85 *)
      (check_line_ending "a: 1\xC2\x85b: 2\xC2\x85");
    Testo.create ~category:[ "encoding" ]
      "LS (U+2028) line endings normalized to LF"
      (* LS encoded as UTF-8: E2 80 A8 *)
      (check_line_ending "a: 1\xE2\x80\xA8b: 2\xE2\x80\xA8");
    Testo.create ~category:[ "encoding" ]
      "PS (U+2029) line endings normalized to LF"
      (* PS encoded as UTF-8: E2 80 A9 *)
      (check_line_ending "a: 1\xE2\x80\xA9b: 2\xE2\x80\xA9");
    (* Invalid UTF-8: must raise Scan_error, not Failure *)
    Testo.create ~category:[ "encoding" ] "invalid UTF-8 byte raises Scan_error"
      (fun () ->
        (* 0xB6 is not a valid UTF-8 lead byte *)
        match YAMLx.Nodes.of_yaml_exn "hello: \xB6" with
        | exception YAMLx.Error (YAMLx.Scan_error _) -> ()
        | _ -> failwith "expected Scan_error for invalid UTF-8 byte");
    Testo.create ~category:[ "encoding" ]
      "truncated 2-byte UTF-8 sequence raises Scan_error" (fun () ->
        (* 0xC2 starts a 2-byte sequence but nothing follows *)
        match YAMLx.Nodes.of_yaml_exn "hello: \xC2" with
        | exception YAMLx.Error (YAMLx.Scan_error _) -> ()
        | _ -> failwith "expected Scan_error for truncated UTF-8 sequence");
    Testo.create ~category:[ "encoding" ]
      "truncated 4-byte UTF-8 sequence raises Scan_error" (fun () ->
        (* 0xF0 starts a 4-byte sequence but only one continuation byte follows *)
        match YAMLx.Nodes.of_yaml_exn "hello: \xF0\x9F" with
        | exception YAMLx.Error (YAMLx.Scan_error _) -> ()
        | _ -> failwith "expected Scan_error for truncated UTF-8 sequence");
    (* Invalid escape sequences in double-quoted scalars *)
    Testo.create ~category:[ "encoding" ]
      "non-hex digit in \\uXXXX escape raises Scan_error" (fun () ->
        match YAMLx.Nodes.of_yaml_exn {|"\uZZZZ"|} with
        | exception YAMLx.Error (YAMLx.Scan_error _) -> ()
        | _ -> failwith "expected Scan_error for non-hex digit in \\u escape");
    Testo.create ~category:[ "encoding" ]
      "non-hex digit in \\xXX escape raises Scan_error" (fun () ->
        match YAMLx.Nodes.of_yaml_exn {|"\xZZ"|} with
        | exception YAMLx.Error (YAMLx.Scan_error _) -> ()
        | _ -> failwith "expected Scan_error for non-hex digit in \\x escape");
    Testo.create ~category:[ "encoding" ]
      "\\UXXXXXXXX out of Unicode range raises Scan_error" (fun () ->
        match YAMLx.Nodes.of_yaml_exn {|"\U11000000"|} with
        | exception YAMLx.Error (YAMLx.Scan_error _) -> ()
        | _ -> failwith "expected Scan_error for out-of-range \\U escape");
    Testo.create ~category:[ "encoding" ]
      "valid \\UXXXXXXXX emoji parses correctly" (fun () ->
        (* U+1F600 GRINNING FACE, encoded as 4-byte UTF-8 in the output *)
        match YAMLx.Values.of_yaml_exn {|"\U0001F600"|} with
        | [ YAMLx.String (_, s) ] when s = "\xF0\x9F\x98\x80" -> ()
        | _ -> failwith "expected grinning-face string from \\U0001F600");
  ]

(* ------------------------------------------------------------------ *)
(* Round-trip tests                                                       *)
(* ------------------------------------------------------------------ *)

type values = YAMLx.value list [@@deriving show { with_path = false }]

let equal_values a b = List.equal YAMLx.Value.equal a b
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
      "comment on block scalar header is preserved"
      (check "desc: |  # note\n  content\n" "desc: |  # note\n    content\n");
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
    Testo.create ~category:[ "comments" ]
      "double-hash head comment preserved verbatim"
      (* '## hello' must not become '# # hello' *)
      (check "## hello\na\n" "## hello\na\n");
    Testo.create ~category:[ "comments" ]
      "comment with no space after hash preserved verbatim"
      (* '#hello' (no space) must not become '# hello' *)
      (check "#hello\na\n" "#hello\na\n");
    Testo.create ~category:[ "comments" ]
      "double-hash inline comment preserved verbatim"
      (check "a: x  ## note\n" "a: x  ## note\n");
    Testo.create ~category:[ "comments" ]
      "inline comment with no space after hash preserved verbatim"
      (check "a: x  #note\n" "a: x  #note\n");
  ]

(* ------------------------------------------------------------------ *)
(* Node.has_comments / Nodes.has_comments tests                          *)
(* ------------------------------------------------------------------ *)

let has_comments_tests () =
  let parse s = YAMLx.Nodes.of_yaml_exn s in
  let has s = YAMLx.Nodes.has_comments (parse s) in
  [
    Testo.create ~category:[ "has-comments" ] "no comments → false" (fun () ->
        assert (not (has "key: value\n")));
    Testo.create ~category:[ "has-comments" ] "head comment → true" (fun () ->
        assert (has "# preamble\nkey: value\n"));
    Testo.create ~category:[ "has-comments" ] "line comment → true" (fun () ->
        assert (has "key: value  # note\n"));
    Testo.create ~category:[ "has-comments" ] "foot comment on sequence → true"
      (fun () -> assert (has "- a\n- b\n# trailing\n"));
    Testo.create ~category:[ "has-comments" ]
      "comment nested in mapping value → true" (fun () ->
        assert (has "outer:\n  # inner\n  inner: x\n"));
    Testo.create ~category:[ "has-comments" ]
      "comment in second document → true" (fun () ->
        assert (has "a: 1\n---\n# doc2\nb: 2\n"));
    Testo.create ~category:[ "has-comments" ]
      "Node.has_comments on single node without comment → false" (fun () ->
        let node = List.hd (parse "plain\n") in
        assert (not (YAMLx.Node.has_comments node)));
    Testo.create ~category:[ "has-comments" ]
      "Node.has_comments on single node with head comment → true" (fun () ->
        let node = List.hd (parse "# head\nplain\n") in
        assert (YAMLx.Node.has_comments node));
  ]

(* ------------------------------------------------------------------ *)
(* Node-level comment attachment tests                                   *)
(* ------------------------------------------------------------------ *)

(** Verify that the comment attacher places comments on the correct node fields
    rather than just checking that roundtrip output is unchanged. *)
let comment_node_tests () =
  let ss = Testo.(list string) in
  let os = Testo.(option string) in
  [
    Testo.create ~category:[ "comment-nodes" ]
      "head comment attaches to sequence value, not mapping key" (fun () ->
        (* "# before first" is between the mapping key "items" and its sequence
           value.  It must land on the sequence's head_comments. *)
        let nodes =
          YAMLx.Nodes.of_yaml_exn "items:\n  # before first\n  - a\n  - b\n"
        in
        match nodes with
        | [ Mapping_node { pairs = [ (_, seq) ]; _ } ] -> (
            match seq with
            | Sequence_node { head_comments; _ } ->
                Testo.check ss [ " before first" ] head_comments
            | _ -> failwith "expected Sequence_node for value")
        | _ -> failwith "expected single-pair mapping");
    Testo.create ~category:[ "comment-nodes" ]
      "trailing comment attaches as foot of sequence, not last item" (fun () ->
        (* "# trailing" is after the last item of the sequence and must be in
           the sequence's foot_comments (indented correctly), not the last
           item's foot_comments (which would be unindented). *)
        let nodes =
          YAMLx.Nodes.of_yaml_exn
            "items:\n  - a\n  - b\n  # trailing\nnext: x\n"
        in
        match nodes with
        | [ Mapping_node { pairs = (_, seq) :: _; _ } ] -> (
            match seq with
            | Sequence_node { items; foot_comments; _ } -> (
                Testo.check ss [ " trailing" ] foot_comments;
                (* also verify the last item itself has no foot comment *)
                match List.rev items with
                | Scalar_node { foot_comments = item_feet; _ } :: _ ->
                    Testo.check ss [] item_feet
                | _ -> failwith "expected scalar last item")
            | _ -> failwith "expected Sequence_node")
        | _ -> failwith "expected mapping with at least one pair");
    Testo.create ~category:[ "comment-nodes" ]
      "block scalar header comment attaches as line_comment" (fun () ->
        (* "| # note" — the comment on the block scalar header line must be
           captured as the scalar's line_comment. *)
        let nodes = YAMLx.Nodes.of_yaml_exn "desc: |  # note\n  content\n" in
        match nodes with
        | [ Mapping_node { pairs = [ (_, scalar) ]; _ } ] -> (
            match scalar with
            | Scalar_node { line_comment; style = Literal; _ } ->
                Testo.check os (Some " note") line_comment
            | _ -> failwith "expected Literal Scalar_node for value")
        | _ -> failwith "expected single-pair mapping");
    Testo.create ~category:[ "comment-nodes" ]
      "comment between key and value attaches as head of value" (fun () ->
        (* "# about child" is on a line between "parent:" and "child: x".
           It must land on the inner mapping's head_comments, not disappear
           into the outer key's foot_comments. *)
        let nodes =
          YAMLx.Nodes.of_yaml_exn "parent:\n  # about child\n  child: x\n"
        in
        match nodes with
        | [ Mapping_node { pairs = [ (_, inner) ]; _ } ] -> (
            match inner with
            | Mapping_node { head_comments; _ } ->
                Testo.check ss [ " about child" ] head_comments
            | _ -> failwith "expected Mapping_node for value")
        | _ -> failwith "expected single-pair mapping");
    Testo.create ~category:[ "comment-nodes" ]
      "comment before --- attaches as foot of preceding document" (fun () ->
        (* "#c" appears after the content of doc 1 and before the "---"
           separator of doc 2.  It must be in doc 1's foot_comments and must
           NOT appear in doc 2's head_comments. *)
        let nodes = YAMLx.Nodes.of_yaml_exn "xxx\n#c\n---\nyyy\n" in
        match nodes with
        | [ doc1; doc2 ] -> (
            (match doc1 with
            | Scalar_node { value = "xxx"; foot_comments; _ } ->
                Testo.check ss [ "c" ] foot_comments
            | _ -> failwith "expected scalar 'xxx' for doc1");
            match doc2 with
            | Scalar_node { value = "yyy"; head_comments; _ } ->
                Testo.check ss [] head_comments
            | _ -> failwith "expected scalar 'yyy' for doc2")
        | _ -> failwith "expected exactly two documents");
    Testo.create ~category:[ "comment-nodes" ]
      "head comment attaches to document root node" (fun () ->
        (* A comment before the only node in a document belongs on that
           node's head_comments. *)
        let nodes = YAMLx.Nodes.of_yaml_exn "# preamble\nkey: value\n" in
        match nodes with
        | [ Mapping_node { head_comments; _ } ] ->
            Testo.check ss [ " preamble" ] head_comments
        | _ -> failwith "expected single mapping");
    Testo.create ~category:[ "comment-nodes" ]
      "line comment attaches to scalar value node" (fun () ->
        let nodes = YAMLx.Nodes.of_yaml_exn "key: value  # inline\n" in
        match nodes with
        | [ Mapping_node { pairs = [ (_, v) ]; _ } ] -> (
            match v with
            | Scalar_node { line_comment; _ } ->
                Testo.check os (Some " inline") line_comment
            | _ -> failwith "expected scalar value")
        | _ -> failwith "expected single-pair mapping");
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
           scanner.  We measure per-iteration time by running until at least
           [min_total_s] seconds have elapsed; this ensures a reliable
           baseline even on systems with coarse timers (e.g. Windows where
           a single sub-millisecond run cannot be measured accurately).
           Doubling the depth should roughly double the per-iteration time
           (O(n)); we allow up to 16× headroom before failing. *)
        let min_total_s = 0.05 in
        let time_per_iter n =
          let input = String.make n '[' ^ String.make n ']' in
          (* warm-up run to fill caches *)
          (try ignore (YAMLx.Nodes.of_yaml_exn ~max_depth:n input) with
          | _ -> ());
          (* accumulate until total >= min_total_s *)
          let total = ref 0.0 in
          let count = ref 0 in
          while !total < min_total_s do
            let t0 = Unix.gettimeofday () in
            (try ignore (YAMLx.Nodes.of_yaml_exn ~max_depth:n input) with
            | _ -> ());
            total := !total +. (Unix.gettimeofday () -. t0);
            incr count
          done;
          !total /. float_of_int !count
        in
        let t1 = time_per_iter 4000 in
        let t2 = time_per_iter 8000 in
        Printf.printf "depth 4000 = %.4fs, depth 8000 = %.4fs (ratio %.1f)\n%!"
          t1 t2 (t2 /. t1);
        (* t2 / t1 should be close to 2 for O(n); allow up to 16× for noise *)
        if t2 /. t1 > 16.0 then
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
      (check "foo\n---\nbar\n" "foo\n---\nbar\n");
    Testo.create ~category:[ "printer" ]
      "multi-document: block collection gets own line after ---"
      (check "foo\n---\na: 1\n" "foo\n---\na: 1\n");
  ]

(* ------------------------------------------------------------------ *)
(* Node ↔ Value conversion roundtrip tests                               *)
(* ------------------------------------------------------------------ *)

(** Helpers for building value trees by hand, using [zero_loc] throughout so
    that position fields carry no meaning. *)
let z = YAMLx.zero_loc

let mk_null () = YAMLx.Null z
let mk_bool b = YAMLx.Bool (z, b)
let mk_int n = YAMLx.Int (z, Int64.of_int n)
let mk_float f = YAMLx.Float (z, f)
let mk_str s = YAMLx.String (z, s)
let mk_seq vs = YAMLx.Seq (z, vs)
let mk_map pairs = YAMLx.Map (z, List.map (fun (k, v) -> (z, k, v)) pairs)

(** [value_rt v] performs the value → nodes → value roundtrip and returns the
    result. A correct implementation must satisfy [Value.equal v (value_rt v)].
*)
let value_rt v =
  let nodes = YAMLx.Values.to_nodes [ v ] in
  match YAMLx.Values.of_nodes_exn nodes with
  | [ v' ] -> v'
  | vs ->
      failwith
        (Printf.sprintf "value_rt: expected 1 value, got %d" (List.length vs))

(** [nodes_rt yaml] performs the nodes → values → nodes roundtrip on [yaml] and
    returns both the intermediate values and the final values obtained by
    parsing the re-serialised nodes. A correct roundtrip must produce equal
    value lists. *)
let nodes_rt yaml =
  let nodes = YAMLx.Nodes.of_yaml_exn yaml in
  let values = YAMLx.Values.of_nodes_exn nodes in
  let nodes2 = YAMLx.Values.to_nodes values in
  let values2 = YAMLx.Values.of_nodes_exn nodes2 in
  (values, values2)

let conversion_tests () =
  let check_value_rt label v =
    Testo.create ~category:[ "conversion" ] ("value → node → value: " ^ label)
      (fun () ->
        let v' = value_rt v in
        Testo.(check yamlx_values) [ v ] [ v' ])
  in
  let check_nodes_rt label yaml =
    Testo.create ~category:[ "conversion" ] ("node → value → node: " ^ label)
      (fun () ->
        let vs, vs2 = nodes_rt yaml in
        Testo.(check yamlx_values) vs vs2)
  in
  [
    (* value → node → value: primitives *)
    check_value_rt "null" (mk_null ());
    check_value_rt "bool true" (mk_bool true);
    check_value_rt "bool false" (mk_bool false);
    check_value_rt "int zero" (mk_int 0);
    check_value_rt "int positive" (mk_int 42);
    check_value_rt "int negative" (mk_int (-1));
    check_value_rt "float" (mk_float 3.14);
    check_value_rt "float nan" (mk_float Float.nan);
    check_value_rt "float inf" (mk_float Float.infinity);
    check_value_rt "float neg inf" (mk_float Float.neg_infinity);
    check_value_rt "string simple" (mk_str "hello");
    check_value_rt "string empty" (mk_str "");
    check_value_rt "string with newline" (mk_str "line1\nline2");
    check_value_rt "string that looks like int" (mk_str "42");
    check_value_rt "string that looks like null" (mk_str "null");
    check_value_rt "string that looks like bool" (mk_str "true");
    (* value → node → value: collections *)
    check_value_rt "empty sequence" (mk_seq []);
    check_value_rt "sequence of ints" (mk_seq [ mk_int 1; mk_int 2; mk_int 3 ]);
    check_value_rt "empty mapping" (mk_map []);
    check_value_rt "mapping str→int"
      (mk_map [ (mk_str "a", mk_int 1); (mk_str "b", mk_int 2) ]);
    check_value_rt "nested mapping"
      (mk_map
         [
           (mk_str "x", mk_map [ (mk_str "y", mk_int 7) ]);
           (mk_str "z", mk_seq [ mk_null (); mk_bool true ]);
         ]);
    (* node → value → node: parsed YAML inputs *)
    check_nodes_rt "plain scalars" "foo\n";
    check_nodes_rt "null" "~\n";
    check_nodes_rt "bool" "true\n";
    check_nodes_rt "int" "42\n";
    check_nodes_rt "float" "3.14\n";
    check_nodes_rt "block mapping" "a: 1\nb: hello\n";
    check_nodes_rt "block sequence" "- 1\n- two\n- ~\n";
    check_nodes_rt "nested" "outer:\n  inner: 99\n";
    check_nodes_rt "alias expanded" "x: &a 1\ny: *a\n";
    check_nodes_rt "multi-document" "1\n---\ntwo\n";
  ]

(* ------------------------------------------------------------------ *)
(* Block-style selection tests (from Values.to_nodes)                    *)
(* ------------------------------------------------------------------ *)

(** Test that [Values.to_nodes] picks the right scalar style for strings, and
    that the value round-trip through YAML serialisation is correct. *)
let block_style_tests () =
  (* A long safe string with internal newlines should produce Literal style. *)
  let long_multiline =
    "This is the first line of a fairly long YAML string value.\n"
    ^ "This second line pushes the content well past the threshold."
  in
  (* A long prose string with spaces (and no newlines) → Folded. *)
  let long_prose =
    "This is a long prose string that contains many spaces and will \
     comfortably exceed the seventy-character threshold for block scalar \
     selection."
  in
  (* A short string that stays on one line → no change in behaviour. *)
  let short = "hello world" in
  let check_style label s expected_style =
    Testo.create ~category:[ "block-style" ] ("style for " ^ label) (fun () ->
        let node = List.hd (YAMLx.Values.to_nodes [ YAMLx.String (z, s) ]) in
        match node with
        | YAMLx.Scalar_node { style; _ } ->
            if style <> expected_style then
              failwith
                (Printf.sprintf "expected %s, got %s"
                   (YAMLx.show_scalar_style expected_style)
                   (YAMLx.show_scalar_style style))
        | _ -> failwith "expected Scalar_node")
  in
  let check_rt label s =
    Testo.create ~category:[ "block-style" ] ("round-trip for " ^ label)
      (fun () ->
        let v = value_rt (YAMLx.String (z, s)) in
        Testo.(check yamlx_values) [ YAMLx.String (z, s) ] [ v ])
  in
  [
    check_style "long multiline string" long_multiline YAMLx.Literal;
    check_style "long prose string" long_prose YAMLx.Folded;
    check_style "short string" short YAMLx.Plain;
    check_rt "long multiline string" long_multiline;
    check_rt "long prose string" long_prose;
    check_rt "short string" short;
    (* A string with only trailing newlines should NOT use Literal. *)
    check_style "trailing-newline-only string (short)" "short\n"
      YAMLx.Double_quoted;
    (* A long string with no spaces and no newlines stays plain. *)
    check_style "long plain string" (String.make 80 'a') YAMLx.Plain;
    (* A long string with spaces AND a control character must NOT use Folded
       because is_safe_for_folded rejects control characters. *)
    Testo.create ~category:[ "block-style" ]
      "long string with control char does not use Folded style" (fun () ->
        let s = "hello world " ^ String.make 70 'x' ^ "\x01 end" in
        let node = List.hd (YAMLx.Values.to_nodes [ YAMLx.String (z, s) ]) in
        match node with
        | YAMLx.Scalar_node { style = YAMLx.Folded; _ } ->
            failwith "should not use Folded for string with control char"
        | YAMLx.Scalar_node { style = YAMLx.Literal; _ } ->
            failwith "should not use Literal for string with control char"
        | _ -> ());
  ]

(* ------------------------------------------------------------------ *)
(* Duplicate key tests                                                   *)
(* ------------------------------------------------------------------ *)

let duplicate_key_tests () =
  let str s = YAMLx.String (z, s) in
  let int_ n = YAMLx.Int (z, Int64.of_int n) in
  let get_map yaml =
    match YAMLx.Values.of_yaml_exn yaml with
    | [ YAMLx.Map (_, pairs) ] -> pairs
    | _ -> failwith "expected a single Map"
  in
  let keys pairs = List.map (fun (_, k, _) -> k) pairs in
  let value_of key pairs =
    List.find_map
      (fun (_, k, v) -> if YAMLx.Value.equal k key then Some v else None)
      pairs
  in
  [
    Testo.create ~category:[ "duplicate-keys" ]
      "last occurrence wins in YAML 1.2" (fun () ->
        let pairs = get_map "a: 1\nb: 2\na: 3\n" in
        (* only one 'a' key *)
        assert (List.length pairs = 2);
        (* its value is 3, not 1 *)
        Testo.(check yamlx_values)
          [ int_ 3 ]
          [ Option.get (value_of (str "a") pairs) ]);
    Testo.create ~category:[ "duplicate-keys" ]
      "order of surviving keys is preserved" (fun () ->
        let pairs = get_map "a: 1\nb: 2\na: 3\nc: 4\n" in
        (* b, a, c — a appears at its last position *)
        Testo.(check yamlx_values) [ str "b"; str "a"; str "c" ] (keys pairs));
    Testo.create ~category:[ "duplicate-keys" ]
      "last occurrence wins in YAML 1.1 (regular pairs)" (fun () ->
        let pairs =
          match
            YAMLx.Values.of_yaml_exn ~schema:YAMLx.Yaml_1_1 "a: 1\nb: 2\na: 3\n"
          with
          | [ YAMLx.Map (_, ps) ] -> ps
          | _ -> failwith "expected Map"
        in
        assert (List.length pairs = 2);
        Testo.(check yamlx_values)
          [ int_ 3 ]
          [ Option.get (value_of (str "a") pairs) ]);
    Testo.create ~category:[ "duplicate-keys" ]
      "regular key overrides merged key in YAML 1.1" (fun () ->
        (* explicit 'b: 2' should win over merged 'b: 99' *)
        let pairs =
          match
            YAMLx.Values.of_yaml_exn ~schema:YAMLx.Yaml_1_1
              "a: 1\nb: 2\n<<:\n  b: 99\n  c: 3\n"
          with
          | [ YAMLx.Map (_, ps) ] -> ps
          | _ -> failwith "expected Map"
        in
        Testo.(check yamlx_values)
          [ int_ 2 ]
          [ Option.get (value_of (str "b") pairs) ]);
  ]

(* ------------------------------------------------------------------ *)
(* YAML 1.1 schema tests                                                 *)
(* ------------------------------------------------------------------ *)

(** Resolve [yaml] with [schema] and return the noloc value list for easy
    matching. *)
let resolve11 yaml = YAMLx.Values.of_yaml_exn ~schema:YAMLx.Yaml_1_1 yaml

let resolve12 yaml = YAMLx.Values.of_yaml_exn ~schema:YAMLx.Yaml_1_2 yaml

let yaml_1_1_tests () =
  let check label yaml expected_12 expected_11 =
    [
      Testo.create ~category:[ "yaml-1.1" ] (label ^ " — 1.2 resolution")
        (fun () -> Testo.(check yamlx_values) expected_12 (resolve12 yaml));
      Testo.create ~category:[ "yaml-1.1" ] (label ^ " — 1.1 resolution")
        (fun () -> Testo.(check yamlx_values) expected_11 (resolve11 yaml));
    ]
  in
  let check_same label yaml expected = check label yaml expected expected in
  let null_ = YAMLx.Null z
  and true_ = YAMLx.Bool (z, true)
  and false_ = YAMLx.Bool (z, false)
  and str s = YAMLx.String (z, s)
  and int_ n = YAMLx.Int (z, Int64.of_int n)
  and float_ f = YAMLx.Float (z, f) in
  (* ------------------------------------------------------------------ *)
  (* Task 1: <<  is an ordinary string key in YAML 1.2                   *)
  (* ------------------------------------------------------------------ *)
  List.concat
    [
      [
        Testo.create ~category:[ "yaml-1.1" ]
          "task 1: << is a plain string key in YAML 1.2" (fun () ->
            match resolve12 "<<: value" with
            | [
             YAMLx.Map
               (_, [ (_, YAMLx.String (_, "<<"), YAMLx.String (_, "value")) ]);
            ] ->
                ()
            | _ -> failwith "expected Map with key \"<<\"");
      ];
      (* ---------------------------------------------------------------- *)
      (* Booleans                                                          *)
      (* ---------------------------------------------------------------- *)
      check "bool y/n" "y" [ str "y" ] [ true_ ];
      check "bool Y/N" "Y" [ str "Y" ] [ true_ ];
      check "bool yes/no" "yes" [ str "yes" ] [ true_ ];
      check "bool Yes/No" "Yes" [ str "Yes" ] [ true_ ];
      check "bool YES/NO" "YES" [ str "YES" ] [ true_ ];
      check "bool n" "n" [ str "n" ] [ false_ ];
      check "bool no" "no" [ str "no" ] [ false_ ];
      check "bool on" "on" [ str "on" ] [ true_ ];
      check "bool On" "On" [ str "On" ] [ true_ ];
      check "bool ON" "ON" [ str "ON" ] [ true_ ];
      check "bool off" "off" [ str "off" ] [ false_ ];
      check "bool Off" "Off" [ str "Off" ] [ false_ ];
      check "bool OFF" "OFF" [ str "OFF" ] [ false_ ];
      check_same "bool true" "true" [ true_ ];
      check_same "bool false" "false" [ false_ ];
      check_same "null ~" "~" [ null_ ];
      check_same "null keyword" "null" [ null_ ];
      check "octal 0755 (decimal in 1.2)" "0755" [ str "0755" ] [ int_ 493 ];
      check "octal 0o755 (both schemas)" "0o755" [ int_ 493 ] [ int_ 493 ];
      check "octal 00 is octal zero in 1.1" "00" [ str "00" ] [ int_ 0 ];
      check "sexagesimal 3:25:45" "3:25:45" [ str "3:25:45" ] [ int_ 12345 ];
      check "sexagesimal 1:00" "1:00" [ str "1:00" ] [ int_ 60 ];
      check "sexagesimal 60:00" "60:00" [ str "60:00" ] [ int_ 3600 ];
      check "sexagesimal float 20:30.15" "20:30.15"
        [ str "20:30.15" ]
        [ float_ 1230.15 ];
      check "sexagesimal float 1:0.0" "1:0.0" [ str "1:0.0" ] [ float_ 60.0 ];
      [
        Testo.create ~category:[ "yaml-1.1" ] "merge key: simple mapping merge"
          (fun () ->
            let yaml = "a: 1\nb: 2\n<<:\n  b: 99\n  c: 3\n" in
            match resolve11 yaml with
            | [ YAMLx.Map (_, pairs) ] -> (
                let get k =
                  List.find_map
                    (fun (_, kv, vv) ->
                      match kv with
                      | YAMLx.String (_, s) when s = k -> Some vv
                      | _ -> None)
                    pairs
                in
                match (get "a", get "b", get "c") with
                | ( Some (YAMLx.Int (_, 1L)),
                    Some (YAMLx.Int (_, 2L)),
                    Some (YAMLx.Int (_, 3L)) ) ->
                    ()
                | _ -> failwith "unexpected merge result")
            | _ -> failwith "expected Map");
      ];
      [
        Testo.create ~category:[ "yaml-1.1" ] "merge key: sequence of mappings"
          (fun () ->
            let yaml = "a: 1\n<<:\n  - b: 2\n  - b: 99\n    c: 3\n" in
            match resolve11 yaml with
            | [ YAMLx.Map (_, pairs) ] -> (
                let keys =
                  List.filter_map
                    (fun (_, k, _) ->
                      match k with
                      | YAMLx.String (_, s) -> Some s
                      | _ -> None)
                    pairs
                in
                if List.mem "<<" keys then failwith "<< should be gone";
                let get k =
                  List.find_map
                    (fun (_, kv, vv) ->
                      match kv with
                      | YAMLx.String (_, s) when s = k -> Some vv
                      | _ -> None)
                    pairs
                in
                match (get "b", get "c") with
                | Some (YAMLx.Int (_, 2L)), Some (YAMLx.Int (_, 3L)) -> ()
                | _ -> failwith "wrong merge result")
            | _ -> failwith "expected Map");
      ];
      [
        Testo.create ~category:[ "yaml-1.1" ]
          "merge key: << is plain string in YAML 1.2" (fun () ->
            let yaml = "a: 1\n<<:\n  b: 2\n" in
            match resolve12 yaml with
            | [ YAMLx.Map (_, pairs) ] ->
                let has_merge =
                  List.exists
                    (fun (_, k, _) ->
                      YAMLx.Value.equal k (YAMLx.String (z, "<<")))
                    pairs
                in
                if not has_merge then failwith "<< key missing in 1.2 result"
            | _ -> failwith "expected Map");
      ];
      [
        Testo.create ~category:[ "yaml-1.1" ]
          "%YAML 1.1 directive auto-selects 1.1 schema" (fun () ->
            match YAMLx.Values.of_yaml_exn "%YAML 1.1\n---\nyes\n" with
            | [ YAMLx.Bool (_, true) ] -> ()
            | _ -> failwith "expected Bool true from %YAML 1.1 doc");
      ];
      [
        Testo.create ~category:[ "yaml-1.1" ]
          "%YAML 1.2 directive auto-selects 1.2 schema" (fun () ->
            match
              YAMLx.Values.of_yaml_exn ~schema:YAMLx.Yaml_1_1
                "%YAML 1.2\n---\nyes\n"
            with
            | [ YAMLx.String (_, "yes") ] -> ()
            | _ -> failwith "expected String \"yes\" from %YAML 1.2 doc");
      ];
      [
        Testo.create ~category:[ "yaml-1.1" ]
          "strict_schema: %YAML 1.2 in 1.1 session raises Schema_error"
          (fun () ->
            match
              YAMLx.Values.of_yaml_exn ~schema:YAMLx.Yaml_1_1
                ~strict_schema:true "%YAML 1.2\n---\nyes\n"
            with
            | exception YAMLx.Error (YAMLx.Schema_error _) -> ()
            | exception YAMLx.Error _ -> failwith "unexpected error kind"
            | _ -> failwith "expected an exception");
      ];
      [
        Testo.create ~category:[ "yaml-1.1" ]
          "strict_schema: matching directive does not raise" (fun () ->
            match
              YAMLx.Values.of_yaml_exn ~schema:YAMLx.Yaml_1_1
                ~strict_schema:true "%YAML 1.1\n---\nyes\n"
            with
            | [ YAMLx.Bool (_, true) ] -> ()
            | _ -> failwith "expected Bool true");
      ];
      [
        Testo.create ~category:[ "yaml-1.1" ]
          "reject_ambiguous: yes raises Schema_error in 1.2 mode" (fun () ->
            match
              YAMLx.Values.of_yaml_exn ~schema:YAMLx.Yaml_1_2
                ~reject_ambiguous:true "yes"
            with
            | exception YAMLx.Error (YAMLx.Schema_error _) -> ()
            | _ -> failwith "expected Schema_error");
      ];
      [
        Testo.create ~category:[ "yaml-1.1" ]
          "reject_ambiguous: 0755 raises Schema_error in 1.2 mode" (fun () ->
            match
              YAMLx.Values.of_yaml_exn ~schema:YAMLx.Yaml_1_2
                ~reject_ambiguous:true "0755"
            with
            | exception YAMLx.Error (YAMLx.Schema_error _) -> ()
            | _ -> failwith "expected Schema_error");
      ];
      [
        Testo.create ~category:[ "yaml-1.1" ]
          "reject_ambiguous: 3:25:45 raises Schema_error in 1.2 mode" (fun () ->
            match
              YAMLx.Values.of_yaml_exn ~schema:YAMLx.Yaml_1_2
                ~reject_ambiguous:true "3:25:45"
            with
            | exception YAMLx.Error (YAMLx.Schema_error _) -> ()
            | _ -> failwith "expected Schema_error");
      ];
      [
        Testo.create ~category:[ "yaml-1.1" ]
          "reject_ambiguous: << key raises Schema_error in 1.2 mode" (fun () ->
            match
              YAMLx.Values.of_yaml_exn ~schema:YAMLx.Yaml_1_2
                ~reject_ambiguous:true "<<: value"
            with
            | exception YAMLx.Error (YAMLx.Schema_error _) -> ()
            | _ -> failwith "expected Schema_error");
      ];
      [
        Testo.create ~category:[ "yaml-1.1" ]
          "reject_ambiguous: unambiguous 1.2 values are accepted" (fun () ->
            (* true, 42, 0o755, .inf should all be fine *)
            ignore
              (YAMLx.Values.of_yaml_exn ~schema:YAMLx.Yaml_1_2
                 ~reject_ambiguous:true "true"));
      ];
    ]

(* ------------------------------------------------------------------ *)
(* Plain-mode tests                                                      *)
(* ------------------------------------------------------------------ *)

let plain_mode_tests () =
  let check_ok label yaml () =
    ignore (YAMLx.Values.of_yaml_exn ~plain:true yaml);
    ignore label
  in
  let check_simplicity_error label yaml () =
    match YAMLx.Values.of_yaml_exn ~plain:true yaml with
    | exception YAMLx.Error (YAMLx.Simplicity_error _) -> ()
    | _ -> failwith (label ^ ": expected Simplicity_error")
  in
  let cat = [ "plain-mode" ] in
  [
    Testo.create ~category:cat "plain scalar accepted" (check_ok "plain" "42");
    Testo.create ~category:cat "plain mapping accepted"
      (check_ok "mapping" "a: 1\nb: 2");
    Testo.create ~category:cat "plain sequence accepted"
      (check_ok "sequence" "- a\n- b");
    Testo.create ~category:cat "anchor raises Simplicity_error"
      (check_simplicity_error "anchor" "&a foo");
    Testo.create ~category:cat "alias raises Simplicity_error"
      (check_simplicity_error "alias" "- &a foo\n- *a");
    Testo.create ~category:cat "explicit tag raises Simplicity_error"
      (check_simplicity_error "tag" "!!str foo");
    Testo.create ~category:cat "anchor on sequence raises Simplicity_error"
      (check_simplicity_error "seq anchor" "&seq\n- a\n- b");
    Testo.create ~category:cat "anchor on mapping raises Simplicity_error"
      (check_simplicity_error "map anchor" "&m\na: 1");
    Testo.create ~category:cat "merge key in YAML 1.1 raises Simplicity_error"
      (fun () ->
        match
          YAMLx.Values.of_yaml_exn ~schema:YAMLx.Yaml_1_1 ~plain:true
            "- &base\n  x: 1\n- <<: *base\n  y: 2"
        with
        | exception YAMLx.Error (YAMLx.Simplicity_error _) -> ()
        | _ -> failwith "expected Simplicity_error for merge key");
    Testo.create ~category:cat
      "merge key in YAML 1.2 accepted (it is a plain string key)" (fun () ->
        match
          YAMLx.Values.of_yaml_exn ~schema:YAMLx.Yaml_1_2 ~plain:true
            "<<: value"
        with
        | [
         YAMLx.Map
           (_, [ (_, YAMLx.String (_, "<<"), YAMLx.String (_, "value")) ]);
        ] ->
            ()
        | _ -> failwith "expected Map with plain key \"<<\"");
  ]

(* ------------------------------------------------------------------ *)
(* Strict-keys tests                                                     *)
(* ------------------------------------------------------------------ *)

let strict_keys_tests () =
  let check_ok label yaml () =
    ignore (YAMLx.Values.of_yaml_exn ~strict_keys:true yaml);
    ignore label
  in
  let check_dup_error label yaml () =
    match YAMLx.Values.of_yaml_exn ~strict_keys:true yaml with
    | exception YAMLx.Error (YAMLx.Duplicate_key_error _) -> ()
    | _ -> failwith (label ^ ": expected Duplicate_key_error")
  in
  let cat = [ "strict-keys" ] in
  [
    Testo.create ~category:cat "unique keys accepted"
      (check_ok "unique" "a: 1\nb: 2\nc: 3");
    Testo.create ~category:cat "empty mapping accepted" (check_ok "empty" "{}");
    Testo.create ~category:cat "duplicate key raises Duplicate_key_error"
      (check_dup_error "dup" "a: 1\nb: 2\na: 3");
    Testo.create ~category:cat "duplicate key in nested mapping raises"
      (check_dup_error "nested dup" "outer:\n  x: 1\n  x: 2");
    Testo.create ~category:cat
      "duplicate key in YAML 1.1 raises Duplicate_key_error" (fun () ->
        match
          YAMLx.Values.of_yaml_exn ~schema:YAMLx.Yaml_1_1 ~strict_keys:true
            "a: 1\nb: 2\na: 3"
        with
        | exception YAMLx.Error (YAMLx.Duplicate_key_error _) -> ()
        | _ -> failwith "expected Duplicate_key_error");
    Testo.create ~category:cat
      "without strict_keys duplicate key is silently deduplicated" (fun () ->
        match YAMLx.Values.of_yaml_exn "a: 1\na: 2" with
        | [ YAMLx.Map (_, [ (_, YAMLx.String (_, "a"), YAMLx.Int (_, 2L)) ]) ]
          ->
            ()
        | _ -> failwith "expected last value to win");
  ]

(* ------------------------------------------------------------------ *)
(* Cycle-detection tests                                                 *)
(* ------------------------------------------------------------------ *)

let cycle_tests () =
  let check_cycle label yaml () =
    (* of_yaml wraps errors as Result; verify cyclic YAML returns an error *)
    match YAMLx.Values.of_yaml yaml with
    | Error _ -> ignore label
    | Ok _ -> failwith (label ^ ": expected cycle error but succeeded")
  in
  let check_cycle_exn label yaml () =
    match YAMLx.Values.of_yaml_exn yaml with
    | _ -> failwith (label ^ ": expected Cycle_error but succeeded")
    | exception YAMLx.Error (YAMLx.Cycle_error _) -> ()
    | exception YAMLx.Error _ ->
        failwith (label ^ ": expected Cycle_error, got a different error")
  in
  let check_ok_yaml label yaml () =
    (* Non-cyclic aliases still resolve fine *)
    ignore (YAMLx.Values.of_yaml_exn yaml);
    ignore label
  in
  let cat = [ "cycle" ] in
  [
    Testo.create ~category:cat "self-referential mapping raises Cycle_error"
      (check_cycle_exn "self-map" "&doc\na: *doc");
    Testo.create ~category:cat "self-referential sequence raises Cycle_error"
      (check_cycle_exn "self-seq" "&doc\n- *doc");
    Testo.create ~category:cat "cycle error reported via of_yaml result"
      (check_cycle "result" "&doc\na: *doc");
    Testo.create ~category:cat "non-cyclic shared alias resolves fine"
      (check_ok_yaml "shared" "anchor: &v hello\nfirst: *v\nsecond: *v");
    Testo.create ~category:cat "-f yaml prints cyclic YAML without error"
      (fun () ->
        let nodes = YAMLx.Nodes.of_yaml_exn "&doc\na: *doc" in
        let yaml = YAMLx.Nodes.to_yaml nodes in
        if not (String.length yaml > 0) then
          failwith "expected non-empty YAML output");
  ]

(* ------------------------------------------------------------------ *)
(* Entry point                                                           *)
(* ------------------------------------------------------------------ *)

let () =
  YAMLx.register_exception_printers ();
  Testo.interpret_argv ~project_name:"yamlx" (fun _tags ->
      unit_tests () @ loc_tests () @ encoding_tests () @ roundtrip_tests ()
      @ comment_tests () @ has_comments_tests () @ comment_node_tests ()
      @ anchor_tests () @ printer_tests () @ expansion_limit_tests ()
      @ depth_limit_tests () @ performance_tests () @ conversion_tests ()
      @ block_style_tests () @ duplicate_key_tests () @ yaml_1_1_tests ()
      @ plain_mode_tests () @ strict_keys_tests () @ cycle_tests ()
      @ suite_tests ())
