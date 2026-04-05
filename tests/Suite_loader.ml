(** Bootstrap loader for the yaml-test-suite test files. The test suite stores
    its data in YAML format (src/*.yaml), but we cannot use YAMLx itself to
    parse them (it doesn't exist yet when we first run the tests). This module
    is therefore a hand-written, line-based parser that understands the specific
    subset of YAML used by the test suite.

    File format ~~~~~~~~~~~ Each src/*.yaml file contains a YAML sequence (a
    list) of test case mappings. Every item looks like:

    - name: Short description from: source reference tags: tag1 tag2 yaml: | the
      yaml input to test tree: | +STR +DOC ... fail: true (only present when the
      test expects a parse failure)

    Block scalar fields ([yaml: |], [tree: |]) are identified by a [: |] suffix;
    their content is the following lines with exactly 4 spaces of indentation (2
    for the mapping value, 2 more for content).

    Special visual characters in the [yaml:] field must be substituted: ␣
    (U+2423) → space » (U+00BB) → tab —» (U+2014 U+00BB) → tab ——» → tab ———» →
    tab ← (U+2190) → carriage return ⇔ (U+21D4) → BOM (U+FEFF) ↵ (U+21B5) →
    empty (marks a blank line; the surrounding newlines already produce the
    blank line) ∎ (U+220E) → strip the preceding and trailing newline (indicates
    absence of final newline) *)

(* ------------------------------------------------------------------ *)
(* Types                                                                 *)
(* ------------------------------------------------------------------ *)

type test_case = {
  id : string;  (** derived from the source file name, e.g. '229Q' *)
  name : string;  (** human-readable name, or '' if absent *)
  tags : string list;
  yaml : string;  (** YAML input with visual characters substituted *)
  tree : string option;  (** expected event tree, or None *)
  fail : bool;  (** true = parsing should fail *)
}

(* ------------------------------------------------------------------ *)
(* Visual character substitution                                         *)
(* ------------------------------------------------------------------ *)

(** Apply the yaml-test-suite visual character conventions to a string. *)
let substitute_visual (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let i = ref 0 in
  let n = String.length s in
  (* We work with raw bytes; all of the special characters are multi-byte
     UTF-8 sequences that do not overlap with ASCII, so substring matching
     on bytes is safe. *)
  while !i < n do
    (* Check for multi-byte prefixes in length order (longest first) *)
    let advance_by k str_to_add =
      Buffer.add_string buf str_to_add;
      i := !i + k
    in
    let remaining = n - !i in
    (* ∎  U+220E  E2 88 8E  (3 bytes) – no final newline *)
    if
      remaining >= 3
      && Char.code s.[!i] = 0xE2
      && Char.code s.[!i + 1] = 0x88
      && Char.code s.[!i + 2] = 0x8E
    then begin
      (* Strip the ∎ and the preceding \n that the block scalar added *)
      let cur = Buffer.contents buf in
      Buffer.clear buf;
      let len = String.length cur in
      (* Remove trailing newline if present *)
      let trim = if len > 0 && cur.[len - 1] = '\n' then len - 1 else len in
      Buffer.add_string buf (String.sub cur 0 trim);
      i := !i + 3
    end (* ↵  U+21B5  E2 86 B5  (3 bytes) – empty line marker, just drop it *)
    else if
      remaining >= 3
      && Char.code s.[!i] = 0xE2
      && Char.code s.[!i + 1] = 0x86
      && Char.code s.[!i + 2] = 0xB5
    then advance_by 3 "" (* ⇔  U+21D4  E2 87 94  (3 bytes) – BOM *)
    else if
      remaining >= 3
      && Char.code s.[!i] = 0xE2
      && Char.code s.[!i + 1] = 0x87
      && Char.code s.[!i + 2] = 0x94
    then advance_by 3 "\xEF\xBB\xBF"
      (* UTF-8 encoded BOM *)
      (* ←  U+2190  E2 86 90  (3 bytes) – carriage return *)
    else if
      remaining >= 3
      && Char.code s.[!i] = 0xE2
      && Char.code s.[!i + 1] = 0x86
      && Char.code s.[!i + 2] = 0x90
    then advance_by 3 "\r"
      (* ———»  3× U+2014 + U+00BB  = E2 80 94 E2 80 94 E2 80 94 C2 BB  (9 bytes) – tab *)
    else if
      remaining >= 9
      && Char.code s.[!i] = 0xE2
      && Char.code s.[!i + 1] = 0x80
      && Char.code s.[!i + 2] = 0x94
      && Char.code s.[!i + 3] = 0xE2
      && Char.code s.[!i + 4] = 0x80
      && Char.code s.[!i + 5] = 0x94
      && Char.code s.[!i + 6] = 0xE2
      && Char.code s.[!i + 7] = 0x80
      && Char.code s.[!i + 8] = 0x94
    then
      (* the '»' comes after; consume 9 bytes of em-dashes, let the next
         iteration consume the '»' *)
      advance_by 9 "" (* ——»  2× U+2014 + U+00BB  (7 bytes) – tab *)
    else if
      remaining >= 7
      && Char.code s.[!i] = 0xE2
      && Char.code s.[!i + 1] = 0x80
      && Char.code s.[!i + 2] = 0x94
      && Char.code s.[!i + 3] = 0xE2
      && Char.code s.[!i + 4] = 0x80
      && Char.code s.[!i + 5] = 0x94
    then advance_by 6 "" (* —»  1× U+2014 + U+00BB  (5 bytes) – tab *)
    else if
      remaining >= 5
      && Char.code s.[!i] = 0xE2
      && Char.code s.[!i + 1] = 0x80
      && Char.code s.[!i + 2] = 0x94
    then advance_by 3 ""
      (* »  U+00BB  C2 BB  (2 bytes) – tab (final component after em-dashes or alone) *)
    else if
      remaining >= 2 && Char.code s.[!i] = 0xC2 && Char.code s.[!i + 1] = 0xBB
    then advance_by 2 "\t" (* ␣  U+2423  E2 90 A3  (3 bytes) – space *)
    else if
      remaining >= 3
      && Char.code s.[!i] = 0xE2
      && Char.code s.[!i + 1] = 0x90
      && Char.code s.[!i + 2] = 0xA3
    then advance_by 3 " "
    else begin
      Buffer.add_char buf s.[!i];
      incr i
    end
  done;
  Buffer.contents buf

(* ------------------------------------------------------------------ *)
(* Line-based parser for the test YAML format                            *)
(* ------------------------------------------------------------------ *)

(** Strip a fixed [prefix_len] leading spaces from [s]. If [s] has fewer than
    [prefix_len] spaces, return the stripped portion. *)
let strip_indent (s : string) (prefix_len : int) : string =
  let n = String.length s in
  let count = ref 0 in
  while !count < prefix_len && !count < n && s.[!count] = ' ' do
    incr count
  done;
  String.sub s !count (n - !count)

(** Split a string into lines (dropping the final '\n' if present). *)
let split_lines (s : string) : string list =
  String.split_on_char '\n' s |> fun ls ->
  (* Drop a trailing empty string caused by a final \n *)
  match List.rev ls with
  | "" :: rest -> List.rev rest
  | _ -> ls

(** Check if [line] starts with [prefix]. *)
let starts_with (prefix : string) (line : string) : bool =
  let plen = String.length prefix in
  let llen = String.length line in
  llen >= plen && String.sub line 0 plen = prefix

(* ------------------------------------------------------------------ *)
(* Main parser                                                           *)
(* ------------------------------------------------------------------ *)

(** Parse a single source file and return the list of test cases it contains.
    [file_id] is the stem of the file name (e.g. '229Q'). *)
let parse_file (file_id : string) (content : string) : test_case list =
  let lines = split_lines content in
  let tests : test_case list ref = ref [] in
  (* Current test being built *)
  let cur_name = ref "" in
  let cur_tags = ref [] in
  let cur_yaml = ref None in
  let cur_tree = ref None in
  let cur_fail = ref false in
  let in_test = ref false in

  (* When reading a block scalar, this holds the field name and
     the accumulated lines (without leading indentation). *)
  let block_field : string option ref = ref None in
  let block_lines : string list ref = ref [] in
  let block_indent = 4 in
  (* test files use 4 spaces for block scalar content *)

  let finish_block () =
    (match !block_field with
    | None -> ()
    | Some field -> (
        let content = String.concat "\n" (List.rev !block_lines) ^ "\n" in
        match field with
        | "yaml" -> cur_yaml := Some content
        | "tree" -> cur_tree := Some content
        | _ -> ()
        (* json, dump, emit: ignored for now *)));
    block_field := None;
    block_lines := []
  in

  let save_test () =
    finish_block ();
    if !in_test then
      begin match !cur_yaml with
      | Some raw ->
          let yaml_substituted = substitute_visual raw in
          tests :=
            {
              id = file_id;
              name = !cur_name;
              tags = !cur_tags;
              yaml = yaml_substituted;
              tree = !cur_tree;
              fail = !cur_fail;
            }
            :: !tests
      | None -> ()
      (* skip incomplete tests *)
      end;
    cur_name := "";
    cur_tags := [];
    cur_yaml := None;
    cur_tree := None;
    cur_fail := false;
    in_test := false
  in

  List.iter
    (fun line ->
      (* If we are accumulating a block scalar, check if the line is still
       part of it (starts with [block_indent] spaces or is empty). *)
      match !block_field with
      | Some _ ->
          if line = "" then
            (* Blank line: may still be part of block scalar – include it *)
            block_lines := "" :: !block_lines
          else if starts_with (String.make block_indent ' ') line then
            block_lines := strip_indent line block_indent :: !block_lines
          else begin
            (* Dedented: block scalar ends, process this line normally *)
            finish_block ();
            (* Fall through to normal line processing below *)
            match line with
            | "---" ->
                save_test ();
                in_test := false
            | _ ->
                if starts_with "- " line then begin
                  save_test ();
                  in_test := true;
                  (* The item may already have a field on the same line: '- name: foo' *)
                  let rest = String.sub line 2 (String.length line - 2) in
                  match String.split_on_char ':' rest with
                  | key :: value_parts when String.trim key <> "" -> (
                      let value =
                        String.concat ":" value_parts |> String.trim
                      in
                      match key with
                      | "name" -> cur_name := value
                      | "tags" ->
                          cur_tags :=
                            String.split_on_char ' ' value
                            |> List.filter (fun s -> s <> "")
                      | "fail" when value = "true" -> cur_fail := true
                      | _ -> ())
                  | _ -> ()
                end
                else if starts_with "  " line then begin
                  let rest = String.sub line 2 (String.length line - 2) in
                  let colon_idx =
                    try Some (String.index rest ':') with
                    | Not_found -> None
                  in
                  match colon_idx with
                  | Some idx -> (
                      let key = String.sub rest 0 idx |> String.trim in
                      let after =
                        String.sub rest (idx + 1) (String.length rest - idx - 1)
                      in
                      let value = String.trim after in
                      match value with
                      | "|" ->
                          block_field := Some key;
                          block_lines := []
                      | _ -> (
                          match key with
                          | "name" -> cur_name := value
                          | "tags" ->
                              cur_tags :=
                                String.split_on_char ' ' value
                                |> List.filter (fun s -> s <> "")
                          | "fail" when value = "true" -> cur_fail := true
                          | _ -> ()))
                  | None -> ()
                end
          end
      | None -> (
          (* Normal line processing *)
          match line with
          | "---" ->
              save_test ();
              in_test := false
          | _ ->
              if starts_with "- " line then begin
                save_test ();
                in_test := true;
                let rest = String.sub line 2 (String.length line - 2) in
                match String.split_on_char ':' rest with
                | key :: value_parts when String.trim key <> "" -> (
                    let value = String.concat ":" value_parts |> String.trim in
                    match key with
                    | "name" -> cur_name := value
                    | "tags" ->
                        cur_tags :=
                          String.split_on_char ' ' value
                          |> List.filter (fun s -> s <> "")
                    | "fail" when value = "true" -> cur_fail := true
                    | _ -> ())
                | _ -> ()
              end
              else if starts_with "  " line then begin
                let rest = String.sub line 2 (String.length line - 2) in
                let colon_idx =
                  try Some (String.index rest ':') with
                  | Not_found -> None
                in
                match colon_idx with
                | Some idx -> (
                    let key = String.sub rest 0 idx |> String.trim in
                    let after =
                      String.sub rest (idx + 1) (String.length rest - idx - 1)
                    in
                    let value = String.trim after in
                    match value with
                    | "|" ->
                        block_field := Some key;
                        block_lines := []
                    | _ -> (
                        match key with
                        | "name" -> cur_name := value
                        | "tags" ->
                            cur_tags :=
                              String.split_on_char ' ' value
                              |> List.filter (fun s -> s <> "")
                        | "fail" when value = "true" -> cur_fail := true
                        | _ -> ()))
                | None -> ()
              end))
    lines;
  save_test ();
  List.rev !tests

(* ------------------------------------------------------------------ *)
(* File loading                                                          *)
(* ------------------------------------------------------------------ *)

(** Read a file as a string. *)
let read_file (path : string) : string =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (* Normalize CRLF → LF so that the line-based parser sees plain \n on
     every platform.  Without this, blank lines inside block scalars come
     out as "\r" instead of "" on Windows, which breaks the block-scalar
     accumulation logic. *)
  let raw = Bytes.to_string s in
  let buf = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    if raw.[!i] = '\r' && !i + 1 < n && raw.[!i + 1] = '\n' then begin
      Buffer.add_char buf '\n';
      i := !i + 2
    end
    else begin
      Buffer.add_char buf raw.[!i];
      incr i
    end
  done;
  Buffer.contents buf

(** Load all test cases from a single [.yaml] file. [path] must be the full file
    path; the file stem is used as the test id. *)
let load_file (path : string) : test_case list =
  let base = Filename.basename path in
  let id = Filename.remove_extension base in
  let content = read_file path in
  parse_file id content

(** Load all test cases from all [.yaml] files in [dir]. *)
let load_dir (dir : string) : test_case list =
  let files =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.extension f = ".yaml")
    |> List.sort String.compare
  in
  List.concat_map (fun f -> load_file (Filename.concat dir f)) files
