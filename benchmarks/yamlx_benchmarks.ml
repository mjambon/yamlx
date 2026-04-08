(** Benchmark YAMLx against ocaml-yaml (libyaml bindings).

    Usage: bench <file.yaml>

    Large test files:

    - https://github.com/prometheus-operator/prometheus-operator/blob/main/bundle.yaml
      -> crashes ocaml-yaml during printing: Invalid_argument("scalar failed")
    - saltern.yml https://github.com/aaubry/YamlDotNet/issues/519 (save the
      "saltern.yml" attachment, ~1.5 MB) -> unsupported by ocaml-yaml due to
      complex keys

    Smaller test files that are supported by all parsers:

    - https://github.com/iherbllc/bitnami-charts/blob/cb930d302faf70eccc8a6383df4584ba44a2a4cc/bitnami/kafka/values.yaml
      (2500 lines)

    - https://github.com/kumulustech/legendary-octo-waddle/blob/7a9f65b42055f52984140855d006fbc18d4341c7/helm-prometheus/prometheus_and_alertmanager_default_values.yaml
      (1700 lines) *)

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let time label iters f =
  let t0 = Unix.gettimeofday () in
  for _ = 1 to iters do
    f ()
  done;
  let ms = (Unix.gettimeofday () -. t0) /. float iters *. 1000. in
  Printf.printf "  %-40s %7.1f ms\n%!" label ms

let () =
  YAMLx.register_exception_printers ();
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Usage: %s FILE.yaml\n" Sys.argv.(0);
    exit 1);
  let path = Sys.argv.(1) in
  let input = read_file path in
  let iters = 10 in
  Printf.printf "File: %s  (%d bytes, %d iterations)\n\n" path
    (String.length input) iters;

  (* ------------------------------------------------------------------ *)
  (* YAMLx                                                                *)
  (* ------------------------------------------------------------------ *)
  Printf.printf "YAMLx:\n";

  (* Step 1: tokenize + parse + compose (string → node list) *)
  time "parse  (string → nodes)" iters (fun () ->
      ignore (YAMLx.Nodes.of_yaml_exn input));
  let nodes = YAMLx.Nodes.of_yaml_exn input in

  (* Step 2: resolve scalars to typed values (node list → value list) *)
  time "resolve (nodes → values)" iters (fun () ->
      ignore (YAMLx.Values.of_nodes_exn nodes));
  let values = YAMLx.Values.of_nodes_exn nodes in

  (* Step 3: serialize back to YAML (value list → string) *)
  time "print  (values → string)" iters (fun () ->
      ignore (YAMLx.Values.to_yaml values));

  Printf.printf "\n";

  (* ------------------------------------------------------------------ *)
  (* yaml (ocaml-yaml, wraps libyaml)                                    *)
  (* ------------------------------------------------------------------ *)
  Printf.printf "yaml (ocaml-yaml / libyaml):\n";

  (* parse+resolve are combined in one call in this library *)
  match Yaml.of_string input with
  | Error (`Msg msg) -> Printf.printf "  (skipped: %s)\n" msg
  | Ok yaml_val ->
      time "parse+resolve (string → value)" iters (fun () ->
          ignore (Yaml.of_string_exn input));
      time "print  (value → string)" iters (fun () ->
          ignore (Yaml.to_string_exn yaml_val))
