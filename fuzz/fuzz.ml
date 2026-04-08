(* The fuzz harness

   Run 'dune exec -- ./fuzz.exe --help' to see how to
   generate test inputs and check them with or without afl.
*)

open Printf
open Crowbar

(* Time a function *)
let time f =
  let t1 = Unix.gettimeofday () in
  let res = f () in
  let t2 = Unix.gettimeofday () in
  (res, t2 -. t1)

let () =
  Crowbar.add_test ~name:"YAML roundtrip" [ Crowbar.bytes ] (fun input ->
      let parse_res, parse_time = time (fun () -> YAMLx.Values.of_yaml input) in
      if parse_time > 0.1 then
        Crowbar.fail (sprintf "parsing timeout: %g s" parse_time);
      match parse_res with
      | Ok values ->
          let _yaml_out, print_time =
            time (fun () -> YAMLx.Values.to_yaml values)
          in
          if print_time > 0.1 then
            Crowbar.fail (sprintf "printing timeout: %g s" print_time);
          ()
      | Error _ ->
          (* Ignore malformed YAML if it results in a clean error *)
          Crowbar.bad_test ())
