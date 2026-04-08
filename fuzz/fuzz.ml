(* The fuzz harness

   Run 'dune exec -- ./fuzz.exe --help' to see how to
   generate test inputs and check them with or without afl.
*)

open Printf

(* Time a function *)
let time f =
  let t1 = Unix.gettimeofday () in
  let res = f () in
  let t2 = Unix.gettimeofday () in
  (res, t2 -. t1)

let test_yaml_roundtrip input =
  let parse_res, parse_time = time (fun () -> YAMLx.Values.of_yaml input) in
  if parse_time > 0.1 then failwith (sprintf "parsing timeout: %g s" parse_time);
  match parse_res with
  | Ok values ->
      let _yaml_out, print_time =
        time (fun () -> YAMLx.Values.to_yaml values)
      in
      if print_time > 0.1 then
        failwith (sprintf "printing timeout: %g s" print_time);
      ()
  | Error _ -> ()

let get_input_file () =
  match Sys.argv with
  | [| _; file |] -> file
  | _ -> failwith "missing argument: should be an input file"

let read_file file =
  In_channel.with_open_bin file (fun ic -> In_channel.input_all ic)

let run test =
  let file = get_input_file () in
  AflPersistent.run (fun () ->
      let input = read_file file in
      test input)

let main () =
  Printexc.record_backtrace true;
  try run test_yaml_roundtrip with
  | e ->
      eprintf "Uncaught exception: %s\n" (Printexc.to_string e);
      Printexc.print_backtrace stderr;
      exit 1

let () = main ()
