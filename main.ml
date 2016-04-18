open Compile
open Runner
open Printf
open Lexing
open Instruction

let rec take l n =
  if n == 0 then []
  else
    match l with
      | [] -> failwith "took too many"
      | x::xs -> x::(take xs (n - 1))

let () =
  let name = Sys.argv.(1) in
  let numregs = Sys.argv.(2) in
  spare_regs := take all_regs (int_of_string numregs);
  let input_file = open_in name in
  try
    let program = compile_file_to_string name input_file in
    printf "%s\n" program
  with
    | Failure(f) ->
      eprintf "Compilation failed: \n%s\n" f;
      exit 1

