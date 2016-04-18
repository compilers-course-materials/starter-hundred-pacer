open Compile
open Instruction
open Expr
open Runner
open Printf
open OUnit2
open ExtLib
open Color

let rec take l n =
  if n == 0 then []
  else
    match l with
      | [] -> failwith "took too many"
      | x::xs -> x::(take xs (n - 1))


let t name program expected = name>::test_run program name expected;;
let tvg name program expected = name>::test_run_valgrind program name expected;;
let terr name program expected = name>::test_err program name expected;;
let tdep name program expected = name>::fun ctxt ->
  let p = parse_string name program in
  let a = anf_program p in
  let answer = match a with
    | AProgram(decls, body) ->
      let (_, deps, _) = (dep_graph body) in
      deps in
  let c = Pervasives.compare in
  assert_equal (List.sort ~cmp:c expected) (List.sort ~cmp:c answer) ~printer:dump;;

let tcolor name program expected_colors = name>::fun ctxt ->
  let p = parse_string name program in
  let a = anf_program p in
  let body = match a with
    | AProgram(decls, body) -> body in
  let vars = getvars body in
  let (_, edges, _) = (dep_graph body) in
  let coloring = get_colors [] vars edges in
  let stackmax = List.fold_right (fun (_, l) m ->
    match l with
      | LStack(n) -> if n > m then n else m
      | LReg _ -> m) coloring 0 in
  assert_equal expected_colors stackmax ~printer:dump;;

let deps = [
  tdep "d0" "let x = 5 in x"
    [];
    
  tdep "writeup1"
    "
let n = 5 * 5 in
let m = 6 * 6 in
let x = n + 1 in
let y = m + 1 in
x + y
    "
    [("m", "n"); ("x", "m"); ("x", "n"); ("y", "m"); ("y", "x")];

]

let colors = [
  tcolor "writeup2c"
    "
let n = 5 * 5 in
let m = 6 * 6 in
let x = n + 1 in
let y = m + 1 in
let z = x + y in
let k = z * z in
let g = k + 5 in
k + 3
    "
    3;

]

let suite =
"suite">::: deps @ colors

let () =
  run_test_tt_main suite
;;

