open Graph
open Instruction
open Expr

let rec find ls x =
  match ls with
    | [] -> None
    | (y,v)::rest ->
      if y = x then Some(v) else find rest x

let findknown ls x =
  match find ls x with
    | None -> failwith "Not found in findknown"
    | Some(v) -> v

(* This example was helpful in generating the graph code: http://ocamlgraph.lri.fr/sudoku.ml *)

(* A module for constructing graphs whose vertices are strings *)
module G = Imperative.Graph.Abstract(struct type t = string end)

(* A module for coloring G-graphs *)
module C = Coloring.Mark(G)

let color_graph (colors : int) (nodes : string list) (edges : (string * string) list) : ((string * int) list) option =
  let g = G.create () in
  let add_vertex s =
    let vertex = G.V.create s in
    G.add_vertex g vertex;
    (s, vertex) in
  let vertices = List.map add_vertex nodes in
  let add_edge (source, dest) =
    let v1, v2 = (findknown vertices source, findknown vertices dest) in
    G.add_edge g v1 v2 in
  List.iter add_edge edges;
  begin try
    C.coloring g colors;
    Some(List.map (fun v -> (v, G.Mark.get (findknown vertices v))) nodes)
  with
    | _ -> None
  end

(* Return all the identifiers that are defined in this expression (e.g. appear
in let bindings) to use for building an environment *)
let getvars (ae : aexpr) : string list =
  []
  
let get_colors (registers : reg list) (varlist : string list) (edgelist : (string * string) list) : (location envt) =
  []

let dep_graph (ae : aexpr) : (string * string) list =
  []

let colorful_env (ae : aexpr) : location envt =
  let deps = dep_graph ae in
  (* spare_regs is a list that contains the usable registers for your
  implementation; it is set by the NUMREGS option as well *)
  get_colors !spare_regs (getvars ae) deps

