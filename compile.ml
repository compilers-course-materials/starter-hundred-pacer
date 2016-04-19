open Expr
open Instruction
open Printf
open ExtLib
open Color

let offset = 8

let count = ref 0

let rec remove x xs =
  match xs with
    | y::xs -> if x = y then remove x xs else y::(remove x xs)
    | [] -> []

let rec remove_duplicates xs =
  match xs with
    | x::xs -> x::(remove_duplicates (remove x xs))
    | [] -> []

let gen_temp base =
  count := !count + 1;
  sprintf "temp_%s_%d" base !count

type hole =
  | CHole of (cexpr -> aexpr)
  | ImmHole of (immexpr -> aexpr)

let fill_imm (h : hole) (v : immexpr) : aexpr =
  match h with
    | CHole(k) -> (k (CImmExpr(v)))
    | ImmHole(k) -> (k v)

let fill_c (h : hole) (c : cexpr) : aexpr =
  match h with
    | CHole(k) -> (k c)
    | ImmHole(k) ->
      let tmp = gen_temp "" in
      ALet(tmp, c, k (ImmId(tmp)))

let return_hole = CHole(fun ce -> ACExpr(ce))

let rec anf_list (es : expr list) (k : immexpr list -> aexpr) : aexpr =
  match es with
    | [] -> k []
    | e::rest ->
      anf e (ImmHole(fun imm ->
        anf_list rest (fun imms -> k (imm::imms))))

and anf (e : expr) (h : hole) : aexpr =
  match e with
    | ENumber(n) -> fill_imm h (ImmNumber(n)) 
    | EBool(b) -> fill_imm h (ImmBool(b)) 
    | EId(x) -> fill_imm h (ImmId(x))
    | EPrim1(op, e) ->
      anf e (ImmHole(fun imm -> (fill_c h (CPrim1(op, imm)))))
    | EPrim2(op, left, right) ->
      anf left (ImmHole(fun limm ->
        anf right (ImmHole(fun rimm ->
          (fill_c h (CPrim2(op, limm, rimm)))))))
    | EApp(f, args) ->
      anf_list args (fun aimms -> fill_c h (CApp(f, aimms)))
    | EIf(cond, thn, els) ->
      anf cond (ImmHole(fun cimm ->
        (fill_c h (CIf(cimm, (anf thn return_hole), (anf els return_hole))))))
    | ELet([], body) -> anf body h
    | ELet((name, value)::rest, body) ->
      anf value (CHole(fun ce ->
        ALet(name, ce, anf (ELet(rest, body)) h)))

let anf_decl (d : decl) : adecl =
  match d with
    | DFun(name, args, body) ->
      ADFun(name, args, anf body return_hole)

let anf_program (p : program) : aprogram =
  match p with
    | Program(decls, main) ->
      AProgram(List.map anf_decl decls, anf main return_hole)

let rec find ls x =
  match ls with
    | [] -> None
    | (y,v)::rest ->
      if y = x then Some(v) else find rest x

let findknown ls x =
  match find ls x with
    | None -> failwith "Not found in findknown"
    | Some(v) -> v


let const_true = HexConst(0x0000001f)
let const_false = HexConst(0x0000000f)
  

let acompile_imm_arg (i : immexpr) (env : location envt) : arg =
  match i with
    | ImmNumber(n) ->
      Sized(DWORD_PTR, Const((n lsl 1)))
    | ImmBool(b) ->
      if b then const_true else const_false
    | ImmId(name) ->
      begin match find env name with
        | Some(LStack(n)) -> Sized(DWORD_PTR, RegOffset(-1 * offset * n, RBP))
        | Some(LReg(r)) -> Reg(r)
        | None -> failwith ("Unbound identifier" ^ name)
      end

let dest_for l =
  match l with
    | LReg(r) -> Reg(r)
    | LStack(i) -> Sized(DWORD_PTR, RegOffset(-1 * offset * i, RBP))

let comb (build : arg -> arg -> instruction) (into : location) (from : arg)  =
  match into, from with
    | LReg _, _
    | _, Reg _
    | _, Sized(_, Reg _) -> [ build (dest_for into) from ]
    | _, _ ->
      [
        IMov(Reg(RAX), from);
        build (dest_for into) (Reg(RAX))
      ]

let comb_mov = comb (fun a1 a2 -> IMov(a1, a2))

let acompile_imm (i : immexpr) (env : location envt) (into : location) : instruction list =
  comb_mov into (acompile_imm_arg i env)

let throw_err code = 
  [
    IMov(Reg(RDI), Const(code));
    ICall("error");
  ]

let check_overflow = IJo("overflow_check")
let error_non_int = "error_non_int"
let error_non_bool = "error_non_bool"

let check_bool =
  [
    IAnd(Reg(RCX), Const(0x0000000000000001));
    ICmp(Reg(RCX), Const(0x0000000000000001));
    IJne(error_non_bool);
  ]

let check_num =
  [
    IAnd(Reg(RCX), Const(0x0000000000000001));
    ICmp(Reg(RCX), Const(0x0000000000000000));
  ]

let rec count_c_vars (ce : cexpr) : int =
  match ce with
    | CIf(_, thn, els) ->
      max (count_vars thn) (count_vars els)
    | _ -> 0

and count_vars (ae : aexpr) : int =
  match ae with
    | ALet(x, bind, body) -> 
      1 + (max (count_c_vars bind) (count_vars body))
    | ACExpr(ce) -> count_c_vars ce

let check_nums arg1 arg2 =
  [
    IMov(Reg(RCX), arg1) 
  ] @ check_num @ [
    IJne(error_non_int);
    IMov(Reg(RCX), arg2);
  ] @ check_num @ [
    IJne(error_non_int);
  ]

let save_restore env =
  let regs = remove_duplicates (List.filter (fun l ->
    match l with
      | LReg _ -> true
      | LStack _ -> false) (List.map snd env)) in
  (List.map (fun l ->
    match l with
      | LReg(r) -> IPush(Reg(r))
      | LStack _ -> failwith "cannot happen") regs,
   List.rev_map (fun l ->
    match l with
      | LReg(r) -> IPop(Reg(r))
      | LStack _ -> failwith "cannot happen") regs)

let rec acompile_step (s : cexpr) (env : location envt) (into : location) (is_tail : bool) : instruction list =
  match s with
    | CApp(f, iargs) ->
      if is_tail then
        let argpushes = List.rev_map (fun a -> IPush(Sized(DWORD_PTR, acompile_imm_arg a env))) iargs in
        let argmovpairs = List.mapi (fun i a ->
          [
            IMov(Reg(RAX), Sized(DWORD_PTR, RegOffset(i * offset, RSP)));
            IMov(Sized(DWORD_PTR, RegOffset((i + 1) * offset * -1, RBP)), Reg(RAX))
          ])
          iargs in
        let argmovs = List.flatten argmovpairs in
        argpushes @
        argmovs @ [
          IMov(Reg(RSP), Reg(RBP));
          ISub(Reg(RSP), Const((List.length iargs) * offset));
          IJmp(LabelVal(f))
        ]
      else
        let argpushes = List.map (fun a -> IPush(Sized(DWORD_PTR, acompile_imm_arg a env))) iargs in
        let donecall = gen_temp "donecall" in
        [
          IMov(Reg(RAX), LabelVal(donecall));
          IPush(Reg(RAX));
          IPush(Reg(RBP));
        ] @
        argpushes @ [
          IJmp(LabelVal(f))
        ] @ [
          ILabel(donecall);
          IMov(dest_for into, Reg(RAX))
        ]
    | CPrim1(op, e) ->
      let prelude = acompile_imm e env into in
      begin match op with
        | Add1 ->
          prelude @ [
            IAdd(dest_for into, Const(2))
          ]
        | Sub1 ->
          prelude @ [
            IAdd(dest_for into, Const(-2))
          ]
        | IsNum ->
          prelude @ [
            IAnd(dest_for into, Const(0x1));
            IShl(dest_for into, Const(4));
            IXor(dest_for into, Const(0x0000001f));
          ]
        | IsBool ->
          prelude @ [
            IAnd(dest_for into, Const(0x1));
            IShl(dest_for into, Const(4));
            IOr(dest_for into, Const(0x0000000f));
          ]
        | Print ->
          let (active_pushes, active_pops) = save_restore env in
          active_pushes @
          [
            IMov(Reg(RDI), Sized(DWORD_PTR, acompile_imm_arg e env));
            ICall("print");
          ] @
          active_pops @ [
            IMov(dest_for into, Reg(RAX));
          ]
      end

    | CPrim2(op, left, right) ->
      let left_as_arg = acompile_imm_arg left env in
      let right_as_arg = acompile_imm_arg right env in
      let checked = check_nums left_as_arg right_as_arg in
      begin match op with
        | Plus ->
          checked @
          (comb_mov into left_as_arg) @
          (comb (fun a1 a2 -> IAdd(a1, a2)) into right_as_arg) @
          [
            check_overflow
          ]
        | Minus ->
          checked @
          (comb_mov into left_as_arg) @
          (comb (fun a1 a2 -> ISub(a1, a2)) into right_as_arg) @
          [
            check_overflow
          ]
        | Times ->
          checked @
          (comb_mov into left_as_arg) @
          [ ISar(dest_for into, Const(1)) ] @
          (comb (fun a1 a2 -> IMul(a1, a2)) into right_as_arg) @
          [
            check_overflow;
          ]
        | Less ->
          let is_less = gen_temp "is_less" in
          let done_less = gen_temp "done" in
          checked @
          (comb_mov into left_as_arg) @
          (comb (fun a1 a2 -> ISub(a1, a2)) into right_as_arg) @
          [
            IJl(is_less);
            IMov(dest_for into, const_false);
            IJmp(LabelVal(done_less));
            ILabel(is_less);
            IMov(dest_for into, const_true);
            ILabel(done_less);
          ]
        | Greater ->
          let is_greater = gen_temp "is_greater" in
          let done_greater = gen_temp "done" in
          checked @
          (comb_mov into left_as_arg) @
          (comb (fun a1 a2 -> ISub(a1, a2)) into right_as_arg) @
          [
            IJg(is_greater);
            IMov(dest_for into, const_false);
            IJmp(LabelVal(done_greater));
            ILabel(is_greater);
            IMov(dest_for into, const_true);
            ILabel(done_greater);
          ]
        | Equal ->
          let leave_false = gen_temp "equals" in
          (comb_mov into left_as_arg) @
          (comb (fun a1 a2 -> ICmp(a1, a2)) into right_as_arg) @
          [
            IMov(dest_for into, const_false);
            IJne(leave_false);
            IMov(dest_for into, const_true);
            ILabel(leave_false);
          ]
       end
    | CImmExpr(i) -> acompile_imm i env into
    | CIf(cond, thn, els) ->
      let prelude = acompile_imm cond env (LReg(RCX)) in
      let check = check_bool  in
      let thn = acompile_expr thn env into is_tail in
      let els = acompile_expr els env into is_tail in
      let label_then = gen_temp "then" in
      let label_else = gen_temp "else" in
      let label_end = gen_temp "end" in
      prelude @ check @ prelude @ [
        ICmp(Reg(RCX), const_true);
        IJe(label_then);
        ICmp(Reg(RCX), const_false);
        IJe(label_else);
        IJmp(LabelVal(error_non_bool));
        ILabel(label_then)
      ] @
      thn @
      [ IJmp(LabelVal(label_end)); ILabel(label_else) ] @
      els @
      [ ILabel(label_end) ]

and acompile_expr (e : aexpr) (env : location envt) (into : location) (is_tail : bool) : instruction list =
  match e with
    | ALet(id, e, body) ->
      begin match find env id with
        | Some(loc) ->
          let prelude = match loc with
            | LStack(l) ->
              (acompile_step e env (LReg(RAX)) false)
              @
              [IMov(dest_for loc, Reg(RAX))]
            | LReg(r) -> 
              acompile_step e env loc false
          in
          let postlude = acompile_expr body env into is_tail in
          prelude @ postlude
        | None -> failwith ("Don't know where to put: " ^ id)
      end
    | ACExpr(s) -> acompile_step s env into is_tail

let acompile_decl (ad : adecl) : instruction list =
  match ad with
    | ADFun(name, args, body) ->
      let max = ref 0 in
      let env = colorful_env body in
      let env = List.map (fun (s, l) ->
        (s, match l with
          | LReg(r) -> l
          | LStack(l) ->
            if l > !max then max := l;
            LStack(l + 1 + (List.length args)))) env in
      let (pushes, pops) = save_restore env in
      let arglocs = List.mapi (fun i a -> (a, LStack(i + 1))) args in
      [
        ILabel(name);
        IMov(Reg(RBP), Reg(RSP));
        IAdd(Reg(RBP), Const(offset * (List.length args)));
        ISub(Reg(RSP), Const(offset * (!max + 1)))
      ] @
      pushes @
      (acompile_expr body (arglocs @ env) (LReg(RAX)) true) @
      pops @
      [
        IMov(Reg(RSP), Reg(RBP));
        IPop(Reg(RBP));
        IRet;
      ]

let rec find_decl (ds : decl list) (name : string) : decl option =
  match ds with
    | [] -> None
    | (DFun(fname, _, _) as d)::ds_rest ->
      if name = fname then Some(d) else find_decl ds_rest name

let rec find_one (l : 'a list) (elt : 'a) : bool =
  match l with
    | [] -> false
    | x::xs -> (elt = x) || (find_one xs elt)

let rec find_dup (l : 'a list) : 'a option =
  match l with
    | [] -> None
    | [x] -> None
    | x::xs ->
      if find_one xs x then Some(x) else find_dup xs

let rec well_formed_e (e : expr) (ds : decl list) (env : bool envt) =
  match e with
    | ENumber(_)
    | EBool(_) -> []
    | EId(x) ->
      begin match find env x with
        | None -> ["Unbound identifier: " ^ x]
        | Some(_) -> []
      end
    | EPrim1(op, e) ->
      well_formed_e e ds env
    | EPrim2(op, left, right) ->
      (well_formed_e left ds env) @ (well_formed_e right ds env)
    | EIf(cond, thn, els) ->
      (well_formed_e cond ds env) @
      (well_formed_e thn ds env) @
      (well_formed_e els ds env)
    | EApp(name, args) ->
      let from_args = List.flatten (List.map (fun a -> well_formed_e a ds env) args) in
      begin match find_decl ds name with
        | None -> ("No such function: " ^ name)::from_args
        | Some(DFun(_, params, _)) ->
          let expect_len = (List.length params) in
          let actual_len = (List.length args) in
          if expect_len = actual_len then
            from_args
          else
            let err = sprintf "Arity mismatch: Expected %d args when calling %s, but got %d" expect_len name actual_len in
            err::from_args
      end
    | ELet(binds, body) ->
      let names = List.map fst binds in
      let env_from_binds = List.map (fun a -> (a, true)) names in
      let from_body = well_formed_e body ds (env_from_binds @ env) in
      begin match find_dup names with
        | None -> from_body
        | Some(name) -> ("Duplicate name in let: " ^ name)::from_body
      end

let well_formed_d (d : decl) (ds : decl list) : string list =
  match d with
    | DFun(name, args, body) ->
      let env = List.map (fun a -> (a, true)) args in
      let from_body = well_formed_e body ds env in
      begin match find_dup args with
        | None -> from_body
        | Some(v) -> ("Duplicate parameter: " ^ v)::from_body
      end

let well_formed_p (p : program) : string list =
  match p with
    | Program(ds, maine) ->
      let names = List.map (fun (DFun(name, _, _)) -> name) ds in
      let subexpr_errs = (well_formed_e maine ds []) @
        (List.flatten (List.map (fun d -> well_formed_d d ds) ds)) in
      begin match find_dup names with
        | None -> subexpr_errs
        | Some(v) -> ("Duplicate function definition: " ^ v)::subexpr_errs
      end

let compile_to_string prog =
  count := 0;
  match well_formed_p prog with
    | x::rest ->
      let errstr = (List.fold_left (fun x y -> x ^ "\n" ^ y) "" (x::rest)) in
      failwith errstr
    | [] ->
      let anfed = (anf_program prog) in
      match anfed with
        | AProgram(decls, main) ->
          let compiled_decls = List.map acompile_decl decls in
          let env = colorful_env main in
          let compiled_main = (acompile_expr main env (LReg(RAX)) false) in
          let varcount = count_vars main in
          let stackjump = offset * varcount in
          let prelude = "
section .text
extern error
extern print
global our_code_starts_here" in
          let main_start = [
            ILabel("our_code_starts_here");
            IPush(Reg(RBP));
            IMov(Reg(RBP), Reg(RSP));
            ISub(Reg(RSP), Const(stackjump))
          ] in
          let postlude = [
            IMov(Reg(RSP), Reg(RBP));
            IPop(Reg(RBP));
            IRet;
            ILabel("overflow_check")
          ]
          @ (throw_err 3)
          @ [ILabel(error_non_int)] @ (throw_err 1)
          @ [ILabel(error_non_bool)] @ (throw_err 2) in
          let as_assembly_string = (to_asm (
            (List.flatten compiled_decls) @
            main_start @
            compiled_main @
            postlude)) in
          sprintf "%s%s\n" prelude as_assembly_string

