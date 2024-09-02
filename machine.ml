open Lib
open Isa

type 't mtag =
  [ `RetTag
  | `LeaveTag
  | `Tag of 't ]

type ('a, 't, 'rs) effrcd = 
| LRcd of ('a * ('a, 't, 'rs) effrcd list * 'rs)
| HRcd of ((('t htag * 'a) list * 'rs) * ('a * ('a, 't, 'rs) effrcd list * 'rs))
| KRcd of (('a * ('a,'t,'rs) effrcd list * 'rs) 
          * ((('t htag * 'a) list * 'rs) * ('a * ('a,'t,'rs) effrcd list * 'rs)))

let lrcd_of_hrcd (_,l) = LRcd l
let hrcd_of_krcd (_,h) = HRcd h

let lrcd_of_effrcd = function
| LRcd _ as l -> l
| HRcd h -> LRcd (h |> snd)
| KRcd k -> LRcd (k |> snd |> snd)

let hrcd_of_effrcd = function
| HRcd _ as h -> h
| KRcd k -> HRcd (k |> snd)
| _ -> assert false


(* location, value, state *)
type ('l, 'v, 's) state = {
	get : 's -> 'l -> 'v;
	put : 's -> ('l * 'v) -> 's;
}

(* the memory holds words, which are instructions, including pseudo-instructions "DW" holding data *)
type ('t, 'r) word = (int, 't, 'r, int) Isa.instr

(* tag, reg, register file, memory array *)
type ('t, 'r, 'rs, 'ms) machine = {
        pc 		: 'r; 
        regs		: 'rs;
        mem		: 'ms;
        regstate	: ('r, ('t, 'r) word, 'rs) state;
        memstate	: (int, ('t, 'r) word, 'ms) state;
        ectx 		: (int, 't, 'rs) effrcd list;
	decode 		: ('t, 'r) word -> (int,'t,'r,int) Isa.instr;
}

let getreg m = m.regstate.get m.regs
let setreg m = m.regstate.put m.regs
let readmem m = m.memstate.get m.mem
let writemem m = m.memstate.put m.mem

exception InadequateECtx of string
exception MachineTrap

let w_of_addr a = DW a
let addr_of_w = function
| DW w -> w
| _ -> 0 (* though we should never rely on that *)

let madd w1 w2 = match w1, w2 with
| DW v1, DW v2 -> DW (v1 + v2)
| _ -> assert false

let msub w1 w2 = match w1, w2 with
| DW v1, DW v2 -> DW (v1 - v2)
| _ -> assert false

let mlt pc w1 w2 = match w1, w2 with 
| DW v1, DW v2 -> v1 < v2
| _ -> assert false

let primop (pc:'r) (regstate:('r,('t,'r) word, 'rs) state) (memstate:(int, ('t,'r) word, 'ms) state)
  (regs : 'rs) (mem : 'ms) = function
| Load (rdst, a) -> 
    let v = memstate.get mem a in
    let regs' = regstate.put regs (rdst, v) in
    regs', mem
| Store (a, rsrc) ->
    let v = regstate.get regs rsrc in
    let mem' = memstate.put mem (a, v) in
    regs, mem'
| MovReg (rdst, rsrc) ->
    let v = regstate.get regs rsrc in
    let regs' = regstate.put regs (rdst, v) in
    regs', mem
| LoadImm (rdst, w) ->
    let regs' = regstate.put regs (rdst, DW w) in
    regs', mem
| JmpReg reg ->
    let v = regstate.get regs reg in
    let regs' = regstate.put regs (pc, v) in
    regs', mem
| Add (rdst, rsrc1, rsrc2) ->
    let v1 = regstate.get regs rsrc1 in
    let v2 = regstate.get regs rsrc2 in
    let regs' = regstate.put regs (rdst, madd v1 v2) in
    regs', mem
| Sub (rdst, rsrc1, rsrc2) ->
    let v1 = regstate.get regs rsrc1 in
    let v2 = regstate.get regs rsrc2 in
    let regs' = regstate.put regs (rdst, msub v1 v2) in
    regs', mem
| BranchLT (a, rsrc1, rsrc2) ->
    let v1 = regstate.get regs rsrc1 in
    let v2 = regstate.get regs rsrc2 in
    let regs' = if (mlt pc v1 v2) then regstate.put regs (pc, w_of_addr a) else regs in
    regs', mem

let find_tag tag ectx =
  let rec inner = function
  | (tag', atag')::l when tag = tag' -> Some atag'
  | _::l -> inner l
  | _ -> None
  in
  let rec aux acc = function
  | HRcd (((hs, stateregs), lrcd) as hrcd) as r::c -> (match inner hs with
    | Some atag' -> acc, atag', hrcd, c
    | None -> aux (acc@[r]) c)
  | r::c -> aux (acc@[r]) c
  | _ -> raise (InadequateECtx "Unhandled tag in ectx");
  in aux [] ectx

let rec find_state = function
| KRcd (_,(_,(_,_,s)))::_ -> s
| _::c -> find_state c
| _ -> raise (InadequateECtx "No accessible state in ectx")

let find_resume ectx =
  let rec aux acc = function
  | KRcd ((ar, dr, csave), hrcd)::c -> acc, ar, dr, csave, hrcd, c
  | r::c -> aux (acc@[r]) c
  | _ -> raise (InadequateECtx "No accessible resumption in ectx")
  in aux [] ectx

let find_return ectx = find_tag `RetTag ectx

let find_leave ectx =
  let rec aux acc = function
  | LRcd (al, dl, esave)::c -> acc, al, dl, esave, c
  | r::c -> aux (acc@[r]) c
  | _ -> raise (InadequateECtx "No accessible leave in ectx")
  in aux [] ectx

let machine_step (m : ('t, 'r, 'rs, 'ms) machine) =
  let pc = m.pc in
  let apc = pc |> getreg m |> addr_of_w in
  let instr = apc |> readmem m |> m.decode in
  let apc' = apc + 1 in
  let m' = { m with regs = (pc, apc' |> w_of_addr) |> setreg m; } in
  let getregs regs = List.combine regs (List.map (getreg m) regs) in 
  let setregs regs = regs |> List.fold_left m.regstate.put m.regs in
  try
    match instr with
    | Enter (a_cli, hs, statusregs, esaveregs) ->
        let esave = getregs esaveregs in
        let status = getregs statusregs in
        { m with regs = (pc, a_cli |> w_of_addr) |> setreg m;
  		 ectx = HRcd ((hs, status), (apc', [], esave))::m.ectx; }
    | Do (tag, csaveregs) -> 
        let dr, a_htag, (((_, state),_) as hrcd), c = m.ectx |> find_tag (`Tag tag) in
        let csave = getregs csaveregs in
        { m with regs = (* (pc, a_htag |> w_of_addr) |> setreg m; *)
                        setregs ((pc, a_htag |> w_of_addr)::state);
  		 ectx = KRcd ((apc', dr, csave), hrcd) :: c; }
    | Resume ohs ->
        let d, ar, dr, csave, ((hs, state), lrcd), c = m.ectx |> find_resume in
        let stateregs = List.map fst state in
        (* obtain updated handler clauses/stateregs, or keep the old ones if ohs = None *)
        let hs',stateregs' = ohs |> unopt (hs,stateregs) in
        (* new state to be saved to the ectx *)
        let state' = getregs stateregs' in
        { m with regs = (* (pc, ar |> w_of_addr) |> setreg m; *)
                        setregs ((pc, ar |> w_of_addr)::csave);
  		 ectx = dr @ HRcd ((hs', state'), lrcd) :: c; }
    | Return ->
        let d, a_ret, ((_, stateregs), lrcd), c = m.ectx |> find_return in
        { m with regs = (* (pc, a_ret |> w_of_addr) |> setreg m; *)
                        setregs ((pc, a_ret |> w_of_addr)::stateregs);
  		 ectx = LRcd lrcd :: c; }
    | Exit ->
        let d, al, dl, esave, c = m.ectx |> find_leave in
        { m with regs = (* (pc, al |> w_of_addr) |> setreg m; *)
                        setregs ((pc, al |> w_of_addr)::esave);
  		 ectx = dl @ c; }
    | Break ->
        let d, ar, dr, _csave,  (hs, (al, dl, esave)), c = m.ectx |> find_resume in
        { m with regs = (* (pc, al |> w_of_addr) |> setreg m; *)
                        setregs ((pc, al |> w_of_addr)::esave);
  		 ectx = dl @ c; }
    | Reenter (ohs, hsaveregs) ->
        let d, ar, dr, csave, ((hs, state), (al, dl, _esave) as hrcd), c = m.ectx |> find_resume in
        let stateregs = List.map fst state in
        (* obtain updated handler clauses/stateregs, or keep the old ones if ohs = None *)
        let hs',stateregs' = ohs |> unopt (hs,stateregs) in
        (* new state to be saved to the ectx *)
        let state' = getregs stateregs' in
        (* handler register values to be saved to the ectx *)
        let hsave = getregs hsaveregs in
        (* somewhat interestingly, the saved client registers are now restored, but they are 
         * also preserved in the KRcd in case of further calls to the resumption *)
        { m with regs = (* (pc, ar |> w_of_addr) |> setreg m; *)
                        setregs ((pc, ar |> w_of_addr)::csave);
          	 ectx = dr @ HRcd ((hs', state'), (apc', d @ [ KRcd ((ar, dr, csave), hrcd) ], hsave)) :: c; }
    | FinalReenter (ohs, hsaveregs) ->
        let d, ar, dr, csave, ((hs, state), (al, dl, _esave as lrcd) as hrcd), c = m.ectx |> find_resume in
        let stateregs = List.map fst state in
        (* obtain updated handler clauses/stateregs, or keep the old ones if ohs = None *)
        let hs',stateregs' = ohs |> unopt (hs,stateregs) in
        (* new state to be saved to the ectx *)
        let state' = getregs stateregs' in
        (* handler register values to be saved to the ectx *)
        let hsave = getregs hsaveregs in
        { m with regs = (* (pc, ar |> w_of_addr) |> setreg m; *)
                        setregs ((pc, ar |> w_of_addr)::csave);
          	 ectx = dr @ HRcd ((hs', state'), (apc', d @ [ LRcd lrcd ], hsave)) :: c; }
    | Primitive i ->
        let regs', mem' = i |> primop m'.pc m'.regstate m'.memstate m'.regs m'.mem in
        { m' with regs = regs'; mem = mem' }
    | Stop | DW _ -> raise MachineTrap
  with InadequateECtx _ -> raise MachineTrap
;;


type register =
| Arg of int    (* $ax *)
| Val of int    (* $vx *)
| Temp of int   (* $tx *)
| Save of int   (* $sx *)
| PC            (* program counter *)
| RA            (* $ra *)
| Zero          (* $0  *)

let pp_regname = function
| Arg x  -> "$a" ^ string_of_int x
| Val x  -> "$v" ^ string_of_int x
| Temp x -> "$t" ^ string_of_int x
| Save x -> "$s" ^ string_of_int x
| PC -> "$pc"
| RA -> "$ra"
| Zero -> "$0"

let memory = 
  (* 'main *)
  (*  0 *)      [ Enter (2 (*'budget_client*), [(`RetTag, 5 (*'budgRet*)); (`Tag `Charge, 8 (*'budgCharge*))], [], [])
  (*  1 *)      ; Stop
  (* 'budget_client *)
  (*  2 *)      ; Primitive (LoadImm (Arg 0, 10))
  (*  3 *)      ; Do (`Charge, [])
  (*  4 *)      ; Return
  (* 'budgRet *)
  (*  5 *)      ; Primitive (MovReg (Arg 0, Val 0))
  (*  6 *)      ; Primitive (Load (Val 1, 16 (*'budget*)))
  (*  7 *)      ; Exit
  (* 'budgCharge *)
  (*  8 *)      ; Primitive (Load (Temp 0, 16 (*'budget*)))
  (*  9 *)      ; Primitive (BranchLT (13 (*'budgBreak*), Temp 0, Arg 0))
  (* 10 *)      ; Primitive (Sub (Temp 0, Temp 0, Arg 0))
  (* 11 *)      ; Primitive (Store (16 (*'budget*), Temp 0))
  (* 12 *)      ; Resume None
  (* 'budgBreak *)
  (* 13 *)      ; Primitive (LoadImm (Val 0, 0))
  (* 14 *)      ; Primitive (LoadImm (Val 1, -1))
  (* 15 *)      ; Break
  (* 'budget *)
  (* 16 *)      ; DW 100]

let memory2 = 
  (* 'main *)
  (*  0 *)      [ Enter (1 (*'addrA*), [(`RetTag, 5 (*'retA*)); (`Tag `TagA, 6 (*'hA*))], [], [])
  (* 'addrA *)
  (*  1 *)      ; Enter (2 (*'addrB*), [(`RetTag, 7 (*'retB*)); (`Tag `TagB, 8 (*'hB*))], [], [])
  (* 'addrB *)
  (*  2 *)      ; Do (`TagB, [])
  (*  3 *)      ; Do (`TagA, [])
  (*  4 *)      ; Return
  (* 'retA *)
  (*  5 *)      ; DW 0
  (* 'hA *)
  (*  6 *)      ; Reenter (Some ([(`RetTag, 9); (`Tag `TagA, 10)],[]), [])
  (* 'retB *)
  (*  7 *)      ; DW 0
  (* 'hB *)     
  (*  8 *)      ; Reenter (Some ([(`RetTag, 11); (`Tag `TagB, 12)],[]), [])
  (* 'retA' *)  
  (*  9 *)      ; DW 0
  (* 'hA' *)    
  (*  10 *)     ; Resume None
  (* 'retB' *)  
  (*  11 *)     ; DW 0
  (* 'hB'*)    
  (*  12 *)     ; DW 100]

let fibomem = 
  (* 'main *)
  (*  0 *)      [ Primitive (LoadImm (Arg 0, 9))
  (*  1 *)      ; Enter (3 (*'fibo*), [(`RetTag, 15 (*'fiboret*))], [], [])
  (*  2 *)      ; Stop
  (* 'fibo *)
  (*  3 *)      ; Primitive (LoadImm (Temp 0, 2))
  (*  4 *)      ; Primitive (BranchLT (14 (*'fibothen*), Arg 0, Temp 0))
  (* 'fiboelse *)
  (*  5 *)      ; Primitive (LoadImm (Temp 0, 1))
  (*  6 *)      ; Primitive (Sub (Arg 0, Arg 0, Temp 0))
  (*  7 *)      ; Enter (3 (*'fibo*), [(`RetTag, 15 (*'fiboret*))], [], [Arg 0])
  (*  8 *)      ; Primitive (MovReg (Save 0, Val 0))
  (*  9 *)      ; Primitive (LoadImm (Temp 0, 1))
  (* 10 *)      ; Primitive (Sub (Arg 0, Arg 0, Temp 0))
  (* 11 *)      ; Enter (3 (*'fibo*), [(`RetTag, 15 (*'fiboret*))], [], [Arg 0; Save 0])
  (* 12 *)      ; Primitive (Add (Arg 0, Val 0, Save 0))
  (* 13 *)      ; Return
  (* 'fibothen *)
  (* 14 *)      ; Return
  (* 'fiboret *)
  (* 15 *)      ; Primitive (MovReg (Val 0, Arg 0))
  (* 16 *)      ; Exit]

let registers = []

(* the register file is an associative list pairing reg names with words; Zero is always 0 and never
 * explicitly in the list *)
let regstate = {
        get = (fun rs r -> if r = Zero then DW 0 else try List.assoc r rs with Not_found -> DW 0);
        put = (fun rs p -> if fst p = Zero then rs else p::List.remove_assoc (fst p) rs);
}

(* don't call with negative n *)
let mem_seek l n =
  let rec aux (ll,x,lr as acc) n = 
    if n = 0 then acc
    else match lr with
    | [] -> aux (ll@[x],DW 0,lr) (n-1)
    | y::lr' -> aux (ll@[x],y,lr') (n-1)
  in match l with
  | [] -> aux ([],DW 0,l) n
  | y::l' -> aux ([],y,l') n

(* the memory is a list of words, accessed by index (address) *)
let memstate = {
        get = (fun ms a -> try List.nth ms a with Failure _ | Invalid_argument _ -> DW 0);
        put = (fun ms (a, x) -> let (l,_,r) = mem_seek ms a in l@x::r);
}

let decode i = i


(* an example with a budget handler and a #charge effect *)
let ex_machine = {
        pc = PC;
        regs = registers;
        mem = memory;
        regstate = regstate;
        memstate = memstate;
        ectx = [];
        decode = decode;
}

(* a meaningless example to test reenter *)
let ex_machine2 = {
        pc = PC;
        regs = registers;
        mem = memory2;
        regstate = regstate;
        memstate = memstate;
        ectx = [];
        decode = decode;
}

(* a recursive Fibonacci implemented with a trivial handler *)
let ex_fibo = {
        pc = PC;
        regs = registers;
        mem = fibomem;
        regstate = regstate;
        memstate = memstate;
        ectx = [];
        decode = decode;
}

let pp_tag = function `Charge -> "charge" | `TagA -> "tagA" | `TagB -> "tagB"

let pp_htag = function `RetTag -> "return" | `Tag t -> pp_tag t

let pp_hclause (tag, a) = Printf.sprintf "(%s >> %d)" (pp_htag tag) a

let pp_handler hs = "[" ^ (hs |> List.map pp_hclause |> String.concat "; ") ^ "]"

let pp_reglist regs = "[" ^ (regs |> List.map pp_regname |> String.concat "; ") ^ "]"

let pp_instruction = function
| Primitive (Load (r,a)) -> Printf.sprintf "%s := mem[%d]" (pp_regname r) a
| Primitive (Store (a,r)) -> Printf.sprintf "mem[%d] := %s" a (pp_regname r)
| Primitive (MovReg (rdst, rsrc)) -> Printf.sprintf "%s := %s" (pp_regname rdst) (pp_regname rsrc)
| Primitive (LoadImm (r,w)) -> Printf.sprintf "%s := %d" (pp_regname r) w
| Primitive (JmpReg r) -> Printf.sprintf "jr %s" (pp_regname r)
| Primitive (Add (rdst,rsrc1,rsrc2)) -> 
    Printf.sprintf "%s := %s + %s" (pp_regname rdst) (pp_regname rsrc1) (pp_regname rsrc2)
| Primitive (Sub (rdst,rsrc1,rsrc2)) -> 
    Printf.sprintf "%s := %s - %s" (pp_regname rdst) (pp_regname rsrc1) (pp_regname rsrc2)
| Primitive (BranchLT (a,r1,r2)) -> 
    Printf.sprintf "if (%s < %s) then $pc := %d" (pp_regname r1) (pp_regname r2) a
| Enter (a,hs,stateregs,esaveregs) -> 
    Printf.sprintf "enter %d,%s,%s,%s" a (pp_handler hs) (pp_reglist stateregs) (pp_reglist esaveregs) 
| Do (tag,csaveregs) -> Printf.sprintf "do %s,%s" (pp_tag tag) (pp_reglist csaveregs)
| Resume None -> Printf.sprintf "resume"
| Resume (Some (hs,stateregs)) -> Printf.sprintf "resume %s,%s" (pp_handler hs) (pp_reglist stateregs)
| Return -> "return"
| Exit -> "exit"
| Break -> "break"
| Reenter (None,hsaveregs) -> Printf.sprintf "reenter %s" (pp_reglist hsaveregs)
| Reenter (Some (hs,stateregs),hsaveregs) -> Printf.sprintf "reenter %s,%s,%s" (pp_handler hs) (pp_reglist stateregs) (pp_reglist hsaveregs)
| FinalReenter (None,hsaveregs) -> Printf.sprintf "finalreenter %s" (pp_reglist hsaveregs)
| FinalReenter (Some (hs,stateregs),hsaveregs) -> Printf.sprintf "finalreenter %s,%s,%s" (pp_handler hs) (pp_reglist stateregs) (pp_reglist hsaveregs)
| Stop -> "stop"
| DW w -> Printf.sprintf "DW %d" w

let pp_register (regname, w) = Printf.sprintf "%s = %s" (pp_regname regname) (pp_instruction w)

let pp_registers regs = List.map pp_register regs |> String.concat "; "

let pp_ercd = function 
| LRcd (_,_,rs) -> Printf.sprintf "Leave %s" (pp_registers rs)
| HRcd ((_,rs),_) -> Printf.sprintf "Handler %s" (pp_registers rs)
| KRcd ((_,_,rs),_) -> Printf.sprintf "Cont %s" (pp_registers rs)

let pp_ectx c = "[" ^ (List.map pp_ercd c |> String.concat "; ") ^ "]"

let pp_machine m = 
  let pc = regstate.get m.regs PC |> addr_of_w in
  let instr = memstate.get m.mem pc in
  Printf.sprintf "Registers: %s\nInstruction: %s\nECtx = %s" (pp_registers m.regs) (pp_instruction instr) (pp_ectx m.ectx)

let rec run_test n m =
  Printf.printf "%s\n================\n" (pp_machine m);
  if n <= 0 then ()
  else try run_test (n-1) (machine_step m) with MachineTrap -> ()

let _ = run_test 1000 ex_fibo
