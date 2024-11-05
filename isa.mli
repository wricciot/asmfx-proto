type 't htag =
  [ `RetTag
  | `Tag of 't ]

type ('a, 'r, 'w) primitive =
| Load          of 'r * 'a      (* $rdst := mem[addr] *)
| Store         of 'a * 'r      (* mem[addr] := $rsrc *)
| MovReg        of 'r * 'r      (* $rdst := $rsrc *)
| LoadImm       of 'r * 'w      (* $rdst := word *)
| JmpReg        of 'r           (* jr $reg // $pc := $reg *)
| Add           of 'r * 'r * 'r (* $rdst := $rsrc1 + $rsrc2 *)
| Sub           of 'r * 'r * 'r (* $rdst := $rsrc1 - $rsrc2 *)
| BranchLT      of 'a * 'r * 'r (* if ($reg1 < $reg2) then $pc := addr *)
and ('a, 't, 'r, 'w) instr =
| Enter 	of 'a * ('t htag * 'a) list * 'r list * 'r list
| Do 		of 't * 'r list
| Resume	of (('t htag * 'a) list * 'r list) option
| Return
| Exit
| Break
| FinalReenter  of (('t htag * 'a) list * 'r list) option * 'r list
| Reenter	of (('t htag * 'a) list * 'r list) option * 'r list
| Primitive	of ('a, 'r, 'w) primitive
| Stop
| DW            of 'w
| Nop
