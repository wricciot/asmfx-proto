type 't mtag =
  [ `RetTag
  | `LeaveTag
  | `Tag of 't ]

type ('a, 't, 'rs) effrcd = 
| LRcd of ('a * ('a, 't, 'rs) effrcd list * 'rs)
| HRcd of ((('t Isa.htag * 'a) list * 'rs) * ('a * ('a, 't, 'rs) effrcd list * 'rs))
| KRcd of (('a * ('a,'t,'rs) effrcd list * 'rs) 
          * ((('t Isa.htag * 'a) list * 'rs) * ('a * ('a,'t,'rs) effrcd list * 'rs)))

val lrcd_of_hrcd : 
  ((('t Isa.htag * 'a) list * 'rs) 
   * ('a * ('a,'t,'rs) effrcd list * 'rs)) 
  -> ('a,'t,'rs) effrcd
val hrcd_of_krcd : 
  (('a * ('a,'t,'w) effrcd list * 'rs) * ((('t Isa.htag * 'a) list * 'rs) 
   * ('a * ('a,'t,'rs) effrcd list * 'rs))) 
  -> ('a,'t,'rs) effrcd

(* location, value, state *)
type ('l, 'v, 's) state = {
	get : 's -> 'l -> 'v;
	put : 's -> ('l * 'v) -> 's;
}

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

exception InadequateECtx of string

val primop : 
        'r -> ('r, ('t, 'r) word, 'rs) state -> (int, ('t, 'r) word, 'ms) state ->
        'rs -> 'ms -> (int, 'r, int) Isa.primitive -> 'rs * 'ms

val machine_step :
        ('t, 'r, ('r * ('t,'r) word) list, 'ms) machine -> 
        ('t, 'r, ('r * ('t,'r) word) list, 'ms) machine 
