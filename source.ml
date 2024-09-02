type global =
| Word          of string
| Array         of string * int

type fundef =
| Fun           of string * string list * expr
and handlerdef =
| Handler       of string * (string option * string list * expr) list
and expr =
| Var           of string
| VarAt         of string * int
| Primitive     of string * expr list
| FunApp        of string * expr list
| Assign        of string * expr
| AssignAt      of string * int * expr
| Let           of string * expr * expr
| Handle        of expr * string
| Do            of string * expr list
| Resume        of expr * string option

type program = (global list * fundef list * handlerdef list) * expr
