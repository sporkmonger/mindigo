type expr =
  Noop
| Int      of int
| Float    of float
| Seq      of expr list
| Call     of expr * string * expr (* TODO: * params *)

val print_ast : expr -> unit
