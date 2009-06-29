type expr =
  Noop
| Int      of int
| Float    of float
| Seq      of expr list
| Call     of expr * string * expr (* * params *)

let print_ast ast =
  let rec print_ast_internal = function
    Noop -> "Noop"
  | Int(n) -> (Printf.sprintf "Int %d" n)
  | Float(n) -> (Printf.sprintf "Float %f" n)
  | Seq(expressions) -> (
      String.concat "" [
        "Seq(";
        String.concat ", " (List.map print_ast_internal expressions);
        ")"
      ]
    )
  | Call(target, message, param) -> (
      String.concat "" [
        "Call(";
        String.concat ", " [
          print_ast_internal(target);
          message;
          print_ast_internal(param)
        ];
        ")"
      ]
    )
  in (Printf.printf "%s\n" (print_ast_internal ast))
