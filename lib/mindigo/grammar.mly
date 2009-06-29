%{
  (* header *)
  open Ast;;
%}
  /* declarations */

  %token <int> INT
  %token <float> FLOAT
  %token SEMI
  %token PLUS MINUS TIMES DIV
  %token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
  %token EOL

  %nonassoc SEMI
  %left PLUS MINUS        /* low precedence */
  %left TIMES DIV         /* medium precedence */
  %nonassoc UMINUS        /* highest precedence */

  %start main             /* the entry point */
  %type <Ast.expr> main
  
%%
  /* rules */

  main:
      expr EOL            { $1 }
  ;
  expr:
      INT                     { Int($1) }
    | FLOAT                   { Float($1) }
    | expr SEMI               { Seq([$1; Noop]) }
    | expr SEMI expr          {
      match $1 with
        Seq(expressions) -> Seq(List.append expressions [$3])
      | single_expression -> Seq([single_expression; $3])
    }
    | LPAREN expr RPAREN      { $2 }
    | expr PLUS expr          { Call($1, "+", $3) }
    | expr MINUS expr         { Call($1, "-", $3) }
    | expr TIMES expr         { Call($1, "*", $3) }
    | expr DIV expr           { Call($1, "/", $3) }
    | MINUS expr %prec UMINUS {
      match $2 with
        Int(value) -> Int(-value)
      | Float(value) -> Float(-.value)
      | _ -> failwith "Unary minus can only be applied to numeric literals."
    }
  ;

%%
  (* trailer *)
