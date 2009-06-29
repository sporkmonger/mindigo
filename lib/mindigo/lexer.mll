{
open Grammar;;        (* The type token is defined in grammar.mli *)
exception Eof;;
}

let decimal_literal = ['0'-'9']+
let hex_literal =
  '0' ['x' 'X']['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f']*
let oct_literal = '0' ['o' 'O']['0'-'7']['0'-'7']*
let bin_literal = '0' ['b' 'B']['0'-'1']['0'-'1']*
let int_literal = decimal_literal | hex_literal | oct_literal | bin_literal

rule token = parse
    [' ' '\t']        { token lexbuf }     (* skip blanks *)
  | ['\n' ]           { EOL }
  | int_literal       {
      INT(int_of_string(Lexing.lexeme lexbuf))
    }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { TIMES }
  | '/'               { DIV }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | '['               { LBRACK }
  | ']'               { RBRACK }
  | '{'               { LBRACE }
  | '}'               { RBRACE }
  | ';'               { SEMI }
  | eof               { raise Eof }
