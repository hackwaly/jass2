{
open Parser
open Lexing

let quote_regexp = Str.regexp_string "\\\""
let replace_quotes = Str.global_replace quote_regexp "\""
}

let digit = ['0'-'9']
let hex_digit = ['0'-'9' 'A'-'F' 'a'-'f']
let cc = [^'\'']
let alpha = ['A'-'Z' 'a'-'z']
let decimal = ['1'-'9'] ['0'-'9']* | '0'

rule token = parse
  | eof                                 { EOF }
  | [' ' '\t']                          { token lexbuf }
  | "//" [^'\n']*                       { token lexbuf }
  | '\n' {
    Lexing.new_line lexbuf;
    NL
  }
  | "and"                               { AND }
  | "array"                             { ARRAY }
  | "boolean"                           { BOOLEAN }
  | "call"                              { CALL }
  | "code"                              { CODE }
  | "constant"                          { CONSTANT }
  | "else"                              { ELSE }
  | "elseif"                            { ELSEIF }
  | "endfunction"                       { ENDFUNCTION }
  | "endglobals"                        { ENDGLOBALS }
  | "endif"                             { ENDIF }
  | "endloop"                           { ENDLOOP }
  | "exitwhen"                          { EXITWHEN }
  | "extends"                           { EXTENDS }
  | "false"                             { FALSE }
  | "function"                          { FUNCTION }
  | "globals"                           { GLOBALS }
  | "handle"                            { HANDLE }
  | "if"                                { IF }
  | "integer"                           { INTEGER }
  | "local"                             { LOCAL }
  | "loop"                              { LOOP }
  | "native"                            { NATIVE }
  | "not"                               { NOT }
  | "nothing"                           { NOTHING }
  | "null"                              { NULL }
  | "or"                                { OR }
  | "real"                              { REAL }
  | "return"                            { RETURN }
  | "returns"                           { RETURNS }
  | "set"                               { SET }
  | "string"                            { STRING }
  | "takes"                             { TAKES }
  | "then"                              { THEN }
  | "true"                              { TRUE }
  | "type"                              { TYPE }
  | ","                                 { COMMA }
  | "="                                 { EQ }
  | "["                                 { LBRACKET }
  | "]"                                 { RBRACKET }
  | "("                                 { LPAREN }
  | ")"                                 { RPAREN }
  | "=="                                { EQEQ }
  | "!="                                { NEQ }
  | "<"                                 { LT }
  | ">"                                 { GT }
  | "<="                                { LEQ }
  | ">="                                { GEQ }
  | "+"                                 { PLUS }
  | "-"                                 { MINUS }
  | "*"                                 { MULT }
  | "/"                                 { DIV }
  | '"' (("\\\"" | [^'"'])* as m) '"' {
    STRING_LITERAL (replace_quotes m)
  }
  | '\'' (cc as c1) (cc as c2) (cc as c3) (cc as c4) '\'' {
    let c1 = (Char.code c1) lsl 24 in
    let c2 = (Char.code c2) lsl 16 in
    let c3 = (Char.code c3) lsl 8 in
    let c4 = Char.code c4 in
    INT_LITERAL (c1 lor c2 lor c3 lor c4)
  }
  | decimal '.' digit+ as m {
    REAL_LITERAL (float_of_string m)
  }
  | decimal as m {
    INT_LITERAL (int_of_string m)
  }
  | '0' (['0'-'7']+ as m) {
    INT_LITERAL (int_of_string ("0o" ^ m))
  }
  | '0'                                 { INT_LITERAL 0 }
  | "0x" (hex_digit+ as m) {
    INT_LITERAL (int_of_string ("0x" ^ m))
  }
  | alpha ((alpha | '_' | digit)* (alpha | digit))? as m {
    IDENT m
  }
