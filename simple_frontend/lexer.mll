(* File lexer.mll *)
{
 open Parser  
 exception No_such_symbol
 open Lexing
 
 let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
            { pos with pos_bol = lexbuf.lex_curr_pos;
                    pos_lnum = pos.pos_lnum + 1
            }
 let save_next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
        Count.line := pos.pos_lnum
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule lexer = parse
    | digit+ as num  { NUM (int_of_string num) }
    | "if"                    { IF }
    | "else"                  { ELSE }
    | "while"                 { WHILE }
    | "scan"                  { SCAN }
    | "sprint"                { SPRINT }
    | "iprint"                { IPRINT }
    | "int"                   { INT }
    | "return"                { RETURN }
    | "type"                  { TYPE }
    | "void"                  { VOID }
    | id as text              { ID text }
    | '\"'[^'\"']*'\"' as str { STR str }
    | '='                     { ASSIGN }
    | "=="                    { EQ }
    | "!="                    { NEQ }
    | '>'                     { GT }
    | '<'                     { LT }
    | ">="                    { GE }
    | "<="                    { LE }
    | '+'                     { PLUS }
    | '-'                     { MINUS }
    | '*'                     { TIMES }
    | '/'                     { DIV }
    | '{'                     { LB  }
    | '}'                     { RB  }
    | '['                     { LS }
    | ']'                     { RS }
    | '('                     { LP  }
    | ')'                     { RP  }
    | ','                     { COMMA }
    | ';'                     { SEMI }
    | "//"                    { comment lexbuf; lexer lexbuf}
    | [' ' '\t']              { lexer lexbuf }(* eat up whitespace *) 
    | '\n'                    { next_line lexbuf; save_next_line lexbuf; lexer lexbuf }
    | eof                     { raise End_of_file }
    | _                       { raise No_such_symbol }
and comment = parse
    | '\n'                    { () }
    | eof                     { () }
    | _                       { comment lexbuf }
    