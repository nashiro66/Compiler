%{

open Printf
open Ast
open Count

%}

/* File parser.mly */
%token <int> NUM
%token <string> STR ID
%token INT IF WHILE SPRINT IPRINT SCAN EQ NEQ GT LT GE LE ELSE RETURN NEW
%token PLUS MINUS TIMES DIV LB RB LS RS LP RP ASSIGN SEMI COMMA TYPE VOID NEXTLINE
%type <Ast.stmt> prog


%nonassoc GT LT EQ NEQ GE LE
%left PLUS MINUS         /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS      /* highest precedence */


%start prog           /* the entry point */

%%

prog : stmt  {  $1  }
     ;

ty   : INT { IntTyp }
     | INT LS NUM RS { ArrayTyp ($3, IntTyp) }
     | ID	     { NameTyp $1 }
     ;

decs : decs dec { $1@$2 }
     |          { [] }
     ;

dec  : ty ids SEMI   { List.map (fun x -> VarDec ($1,x)) $2 }
     | TYPE ID ASSIGN ty SEMI { [TypeDec ($2,$4)] }
     | ty ID LP fargs_opt RP block  { [FuncDec($2, $4, $1, $6)] }
     | VOID ID LP fargs_opt RP block  { [FuncDec($2, $4, VoidTyp, $6)] }
     ; 

ids  : ids COMMA ID    { $1@[$3] }
     | ID              { [$1]  }
     ;

fargs_opt : /* empty */ { [] }
     | fargs            { $1 }
     ;
     
fargs: fargs COMMA ty ID     { $1@[($3,$4)] }
     | ty ID                 { [($1,$2)] }
     ;
next_line: NEXTLINE { Count.increment() }
     ;
     
stmts: stmts stmt  { $1@[$2] }
     | stmt        { [$1] }
     ;

stmt : ID ASSIGN expr SEMI    { Assign (Var $1, $3) }
     | ID LS expr RS ASSIGN expr SEMI  { Assign (IndexedVar (Var $1, $3), $6) }
     | IF LP cond RP stmt     { If ($3, $5, None) }
     | IF LP cond RP stmt ELSE stmt 
                              { If ($3, $5, Some $7) }
     | WHILE LP cond RP stmt  { While ($3, $5) }
     | SPRINT LP STR RP SEMI  { CallProc ("sprint", [StrExp $3]) }
     | IPRINT LP expr RP SEMI { CallProc ("iprint", [$3]) }
     | SCAN LP ID RP SEMI  { CallProc ("scan", [VarExp (Var $3)]) }
     | NEW LP ID RP SEMI   { CallProc ("new", [ VarExp (Var $3)]) }
     | ID LP aargs_opt RP SEMI  { CallProc ($1, $3) }
     | RETURN expr SEMI    { CallProc ("return", [$2]) }
     | block { $1 }
     | SEMI { NilStmt }
     | error ELSE { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:else"))}
     | error WHILE { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:while"))}
     | error SCAN { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:scan"))}
     | error SPRINT { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:sprint"))}
     | error IPRINT { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:iprint"))}
     | error INT { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:int"))}
     | error RETURN { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:return"))}
     | error TYPE { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:type"))}
     | error VOID { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:void"))}
     | error ID { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:"^ $2))}
     | error STR { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:"^ $2))}
     | error ASSIGN { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:="))}
     | error EQ { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:=="))}
     | error NEQ { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:!="))}
     | error GT { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:>"))}
     | error LT { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:<"))}
     | error GE { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:>="))}
     | error LE { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:<="))}
     | error PLUS { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:+"))}
     | error MINUS { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:-"))}
     | error TIMES { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:*"))}
     | error DIV { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:/"))}
     | error LB { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:{"))}
     | error RB { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:}"))}
     | error LS { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:["))}
     | error RS { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:]"))}
     | error LP { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:("))}
     | error RP { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:)"))}
     | error COMMA { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:,"))}
     | error SEMI { raise (Error("line" ^ string_of_int(!Count.line) ^ "error:;"))}
     ;

aargs_opt: /* empty */     { [] }
        | aargs            { $1 }
        ;

aargs : aargs COMMA expr  { $1@[$3] }
      | expr               { [$1] }
      ;

block: LB decs stmts RB  { Block ($2, $3) }
     ;

expr : NUM { IntExp $1  }
     | ID { VarExp (Var $1) }
     | ID LP aargs_opt RP { CallFunc ($1, $3) } 
     | ID LS expr RS  { VarExp (IndexedVar (Var $1, $3)) }
     | expr PLUS expr { CallFunc ("+", [$1; $3]) }
     | expr MINUS expr { CallFunc ("-", [$1; $3]) }
     | expr TIMES expr { CallFunc ("*", [$1; $3]) }
     | expr DIV expr { CallFunc ("/", [$1; $3]) }
     | MINUS expr %prec UMINUS { CallFunc("!", [$2]) }
     | LP expr RP  { $2 }
     ;

cond : expr EQ expr  { CallFunc ("==", [$1; $3]) }
     | expr NEQ expr { CallFunc ("!=", [$1; $3]) }
     | expr GT expr  { CallFunc (">", [$1; $3]) }
     | expr LT expr  { CallFunc ("<", [$1; $3]) }
     | expr GE expr  { CallFunc (">=", [$1; $3]) }
     | expr LE expr  { CallFunc ("<=", [$1; $3]) }
     ;
%%