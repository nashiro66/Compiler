type id = string
type stmt = Stmts of stmt * stmt
         | Assign of id * exp
         | Print of exp
and exp = Id of id
        | Num of int
        | Plus of exp * exp
        | Minus of exp * exp
        | Times of exp * exp
        | Div of exp * exp
        | StmtExp of stmt * exp

exception No_such_symbol
let e0 = fun _ -> raise No_such_symbol
let update var vl env = fun v -> if v = var then vl else env v
let dummy = Stmts(Print(Id "dummy"), Print(Id "dummy"))

let rec trans_stmt ast env = 
      match ast with
         Stmts (s1,s2) -> let s, env' = trans_stmt s1 env in
                              let s', env'' = trans_stmt s env' in
                                            trans_stmt s2 env'' 
       | Assign (var,e) -> let s, vl = trans_exp e env in
                                       s, update var vl env
       | Print e -> let s, vl = trans_exp e env in 
                         s, (print_int vl; print_string "\n"; env)
and trans_exp ast env =
      match ast with
         Id v -> dummy, env v
       | Num n -> dummy, n
       | Plus (e1,e2) -> let s, vl1 = trans_exp e1 env in
                            let s, vl2 = trans_exp e2 env in
                                    dummy, vl1 + vl2
       | Minus (e1,e2) -> let s, vl1 = trans_exp e1 env in
                            let s, vl2 = trans_exp e2 env in
                                    dummy, vl1 - vl2
       | Times (e1,e2) -> let s, vl1 = trans_exp e1 env in
                            let s, vl2 = trans_exp e2 env in
                                    dummy, vl1 * vl2
       | Div (e1,e2) -> let s, vl1 = trans_exp e1 env in
                            let s, vl2 = trans_exp e2 env in
                                    dummy, vl1 / vl2
       | StmtExp (s, e) -> let s1, env' = trans_stmt s env in
                            let s2, vl = trans_exp e env' in
                                    s, vl 

let prog = Stmts (Assign ("x",Plus (Num 1,Times (Num 2,Num 3))),
              Stmts (Assign ("y",Div (Id "x",Num 4)), Print (Id "y")))
let prog1 = Stmts (Assign ("y",
              StmtExp (Assign ("x", Plus (Num 1, Num 2)), Id "x")), Print (Id "y")) 
let prog2 = Stmts (Assign ("y",
              StmtExp (Assign ("x", Plus (Num 1, Num 2)), Id "x")), Print (Id "x")) ;;
let interp ast = trans_stmt ast e0
