exception Not_implemented
exception Not_ImplementedinA1

(*Type of expressions*)
type exptype = Tint | Tbool | Tfunc of (exptype * exptype)

(*Kind of expressions that can be evaluated*)
type expr =   V of string 
            | Lambda of (string * expr * exptype) 
            | App of (expr * expr) 
            | Plus of (expr * expr) 
            | Mult of (expr * expr) 
            | And of (expr * expr) 
            | Or of (expr * expr) 
            | Bool of bool 
            | Integer of int 
            | Cmp of expr 
            | If_Then_Else of (expr * expr * expr)



type opcode =   VAR of string
              | CLOS of (string * opcode list)
              | RET
              | APP
              | PLUS 
              | MULT 
              | AND 
              | OR 
              | BCONST of bool
              | NCONST of int 
              | CMP
              | IFTE 

exception NotFound
let rec lookup x l = match l with
                      [] -> raise NotFound
                    | hd::tl -> let (a,b) = hd in if (a=x) then b else (lookup x tl)

type closure_Table = (string * closure) list
and closure = Cls of (expr * closure_Table)

let rec findInList x l = match l with
                          [] -> false
                         | hd::tl -> if((fst hd) = (fst x)) then true else (findInList x tl)

(*Augments the list l1 on l2 *)
let rec augment l1 l2 = match l2 with
                         [] -> l1
                        |hd::tl -> if(not(findInList hd l1)) then (hd::(augment l1 tl)) else (augment l1 tl) 

let rec krivine focusClosure stk = let Cls(e, t) = focusClosure in
                                   match e with
                                    V(s) -> krivine (lookup s t) stk
                                  | Lambda(s,e1,ty) -> let a  = (List.hd stk) in (krivine (Cls(e1, (augment [(s,a)] t))) (List.tl stk))
                                  | App(e1,e2) -> krivine (Cls(e1,t)) (Cls(e2,t)::stk)
                                  | Plus(e1,e2) -> let Cls(Integer(a), t1) = krivine (Cls(e1,t)) stk in
                                                   let Cls(Integer(b), t2) = krivine (Cls(e2,t)) stk in
                                                   Cls(Integer(a+b),t)
                                  | Mult(e1,e2) -> let Cls(Integer(a), t1) = krivine (Cls(e1,t)) stk in
                                                   let Cls(Integer(b), t2) = krivine (Cls(e2,t)) stk in
                                                   Cls(Integer(a*b),t)
                                  | And(e1,e2)  -> let Cls(Bool(a), t1) = krivine (Cls(e1,t)) stk in
                                                   let Cls(Bool(b), t2) = krivine (Cls(e2,t)) stk in
                                                   Cls(Bool(a && b),t)
                                  | Or(e1,e2)   -> let Cls(Bool(a), t1) = krivine (Cls(e1,t)) stk in
                                                   let Cls(Bool(b), t2) = krivine (Cls(e2,t)) stk in
                                                   Cls(Bool(a || b),t)
                                  | Bool(b)     -> Cls(e,t)
                                  | Integer(i)  -> Cls(e,t)
                                  | Cmp(e)      -> let Cls(Integer(a), t1) = krivine (Cls(e,t)) stk in
                                                   Cls(Bool(a>0),t)
                                  | If_Then_Else(e1,e2,e3) -> let Cls(Bool(a), t1) = krivine (Cls(e1,t)) stk in
                                                              if a then (krivine (Cls(e2,t)) stk) else (krivine (Cls(e3,t)) stk)


let krivineMachine ex gamma = krivine Cls(ex, gamma) []

type Answer_Table  = (string * answer) list
type answer = Num of int | Bool of bool | Clos of ((string * opcode list) * Answer_Table)

let rec compile ex = match ex with
                      V(s) -> [VAR(s)]
                    | Lambda(s,e,t) -> [CLOS(s, compile e)@[RET]]
                    | App(e1,e2) -> (compile e1) @ (compile e2) @ [APP]
                    | Plus(e1,e2) -> (compile e1) @ (compile e2) @ [PLUS]
                    | Mult(e1,e2) -> (compile e1) @ (compile e2) @ [MULT]
                    | And(e1,e2) -> (compile e1) @ (compile e2) @ [AND]
                    | Or(e1,e2) -> (compile e1) @ (compile e2) @ [OR]
                    | Bool(b) -> [BCONST(b)]
                    | Integer(i) -> [NCONST(i)]
                    | Cmp(e) -> (compile e) @ [CMP]
                    | If_Then_Else(e1,e2,e3) -> (compile(e1)) @ (compile e2) @ (compile e3) @ [IFTE]

let rec secd s e c d = match c with
                        [] -> s
                      | hd::tl -> (match hd with
                                    VAR(x) -> 
                                  | CLOS(x,c_prime) ->
                                  | 
                                  )

let secdMachine ex gamma = secd [] gamma (compile ex) []
