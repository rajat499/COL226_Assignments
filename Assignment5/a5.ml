exception Not_implemented
exception Not_ImplementedinA1

(*Type of expressions*)
type exptype = Tint | Tbool | Tfunc of (exptype * exptype)

(*Kind of expressions that can be evaluated*)
type expr =   V of string 
            | Lambda of (string * expr) 
            | App of (expr * expr) 
            | Plus of (expr * expr) 
            | Mult of (expr * expr) 
            | Sub of (expr * expr) 
            | Div of (expr * expr) 
            | Rem of (expr * expr)
            | Abs of expr
            | Negative of expr 
            | And of (expr * expr)
            | Or of (expr * expr)
            | Not of expr 
            | Bool of bool 
            | Integer of int 
            | Cmp of expr 
            | If_Then_Else of (expr * expr * expr)
            | InParen of expr
            | Equals of (expr * expr)
            | GreaterTE of expr * expr   (* >= *)
            | LessTE of expr * expr      (* <= *)
            | GreaterT of expr * expr    (* > *)
            | LessT of expr * expr       (* < *)


type opcode =   VAR of string
              | CLOS of (string * opcode list)
              | RET
              | APP
              | PLUS 
              | MULT 
              | SUB
              | DIV
              | REM
              | ABS
              | NEGA
              | AND 
              | OR 
              | NOT
              | BCONST of bool
              | NCONST of int 
              | CMP
              | EQ
              | GTE
              | GT
              | LTE
              | LT
              | COND of ((opcode list) * (opcode list))

exception NotFound
let rec lookup x l = match l with
                      [] -> raise NotFound
                    | hd::tl -> let (a,b) = hd in if (a=x) then b else (lookup x tl)

type answer = Num of int | BoolVal of bool | Func of (string * opcode list)

type closure_Table = (string * closure) list
and closure = Cls of (expr * closure_Table) | VCls of (answer * closure_Table)

type answer_Table = (string * answer_Closure) list
and  answer_Closure = ACls of (answer * answer_Table) 

type stack_elem  = PLUS of closure | MULT of closure | SUB of closure | DIV of closure | REM of closure
                  | ABS | NEGA | NOT 
                  | EQ of closure | GTE of closure | LTE of closure | GT of closure | LT of closure
                  | AND of closure | OR of closure | CMP | IFTE of (closure * closure) | APP of closure

type dump = Dump of ((answer_Closure list) * answer_Table * (opcode list))

let rec findInList x l = match l with
                          [] -> false
                         | hd::tl -> if((fst hd) = (fst x)) then true else (findInList x tl)

(*Augments the list l1 on l2 *)
let rec augment l1 l2 = match l2 with
                         [] -> l1
                        |hd::tl -> if(not(findInList hd l1)) then (hd::(augment l1 tl)) else (augment l1 tl) 

exception BadStack
let rec krivine focusClosure stk = match focusClosure with
                                  Cls(e,t) -> (
                                    match e with
                                      V(s) -> (krivine (lookup s t) stk)
                                    | Integer(i) -> (krivine (VCls(Num(i), t)) stk)
                                    | Bool(b) -> (krivine (VCls(BoolVal(b), t)) stk)
                                    | Cmp(e1) -> (krivine (Cls(e1, t)) (CMP::stk))
                                    | Plus(e1, e2) -> (krivine (Cls(e1,t)) (PLUS(Cls(e2,t))::stk))
                                    | Mult(e1, e2) -> (krivine (Cls(e1,t)) (MULT(Cls(e2,t))::stk))
                                    | Sub(e1, e2) -> (krivine (Cls(e1,t)) (SUB(Cls(e2,t))::stk))
                                    | Div(e1, e2) -> (krivine (Cls(e1,t)) (DIV(Cls(e2,t))::stk))
                                    | Rem(e1, e2) -> (krivine (Cls(e1,t)) (REM(Cls(e2,t))::stk))
                                    | Abs(e1) -> (krivine (Cls(e1, t)) (ABS::stk))
                                    | Negative(e1) -> (krivine (Cls(e1, t)) (NEGA::stk))
                                    | Not(e1) -> (krivine (Cls(e1, t)) (NOT::stk))
                                    | And(e1, e2) -> (krivine (Cls(e1,t)) (AND(Cls(e2,t))::stk))
                                    | Or(e1, e2) -> (krivine (Cls(e1,t)) (OR(Cls(e2,t))::stk))
                                    | InParen(e1) -> (krivine (Cls(e1, t)) (stk))
                                    | Equals(e1, e2) -> (krivine (Cls(e1,t)) (EQ(Cls(e2,t))::stk))
                                    | GreaterTE(e1, e2) -> (krivine (Cls(e1,t)) (GTE(Cls(e2,t))::stk))
                                    | LessTE(e1, e2) -> (krivine (Cls(e1,t)) (LTE(Cls(e2,t))::stk))
                                    | GreaterT(e1, e2) -> (krivine (Cls(e1,t)) (GT(Cls(e2,t))::stk))
                                    | LessT(e1, e2) -> (krivine (Cls(e1,t)) (LT(Cls(e2,t))::stk))
                                    | If_Then_Else(e1, e2, e3) -> (krivine (Cls(e1,t)) (IFTE(Cls(e2,t), Cls(e3, t))::stk))
                                    | App(e1, e2) -> (krivine (Cls(e1, t)) (APP(Cls(e2,t))::stk))
                                    | Lambda(s,e1) -> let APP(cl) = (List.hd stk) in (krivine (Cls(e1, augment [(s,cl)] t)) (List.tl stk))
                                  )
                                | VCls(a,t) -> (
                                    match a with
                                      Num(x) -> (
                                        match stk with
                                          [] -> focusClosure
                                        | hd :: tl -> (match hd with
                                                        PLUS(Cls(e,t_dash)) -> (krivine (Cls(e,t_dash)) (PLUS(VCls(a,t))::tl)) 
                                                      | PLUS(VCls(Num(x1),t_dash)) -> (krivine (VCls(Num(x+x1), t)) tl)
                                                      | MULT(Cls(e,t_dash)) -> (krivine (Cls(e,t_dash)) (MULT(VCls(a,t))::tl)) 
                                                      | MULT(VCls(Num(x1),t_dash)) -> (krivine (VCls(Num(x*x1), t)) tl)
                                                      | SUB(Cls(e,t_dash)) -> (krivine (Cls(e,t_dash)) (SUB(VCls(a,t))::tl)) 
                                                      | SUB(VCls(Num(x1),t_dash)) -> (krivine (VCls(Num(x1-x), t)) tl)
                                                      | DIV(Cls(e,t_dash)) -> (krivine (Cls(e,t_dash)) (DIV(VCls(a,t))::tl)) 
                                                      | DIV(VCls(Num(x1),t_dash)) -> (krivine (VCls(Num(x1/x), t)) tl)
                                                      | REM(Cls(e,t_dash)) -> (krivine (Cls(e,t_dash)) (REM(VCls(a,t))::tl)) 
                                                      | REM(VCls(Num(x1),t_dash)) -> (krivine (VCls(Num(x1 mod x), t)) tl)
                                                      | CMP -> (krivine (VCls(BoolVal(x>0), t)) tl)
                                                      | ABS -> (krivine (VCls(Num(Pervasives.abs x), t)) tl)
                                                      | NEGA -> (krivine (VCls(Num((-1)*x), t)) tl)
                                                      | EQ(Cls(e,t_dash)) -> (krivine (Cls(e,t_dash)) (EQ(VCls(a,t))::tl)) 
                                                      | EQ(VCls(Num(x1),t_dash)) -> (krivine (VCls(BoolVal(x = x1), t)) tl)
                                                      | GTE(Cls(e,t_dash)) -> (krivine (Cls(e,t_dash)) (GTE(VCls(a,t))::tl)) 
                                                      | GTE(VCls(Num(x1),t_dash)) -> (krivine (VCls(BoolVal(x1>=x), t)) tl)
                                                      | LTE(Cls(e,t_dash)) -> (krivine (Cls(e,t_dash)) (LTE(VCls(a,t))::tl)) 
                                                      | LTE(VCls(Num(x1),t_dash)) -> (krivine (VCls(BoolVal(x1<=x), t)) tl)
                                                      | GT(Cls(e,t_dash)) -> (krivine (Cls(e,t_dash)) (GT(VCls(a,t))::tl)) 
                                                      | GT(VCls(Num(x1),t_dash)) -> (krivine (VCls(BoolVal(x1 > x), t)) tl)
                                                      | LT(Cls(e,t_dash)) -> (krivine (Cls(e,t_dash)) (LT(VCls(a,t))::tl)) 
                                                      | LT(VCls(Num(x1),t_dash)) -> (krivine (VCls(BoolVal(x1 < x), t)) tl)
                                                      | _ -> raise BadStack
                                                      )
                                      )
                                    | BoolVal(x) -> (
                                        match stk with
                                        [] -> focusClosure
                                      | hd::tl -> (match hd with
                                                    AND(Cls(e,t_dash)) -> (krivine (Cls(e,t_dash)) (AND(VCls(a,t))::tl)) 
                                                  | AND(VCls(BoolVal(x1), t_dash)) -> (krivine (VCls(BoolVal(x && x1), t)) tl) 
                                                  | OR(Cls(e,t_dash)) -> (krivine (Cls(e,t_dash)) (OR(VCls(a,t))::tl)) 
                                                  | OR(VCls(BoolVal(x1), t_dash)) -> (krivine (VCls(BoolVal(x || x1), t)) tl) 
                                                  | NOT -> (krivine (VCls(BoolVal(Pervasives.not x), t)) tl)
                                                  | IFTE(cl1, cl2) -> if x then (krivine cl1 tl) else (krivine cl2 tl)
                                                  | _ -> raise BadStack
                                                  )
                                    )
                                    | _ -> raise BadStack
                                )

let callKrivine ex gamma = krivine (Cls(ex, gamma)) []

let rec compile ex = match ex with
                      V(s) -> [VAR(s)]
                    | Lambda(s,e) -> [CLOS(s, (compile e)@[RET])]
                    | App(e1,e2) -> (compile e1) @ (compile e2) @ [APP]
                    | Plus(e1,e2) -> (compile e1) @ (compile e2) @ [PLUS]
                    | Mult(e1,e2) -> (compile e1) @ (compile e2) @ [MULT]
                    | Sub(e1,e2) -> (compile e1) @ (compile e2) @ [SUB]
                    | Div(e1,e2) -> (compile e1) @ (compile e2) @ [DIV]
                    | Rem(e1,e2) -> (compile e1) @ (compile e2) @ [REM]
                    | Equals(e1,e2) -> (compile e1) @ (compile e2) @ [EQ]
                    | GreaterTE(e1,e2) -> (compile e1) @ (compile e2) @ [GTE]
                    | LessTE(e1,e2) -> (compile e1) @ (compile e2) @ [LTE]
                    | GreaterT(e1,e2) -> (compile e1) @ (compile e2) @ [GT]
                    | LessT(e1,e2) -> (compile e1) @ (compile e2) @ [LT]
                    | Abs(e) -> (compile e) @ [ABS]
                    | Negative(e) -> (compile e) @ [NEGA]
                    | Or(e1,e2) -> (compile e1) @ (compile e2) @ [OR]
                    | And(e1,e2) -> (compile e1) @ (compile e2) @ [AND]
                    | Not(e) -> (compile e) @ [NOT]
                    | Bool(b) -> [BCONST(b)]
                    | Integer(i) -> [NCONST(i)]
                    | Cmp(e) -> (compile e) @ [CMP]
                    | If_Then_Else(e1,e2,e3) -> (compile(e1)) @ [COND(compile e2, compile e3)]
                    | InParen(e) -> (compile e)


exception EmptyStackCan'tPop;;
exception EmptyStackNothingToPeek;;

(*Pops head of the stack and returns the remaining stack.*)
let pop_stack list = match list with 
                [] -> raise EmptyStackCan'tPop
              | hd :: tl -> tl
              ;;

(*Returns the head/top of the stack.*)
let peek_stack list = match list with
                [] -> raise EmptyStackNothingToPeek
              | hd :: tl -> hd
              ;;
(*Pushes the element on top of the stack.*)
let push_stack element list = element :: list ;;
                    
let rec secd s e c d = match c with
                        [] -> let ACls(a, gamma) = (List.hd s) in ACls(a, e)
                      | hd::tl -> (match hd with
                                    VAR(x) -> secd ((lookup x e)::s) e tl d
                                  | CLOS(x,c_prime) -> secd (ACls(Func(x, c_prime), e)::s) e tl d
                                  | APP ->  let s_prime = (pop_stack s) in
                                            let a = peek_stack s in
                                            let ACls(Func(x,c_prime), gamma) = peek_stack s_prime in
                                            secd [] (augment [(x,a)] gamma) c_prime (Dump(pop_stack s_prime , e, tl)::d)
                                  | RET ->  let a = peek_stack s in
                                            let Dump(stk, gamma, c1) = peek_stack d in
                                            secd (a::stk) gamma c1 (pop_stack d)
                                  | PLUS -> let s_prime = (pop_stack s) in
                                            let ACls(Num(x1),g1) = peek_stack s in
                                            let ACls(Num(x2),g2) = peek_stack s_prime in
                                            secd (ACls(Num(x1+x2), e)::(pop_stack s_prime)) e tl d
                                  | MULT -> let s_prime = (pop_stack s) in
                                            let ACls(Num(x1),g1) = peek_stack s in
                                            let ACls(Num(x2),g2) = peek_stack s_prime in
                                            secd (ACls(Num(x1*x2), e)::(pop_stack s_prime)) e tl d
                                  | SUB  -> let s_prime = (pop_stack s) in
                                            let ACls(Num(x1),g1) = peek_stack s in
                                            let ACls(Num(x2),g2) = peek_stack s_prime in
                                            secd (ACls(Num(x2-x1), e)::(pop_stack s_prime)) e tl d
                                  | DIV  -> let s_prime = (pop_stack s) in
                                            let ACls(Num(x1),g1) = peek_stack s in
                                            let ACls(Num(x2),g2) = peek_stack s_prime in
                                            secd (ACls(Num(x2/x1), e)::(pop_stack s_prime)) e tl d
                                  | REM  -> let s_prime = (pop_stack s) in
                                            let ACls(Num(x1),g1) = peek_stack s in
                                            let ACls(Num(x2),g2) = peek_stack s_prime in
                                            secd (ACls(Num(x2 mod x1), e)::(pop_stack s_prime)) e tl d
                                  | ABS  -> let ACls(Num(x1),g1) = peek_stack s in
                                            secd (ACls(Num(Pervasives.abs x1), e)::(pop_stack s)) e tl d
                                  | NEGA -> let ACls(Num(x1),g1) = peek_stack s in
                                          secd (ACls(Num((-1)*x1), e)::(pop_stack s)) e tl d
                                  | AND  -> let s_prime = (pop_stack s) in
                                            let ACls(BoolVal(x1),g1) = peek_stack s in
                                            let ACls(BoolVal(x2),g2) = peek_stack s_prime in
                                            secd (ACls(BoolVal(x1 && x2), e)::(pop_stack s_prime)) e tl d
                                  | OR   -> let s_prime = (pop_stack s) in
                                            let ACls(BoolVal(x1),g1) = peek_stack s in
                                            let ACls(BoolVal(x2),g2) = peek_stack s_prime in
                                            secd (ACls(BoolVal(x1 || x2), e)::(pop_stack s_prime)) e tl d
                                  | NOT ->  let ACls(BoolVal(x1),g1) = peek_stack s in
                                            secd (ACls(BoolVal(Pervasives.not x1), e)::(pop_stack s)) e tl d
                                  | BCONST(x) -> secd (ACls(BoolVal(x), e)::s) e tl d
                                  | NCONST(x) -> secd (ACls(Num(x), e)::s) e tl d
                                  | CMP -> let ACls(Num(x1),g1) = peek_stack s in
                                           secd (ACls(BoolVal(x1>0), e)::(pop_stack s)) e tl d
                                  | COND(c2, c3) -> let ACls(BoolVal(x1),g1) = peek_stack s in
                                                    if x1 then (secd (pop_stack s) e (c2@tl) d)
                                                    else  (secd (pop_stack s) e (c3@tl) d)
                                  | EQ   -> let s_prime = (pop_stack s) in
                                            let ACls(Num(x1),g1) = peek_stack s in
                                            let ACls(Num(x2),g2) = peek_stack s_prime in
                                            secd (ACls(BoolVal(x1=x2), e)::(pop_stack s_prime)) e tl d
                                  | GTE  -> let s_prime = (pop_stack s) in
                                            let ACls(Num(x1),g1) = peek_stack s in
                                            let ACls(Num(x2),g2) = peek_stack s_prime in
                                            secd (ACls(BoolVal(x2>=x1), e)::(pop_stack s_prime)) e tl d
                                  | LTE  -> let s_prime = (pop_stack s) in
                                            let ACls(Num(x1),g1) = peek_stack s in
                                            let ACls(Num(x2),g2) = peek_stack s_prime in
                                            secd (ACls(BoolVal(x2<=x1), e)::(pop_stack s_prime)) e tl d
                                  | GT   -> let s_prime = (pop_stack s) in
                                            let ACls(Num(x1),g1) = peek_stack s in
                                            let ACls(Num(x2),g2) = peek_stack s_prime in
                                            secd (ACls(BoolVal(x2>x1), e)::(pop_stack s_prime)) e tl d
                                  | LT   -> let s_prime = (pop_stack s) in
                                            let ACls(Num(x1),g1) = peek_stack s in
                                            let ACls(Num(x2),g2) = peek_stack s_prime in
                                            secd (ACls(BoolVal(x2<x1), e)::(pop_stack s_prime)) e tl d
                                )

let callSECD  ex gamma = secd [] gamma (compile ex) []



(* let p1 =  App (Lambda ("x", Mult (Integer 3, V "x"), Tint), Integer 4);;
let p2 = If_Then_Else
   (Cmp (Integer 7),
    App (Lambda ("x", Plus (Integer 3, V "x"), Tint), Integer 31), 
    Integer 0);;
let p3 = If_Then_Else
    (Cmp (Integer 0),
    App (Lambda ("x", Plus (Integer 3, V "x"), Tint), Integer 4),
        Abs(Negative(Integer 110)));; *)