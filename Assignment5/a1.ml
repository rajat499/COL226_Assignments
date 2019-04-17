(* Dummy implementation of A1 *)
(*open A0*)
exception Not_implemented
exception IllFormedStack
exception IllFormedDump
exception Error

(* The possible types of expressions in the language of expressions *)
type exptype = Tint | Tbool | Tfunc of (exptype * exptype)
(* abstract syntax *)
type expr = V of string 
           | Lambda of (string * expr * exptype) 
           | App of (expr * expr) 
           | Plus of (expr * expr) 
           | Mult of (expr * expr) 
           | And of (expr * expr) 
           | Or of (expr * expr) 
           | Bool of bool 
           | Integer of int 
           | Cmp of expr 
           | If_Then_Else of (expr * expr * expr);;


(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = VAR of string | NCONST of int | BCONST of bool
              | PLUS | MULT | APP | AND | OR | IFTE
              | RET | CLOS of (string * opcode list) |CMP
                


(* The language should contain the following types of expressions:  integers and booleans *)
type answer = Num of int | Bool of bool | Clos of ((string* opcode list)* table2)
and table2 = (string * answer) list

type closure = Cls of (expr * table)
and table = (string * closure) list

let rec lookup l x=   if l=[] then raise Not_implemented
                       else
                        (let (a,b)=List.hd l in
                         if a=x then b
                         else lookup (List.tl l) x)
let rec compile ex = match ex with V(x)-> [VAR(x)]
                               | Bool(x)-> [BCONST(x)]
                               | Integer(x)-> [NCONST(x)]
                               | Plus(e1,e2)-> compile(e1)@compile(e2)@[PLUS]
                               | Mult(e1,e2)-> compile(e1)@compile(e2)@[MULT]
                               | And(e1,e2)-> compile(e1)@compile(e2)@[AND]
                               | Or(e1,e2)-> compile(e1)@compile(e2)@[OR]
                               | Cmp(e1)-> compile(e1)@[CMP]
                               | If_Then_Else(e1,e2,e3)->compile(e1)@compile(e2)@compile(e3)@[IFTE]
                               | Lambda(x,e1,y)->[CLOS(x,compile(e1)@[RET])]
                               | App(e1,e2) -> compile(e1)@compile(e2)@[APP]

let rec secdmc ex gamma = let rec secd s e c d=match c with []-> s
                                         |VAR(x)::l-> (secd ((lookup e x)::s) e l d)
                                         |NCONST(i)::l->(secd (Num(i)::s) e l d)
                                         |BCONST(x)::l->(secd (Bool(x)::s) e l d)
                                         |PLUS::l->(match s with (y1::(y2::s2))->let Num(a1)=y1 and Num(a2)=y2 in
                                                       (secd ((Num(a1+a2))::s2) e l d)
                                                    | _->raise IllFormedStack)
                                         |MULT::l->(match s with (y1::(y2::s2))->let Num(a1)=y1 and Num(a2)=y2 in
                                                    (secd ((Num(a1*a2))::s2) e l d)
                                                 | _->raise IllFormedStack)
                                         |AND::l->(match s with (y1::(y2::s2))->let Bool(a1)=y1 and Bool(a2)=y2 in
                                                 (secd ((Bool(a1 & a2))::s2) e l d) 
                                              | _->raise IllFormedStack)
                                         |OR::l->(match s with (y1::(y2::s2))->let Bool(a1)=y1 and Bool(a2)=y2 in
                                              (secd ((Bool(a1 or a2))::s2) e l d)
                                           | _->raise IllFormedStack)
                                         |CMP::l->(match s with (y1::s2)->let Num(a1)=y1 in
                                           (secd ((Bool(a1 >0))::s2) e l d)
                                                 | _->raise IllFormedStack)

                                         |IFTE::l->(match s with (y1::(y2::(y3::s2)))->let Bool(a1)=y3 in 
                                                 if a1 then (secd ((y2)::s2) e l d) else
                                                            (secd ((y1)::s2) e l d)
                                             |_->raise IllFormedStack)
                                         |CLOS(x,c1)::l-> secd (Clos((x,c1),e)::s) e l d
                                         |APP::l-> (match s with (y1::(Clos((x,c1),e1)::s2))-> (secd [] ((x,y1)::e1) c1 ((s2,e,l)::d))                                            
                                                        | _->raise IllFormedStack)
                                         |RET::l-> if (l==[]) then
                                                    (match d with (s1,e1,c1)::d1-> (match s with y1::s2-> secd (y1::s1) e1 c1 d1
                                                                                  |_-> raise IllFormedStack)                                            
                                                   | _->raise IllFormedDump)
                                                    else raise Not_implemented 
                                     in secd [] gamma ex []

let rec krivinemc ex gamma = let rec krivine fcl s= 
                                                    match fcl with Cls(e,t)->( match e with V(x)-> krivine (lookup t x) s
                                                                               |Integer(i)-> fcl
                                                                               |Bool(b)->   fcl
                                                                               |Plus(e1,e2)-> let cl1= krivine (Cls(e1,t)) s and cl2= krivine (Cls(e2,t)) s in
                                                                                               let Cls(Integer(i1),t1)=cl1 and Cls(Integer(i2),t2)=cl2 in
                                                                                                   Cls(Integer(i1+i2),t) 
                                                                               |Mult(e1,e2)-> let cl1= krivine (Cls(e1,t)) s and cl2= krivine (Cls(e2,t)) s in
                                                                                               let Cls(Integer(i1),t1)=cl1 and Cls(Integer(i2),t2)=cl2 in
                                                                                                  Cls(Integer(i1*i2),t)
                                                                               |And(e1,e2)-> let cl1= krivine (Cls(e1,t)) s and cl2= krivine (Cls(e2,t)) s in
                                                                                                    let Cls(Bool(i1),t1)=cl1 and Cls(Bool(i2),t2)=cl2 in
                                                                                                        Cls(Bool(i1 & i2),t) 
                                                                              |Or(e1,e2)-> let cl1= krivine (Cls(e1,t)) s and cl2= krivine (Cls(e2,t)) s in
                                                                                                     let Cls(Bool(i1),t1)=cl1 and Cls(Bool(i2),t2)=cl2 in
                                                                                                      Cls(Bool(i1  or i2),t) 
                                                                              |Cmp(e1)-> let cl1= krivine (Cls(e1,t)) s in
                                                                                              let Cls(Integer(i1),t1)=cl1 in
                                                                                              Cls(Bool(i1>0),t) 
                                                                              |If_Then_Else(e1,e2,e3)-> let cl1= krivine (Cls(e1,t)) s in
                                                                                                           let Cls(Bool(i1),t1)=cl1 in
                                                                                                           if(i1) then krivine (Cls(e2,t)) s else
                                                                                                                       krivine (Cls(e3,t)) s
                                                                               |App(e1,e2)-> krivine (Cls(e1,t)) (Cls(e2,t)::s)
                                                                               |Lambda(x,e1,y)-> (match s with a::s2-> krivine (Cls(e1,(x,a)::t)) s2
                                                                                                                 |_-> raise IllFormedStack
                                                                                                           )
                                                                                ) 
                                                                                in krivine (Cls(ex,gamma)) [] 

                                                                              
