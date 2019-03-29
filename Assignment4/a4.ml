open A1
exception Not_implemented
exception TypeOfVariableNotFound
exception BadType
exception IllegalList

(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool *)
(* let rec hastype g e t = raise Not_implemented *)

let rec giveType g x = match g with
                        [] -> raise TypeOfVariableNotFound
                      | hd :: tl -> let (a,b) = hd in
                                    (if (a=x) then b else
                                      (giveType tl x));;

(*Gives ith member of a list. Head of the list is first member*)
let rec get_ith list i = if(i<0) then (raise IllegalList) else
                              ( match i with
                               0 -> raise IllegalList
                              |1 -> List.hd list
                              |_ -> get_ith (List.tl list) (i-1))

(* let rec getType g e =  let rec tupleType t = (match t with [] -> []
                                                            | hd:: tl -> (getType g hd):: (tupleType tl)) in
                      (match e with
                        Var(x) -> (giveType g x)
                      | N(i)   -> Tint
                      | B(b)   -> Tbool
                      | Abs(t) -> getType g t
                      | Negative(t) -> getType g t
                      | Not(t)      -> getType g t
                      | Add(t1, t2) -> if (((getType g t1) = Tint) &&  ((getType g t2) = Tint)) then Tint else raise BadType
                      | Sub(t1, t2) -> if (((getType g t1) = Tint) &&  ((getType g t2) = Tint)) then Tint else raise BadType
                      | Mult(t1, t2)-> if (((getType g t1) = Tint) &&  ((getType g t2) = Tint)) then Tint else raise BadType
                      | Div(t1, t2) -> if (((getType g t1) = Tint) &&  ((getType g t2) = Tint)) then Tint else raise BadType
                      | Rem(t1, t2) -> if (((getType g t1) = Tint) &&  ((getType g t2) = Tint)) then Tint else raise BadType
                      | Conjunction(t1, t2) -> if (((getType g t1) = Tbool) &&  ((getType g t2) = Tbool)) then Tbool else raise BadType
                      | Disjunction(t1, t2) -> if (((getType g t1) = Tbool) &&  ((getType g t2) = Tbool)) then Tbool else raise BadType
                      | Equals(t1, t2)      -> if (((getType g t1) = Tint) &&  ((getType g t2) = Tint)) then Tbool else raise BadType
                      | GreaterTE(t1, t2)   -> if (((getType g t1) = Tint) &&  ((getType g t2) = Tint)) then Tbool else raise BadType
                      | LessT(t1, t2)       -> if (((getType g t1) = Tint) &&  ((getType g t2) = Tint)) then Tbool else raise BadType
                      | GreaterT(t1, t2)    -> if (((getType g t1) = Tint) &&  ((getType g t2) = Tint)) then Tbool else raise BadType
                      | LessTE(t1, t2)      -> if (((getType g t1) = Tint) &&  ((getType g t2) = Tint)) then Tbool else raise BadType
                      | InParen(t)          -> (getType g t)
                      | IfThenElse(t1,t2,t3)-> let ty = (getType g t2) in if (((getType g t1) = Tbool) &&  ((getType g t3) = ty)) then ty else raise BadType
                      | Tuple(n, x) -> Ttuple(tupleType x)
                      | Project((i,n), x) -> let Tuple(a,b) = x in 
                                             let l = (tupleType b) in                                                         
                                              if(n= (List.length l)) then (get_ith l i) else raise BadType
                      | FunctionAbstraction(s, x) ->  let ty  = (giveType g s) in Tfunc(ty, getType g x)
                      | FunctionCall (t1, t2) -> let x = (getType g t2) in
                                                 let Tfunc(y,z) = (getType g t1) in
                                                 if(y = x) then z else raise BadType 
                      | Let(d,x) ->let g_dash = (getYield g d) in (getType (g_dash::g) x)
                      ) *)

let rec getYield g d = match d with
                     Simple(s,e) -> [(s, (getType g e))]
                    |Parallel(l) -> (match l with 
                                    [] ->[]
                                    |hd::tl -> let a = (getYield g hd) in ((getYield g (Parallel(tl))) @ a)  
                                    )
                    |Sequence(l) -> (match l with 
                                    [] ->[]
                                   |hd::tl -> let a = (getYield g hd) in ((getYield (a@g) (Sequence(tl))) @ a) 
                                    )
                   |Local(d1,d2)-> getYield ((getYield g d1) @ g) d2

and getType g e =  let rec tupleType t = (match t with [] -> []
                                          | hd:: tl -> (getType g hd):: (tupleType tl)) in
                                          (match e with
                                          Var(x) -> (giveType g x)
                                          | N(i)   -> Tint
                                          | B(b)   -> Tbool
                                          | Abs(t) -> getType g t
                                          | Negative(t) -> getType g t
                                          | Not(t)      -> getType g t
                                          | Add(t1, t2) -> if (((getType g t1) = Tint) &&  ((getType g t2) = Tint)) then Tint else raise BadType
                                          | Sub(t1, t2) -> if (((getType g t1) = Tint) &&  ((getType g t2) = Tint)) then Tint else raise BadType
                                          | Mult(t1, t2)-> if (((getType g t1) = Tint) &&  ((getType g t2) = Tint)) then Tint else raise BadType
                                          | Div(t1, t2) -> if (((getType g t1) = Tint) &&  ((getType g t2) = Tint)) then Tint else raise BadType
                                          | Rem(t1, t2) -> if (((getType g t1) = Tint) &&  ((getType g t2) = Tint)) then Tint else raise BadType
                                          | Conjunction(t1, t2) -> if (((getType g t1) = Tbool) &&  ((getType g t2) = Tbool)) then Tbool else raise BadType
                                          | Disjunction(t1, t2) -> if (((getType g t1) = Tbool) &&  ((getType g t2) = Tbool)) then Tbool else raise BadType
                                          | Equals(t1, t2)      -> if (((getType g t1) = Tint) &&  ((getType g t2) = Tint)) then Tbool else raise BadType
                                          | GreaterTE(t1, t2)   -> if (((getType g t1) = Tint) &&  ((getType g t2) = Tint)) then Tbool else raise BadType
                                          | LessT(t1, t2)       -> if (((getType g t1) = Tint) &&  ((getType g t2) = Tint)) then Tbool else raise BadType
                                          | GreaterT(t1, t2)    -> if (((getType g t1) = Tint) &&  ((getType g t2) = Tint)) then Tbool else raise BadType
                                          | LessTE(t1, t2)      -> if (((getType g t1) = Tint) &&  ((getType g t2) = Tint)) then Tbool else raise BadType
                                          | InParen(t)          -> (getType g t)
                                          | IfThenElse(t1,t2,t3)-> let ty = (getType g t2) in if (((getType g t1) = Tbool) &&  ((getType g t3) = ty)) then ty else raise BadType
                                          | Tuple(n, x) -> Ttuple(tupleType x)
                                          | Project((i,n), x) -> let Tuple(a,b) = x in 
                                          let l = (tupleType b) in                                                         
                                          if(n= (List.length l)) then (get_ith l i) else raise BadType
                                          | FunctionAbstraction(s, x) ->  let ty  = (giveType g s) in Tfunc(ty, getType g x)
                                          | FunctionCall (t1, t2) -> let x = (getType g t2) in
                                          let Tfunc(y,z) = (getType g t1) in
                                          if(y = x) then z else raise BadType 
                                          | Let(d,x) ->let g_dash = (getYield g d) in (getType (g_dash @ g) x)
                                          )
                   
(*                       
let rec hastype g e t =   try
                          ( match e with
                            Var(x) -> if ((giveType g x) = t) then true else false
                          | N(i)   -> if t = Tint then true else false
                          | B(b)   -> if t = Tbool then true else false
                          | Abs(x) -> (hasType g x Tint) && (t = Tint)
                          | Negative(x) -> (hasType g x Tint) && (t = Tint)
                          | Not(x) -> (hasType g x Tbool) && (t = Tbool)
                          | Add(x1, x2) -> (hasType g x1 Tint) && (hasType g x2 Tint) && (t = Tint)
                          | Sub(x1, x2) -> (hasType g x1 Tint) && (hasType g x2 Tint) && (t = Tint)
                          | Mult(x1, x2)-> (hasType g x1 Tint) && (hasType g x2 Tint) && (t = Tint)
                          | Div(x1, x2) -> (hasType g x1 Tint) && (hasType g x2 Tint) && (t = Tint)
                          | Rem(x1, x2) -> (hasType g x1 Tint) && (hasType g x2 Tint) && (t = Tint)
                          | Conjunction(x1, x2) -> (hasType g x1 Tbool) && (hasType g x2 Tbool) && (t = Tbool)
                          | Disjunction(x1, x2) -> (hasType g x1 Tbool) && (hasType g x2 Tbool) && (t = Tbool)
                          | Equals(x1, x2)      -> (hasType g x1 Tint) && (hasType g x2 Tint) && (t = Tbool)
                          | GreaterTE(x1, x2)   -> (hasType g x1 Tint) && (hasType g x2 Tint) && (t = Tbool)
                          | LessTE(x1, x2)      -> (hasType g x1 Tint) && (hasType g x2 Tint) && (t = Tbool)
                          | GreaterT(x1, x2)    -> (hasType g x1 Tint) && (hasType g x2 Tint) && (t = Tbool) 
                          | LessT(x1, x2)       -> (hasType g x1 Tint) && (hasType g x2 Tint) && (t = Tbool)
                          | InParen(x)          -> (hasType g x t)
                          | IfThenElse(x1, x2, x3) -> (hasType g x1 Tbool) && (hasType g x2 t) && (hasType g x3 t)
                          | Tuple(n, x) -> (match x with
                                            

                                            )
                          | Project(i,n,x) ->
                          | Let(d,x) ->
                          | FunctionAbstraction(s, x) ->  let Tfunc(tau1, tau2)  = t in
                                                          let type = (giveType g s) in 
                                                          (type = tau1) && (hasType ((s,type)::g) x tau2)
                          | FunctionCall(x1, x2) -> ((getType g e) = t)
                          )
                          with TypeException -> false
                          with IllegalList -> false *)



let rec hastype g e t =   try
                          ( match e with
                              FunctionAbstraction(s,x) -> let Tfunc(t1, t2) = t in (getType ((s,t1)::g) x) = t2
                            | _ -> if (t = (getType g e)) then true else false
                          )
                          with BadType -> false
                          | IllegalList -> false
 
                          
let rec compareExpTypeList l1 l2 = match l1 with
                                  [] -> false
                                | hd::tl -> let (a,b) = hd in if(tl=[]) then ((giveType l2 a) = b) else (((giveType l2 a) = b) && (compareExpTypeList tl l2))


(* yields : ((string * exptype) list) -> definition -> ((string * exptype) list) -> bool *)
let rec yields g d g_dash = try
                            ( let g_prime = (getYield g d) in  (compareExpTypeList g_prime g_dash))
                            with BadType -> false
                            | IllegalList -> false  
