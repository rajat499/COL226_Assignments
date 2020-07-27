open A1
exception Not_implemented
exception TypeOfVariableNotFound
exception BadType
exception IllegalList

let rec findInList x l = match l with
                          [] -> false
                         | hd::tl -> if((fst hd) = (fst x)) then true else (findInList x tl)

(*Augments the list l1 on l2 *)
let rec augment l1 l2 = match l2 with
                         [] -> l1
                        |hd::tl -> if(not(findInList hd l1)) then (hd::(augment l1 tl)) else (augment l1 tl) 

(*Tells Whether two lists have intersection or not*)
let rec hasIntersection l1 l2 = match l1 with
                             [] -> false
                             | hd::tl -> if(findInList hd l2) then true else (hasIntersection tl l2)

(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool *)
(* let rec hastype g e t = raise Not_implemented *)

(*Given a variable x, it looks for x in table/list g and returns it's type*)
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

(*Takes a table gamma and a definition d and returns the augment gamma'
In case of Sequence gamma1[gamma2] is returned as well as in case of parallel*)
let rec getYield g d = match d with
                     Simple(s,e,t) -> if ((getType g e) = t) then [(s,t)] else raise BadType
                    |Parallel(l) -> (match l with 
                                    [] ->[]
                                    |hd::tl -> let a = (getYield g hd) in 
                                               let b = getYield g (Parallel(tl)) in
                                               if(hasIntersection a b) then raise BadType
                                               else (augment b a)  
                                    )
                    |Sequence(l) -> (match l with 
                                    [] ->[]
                                   |hd::tl -> let a = (getYield g hd) in (augment (getYield (augment a g) (Sequence(tl))) a) 
                                    )
                   |Local(d1,d2)-> getYield (augment (getYield g d1) g) d2

(*Takes an exptree and a table gamma and tells the type of exptree.
getType and getYield are two mutually recursive function.*)
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
                                          | Project((i,n), x) -> let Ttuple(l) = (getType g x) in                                                         
                                                                if(n= (List.length l)) then (get_ith l i) else raise BadType
                                          | FunctionAbstraction(s, x, t) -> Tfunc(t, getType (augment [(s,t)] g) x)
                                          | FunctionCall (t1, t2) -> let x = (getType g t2) in
                                                                      let Tfunc(y,z) = (getType g t1) in
                                                                      if(y = x) then z else raise BadType 
                                          | Let(d,x) ->let g_dash = (getYield g d) in (getType (g_dash @ g) x)
                                          )
                   
(*Takes a table gamma and expression e and type t and tells whether type of e is t or not*)
let rec hastype g e t =   try
                          ( if (t = (getType g e)) then true else false
                          )
                          with BadType -> false
                          | _ -> false
 
(*Takes two table and tells whether they are equivalent or not*)                          
let rec compareExpTypeList l1 l2 = match l1 with
                                  [] -> false
                                | hd::tl -> let (a,b) = hd in if(tl=[]) then ((giveType l2 a) = b) else (((giveType l2 a) = b) && (compareExpTypeList tl l2))

(*Takes a table gamma and definition d and tells when definition d is augmented on gamma then 
obtained table is equivalent to gamma dash or not*)
(* yields : ((string * exptype) list) -> definition -> ((string * exptype) list) -> bool *)
let rec yields g d g_dash = try
                            ( let g_prime = (getYield g d) in (compareExpTypeList g_dash g_prime))
                            with BadType -> false
                            | _ -> false  



(*It has been assumed that the variable in function abstraction will have a type in table. The first layer of function abstraction
can derive the type of its variable for the type passed on to it to check. From second layer onwards it should have its type in table*)