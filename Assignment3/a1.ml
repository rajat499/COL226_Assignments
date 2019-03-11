(* Dummy implementation of A1 *)
open A0
exception Not_implemented

(* abstract syntax *)
type  exptree =  
  Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | N of int      (* Integer constant *)
  | B of bool     (* Boolean constant *)
  (* unary operators on integers *)
  | Abs of exptree                   (* abs *)
  | Negative of exptree              (* unary minus ~ *)
  (* unary operators on booleans *)
  | Not of exptree
  (* binary operators on integers *)
  | Add of exptree * exptree         (* Addition + *)
  | Sub of exptree * exptree         (* Subtraction - *)
  | Mult of exptree * exptree        (* Multiplication * *)
  | Div of exptree * exptree         (* div *)
  | Rem of exptree * exptree         (* mod *)
  (* binary operators on booleans *)
  | Conjunction of exptree * exptree (* conjunction /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Equals of exptree * exptree      (* = *)
  | GreaterTE of exptree * exptree   (* >= *)
  | LessTE of exptree * exptree      (* <= *)
  | GreaterT of exptree * exptree    (* > *)
  | LessT of exptree * exptree       (* < *)
  (* expressions using parenthesis *)
  | InParen of exptree               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse of exptree * exptree * exptree (* if then else fi  *)
  (* creating n-tuples (n >= 0) *)
  | Tuple of int * (exptree list)
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project of (int*int) * exptree   (* Proj((i,n), e)  0 < i <= n *)

(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = VAR of string | NCONST of bigint | BCONST of bool | ABS | UNARYMINUS | NOT
  | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT
  | PAREN | IFTE | TUPLE of int | PROJ of int*int

(* The language should contain the following types of expressions:  integers and booleans *)
type answer = Num of bigint | Bool of bool | Tup of int * (answer list)

exception IllegalOperationOnWrongOperand

let rec eval ex rho = match ex with
                      Var(s)        -> rho s
                    | N(i)          -> Num(mk_big i)
                    | B(b)          -> Bool(b)
                    
                    | Abs(t)        -> let a = eval t rho in
                                        match a with
                                        Num(i) -> Num(abs i)
                                        | _     -> raise IllegalOperationOnWrongOperand
                    
                    | Negative(t)   -> let a = eval t rho in
                                        match a with
                                        Num(i) -> Num(minus i)
                                        | _     -> raise IllegalOperationOnWrongOperand
                    
                    | Not(t)        -> let a = eval t rho in
                                        match a with
                                        Bool(b) -> if b=true then Bool(false) else Bool(true)
                                        | _     -> raise IllegalOperationOnWrongOperand
                    
                    | Add(t1, t2)   -> let x = eval t1 rho in
                                       let y = eval t2 rho in
                                       match (x,y) with
                                        (Num(b1),Num(b2)) -> Num(add b1 b2)
                                       | _                -> raise IllegalOperationOnWrongOperand
                    
                    | Sub(t1, t2)   -> let x = eval t1 rho in
                                       let y = eval t2 rho in
                                       match (x,y) with
                                        (Num(b1),Num(b2)) -> Num(sub b1 b2)
                                       | _                -> raise IllegalOperationOnWrongOperand
                    
                    | Mult(t1, t2)  -> let x = eval t1 rho in
                                       let y = eval t2 rho in
                                       match (x,y) with
                                        (Num(b1),Num(b2)) -> Num(mult b1 b2)
                                       | _                -> raise IllegalOperationOnWrongOperand
                    
                    | Div(t1, t2)   -> let x = eval t1 rho in
                                       let y = eval t2 rho in
                                       match (x,y) with
                                        (Num(b1),Num(b2)) -> Num(div b1 b2)
                                       | _                -> raise IllegalOperationOnWrongOperand 

                    | Rem(t1, t2)   -> let x = eval t1 rho in
                                       let y = eval t2 rho in
                                       match (x,y) with
                                        (Num(b1),Num(b2)) -> Num(rem b1 b2)
                                       | _                -> raise IllegalOperationOnWrongOperand

                    | Conjunction(t1, t2) ->  let x = eval t1 rho in
                                              let y = eval t2 rho in
                                              match (x,y) with
                                                (Bool(b1),Bool(b2)) -> Bool(rem b1 b2)
                                              | _                   -> raise IllegalOperationOnWrongOperand
let stackmc stk rho pgm = raise Not_implemented
let compile ex = raise Not_implemented
