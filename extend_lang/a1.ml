(* Dummy implementation of A1 *)
open A0
exception Not_implemented
exception Not_ImplementedinA1

(* The possible types of expressions in the language of expressions *)
type exptype = Tint | Tunit | Tbool | Ttuple of (exptype list) | Tfunc of (exptype * exptype)
(* abstract syntax *)
and  exptree =
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
  | Let of definition * exptree
  | FunctionAbstraction of string * exptree * exptype
  | FunctionCall of exptree * exptree
(* definition *)
and definition =
    Simple of string * exptree * exptype
  | Sequence of (definition list)
  | Parallel of (definition list)
  | Local of definition * definition

(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = VAR of string | NCONST of bigint | BCONST of bool | ABS | UNARYMINUS | NOT
  | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT
  | PAREN | IFTE | TUPLE of int | PROJ of int*int | LET | FABS | FCALL
  | SIMPLEDEF | SEQCOMPOSE | PARCOMPOSE | LOCALDEF

(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool | TupVal of int * (value list)

(* The language should contain the following types of expressions:  integers and booleans *)
type answer = Num of bigint | Bool of bool | Tup of int * (answer list)

exception IllegalOperationOnWrongOperand
exception IllegalProjection
exception IllegalTuple

(*Gives xth member of a list. Head of the list is first member*)
let rec evaluateProj list x =( match x with
                               0 -> raise IllegalProjection
                              |1 -> List.hd list
                              |_ -> evaluateProj (List.tl list) (x-1))


(*Recursively evaluates the value of exptree.
Base case- if exptree is just N(i) the gives NumVal(i), B(bool) gives BoolVal(bool)
and Var(string) looks for the value of that string in rho function 
Otherwise calculates value of subtree/s and applies 
appropriate opeartion on that value/s to give final answer.*)
let rec eval ex rho = match ex with
                      Var(s)        -> rho s
                    | N(i)          -> NumVal(i)
                    | B(b)          -> BoolVal(b)
                    
                    | Abs(t)        -> let a = eval t rho in
                                        (match a with
                                        NumVal(i) -> NumVal(Pervasives.abs i)
                                        | _     -> raise IllegalOperationOnWrongOperand)
                    
                    | Negative(t)   -> let a = eval t rho in
                                        (match a with
                                        NumVal(i) -> NumVal(-1*i)
                                        | _     -> raise IllegalOperationOnWrongOperand)
                    
                    | Not(t)        -> let a = eval t rho in
                                       (match a with
                                        BoolVal(b) -> if b=true then BoolVal(false) else BoolVal(true)
                                        | _     -> raise IllegalOperationOnWrongOperand)
                    
                    | Add(t1, t2)   -> let x = eval t1 rho in
                                       let y = eval t2 rho in
                                       (match (x,y) with
                                        (NumVal(b1),NumVal(b2)) -> NumVal(b1+b2)
                                       | _                -> raise IllegalOperationOnWrongOperand)
                    
                    | Sub(t1, t2)   -> let x = eval t1 rho in
                                       let y = eval t2 rho in
                                       (match (x,y) with
                                        (NumVal(b1),NumVal(b2)) -> NumVal(b1-b2)
                                       | _                -> raise IllegalOperationOnWrongOperand)
                    
                    | Mult(t1, t2)  -> let x = eval t1 rho in
                                       let y = eval t2 rho in
                                       (match (x,y) with
                                        (NumVal(b1),NumVal(b2)) -> NumVal(b1*b2)
                                       | _                -> raise IllegalOperationOnWrongOperand)
                    
                    | Div(t1, t2)   -> let x = eval t1 rho in
                                       let y = eval t2 rho in
                                       (match (x,y) with
                                        (NumVal(b1),NumVal(b2)) -> NumVal(b1/b2)
                                       | _                -> raise IllegalOperationOnWrongOperand) 

                    | Rem(t1, t2)   -> let x = eval t1 rho in
                                       let y = eval t2 rho in
                                       (match (x,y) with
                                        (NumVal(b1),NumVal(b2)) -> NumVal(b1 mod b2)
                                       | _                -> raise IllegalOperationOnWrongOperand)

                    | Conjunction(t1, t2) ->  let x = eval t1 rho in
                                              let y = eval t2 rho in
                                              (match (x,y) with
                                                (BoolVal(b1),BoolVal(b2)) -> BoolVal(b1 && b2)
                                              | _                   -> raise IllegalOperationOnWrongOperand)

                    | Disjunction(t1, t2) ->  let x = eval t1 rho in
                                              let y = eval t2 rho in
                                              (match (x,y) with
                                                (BoolVal(b1),BoolVal(b2)) -> BoolVal(b1 || b2)
                                              | _                   -> raise IllegalOperationOnWrongOperand)

                    | Equals(t1, t2)      ->  let x = eval t1 rho in
                                              let y = eval t2 rho in
                                              (match (x,y) with
                                                (NumVal(b1),NumVal(b2)) -> BoolVal(b1=b2)
                                              | _                 -> raise IllegalOperationOnWrongOperand)
 

                    | GreaterTE(t1, t2)   ->  let x = eval t1 rho in
                                              let y = eval t2 rho in
                                              (match (x,y) with
                                                (NumVal(b1),NumVal(b2)) -> BoolVal(b1>=b2)
                                              | _                 -> raise IllegalOperationOnWrongOperand)

                    | LessTE(t1, t2)      ->  let x = eval t1 rho in
                                              let y = eval t2 rho in
                                              (match (x,y) with
                                                (NumVal(b1),NumVal(b2)) -> BoolVal(b1<=b2)
                                              | _                 -> raise IllegalOperationOnWrongOperand)

                    | GreaterT(t1, t2)    ->  let x = eval t1 rho in
                                              let y = eval t2 rho in
                                              (match (x,y) with
                                                (NumVal(b1),NumVal(b2)) -> BoolVal(b1>b2)
                                              | _                 -> raise IllegalOperationOnWrongOperand)

                    | LessT(t1, t2)       ->  let x = eval t1 rho in
                                              let y = eval t2 rho in
                                              (match (x,y) with
                                                (NumVal(b1),NumVal(b2)) -> BoolVal(b1<b2)
                                              | _                 -> raise IllegalOperationOnWrongOperand)
                    
                    | InParen(t1)         ->  eval t1 rho

                    | IfThenElse(t1, t2, t3) -> let a = eval t1 rho in
                                                if a=BoolVal(true) then eval t2 rho
                                                else if a=BoolVal(false) then eval t3 rho
                                                else raise IllegalOperationOnWrongOperand
                    
                    | Tuple(n, list)      -> let rec evaluateTuple l =(match l with
                                                                      [] -> []
                                                                    | hd::tl -> (eval hd rho) :: (evaluateTuple tl))
                                             in

                                             let x = evaluateTuple list in
                                             if (n= (List.length x)) then  TupVal(n, x) else raise IllegalOperationOnWrongOperand


                    | Project((i,n),t)    -> if(i<=0 || i>n) then raise IllegalProjection
                                             else

                                             let x = eval t rho in

                                             (match x with
                                              TupVal(a,e) -> if a=n then (evaluateProj e i) else raise IllegalTuple
                                             |_        -> raise IllegalProjection)

                    | _                    -> raise Not_ImplementedinA1

                                                                     
(*Recursively does postorder traversal of exptree.
base case- exptree is just N(i) -> It converts i to bigint and then inserts Const(i in bigint form) at head of an empty list.
Otherwise compiles left subtree then append compile of right subtree at end of left subtree and then finally appends the OpCode at the end of list.
In case where there is just one subtree then it compiles only that subtree and appends the OpCode at the end of that list. *)
let rec compile ex = match ex with
                      Var(s)       -> [VAR(s)]
                    | N(i)         -> [NCONST(mk_big i)]
                    | B(b)         -> [BCONST(b)]
                    | Abs(t)       -> (compile t) @ [ABS]
                    | Negative(t)  -> (compile t) @ [UNARYMINUS]
                    | Not(t)       -> (compile t) @ [NOT]
                    | Add(t1, t2)  -> (compile t1) @ (compile t2) @ [PLUS]
                    | Sub(t1, t2)  -> (compile t1) @ (compile t2) @ [MINUS]
                    | Mult(t1, t2) -> (compile t1) @ (compile t2) @ [MULT]
                    | Div(t1, t2)  -> (compile t1) @ (compile t2) @ [DIV]
                    | Rem(t1, t2)  -> (compile t1) @ (compile t2) @ [REM]
                    | Conjunction(t1,t2) -> (compile t1) @ (compile t2) @ [CONJ]
                    | Disjunction(t1,t2) -> (compile t1) @ (compile t2) @ [DISJ]
                    | Equals(t1, t2)     -> (compile t1) @ (compile t2) @ [EQS]
                    | GreaterTE(t1, t2)  -> (compile t1) @ (compile t2) @ [GTE]
                    | LessTE(t1, t2)     -> (compile t1) @ (compile t2) @ [LTE]
                    | GreaterT(t1, t2)   -> (compile t1) @ (compile t2) @ [GT]
                    | LessT(t1, t2)      -> (compile t1) @ (compile t2) @ [LT]
                    | InParen(t)         -> (compile t)  @ [PAREN]
                    | IfThenElse(t1,t2,t3) -> (compile t1) @ (compile t2) @ (compile t3) @ [IFTE]
                    | Tuple(n, list)     -> let rec compileTuple l =(match l with
                                                                     [] -> []
                                                                    |hd::tl -> (compile hd) @ (compileTuple tl)) in
                                            
                                            (compileTuple list) @ [TUPLE(n)]
                    | Project((i,n),t)   -> (compile t) @ [PROJ(i,n)] 
                                             
                    | _                    -> raise Not_ImplementedinA1                      

exception EmptyStackCan'tPop
exception EmptyStackNothingToPeek

(*Pops head of the stack and returns the remaining stack.*)
let pop_stack list = match list with 
                [] -> raise EmptyStackCan'tPop
              | hd :: tl -> tl
              

(*Returns the head/top of the stack.*)
let peek_stack list = match list with
                [] -> raise EmptyStackNothingToPeek
              | hd :: tl -> hd
              
(*Pushes the element on top of the stack.*)
let push_stack element list = element :: list 

exception IllegalOperationOnWrongOperandInStack

let abs_stack stk = let a = peek_stack stk in
                    (match a with
                      Num(i) -> (push_stack (Num(abs i)) (pop_stack stk))
                     | _ -> raise IllegalOperationOnWrongOperandInStack)

let unary_minus_stack stk = let a = peek_stack stk in
                            (match a with
                             Num(i) -> (push_stack (Num(minus i)) (pop_stack stk))
                            | _ -> raise IllegalOperationOnWrongOperandInStack)
                     
let not_stack stk = let a = peek_stack stk in
                    (match a with
                     Bool(b) -> (push_stack (if b=true then Bool(false) else Bool(true)) (pop_stack stk))
                     | _ -> raise IllegalOperationOnWrongOperandInStack)

(* Pops the stack two times and adds both the popped element and then 
pushes back the result into the stack and returns the final stack*)
let add_stack stk = let b_stk = pop_stack stk in
                    let x = peek_stack stk in
                    let y = peek_stack b_stk in
                    (match(x,y) with
                     (Num(b1),Num(b2)) -> (push_stack (Num(add b1 b2)) (pop_stack b_stk))
                    | _                -> raise IllegalOperationOnWrongOperandInStack)

(* Pops the stack two times and subtracts first popped element(x) from the second
popped element(y) and then pushes back the result into the stack and returns the final stack*)
let sub_stack stk = let b_stk = pop_stack stk in
                    let x = peek_stack stk in
                    let y = peek_stack b_stk in
                    (match(x,y) with
                     (Num(b1),Num(b2)) -> (push_stack (Num(sub b2 b1)) (pop_stack b_stk))
                    | _                -> raise IllegalOperationOnWrongOperandInStack)

let mult_stack stk = let b_stk = pop_stack stk in
                    let x = peek_stack stk in
                    let y = peek_stack b_stk in
                    (match(x,y) with
                     (Num(b1),Num(b2)) -> (push_stack (Num(mult b1 b2)) (pop_stack b_stk))
                    | _                -> raise IllegalOperationOnWrongOperandInStack)

let div_stack stk = let b_stk = pop_stack stk in
                    let x = peek_stack stk in
                    let y = peek_stack b_stk in
                    (match(x,y) with
                     (Num(b1),Num(b2)) -> (push_stack (Num(div b2 b1)) (pop_stack b_stk))
                    | _                -> raise IllegalOperationOnWrongOperandInStack)

let rem_stack stk = let b_stk = pop_stack stk in
                    let x = peek_stack stk in
                    let y = peek_stack b_stk in
                    (match(x,y) with
                     (Num(b1),Num(b2)) -> (push_stack (Num(rem b2 b1)) (pop_stack b_stk))
                    | _                -> raise IllegalOperationOnWrongOperandInStack)

let conj_stack stk = let b_stk = pop_stack stk in
                    let x = peek_stack stk in
                    let y = peek_stack b_stk in
                    (match(x,y) with
                     (Bool(b1),Bool(b2)) -> (push_stack (Bool(b1 && b2)) (pop_stack b_stk))
                    | _                  -> raise IllegalOperationOnWrongOperandInStack)

let disj_stack stk = let b_stk = pop_stack stk in
                    let x = peek_stack stk in
                    let y = peek_stack b_stk in
                    (match(x,y) with
                     (Bool(b1),Bool(b2)) -> (push_stack (Bool(b1 || b2)) (pop_stack b_stk))
                    | _                  -> raise IllegalOperationOnWrongOperandInStack)

let eq_stack stk = let b_stk = pop_stack stk in
                    let x = peek_stack stk in
                    let y = peek_stack b_stk in
                    (match(x,y) with
                     (Num(b1),Num(b2)) -> (push_stack (Bool(eq b1 b2)) (pop_stack b_stk))
                    | _                -> raise IllegalOperationOnWrongOperandInStack)

let gte_stack stk = let b_stk = pop_stack stk in
                    let x = peek_stack stk in
                    let y = peek_stack b_stk in
                    (match(x,y) with
                     (Num(b1),Num(b2)) -> (push_stack (Bool(geq b2 b1)) (pop_stack b_stk))
                    | _                -> raise IllegalOperationOnWrongOperandInStack)

let lte_stack stk = let b_stk = pop_stack stk in
                    let x = peek_stack stk in
                    let y = peek_stack b_stk in
                    (match(x,y) with
                     (Num(b1),Num(b2)) -> (push_stack (Bool(leq b2 b1)) (pop_stack b_stk))
                    | _                -> raise IllegalOperationOnWrongOperandInStack)

let gt_stack stk = let b_stk = pop_stack stk in
                    let x = peek_stack stk in
                    let y = peek_stack b_stk in
                    (match(x,y) with
                     (Num(b1),Num(b2)) -> (push_stack (Bool(gt b2 b1)) (pop_stack b_stk))
                    | _                -> raise IllegalOperationOnWrongOperandInStack)

let lt_stack stk = let b_stk = pop_stack stk in
                    let x = peek_stack stk in
                    let y = peek_stack b_stk in
                    (match(x,y) with
                     (Num(b1),Num(b2)) -> (push_stack (Bool(lt b2 b1)) (pop_stack b_stk))
                    | _                -> raise IllegalOperationOnWrongOperandInStack)

let ifte_stack stk = let b_stk = pop_stack stk in
                     let c_stk = pop_stack b_stk in
                     let c = peek_stack c_stk in

                     if c=Bool(true) then (push_stack (peek_stack b_stk) (pop_stack c_stk))
                     else if c=Bool(false) then (push_stack (peek_stack stk) (pop_stack c_stk))
                     else raise IllegalOperationOnWrongOperandInStack

let proj_stack stk i n = let x = (peek_stack stk) in
                         (match x with
                          Tup(a,e) -> if a=n then (push_stack (evaluateProj e i) (pop_stack stk)) 
                                      else raise IllegalTuple
                         | _       -> raise IllegalProjection)

let rec tuple_stack stk n = if (n<0) then raise IllegalTuple 
                        else
                        (match n with
                          0 -> ([],stk)
                        | n -> let (x,y) = (tuple_stack (pop_stack stk) (n-1)) in 
                               ((peek_stack stk) :: x, y ))

exception StackEmpty
exception IllegalOpCodeList_StackContainsMoreThanOneElement

(*It takes an empty[] stk(answer_list) which acts as a stack, and an opcode list(generally returned by compile function),and a look up function rho,
and calculates value of that expression as per opcode list. The opcode list is in postorder notation of an
expression evlauation. The final result is stored in the stk[Containing only one element] and that
element is the final answer which is returned. If the stk at last doesn't contain only one element then
the opcode list is not proper and the expression may not be complete. In that case an error is returned. *)

let rec stackmc stk rho pgm = match pgm with
                               [] -> (match stk with
                                      []       -> raise StackEmpty
                                      | hd::tl -> if tl=[] then hd else raise IllegalOpCodeList_StackContainsMoreThanOneElement)

                              | hd :: tl -> (match hd with
                                              VAR(s)    -> (stackmc ((rho s)::stk) rho tl)
                                            | NCONST(i) -> (stackmc (Num(i)::stk) rho tl)
                                            | BCONST(b) -> (stackmc (Bool(b)::stk) rho tl)
                                            | ABS       -> (stackmc (abs_stack stk) rho tl)
                                            | UNARYMINUS-> (stackmc (unary_minus_stack stk) rho tl)
                                            | NOT       -> (stackmc (not_stack stk) rho tl)
                                            | PLUS(*It takes an empty[] b_list which acts as a stack, and an opcode list(generally returned by compile function),
                                            and calculates value of that expression as per opcode list. The opcode list is in postorder notation of an
                                            expression evlauation. The final result is stored in the b_list[Containing only one element] and that
                                            element is the final answer which is returned. If the b_list at last doesn't contain only one element then
                                            the opcode list is not proper and the expression may not be complete. In that case an error is returned. *)
                                                  -> (stackmc (add_stack stk) rho tl)
                                            | MINUS     -> (stackmc (sub_stack stk) rho tl)
                                            | MULT      -> (stackmc (mult_stack stk) rho tl)
                                            | DIV       -> (stackmc (div_stack stk) rho tl)
                                            | REM       -> (stackmc (rem_stack stk) rho tl)
                                            | CONJ      -> (stackmc (conj_stack stk) rho tl)
                                            | DISJ      -> (stackmc (disj_stack stk) rho tl)
                                            | EQS       -> (stackmc (eq_stack stk) rho tl)
                                            | GTE       -> (stackmc (gte_stack stk) rho tl)
                                            | LTE       -> (stackmc (lte_stack stk) rho tl)
                                            | GT        -> (stackmc (gt_stack stk) rho tl)
                                            | LT        -> (stackmc (lt_stack stk) rho tl)
                                            | PAREN     -> (stackmc (stk) rho tl)
                                            | IFTE      -> (stackmc (ifte_stack stk) rho tl)
                                            | TUPLE(n)  -> let (x,y) = (tuple_stack stk n) in (stackmc (Tup(n,List.rev x)::y) rho tl)
                                            | PROJ(i,n) -> (stackmc (proj_stack stk i n) rho tl)
                                            | _         -> raise Not_ImplementedinA1
                                            )