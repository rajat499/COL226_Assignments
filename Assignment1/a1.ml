open A0
(*Defining type of expression tree.*)
type exptree  =     N of int
                |   Plus of exptree * exptree
                |   Minus of exptree * exptree
                |   Mult of exptree * exptree
                |   Div of exptree * exptree
                |   Rem of exptree * exptree
                |   Nega of exptree
                |   Abs of exptree
                ;;
(*Defining type of operation codes.*)
type opcode = CONST of bigint 
              | PLUS 
              | TIMES 
              | MINUS 
              | DIV 
              | REM 
              | ABS 
              | UNARYMINUS 
              ;;
(*Defining expression.*)
exception IllegalExptree;;

(*Recursively evaluates the value of exptree.
Base case- if exptree is just N(i) the gives i
Otherwise calculates value of subtree/s and applies 
appropriate opeartion on that value/s to give final answer.*)
let rec eval t = match t with
                   N(i) -> i
                 | Plus(t1, t2) -> (eval t1) + (eval t2)
                 | Mult(t1, t2) -> (eval t1) * (eval t2)
                 | Minus(t1, t2) -> (eval t1) - (eval t2)
                 | Div(t1, t2) -> (eval t1) / (eval t2)
                 | Rem(t1, t2) -> (eval t1) mod (eval t2)
                 | Nega(t1) -> (-1) * (eval t1)
                 | Abs(t1) -> Pervasives.abs (eval t1)      (*Using in-bulit definition of abs for Ocaml's int.*)
                 | _ -> raise IllegalExptree
                ;;

(*Recursively does postorder traversal of exptree.
base case- exptree is just N(i) -> It converts i to bigint and then inserts Const(i in bigint form) at head of an empty list.
Otherwise compiles left subtree then append compile of right subtree at end of left subtree and then finally appends the OpCode at the end of list.
In case where there is just one subtree then it compiles only that subtree and appends the OpCode at the end of that list. *)
let rec compile t = match t with
                     N(i) -> [ CONST(mk_big i) ]
                   | Plus(t1, t2) -> (compile t1) @ (compile t2) @ [PLUS]
                   | Mult(t1, t2) -> (compile t1) @ (compile t2) @ [TIMES]
                   | Minus(t1, t2) -> (compile t1) @ (compile t2) @ [MINUS]
                   | Div(t1, t2) -> (compile t1) @ (compile t2) @ [DIV]
                   | Rem(t1, t2) -> (compile t1) @ (compile t2) @ [REM]
                   | Abs(t1) -> (compile t1) @ [ABS]
                   | Nega(t1) -> (compile t1) @ [UNARYMINUS] 
                   | _ -> raise IllegalExptree
                   ;;

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

(* Pops the stack two times and adds both the popped element and then 
pushes back the result into the stack and returns the final stack*)
let add_stack b_list = let b1_list = pop_stack b_list in
                       push_stack (add (peek_stack b_list) (peek_stack b1_list)) (pop_stack b1_list)
                       ;;

(* Pops the stack two times and multiplies both the popped element and then 
pushes back the result into the stack and returns the final stack*)
let mult_stack b_list = let b1_list = pop_stack b_list in
                       push_stack (mult (peek_stack b_list) (peek_stack b1_list)) (pop_stack b1_list)
                       ;;

(* Pops the stack two times and subtracts first popped element(r2) from the second
popped element(r1) and then pushes back the result into the stack and returns the final stack*)
let sub_stack b_list = let b1_list = pop_stack b_list in
                       push_stack (sub (peek_stack b1_list) (peek_stack b_list)) (pop_stack b1_list)
                       ;;

(* Pops the stack two times and divides second popped element(r1) by the first
popped element(r2) and then pushes back the result into the stack and returns the final stack*)
let div_stack b_list = let b1_list = pop_stack b_list in
                       push_stack (div (peek_stack b1_list) (peek_stack b_list)) (pop_stack b1_list)
                       ;;

(* Pops the stack two times and gives the remainder when second popped element(r1) is divided by the first
popped element(r2) and then pushes back the result into the stack and returns the final stack*)
let rem_stack b_list = let b1_list = pop_stack b_list in
                       push_stack (rem (peek_stack b1_list) (peek_stack b_list)) (pop_stack b1_list)
                       ;;

(* Pops the stack and calculates absolute value of the popped element and then
pushes back the result into the stack and returns the final stack*)
let abs_stack b_list = push_stack (abs (peek_stack b_list)) (pop_stack b_list)
                       ;;

(* Pops the stack and calculates Negative of the popped element and then
pushes back the result into the stack and returns the final stack*)
let unary_neg_stack b_list = push_stack (minus (peek_stack b_list)) (pop_stack b_list)
                             ;;

(*Exception Declaration.*)
exception IllegalOpCodeList_StackContainsMoreThanOneElement;;
exception IllegalOpCodeList_NotAValidOpCode;;


(*It takes an empty[] b_list which acts as a stack, and an opcode list(generally returned by compile function),
and calculates value of that expression as per opcode list. The opcode list is in postorder notation of an
expression evlauation. The final result is stored in the b_list[Containing only one element] and that
element is the final answer which is returned. If the b_list at last doesn't contain only one element then
the opcode list is not proper and the expression may not be complete. In that case an error is returned. *)
let rec stackmc b_list op_list = match op_list with
                                  [] -> (match b_list with
                                         [] -> (mk_big 0)
                                       | hd :: tl -> if (tl = []) then hd
                                                     else raise IllegalOpCodeList_StackContainsMoreThanOneElement)
                                
                                | hd :: tl -> (match hd with
                                                CONST(i) -> (stackmc (i :: b_list) tl)
                                              | PLUS ->    (stackmc (add_stack b_list) tl)
                                              | TIMES ->   (stackmc (mult_stack b_list) tl)
                                              | MINUS ->   (stackmc (sub_stack b_list) tl)
                                              | DIV ->     (stackmc (div_stack b_list) tl)
                                              | REM ->     (stackmc (rem_stack b_list) tl)
                                              | ABS ->     (stackmc (abs_stack b_list) tl)
                                              | UNARYMINUS -> (stackmc (unary_neg_stack b_list) tl)
                                              | _ -> raise IllegalOpCodeList_NotAValidOpCode )
                                              ;;