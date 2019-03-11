#directory "_build";; (* Consider this folder when looking for files *)
#load "a0.cmo";; (* Load the a0 bytecode *)
#load "a1.cmo";;
#load "a2.cmo";;
#load "a3.cmo";;
open A0;;
open A1;;
open A2;;
open A3;;

exception Not_implemented
(* Helper function to print *)
let rec print_tree tr = match tr with
  Done -> "\n"
  | N a -> "INT " ^ (string_of_int a)
  | _ -> raise Not_implemented
;;
let rec print_answer tr = match tr with
  Num a -> print_num a
  | Bool a -> string_of_bool a
;;


(* Parser accepts the expression as string and binding as hash map with variable to values (integer, boolean, tuple) *)
let parser s binding =
  let result = A3.main A2.read (Lexing.from_string s) in
    (* Return the three versions as abstract syntax tree, value, compiled opcode*)
    (result, (A1.eval result), (A1.compile result))
;;

(* Input is given as string *)
let binding = Hashtbl.create 123456;;
Hashtbl.add binding (Var "x") (N 10);;

let _ = (parser "5;" binding);;
