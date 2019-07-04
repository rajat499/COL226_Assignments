#directory "_build";; (* Consider this folder when looking for files *)
#load "a0.cmo";;
#load "a1.cmo";;
#load "a2.cmo";;
#load "a3.cmo";;
#load "a4.cmo";;
open A0;;
open A1;;
open A2;;
open A3;;
open A4;;

exception Not_implemented
(* Helper function to print *)
let rec print_tree tr = match tr with
  N a -> "INT " ^ (string_of_int a)
  | _ -> raise Not_implemented
;;
let rec print_answer tr = match tr with
  Num a -> print_num a
  | Bool a -> string_of_bool a
  | _ -> raise Not_implemented
;;
let rec print_value tr = match tr with
  NumVal a -> string_of_int a
  | BoolVal a -> string_of_bool a
  | _ -> raise Not_implemented
;;

(* Input is given as value and output is an answer *)
let rec toAnswer v = match v with
  NumVal a     -> Num (mk_big a)
| BoolVal b    -> Bool b
| TupVal (n,xs) -> Tup (n, List.map toAnswer xs);;

(* Input is given as string and output is an answer *)
let binding rho s = toAnswer (rho s);;

(* Both use the same lexer in A1 but different parser in A3 *)
let exp_parser s rho = A3.exp_parser A2.read (Lexing.from_string s) ;;
let def_parser s rho = A3.def_parser A2.read (Lexing.from_string s) ;;

(* Input is given as string and output is a value *)
let rho s = match s with
  "X" -> NumVal 5
  |  "Y" -> BoolVal true
  |  "Z" -> TupVal (3, [NumVal 5; BoolVal true; NumVal 1])
  | _ -> raise Not_implemented
;;

(* Type assumptions as a list of tuples of the form (variable name, type) *)
let g = [("X", Tint); ("Y", Tbool); ("Z", Ttuple [Tint ; Tbool ; Tint]); ("W", Tfunc (Tint, Tbool))];;

(* Helper for testcases *)
let rec type_tostr t = match t with 
 Tint -> "int"
 | Tunit -> "unit"
 | Tbool -> "bool"
 | Ttuple ts -> String.concat "" ["("; String.concat "," (List.map type_tostr ts); ")"]
 | Tfunc(t1,t2) -> String.concat " " [type_tostr t1; "->"; type_tostr t2]
;;

let testcase_pos n ip t =
        print_string("T" ^ string_of_int(n) ^ " : "); print_string ip; print_string " : "; print_string (type_tostr t); print_endline " : expected True ";
	try if (hastype g (exp_parser ip rho) t) 
                then print_endline("  Passed ")  
		else print_endline("  Failed ");
	with e -> print_endline("  Failed : Wrong exception raised : " ^ (Printexc.to_string e))
;;

let testcase_neg n ip t =
        print_string("T" ^ string_of_int(n) ^ " : "); print_string ip; print_string " : "; print_string (type_tostr t); print_endline " : expected False ";
	try if not (hastype g (exp_parser ip rho) t) 
		then print_endline("  Passed ") 
		else print_endline("  Failed ");
	with e -> print_endline("  Passed : exception raised : " ^ (Printexc.to_string e))
;;


let testcase_yields_pos n ip gdash =
	print_string("T" ^ string_of_int(n) ^ " : "); print_string ip; print_string " : "; print_endline " : expected True";
	try if (yields g (def_parser ip rho) gdash) 
		then print_endline("  Passed ") 
		else print_endline("  Failed ");
	with e -> print_endline("  Failed : Wrong exception raised : " ^ (Printexc.to_string e))
;;

let testcase_yields_neg n ip gdash =
	print_string("T" ^ string_of_int(n) ^ " : "); print_string ip; print_string " : "; print_endline " : expected False";
	try if not (yields g (def_parser ip rho) gdash) 
		then print_endline("  Passed ") 
		else print_endline("  Failed ");
	with e -> print_endline("  Passed : exception raised : " ^ (Printexc.to_string e))
;;


(* Testcases *)
(* Base cases and unary ops *)
testcase_pos 1 "5" Tint;;
testcase_pos 2 "T" Tbool;;
testcase_neg 3 "F" Tint;;
testcase_pos 4 "(X, Y, Z)" (Ttuple [Tint; Tbool; Ttuple [Tint; Tbool; Tint]]);;
testcase_pos 5 "not Y" Tbool;;
testcase_pos 6 "~5" Tint;;

(* Binary ops *)
testcase_pos 7 "15+25-3*5mod3+2div20" Tint;;
testcase_pos 8 "(15+25)>(X-5)" Tbool;;
testcase_pos 9 "((15+25)<=(X-5)) /\\ ((15+25)>=(X-5))" Tbool;;
testcase_neg 10 "25<25" Tint;;

(* ifte and tuple/proj *)
testcase_pos 11 "5 + if Y then X else 5 fi" Tint;;
testcase_neg 12 "if Y then X+4div5 else X+5div4 fi" Tbool;;
testcase_neg 13 "if Y then T else 5 fi" Tint;;
testcase_pos 14 "proj(3,3) (1,2,(1,2,3))" (Ttuple [Tint; Tint; Tint]);;
testcase_neg 15 "if Y then A else 5 fi" Tint;; (* A is not in G*)

(* let bindings *)
testcase_neg 16 "let def X:Tbool = Y in X+10 end" Tint;; (* Take local X*)
testcase_neg 17 "let def Y:Tbool = X in F end" Tbool;;
testcase_pos 18 "let def W:Tint = 3; def W:Tbool = Y \\/ T in W end" Tbool;; (* Design decision: Either fail or return Tbool*)
testcase_neg 19 "let def W:Tint = 3; def Q:Tbool = W in Q end" Tbool;;
testcase_pos 20 "let def A:Tint = 3 || def B:Tint = 20 in A+B end" Tint;;
testcase_neg 21 "let def A:Tint = 3 || def B:Tint = A in A+B end" Tint;; 
testcase_pos 22 "let def A:Tint = 3 in let def B:Tint = A in A+B end end" Tint;; 
testcase_neg 23 "let def A:Tint = 3 in (let def B:Tint = A in A+B end) + B end" Tint;; 

(* Function abs *)
testcase_pos 24 "let def Foo:Tbool -> (Tbool * Tbool) = \\X:Tbool.(X,Y) in Foo(T) end" (Ttuple [Tbool; Tbool]) ;;
testcase_pos 25 "let def Foo:Tint->Tint = \\A:Tint.A in let def Y:Tint=8 in Foo(Y) end end" Tint ;;
testcase_pos 26 "let def Foo:Tint->Tbool = \\A:Tint.Y in let def Y:Tint=8 in Foo(Y) end end" Tbool ;;
testcase_pos 27 "\\X:Tint.X" (Tfunc (Tint, Tint));;
testcase_neg 28 "\\X:Tint.X" (Tfunc (Tint, Tbool));;

(* Simple equation solving - dont care type *)
testcase_pos 29 "proj(2,3) (X, 50, Y)" Tint;;
testcase_neg 30 "proj(1,2) (if Y then (3,Y) else (4,4) fi)" Tint;;

(* Yields *)
testcase_yields_pos 31 "def U:Tint = X ; def V:Tbool = Y" [("U", Tint); ("V", Tbool)];
testcase_yields_pos 32 "def U:Tint = X ; def V:Tbool = Y" [("U", Tint); ("V", Tbool); ("X", Tint)];
testcase_yields_pos 33 "local def U:Tint = X in def V:Tint = U end" [("V", Tint)];
testcase_yields_neg 34 "local def U:Tint = X in def V:Tint = U end" [("V", Tint); ("U", Tint)];
testcase_yields_pos 35 "def U:Tint = X ; def U:Tbool = Y" [("U", Tbool)];
testcase_yields_pos 36 "def U:Tint = X" [("U", Tint)];
testcase_yields_neg 37 "def U:Tint = Y" [("U", Tint)];
testcase_yields_pos 38 "def U:Tint = X || def V:Tbool = Y" [("U", Tint); ("V", Tbool)];
