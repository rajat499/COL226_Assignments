#directory "_build";; (* Consider this folder when looking for files *)
#load "a5.cmo";;
#load "parser.cmo";;
#load "lexer.cmo";;
open A5;;
open Parser;;
open Lexer;;


exception Not_implemented

let p1 =  App (Lambda ("x", Mult (Integer 3, V "x")), Integer 4);;
  (*12*)
  
let s1 = "\\X.(3*X)(4)";;

let p2 = If_Then_Else
   (Cmp (Integer 7),
    App (Lambda ("x", Plus (Integer 3, V "x")), Integer 31), 
    Integer 0);;
   (*34*)
let s2 = "if cmp 7 then \\X.(3+X)(31) else 0 fi";;

let p3 = If_Then_Else
    (Cmp (Integer 0),
    App (Lambda ("x", Plus (Integer 3, V "x")), Integer 4),
        Integer 110);;
    (*110*)

let s3 = "if cmp 0 then \\X.(3+X)(31) else 110 fi";;

let p4 = App(Lambda("x", App(Lambda( "y", And(V "x", V "y")), Bool true)), Bool false);;
(*false*)

let s4 = "\\X.\\Y.(X/\\Y)(T)(F)";;

let p5 = App(Lambda("x", App(Lambda( "y", Or(V "x", V "y")), Bool true)), Bool false);;
(*true*)
let s5 = "\\X.\\Y.(X\\/Y)(T)(F)";;

let p6 = App(Lambda("x", Mult(V "x", App(Lambda( "x", App(Lambda( "y", Plus(V "x", V "y")), Integer 4)), Integer 3))), Integer 2);;
(*14*)
let s6 = "\\X.(X*\\X.\\Y.(X+Y)(4)(3))(2)"

let p7 = If_Then_Else(Cmp(App(Lambda( "x", App(Lambda( "y", Plus(V "x", V "y")), Integer 4)), Integer (-5))), Integer (-29), 
App(Lambda( "x", Plus(V "x", App(Lambda( "x", Plus(V "x", Integer 1)), Integer 7))), Integer 5));;
(*13*)
let s7 = "if cmp (\\X.(\\Y.(X+Y)(4))(~5)) then (~29) else \\X.(X+(\\X.(X+1)(7)))(5) fi"

let p8 = App(Lambda( "x", App(Lambda( "y", Plus(V "x", V "y")), Integer 4)), App(Lambda( "x", Mult(V "x", Integer 2)), Integer 3));;
(*10*)
let s8 = "\\X.(\\Y.(X+Y)(4))(\\X.(X*2)(3))";;

let p9 = App(Lambda( "x", App(Lambda( "y", Mult(V "x", V "y")), V "x")), Integer 4);;
(*16*)
let s9 = "\\X.(\\Y.(X*Y)(X))(4)";;

let p10 = App(Lambda( "x", Plus(V "x", App(Lambda( "x", Mult(V "x", Integer 2)), App(Lambda( "x", Plus(V "x", Integer 4)), Integer 3)))), Integer 20);;
(*34*)
let s10 = "\\X.(X+\\X.(X*2)(\\X.(X+4)(3)))(20)";;

let p11 = App(Lambda( "x", App(Lambda( "y", And(V "x", V "y")), V "x")), Bool true);;
(*true*)
let s11 = "\\X.(\\Y.(X/\\Y)(X))(T)";;

let p12 = If_Then_Else(Cmp(App(Lambda( "x", Mult(V "x", Integer 2)), Integer 4)), App(Lambda( "x", App(Lambda( "y", Or(V "x", V "y")), V "x")), Bool false), Bool true);;
(*false*)
let s12 = "if cmp (\\X.(X*2)(4)) then \\X.(\\Y.(X\\/Y)(X))(F) else T fi";;

let p13 = App(Lambda( "x", And(V "x", App(Lambda( "x", And(V "x", Bool true)), App(Lambda( "x", And(V "x", Bool true)), Bool true)))), Bool true);;
(*true*)
let s13 = "\\X.(X/\\(\\X.(X/\\T)(\\X.(X/\\T)(T))))(T)";;

let p14 = App(Lambda( "x", And(V "x", App(Lambda( "x", And(V "x", Bool true)), App(Lambda( "x", And(V "x", Bool true)), Bool true)))), Bool false);;
(*false*)
let s14 = "\\X.(X/\\(\\X.(X/\\T)(\\X.(X/\\T)(T))))(F)";;

let p15 = If_Then_Else(Cmp(App(Lambda( "x", Mult(V "x", App(Lambda( "y", V "y"), V "x"))), Integer 1)), App(Lambda( "x", Plus(V "x", App(Lambda( "x", Plus(V "x", Integer 1)), Integer 3))), Integer 5), Integer (-1));;
(*9*)
let s15 = "if cmp (\\X.(X*\\Y.(Y)(X))(1)) then \\X.(X+\\X.(X+1)(3))(5) else (~1) fi"

(*Your code will go here*)
(*For thise who have implemented lexer parser, modify the testcases in your grammar and you will have to get those tet_cases at the time of the demo*)

let eval_secd inp = let ACls (a,b) = callSECD inp [] in
                    match a with
                      Num(i) -> Integer(i)
                    | BoolVal(b) -> Bool(b)
                    | _ -> raise Not_Implemented;;

let eval_krivine inp = let VCls(a,b) = callKrivine inp [] in
                        match a with
                          Num(i) -> Integer(i)
                        | BoolVal(b) -> Bool(b)
                        | _ -> raise Not_Implemented;;

let exp_parser s = Parser.exp_parser Lexer.read (Lexing.from_string s) ;;
(*Your code ends*)

let p16 = exp_parser "\\X.(if X>2 then \\X.X(T) else abs X fi)(3+4)";;

let check_secd n inp out =
  print_string("T" ^ string_of_int(n) ^ " : ");
  try if (inp = out) 
    then print_string("Passed ") 
    else print_string("Failed ");
  with e -> print_endline("Failed : Wrong exception raised : " ^ (Printexc.to_string e))
;;

let check_krivine n inp out =
  print_string("T" ^ string_of_int(n) ^ " : ");
  try if (inp = out) 
    then print_string("Passed ") 
    else print_string("Failed ");
  with e -> print_endline("Failed : Wrong exception raised : " ^ (Printexc.to_string e))
;;

let print_heading a = print_endline("\n" ^ a ^ " :");;

(*SECD*)
print_heading "SECD test cases\n";;

check_secd 1 (eval_secd p1) (Integer 12);;
check_secd 2 (eval_secd p2) (Integer 34);;
check_secd 3 (eval_secd p3) (Integer 110);;
check_secd 4 (eval_secd p4) (Bool false);;
check_secd 5 (eval_secd p5) (Bool true);;
check_secd 6 (eval_secd p6) (Integer 14);;
check_secd 7 (eval_secd p7) (Integer 13);;
check_secd 8 (eval_secd p8) (Integer 10);;
check_secd 9 (eval_secd p9) (Integer 16);;
check_secd 10 (eval_secd p10) (Integer 34);;
check_secd 11 (eval_secd p11) (Bool true);;
check_secd 12 (eval_secd p12) (Bool false);;
check_secd 13 (eval_secd p13) (Bool true);;
check_secd 14 (eval_secd p14) (Bool false);;
check_secd 15 (eval_secd p15) (Integer 9);; 

print_heading "Krivine test cases";;

check_krivine 1 (eval_krivine p1) (Integer 12);;
check_krivine 2 (eval_krivine p2) (Integer 34);;
check_krivine 3 (eval_krivine p3) (Integer 110);;
check_krivine 4 (eval_krivine p4) (Bool false);;
check_krivine 5 (eval_krivine p5) (Bool true);;
check_krivine 6 (eval_krivine p6) (Integer 14);;
check_krivine 7 (eval_krivine p7) (Integer 13);;
check_krivine 8 (eval_krivine p8) (Integer 10);;
check_krivine 9 (eval_krivine p9) (Integer 16);;
check_krivine 10 (eval_krivine p10) (Integer 34);;
check_krivine 11 (eval_krivine p11) (Bool true);;
check_krivine 12 (eval_krivine p12) (Bool false);;
check_krivine 13 (eval_krivine p13) (Bool true);;
check_krivine 14 (eval_krivine p14) (Bool false);;
check_krivine 15 (eval_krivine p15) (Integer 9);; 
(*Krivine*)


let check_secd n inp out = let ans = (eval_secd inp) in
  print_string("T" ^ string_of_int(n) ^ " : ");
  try if (ans = out) 
    then print_string("Passed ") 
    else print_string("Failed ");
  with e -> print_endline("Failed : Wrong exception raised : " ^ (Printexc.to_string e))
;;

let check_krivine n inp out = let ans = (eval_krivine inp) in
  print_string("T" ^ string_of_int(n) ^ " : ");
  try if (ans = out) 
    then print_string("Passed ") 
    else print_string("Failed ");
  with e -> print_endline("Failed : Wrong exception raised : " ^ (Printexc.to_string e))
;;


(*SECD*)
print_heading "SECD test cases with parser\n";;

check_secd 1 (exp_parser s1) (Integer 12);;
check_secd 2 (exp_parser s2) (Integer 34);;
check_secd 3 (exp_parser s3) (Integer 110);;
check_secd 4 (exp_parser s4) (Bool false);;
check_secd 5 (exp_parser s5) (Bool true);;
check_secd 6 (exp_parser s6) (Integer 14);;
check_secd 7 (exp_parser s7) (Integer 13);;
check_secd 8 (exp_parser s8) (Integer 10);;
check_secd 9 (exp_parser s9) (Integer 16);;
check_secd 10 (exp_parser s10) (Integer 34);;
check_secd 11 (exp_parser s11) (Bool true);;
check_secd 12 (exp_parser s12) (Bool false);;
check_secd 13 (exp_parser s13) (Bool true);;
check_secd 14 (exp_parser s14) (Bool false);;
check_secd 15 (exp_parser s15) (Integer 9);; 

print_heading "Krivine test cases with parser \n";;

check_krivine 1 (exp_parser s1) (Integer 12);;
check_krivine 2 (exp_parser s2) (Integer 34);;
check_krivine 3 (exp_parser s3) (Integer 110);;
check_krivine 4 (exp_parser s4) (Bool false);;
check_krivine 5 (exp_parser s5) (Bool true);;
check_krivine 6 (exp_parser s6) (Integer 14);;
check_krivine 7 (exp_parser s7) (Integer 13);;
check_krivine 8 (exp_parser s8) (Integer 10);;
check_krivine 9 (exp_parser s9) (Integer 16);;
check_krivine 10 (exp_parser s10) (Integer 34);;
check_krivine 11 (exp_parser s11) (Bool true);;
check_krivine 12 (exp_parser s12) (Bool false);;
check_krivine 13 (exp_parser s13) (Bool true);;
check_krivine 14 (exp_parser s14) (Bool false);;
check_krivine 15 (exp_parser s15) (Integer 9);; 
(*Krivine*)