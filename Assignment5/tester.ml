#directory "_build";; (* Consider this folder when looking for files *)
#load "a5.cmo";;
#load "parser.cmo";;
#load "lexer.cmo";;
open A5;;
open Parser;;
open Lexer;;

exception Not_Implemented
let exp_parser s = Parser.exp_parser Lexer.read (Lexing.from_string s) ;;

let print_answer x = match x with
                    Num(n) -> string_of_int n
                  | BoolVal(b) -> string_of_bool b
                  | _ -> raise Not_Implemented

let execute s rho = let exp = exp_parser s in
                    let VCls(k,c) = callKrivine exp [] in
                    let ACls(s,c) = callSECD exp [] in
                    "Krivine- " ^ ( print_answer k) ^ " || SECD-"^( print_answer s );;

let evaluate s rho gamma = let exp = exp_parser s in
                           let k = callKrivine exp rho in
                           let s = callSECD exp gamma in
                           (k,s);; 

let execute s rho gamma = let k = callKrivine s rho in
                          let s = callSECD s gamma in
                          (k,s);;