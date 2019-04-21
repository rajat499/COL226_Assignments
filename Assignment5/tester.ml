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
                    "Krivine- " ^ ( print_answer k) ^ " || SECD- "^( print_answer s );;

let evaluate s rho gamma = let exp = exp_parser s in
                           let k = callKrivine exp rho in
                           let s = callSECD exp gamma in
                           (k,s);; 

let executeCompiled s rho gamma = let k = callKrivine s rho in
                          let s = callSECD s gamma in
                          (k,s);;


(* execute "\\X.(if X>2 then \\X.X(T) else abs X fi)(3+4)" [];;
- : string = "Krivine- true || SECD-true" 

execute "if cmp ~1 then \\X.(X+9)(6+7) else \\X.(X*4)(7) fi" [];;
- : string = "Krivine- 28 || SECD-28"   

execute "\\X.(if T then X*X else X*X*X fi)(5)" [];;
- : string = "Krivine- 25 || SECD-25"

execute "\\X.(if F then X*X else X*X*X fi)(5)" [];;
- : string = "Krivine- 125 || SECD-125"    *)

(* 
execute "\\X.(if X>2 then \\X.X(T) else ~ X fi)(3+4)" [];;
- : string = "Krivine- true || SECD- true"   

execute "\\X.(if X>2 then \\X.X(T) else ~ X fi)(3-4)" [];;                                                                                                                                                                      ─( 21:58:34 )─< command 27 >────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─utop # execute "\\X.(if X>2 then \\X.X(T) else ~ X fi)(3-4)" [];;
- : string = "Krivine- 1 || SECD- 1"  

execute "\\X.(if X>2 then \\X.(X mod 2)(5) else  X/2 fi)(3+4)" [];;
- : string = "Krivine- 1 || SECD- 1"   

execute "\\X.(if X>2 then \\X.(X mod 2)(5) else  X div 2 fi)(3-10)" [];;
- : string = "Krivine- -3 || SECD- -3" 

execute "\\X.(if X>2 then X<10 else  X>0  fi)(3)" [];;
- : string = "Krivine- true || SECD- true"  

execute "\\X.(if X>2 then X<10 else  X>=0  fi)(~3)" [];;
- : string = "Krivine- false || SECD- false"     

execute "~3 + \\X.(if X>0 then 7*X else abs X fi)(3)" [];;
- : string = "Krivine- 18 || SECD- 18"  

execute "~3 + \\X.(if X>0 then 7*X else abs X fi)(~3)" [];;
- : string = "Krivine- 0 || SECD- 0" 

execute "~3 + \\X.(if X>0/\\X<3 then 7*X else abs X fi)(3)" [];;
- : string = "Krivine- 0 || SECD- 0"        

execute "~3 + \\X.(if X>0/\\X<=3\\/X>~2 then 7*X else abs X fi)(3)" [];;
- : string = "Krivine- 18 || SECD- 18"

execute "~3 + \\X.(if X>0/\\X<=3\\/X>~2 then 7*X else abs X fi)(~3)" [];;
- : string = "Krivine- 0 || SECD- 0"         

execute "~3 + \\X.(if not X>0/\\X<=3\\/X>~2 then 7*X else abs X fi)(~3)" [];;
- : string = "Krivine- -24 || SECD- -24" *)