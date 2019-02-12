{
(*Define Tokens.*)    
type token =
   INT of int          (* integer constant, positive or negative w/o leading zeros *)
|  TRUE                (* boolean constant "T" *)
|  FALSE               (* boolean constant "F" *)
|  ABS                 (* unary operator, "abs" *)
|  PLUS                (* arithmetic plus, "+" *)
|  MINUS               (* arithmetic minus, "-" *)
|  MUL                 (* arithmetic multiply, "*" *)
|  DIV                 (* integer div, "div" *)
|  MOD                 (* remainder, "mod" *)
|  EXP                 (* exponentiation, "^" *)
|  LP                  (* left paren, "(" *)
|  RP                  (* right paren, ")" *)
|  NOT                 (* boolean NOT, "not" *)
|  AND                 (* boolean AND, "/\ " *)
|  OR                  (* boolean OR, "\/" *)
|  EQ                  (* equal to, "=" *)
|  GTA                 (* greater than, ">" *)
|  LTA                 (* less than, "<" *)
|  GEQ                 (* greater than/equal to, ">=" *)
|  LEQ                 (* less than/equal to, "<=" *)
|  IF                  (* keyword "if" *)
|  THEN                (* keyword "then" *)
|  ELSE                (* keyword "else" *)
|  ID of string        (* variable identifier, alphanumeric string with first char lowercase *)
|  DEF                 (* definition construct, "def" *)
|  DELIMITER;;         (* delimiter, ";" *)

   exception InvalidToken of char;;                   (*Exception Definition*)

   let lastExp = ref "";;                             (*A Variable to store what Token Was stored last in the string.*)
 
   let initialize () =                                (*A function to initialize the variable, everytime scanner function is called.*) 
                      lastExp:= "";;
}

(*Forming Regex for different Expressions.*)
let digit = ['0'-'9']
let sign = ['+' '-']
let integer = (sign)?('0' | (['1'-'9']digit*)) 
let unaryArithOp = "abs"
let binaryArithOp = ['+' '-' '*' '/' '%' '^']
let openingParantheses = '('
let closingParantheses = ')'
let parantheses = openingParantheses | closingParantheses
let unaryBoolOp = "not"
let boolConst = ['T' 'F']
let binaryBoolOp = "/\\" | "\\/"
let comp = "=" | ">" | "<" | ">=" | "<="
let condOp = "if" | "then" | "else"
let lowerCase = ['a'-'z']
let upperCase = ['A'-'Z']
let letter = lowerCase|upperCase
let id = (lowerCase)(letter|digit)*
let defConstruct = "def"
let delimiter = ';'
let whitespace = [' ' '\t' '\n']+


(*Parsing the string and classifying the tokens. Variable lastExp is everytime updated.*)
rule classifyToken = parse 
 
  "abs"              { 
                        lastExp := "UnaryOp";
                        (ABS :: (classifyToken lexbuf))          
                     }

| ';'                {
                        lastExp := "Delimiter";
                        (DELIMITER :: (classifyToken lexbuf))
                     }

| "def"              { 
                        lastExp:="Construct";
                        (DEF :: (classifyToken lexbuf))
                     }

| '='                { 
                        lastExp:="Comparison";
                        (EQ :: (classifyToken lexbuf))
                     }

| '>'                { 
                        lastExp:="Comparison";
                        (GTA :: (classifyToken lexbuf))
                     }

| '<'                { 
                        lastExp:="Comparison";
                        (LTA :: (classifyToken lexbuf))
                     }

| ">="               { 
                        lastExp:="Comparison";
                        (GEQ :: (classifyToken lexbuf))
                     }

| "<="               { 
                        lastExp:="Comparison";
                        (LEQ :: (classifyToken lexbuf))
                     }

| "if"               {
                        lastExp:="Conditional";
                        (IF :: (classifyToken lexbuf))
                     }

| "then"             {
                        lastExp:="Conditional";
                        (THEN :: (classifyToken lexbuf))
                     }

| "else"             {
                        lastExp:="Conditional";
                        (ELSE :: (classifyToken lexbuf))
                     }

| 'T'                {
                        lastExp:="BoolConst";
                        (TRUE :: (classifyToken lexbuf))
                     }

| 'F'                {
                        lastExp:="BoolConst";
                        (FALSE :: (classifyToken lexbuf))
                     }

| "not"              {
                        lastExp:="UnaryBooleanOp";
                        (NOT :: (classifyToken lexbuf))
                     }

| "/\\"              {
                        lastExp:="BinaryBooleanOp";
                        (AND :: (classifyToken lexbuf))
                     }

| "\\/"              {
                        lastExp:="BinaryBooleanOp";
                        (OR :: (classifyToken lexbuf))
                     }

| '+'                {
                        lastExp:="BinaryArithmeticOp";
                        (PLUS :: (classifyToken lexbuf))
                     }

| '-'                {
                        lastExp:="BinaryArithmeticOp";
                        (MINUS :: (classifyToken lexbuf))
                     }

| '*'                {
                        lastExp:="BinaryArithmeticOp";
                        (MUL :: (classifyToken lexbuf))
                     }

| '^'                {
                        lastExp:="BinaryArithmeticOp";
                        (EXP :: (classifyToken lexbuf))
                     }

| "div"              {
                        lastExp:="BinaryArithmeticOp";
                        (DIV :: (classifyToken lexbuf))
                     }

| "mod"              {
                        lastExp:="BinaryArithmeticOp";
                        (MOD :: (classifyToken lexbuf))
                     }

| ')'                {
                        lastExp:="Parantheses";
                        (RP :: (classifyToken lexbuf))
                     }

| '('                {
                        lastExp:="Parantheses";
                        (LP :: (classifyToken lexbuf))
                     }

| whitespace         {
                        lastExp:="WhiteSpace";
                        classifyToken lexbuf
                     }


| integer as s       {
                        if((!lastExp) = "Integer") then
                          (
                              (*If previous token is an integer then the sign of the next token(if integer), its sign will be treated as
                              operator rather than sign.*)
                              lastExp:="Integer";
                              if(s.[0]='-') then (MINUS::(INT(int_of_string (String.sub s 1 ((String.length s) -1)))::(classifyToken lexbuf)))
                              else if(s.[0]='+') then (PLUS::(INT(int_of_string (String.sub s 1 ((String.length s) -1)))::(classifyToken lexbuf)))
                              else (INT(int_of_string(s)) :: (classifyToken lexbuf))
                          )              
                        else
                          (
                              (*Ocaml's Older version doesn't except int_of_string("+45"), therefore handling that by ignoring first character if it is '+'.*)
                              lastExp := "Integer";
                              if(s.[0]='+') then (INT(int_of_string (String.sub s 1 ((String.length s) -1)))::(classifyToken lexbuf))
                              else (INT(int_of_string(s)) :: (classifyToken lexbuf))                          
                          )
                     }
                     
| id as s            { 
                        lastExp:="Identifier";
                        (ID(s) :: (classifyToken lexbuf))
                     }

| _ as s             { 
                        raise (InvalidToken(s)) 
                     }

| eof                {
                       []
                     }

{
    (*Scanner Function to clssify tokens into a list*)
    let scanner s = initialize();
                      classifyToken (Lexing.from_string s);;
}



(*===============Examples and Counter-Examples=============

Since Ocaml's parser creates error(Unterminated String) 
if double quotes is used in comments, therefore all double quotes are escape-sequenced

# scanner "abs";;
- : token list = [ABS]
# scanner "abs3";;
- : token list = [ID "abs3"]
# scanner "abs 3";;     
- : token list = [ABS; INT 3]
# scanner "def3";;                      
- : token list = [ID "abs3"]
# scanner "def 3";;     
- : token list = [DEF; INT 3]
# scanner "not4";;            
- : token list = [ID "not4"]
# scanner "not 4";;
- : token list = [NOT; INT 4]

# scanner "4not3absT";;
- : token list = [INT 4; ID "not3absT"]  
# scanner "4not 3abs T";;
- : token list = [INT 4; NOT; INT 3; ABS; TRUE] 
# scanner "52 - 5";;
- : token list = [INT 52; MINUS; INT 5] 
# scanner "52 -5";;
- : token list = [INT 52; INT (-5)] 
# scanner "52-5";;
- : token list = [INT 52; MINUS; INT 5]

# scanner "52div5";;
- : token list = [INT 52; ID "div5"]
# scanner "52 div 5";;
- : token list = [INT 52; DIV; INT 5]
# scanner "52div 5";;
- : token list = [INT 52; DIV; INT 5] 
same will go for mod.

# scanner "5*(3+4)";;
- : token list = [INT 5; MUL; LP; INT 3; PLUS; INT 4; RP]
# scanner "-56*(21+4)^2";;
- : token list = [INT (-56); MUL; LP; INT 21; PLUS; INT 4; RP; EXP; INT 2] 
# scanner "rajat;a>=b;if(a<=5)thenx=5";
- : token list = [ID "rajat"; DELIMITER; ID "a"; GEQ; ID "b"; DELIMITER; IF; LP; ID "a"; LEQ; INT 5; RP; ID "thenx"; EQ; INT 5] 
# scanner "rajat;a>=b;if(a<=5)then x=5";
- : token list = [ID "rajat"; DELIMITER; ID "a"; GEQ; ID "b"; DELIMITER; IF; LP; ID "a"; LEQ; INT 5; RP; THEN; ID "x"; EQ; INT 5] 

# scanner "Ax123";;
Exception: InvalidToken 'A'.
# scanner "#00123";;
Exception: InvalidToken '#'.

# scanner "001123";;
- : token list = [INT 0; INT 0; INT 1123]
# scanner "--45";;
- : token list = [MINUS; INT (-45)] 

# scanner "TrueFalse";;
- : token list = [TRUE; ID "rueFalse"] 
# scanner "True False";;
- : token list = [TRUE; ID "rue"; FALSE; ID "alse"]

# scanner "if(a=5) then a=10; else a=12;";; 
- : token list = [IF; LP; ID "a"; EQ; INT 5; RP; THEN; ID "a"; EQ; INT 10; DELIMITER; ELSE; ID "a"; EQ; INT 12; DELIMITER]
# scanner "if(a=5/\\b=10)";;
- : token list = [IF; LP; ID "a"; EQ; INT 5; AND; ID "b"; EQ; INT 10; RP]

# scanner ">>>>>>>=<=<<<<<<<= =";;
- : token list =                                                                                         [GTA; GTA; GTA; GTA; GTA; GTA; GEQ; LEQ; LTA; LTA; LTA; LTA; LTA; LTA; LEQ; EQ]

# scanner "a>5";;
- : token list = [ID "a"; GTA; INT 5]
# scanner ">a5";;
- : token list = [GTA; ID "a5"]
# scanner ">a=5";;
- : token list = [GTA; ID "a"; EQ; INT 5]

*)
