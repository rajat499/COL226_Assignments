{
  open Parser
  exception Not_implemented
  exception Invalid_Token of char
}

(*
  Below is a dummy implementation. Please note the following
  - Tokens are defined in A3.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file (test_a4.ml)
  - This is sample implementation. Please rewrite them as per the specifications
*)
(*Forming Regex for different Expressions.*)
let digit = ['0'-'9']
let sign = ['+' '-']
let integer = ('0' | (['1'-'9']digit*))
let openingParantheses = '('
let closingParantheses = ')'
let parantheses = openingParantheses | closingParantheses
let boolConst = ['T' 'F']
let lowerCase = ['a'-'z']
let upperCase = ['A'-'Z']
let letter = lowerCase|upperCase
let id = (letter)(letter|digit|'_')*
let delimiter = ';'
let whitespace = [' ' '\t' '\n']+

(*Lexing the string and classifying the tokens*)
rule read = parse
   eof             {EOF}
   | ';'           {DELIMITER}
   | ':'           {COLON}
   | '('           {LP}
   | ')'           {RP}
   | 'T'           {TRUE}
   | 'F'           {FALSE}
   | '='           {EQ}
   | ','           {COMMA}
   | "exit"        {EXIT}
   | "call"        {CALL}
   | "return"      {RETURN}
   | "callingStack" {CALLINGSTACK}
   | "callableProc" {CALLABLEPROC}
   | "accessVbls"  {ACCESSIBLEVBLS}
   | "staticLink"  {STATICLINK}
   | id as x       {ID(x)}
   | integer as n  { INT (int_of_string n) }
   | whitespace    {read lexbuf}
   | _ as c        { raise (Invalid_Token(c)) }
