{
  open A3
  exception Invalid_Token
}

(*
  Below is a dummy implementation. Please note the following
  - Tokens are defined in A3.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file
*)

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
let id = (upperCase)(letter|digit|'_'|'\'')*
let defConstruct = "def"
let delimiter = ';'
let whitespace = [' ' '\t' '\n']+

rule read = parse
   eof             {EOF}
   | 'T'           {TRUE}
   | 'F'           {FALSE}
   | "abs"         {ABS}
   | ','           {COMMA}
   | '+'           {PLUS}
   | '-'           {MINUS}
   | '*'           {MUL}
   | "div"         {DIV}
   | "mod"         {MOD}
   | '^'           {EXP}
   | '('           {LP}
   | '~'           {NEGA}
   | ')'           {RP}
   | "not"         {NOT}
   | "/\\"         {AND}
   | "\\/"         {OR}
   | '='           {EQ}
   | '>'           {GTA}
   | '<'           {LTA}
   | ">="          {GEQ}
   | "if"          {IF}
   | "then"        {THEN}
   | "else"        {ELSE}
   | "def"         {DEF}
   | ";"           {DELIMITER}
   | "fi"          {FI}
   | id as x       {ID(x)}
   | integer as n  { INT (int_of_string n) }
   | whitespace    {read lexbuf}
   | _             { raise Invalid_Token }
