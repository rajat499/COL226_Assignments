{
  open A3
  exception Not_implemented
  exception Invalid_Token
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

(*Lexing the string and classifying the tokens*)
rule read = parse
   eof             {EOF}
   | 'T'           {BOOL(true)}
   | 'F'           {BOOL(false)}
   | "abs"         {ABS}
   | "proj"        {PROJ}
   | ','           {COMMA}
   | '+'           {PLUS}
   | '-'           {MINUS}
   | '*'           {TIMES}
   | "div"         {DIV}
   | "mod"         {REM}
   | '('           {LP}
   | '~'           {TILDA}
   | ')'           {RP}
   | "not"         {NOT}
   | "/\\"         {CONJ}
   | "\\/"         {DISJ}
   | '='           {EQ}
   | '>'           {GT}
   | '<'           {LT}
   | '\\'          {BACKSLASH}
   | '.'           {DOT}
   | ';'           {SEMICOLON}
   | "if"          {IF}
   | "then"        {THEN}
   | "else"        {ELSE}
   | "def"         {DEF}
   | "let"         {LET}
   | "end"         {END}
   | "in"          {IN}
   | "||"          {PARALLEL}
   | "local"       {LOCAL}
   | "fi"          {FI}
   | id as x       {ID(x)}
   | integer as n  { INT (int_of_string n) }
   | whitespace    {read lexbuf}
   | _             { raise Invalid_Token }
