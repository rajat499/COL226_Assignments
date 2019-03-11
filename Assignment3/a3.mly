%{
    open A1
%}

/*
- Tokens (token name and rules) are modified wrt to A2. Please make necessary changes in A3
- LP and RP are left and right parenthesis
- Write grammar rules to recognize
  - >= <= from GT EQ LT tokens
  - if then else fi
*/
/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ EOF DEF DELIMITER
%start main
%type <A1.exptree> main /* Return type */
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/

main:
  expression DELIMITER {$1}
  | EOF                { Done }
;

expr:
  bool        {$1}
| int         {$1}
| conditional {$1}
| creation    {$1}
| projection  {$1}
;

bool:
  unaryBool   {$1}
| binaryBool  {$1}
;

int:
  unaryInt    {$1}
| binaryInt   {$1}
;

conditional:
  IF bool THEN expr ELSE expr FI {IfThenElse($2,$4,$6)}
;

creation: 
  DEF ID EQ LP tuple RP    {Tuple($5)}
;

projection:
  PROJ LP int COMMA int COMMA tuple  {Project($3,$5,Tuple($7))}
;
unaryBool:
  NOT bool     {Nega($2)}
| TRUE         {B(true)}
| FALSE        {B(false)}
;

binaryBool:
  bool AND bool   {Conjuction($1,$3)}
| bool OR bool    {Disjunction($1,$3)}
;

tuple:
  expr COMMA tuple  {let (x,y) = $3 in (x+1, $1::y)}
| expr              {(1,[$1])}
;