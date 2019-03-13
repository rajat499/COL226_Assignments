%{
    open A1
    exception BadToken
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
    or_expr EOF        {$1}
  | EOF                { raise BadToken }
;

or_expr:
  or_expr DISJ and_expr       {Disjunction($1, $3)}
| and_expr                    {$1}
;

and_expr:
  and_expr CONJ not_expr      {Conjunction($1, $3)}
| not_expr                    {$1}
;

not_expr:
  NOT comparison           {Not($2)}
| comparison               {$1}
;

comparison:
  add_expr EQ add_expr     {Equals($1, $3)}
| add_expr GT EQ add_expr  {GreaterTE($1, $4)} 
| add_expr GT add_expr     {GreaterT($1, $3)}
| add_expr LT EQ add_expr  {LessTE($1, $4)}
| add_expr LT add_expr     {LessT($1, $3)}
| add_expr                 {$1}
;

add_expr:
  add_expr PLUS sub_expr      {Add($1,$3)}
| sub_expr                    {$1}
;

sub_expr:
  sub_expr MINUS mult_expr    {Sub($1,$3)}
| mult_expr                   {$1}
;

mult_expr:
  mult_expr TIMES div_expr    {Mult($1,$3)}
| div_expr                    {$1}
;

div_expr:
  div_expr DIV rem_expr       {Div($1,$3)}
| rem_expr                    {$1}
;

rem_expr:
  rem_expr REM abs_neg_expr   {Rem($1,$3)}
| abs_neg_expr                {$1}
;

abs_neg_expr:
  ABS abs_neg_expr            {Abs($2)}
| TILDA abs_neg_expr          {Negative($2)}
| conditional                 {$1}
;

conditional:
  IF or_expr THEN or_expr ELSE or_expr FI {IfThenElse($2,$4,$6)}
| projection                              {$1}
;

projection:
  PROJ LP INT COMMA INT RP conditional {Project(($3,$5),$7)}
| tuple                                {$1}
;

tuple:
  LP RP                        {Tuple(0,[])}
| LP tuple_exptree_list RP     {let x = $2 in Tuple(List.length x, x) }
| paren                        {$1}
;

tuple_exptree_list:
  or_expr COMMA or_expr               {$1::[$3]}
| or_expr COMMA tuple_exptree_list    {$1::$3}
;

paren:
  LP or_expr  RP              {InParen($2)} 
| constant                    {$1}
;

constant:
  INT                         {N($1)}
| BOOL                        {B($1)}
| ID                          {Var($1)}
;

/* creation: 
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
; */