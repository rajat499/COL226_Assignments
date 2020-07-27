%{
    open A5
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
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI
BACKSLASH DOT CMP EOF
%start exp_parser
%type <A5.expr> exp_parser /* Returns expression */
/* %type <A1.exptype> type_parser Returns exptype */
%%
/* The grammars written below are dummy. Please rewrite it as per the specifications. */

/*The grammar is written as per the precedence rule:
InParen > Tuple > Project > IfThenElse > (Negative = Abs) > (Div = Mult = Rem) > (Add = Sub) > (GreaterT = LessT = GreaterTE = LessTE = Equals) > Not > Conjunction > Disjunction
Certain rules are enforced in grammar depending upon what kind of expression a token should parse.*/
exp_parser:
    or_expr EOF        {$1}
  | EOF                { raise BadToken }
;

or_expr:
  or_expr DISJ and_expr       {Or($1, $3)}
| and_expr                    {$1}
;

and_expr:
  and_expr CONJ not_expr      {And($1, $3)}
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
| CMP add_expr             {Cmp($2)}
| add_expr                 {$1}
;

add_expr:
  add_expr PLUS sub_expr      {Plus($1,$3)}
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
  IF or_expr THEN or_expr ELSE or_expr FI {If_Then_Else($2,$4,$6)}
| func_call                               {$1}
;

func_call:
  func_call LP or_expr RP    {App($1, $3)}
| func_abs                    {$1}   
;

func_abs:
  BACKSLASH ID DOT func_abs   {Lambda($2, $4)}
| paren                       {$1}
;

paren:
  LP or_expr  RP              {InParen($2)} 
| constant                    {$1}
;

constant:
  INT                         {Integer($1)}
| BOOL                        {Bool($1)}
| ID                          {V($1)}
;

