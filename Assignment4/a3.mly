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
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ
LET IN END BACKSLASH DOT DEF SEMICOLON PARALLEL LOCAL EOF
%start def_parser exp_parser
%type <A1.definition> def_parser /* Returns definitions */
%type <A1.exptree> exp_parser /* Returns expression */
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
  LET def IN or_expr END      {Let($2, $4)}
| BACKSLASH ID DOT or_expr    {FunctionAbstraction($2, $4)}
| or_expr LP or_expr RP       {FunctionCall($1, $3)}
| or_expr DISJ and_expr       {Disjunction($1, $3)}
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

/* Implement the grammar rules for definitions, which may use the parser for expression  */
def_parser:
  def EOF { $1 }
| EOF     {raise  BadToken}
;

def:
  simple_def          {$1}
| para_def            {$1}
| seq_def             {$1}
;

seq_def:
  sequence_def SEMICOLON simple_def {match $1 with 
                                      Sequence(l1) -> Sequence(l1@[$3])
                                      |_           -> Sequence($1::[$3])
                                    }
;

sequence_def:
 simple_def    {Sequence([$1])}
|para_def      {$1}
|sequence_def SEMICOLON simple_def  {let Sequence(l1) = $1 in Sequence(l1@[$3])}
;

para_def:
  parallel_def PARALLEL simple_def {match $1 with 
                                      Parallel(l1) -> Parallel(l1@[$3])
                                      |_           -> Parallel($1::[$3])
                                    }
;

parallel_def:
 simple_def    {Parallel([$1])}
|seq_def       {$1}
|parallel_def PARALLEL simple_def  {let Parallel(l1) = $1 in Parallel(l1@[$3])}
;

simple_def:
  DEF ID EQ or_expr           {Simple($2, $4)}
| LOCAL def IN def END        {Local($2, $4)}
;

