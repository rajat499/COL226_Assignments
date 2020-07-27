/* File parser.mly */
%{
    open Procedure
    exception BadToken
%}

%token EOF DELIMITER COLON LP RP TRUE FALSE EQ COMMA CALL RETURN EXIT
       CALLINGSTACK CALLABLEPROC ACCESSIBLEVBLS STATICLINK MAIN
%token <int> INT
%token <string> ID
%start main             /* the entry point */
%type <Procedure.commands> main   
%type <Procedure.exptree> constant                  /* Specifying the type to be returned for the grammar symbol main */
%%
main:
    proc DELIMITER                 { $1 }          /* $n on the rhs returns the value for nth symbol in the grammar on lhs */
    | EOF                              { raise BadToken }
;
proc:
      CALLABLEPROC { CallableProc }
    | RETURN       { Return }
    | EXIT         { Exit }
    | STATICLINK   { StaticLinkChain }
    | ACCESSIBLEVBLS { AccessibleVbls }
    | CALLINGSTACK { CallingStack }
    | assign       { $1 }
    | funcCall     { $1 }
;

assign:
    ID COLON EQ constant   {Assign($1, $4)}
;

funcCall:
    CALL ID LP argList RP        {FuncCall($2,$4)}
  | CALL MAIN                    {CallingMain}
;

argList:
    constant COMMA constant    {[$1;$3]}
|   constant COMMA argList     {$1::$3}
;

constant:
    ID       {V($1)}
  | INT      {Integer($1)}
  | TRUE     {Bool(true)}
  | FALSE    {Bool(false)}
;

/*TODO
 * Add support in the grammar for parenthesis
 *  - Adding the parenthesis should be able to change the parse tree to effectively modify precedence.
 *  E.g. 1+2*3  ==>        PLUS
 *                        /    \
 *                      NUM1   INTO
 *                            /    \
 *                         NUM 2  NUM 3
 *
 *  vs (1+2)*3  ==>        INTO
 *                        /    \
 *                     PLUS     NUM 3
 *                    /    \
 *                 NUM 1   NUM 2
 *
 * Try completing the calculator for basic arithmetic by adding division and subtraction, while respecting precedence
 * This will require changes right from the lexer.mll and parser.mly to the definition of print and evaluation functions in expression.ml
 *
 * ADVANCED
 * Try creating an expression for assigning new variables in the variable_set in the expression.ml file, so that they can be reused in a later evaluation statement.
 * E.g. myVar:=4.
 *      // Stores the integer value 4 corresponding to the string myVar in variable_set
 *
 *      myVar*3+1
 *      Answer: 13
 * */
