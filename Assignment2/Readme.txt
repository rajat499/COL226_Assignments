In this assignment, you will specify the tokens for a simple arithmetic and boolean calculation language.
You will then generate a scanner for these tokens.  

Provide enough examples and counter examples to show you correctly accept and classify tokens.

The expressions in the language are of the following forms

Integer constants, which have an optional sign, followed by at least one digit, without useless leading zeroes.
Unary arithmetic operations: abs, 
Binary operations: + (addition), - (subtraction), * (multiplication), div, mod, ^ (exponentiation)
Parentheses: (, )
Boolean constants: T and F
Unary boolean operation: not
Binary boolean operations:  /\ (and), \/ (or)
Comparison operators: = (equal) , > (greater than), < (less than) , >= (greater or equal), <= (less or equal)
A conditional operator consisting of three tokens: if then else
Identifiers, which are alphanumeric strings beginning with lower-case letter.
A definition construct: def
A delimiter to terminate the expression: ;
Remember to discard white space.


type token =
--   INT of int          (* integer constant, positive or negative w/o leading zeros *)
--|  TRUE                (* boolean constant "T" *)
--|  FALSE               (* boolean constant "F" *)
--|  ABS                 (* unary operator, "abs" *)
--|  PLUS                (* arithmetic plus, "+" *)
--|  MINUS               (* arithmetic minus, "-" *)
--|  MUL                 (* arithmetic multiply, "*" *)
--|  DIV                 (* integer div, "div" *)
--|  MOD                 (* remainder, "mod" *)
--|  EXP                 (* exponentiation, "^" *)
--|  LP                  (* left paren, "(" *)
--|  RP                  (* right paren, ")" *)
--|  NOT                 (* boolean NOT, "not" *)
--|  AND                 (* boolean AND, "/\ " *)
--|  OR                  (* boolean OR, "\/" *)
--|  EQ                  (* equal to, "=" *)
--|  GTA                 (* greater than, ">" *)
--|  LTA                 (* less than, "<" *)
--|  GEQ                 (* greater than/equal to, ">=" *)
--|  LEQ                 (* less than/equal to, "<=" *)
--|  IF                  (* keyword "if" *)
--|  THEN                (* keyword "then" *)
--|  ELSE                (* keyword "else" *)
--|  ID of string        (* variable identifier, alphanumeric string with first char lowercase *)
--|  DEF                 (* definition construct, "def" *)
--|  DELIMITER;;         (* delimiter, ";" *)