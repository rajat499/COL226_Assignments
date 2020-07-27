In this assignment, you will DESIGN a grammar for a simple expression language, 
taking care to enforce precedence rules (e.g., BODMAS).

The language should contain the following types of expressions:  integers and booleans.

Expressions include:

variables (starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (')
integer constants
boolean constants
expressions using unary operators on integers: unary minus (~) , abs
expressions using unary operators on booleans: not
expressions using binary operators on integers: addition, subtraction, multiplication, div and mod (+, -, *, div, mod)
expressions using binary operators on booleans: conjunction, disjunction (/\, \/)
expressions using comparison operations on integers (=, >=, <=, >, <)
expressions using parenthesis: ( and )
a conditional expression if __ then __ else ___ fi
expressions for creating n-tuples (n >= 0)
expressions for projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n)

After parsing the expression, you will have to build an abstract syntax tree.  
The definition of the OCaml data type for representing the abstract syntax tree will be 
suggested in the test input file provided by the TAs.

Version 1 of the parser will return the abstract syntax tree.

Version 2 of the parser will evaluate the expression (after converting it to an abstract syntax tree), 
given a set of bindings of Values (integers, booleans and tuples) to Variables.  
Use the OCaml types for returning these answers.

Version 3 of the parser will compile the expression into opcodes.  
We have already discussed the opcodes in class.  
The OCaml data type for the opcodes will be specified in the test input files.