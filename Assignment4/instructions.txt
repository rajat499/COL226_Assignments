Extend the language of Assignment 3 with Definitions and first-class functions

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
expressions using locally-scoped definitons let d in e end
expressions for function abstractions on one variable and function call:  \x.e and e1(e2)
Definitions are 
simple  def x = e 
sequential d1 ; d2
 parallel d1 || d2 
locally scoped local d1 in d2 end
After parsing the expression, you will have to build an abstract syntax tree.  The definition of the OCaml data type for representing the abstract syntax tree will be suggested in the test input file provided by the TAs.

You will also be give an OCaml data type for 

             Type = Tint | Tunit | Tbool |  t1 * .... * tn | t1 -> t2



Now build a type checker for this language.  You will be given a set of type assumptions G on variables. 

Write a function hastype that for any given expression e and a type t,   checks whether G |- e : t

And a function yields that for any definition d and incremental type assumption G',  checks whether G |- d :> G'

---

NOTE: For convenience, in this assignment, you can assume that defined variables as well as parameters of functions are given a type. That is definitions are
   def x: t = e 

and functions are \x:t.e



Alternatively you can try to infer the type by adding type-variables to Type ... and each time you have a declared variable or a parameter, assume its type is a fresh type-variable, for which you should then solve any constraints that arise from its use.