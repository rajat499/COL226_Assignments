Consider a tiny language consisting of expressions that are

e ::= x | \x:t.e | e_1 (e_2)

{

In addition, to make things interesting, you should add the booleans T and F, and an if_then_else expression.

You can also introduce the natural numbers, addition (perhaps multiplication) and comparison with 0.

}

For simplicity, you can keep the types as 

Type::=   Tbool | t1 -> t2

and include Tunit,  Tint and Cartesian products  if you wish.

You will need to define an abstract syntax, and a front end.  

You should implement a type-checker, so that you don't execute any ill-typed programs.



For the language, you should design and implement (in OCaml) the following abstract machines

1.  The Krivine Machine (in closure form), that implements Call-by-Name semantics.

For this you need to consider Closures as a pair of an expression and a Table, where a Table is a partial function 
from variables to Closures (including value closures).



2. The SECD machine that implements Call-by-Value semantics.

For this you need value closures in the set of Answers. 

You also need to implement the compile function from the syntax of the language to opcodes of the SECD machine. 



2a.  You will need to implement the execute function that executes a compiled program.

3.  Most importantly, you need to provide inputs that demonstrate that your implementations of the two machines run correctly.  
You will be graded on the quality of programs you can execute. 
For ease of demonstration, you may give a program that is of base type (Tint or Tbool).

3a. In case you implement recursion, for the extended languages you should be able to run factorial or fibonacci functions.