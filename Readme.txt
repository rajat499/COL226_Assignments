bignum - A BIGNUM package, arithmetic for arbitrarily large numbers, using lists of digits to implement an integer.

stack_machine - A simple definitional interpreter and stack machine, modeling the "abstract syntax" of a simple calculator language for integer expressions, and give it "semantic meaning" in terms of OCaml's built-in types. Implementing the calculator as a simple stack-based machine

lexer - Specifying the tokens for a simple arithmetic and boolean calculation language. A scanner for these tokens. 

parser -  A grammar for a simple expression language, taking care of precedence rules (e.g., BODMAS).

extend_lang - Extend the language of parser with definitions and first-class functions. More details in instructions.txt under the directory.

krivine_secd - Krivine Machine (in closure form), that implements Call-by-Name semantics. SECD machine that implements Call-by-Value semantics. Execute function that executes a compiled program.

procedural_lang - the core ideas (not the actual layout for any given architecture or language/compiler, and not the actual code generation) for understanding implementations of the (static/lexical) scoping discipline in Algol-like languages, particularly the visibility rules and the set-up/tear-down involved in procedure call and return. 