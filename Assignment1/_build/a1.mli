(*  Assignment 1: A simple definitional interpreter and stack machine
In this assignment, you will model the "abstract syntax" of a simple calculator language for 
integer expressions, and give it "semantic meaning" in terms of OCaml's built-in types, 
and in the second part, implement the calculator as a simple stack-based machine, 
for which we have opcodes into which we compile the abstract syntax of an expression.
*)

open A0
   (* abstract syntax  *)
  type  exptree =  N of int
    | Plus of exptree * exptree
    | Minus of exptree * exptree
    | Mult of exptree * exptree
    | Div of exptree * exptree
    | Rem of exptree * exptree
    | Nega of exptree (* Neg is for sign in BigInt. Nega is negative of expression  *)
    | Abs of exptree

  (* opcodes of the stack machine *)
  type opcode = CONST of bigint | PLUS | TIMES | MINUS | DIV | REM | ABS | UNARYMINUS

  (* the definitional interpreter *)
  val eval : exptree -> int

  (* For simplicity the stack will be implemented as a list of 
  bigint values, and the program is a list of opcodes.  
  If you have not implemented the bigint package, you may use int for 
  reduced credits. The stack machine is to be defined as a 
  tail-recursive function *)
  val stackmc: (bigint list) -> (opcode list) -> bigint

  (* The compile function is simply a postorder traversal of an 
  abstract syntax tree of an expression. 
  The compiler is to be defined as a recursive function *)
  val compile: exptree -> opcode list

(* Instructions for submission:
Same instructions as Assignment 0

Submit structure_a1.ml as follows
open Structure_a0.A0 (* Should be in same folder*)
open Signature_a1 (* Should be in same folder*)
module A1 : CalculatorLanguage = struct
(* code goes here *)
end

Keep signature_a0.mli. structure_a0.ml and signature_a1.mli in the same folder
 A. Compile as
    ocamlc signature_a0.mli structure_a0.ml signature_a1.mli
    This creates a .cmi, .cmo, .out files
 B. Use the top level to test your code
    #use "structure_a1.ml";;
    open A1;;
    eval (N 0);;
*)
