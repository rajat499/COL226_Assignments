open Procedure

(* This is the top level file for the expression evaluator. It is basically an infinite loop that consumes legal expression inputs
 * and prints their corresponding parse tree and evaluated output *)
let _ =
    Printf.printf "==> ";flush stdout;
    try
        let lexbuf = Lexing.from_channel stdin in
            while true do
            let result = Parser.main Lexer.read lexbuf in
            Procedure.main result;Printf.printf "==> "; flush stdout
            (* flush ensures that the evaluated information gets printed to stdout *)
            done
        with Lexer.Invalid_Token(s) -> print_string (String.make 1 s);exit 0
