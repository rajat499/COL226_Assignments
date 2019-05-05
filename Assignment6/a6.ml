exception InvalidReferenceInStack

let rec nth_in_list l n = if (n>0) then (match n with
                          |  1 -> List.hd l
                          |  x -> find_in_list (List.tl l) (n-1)
                          )
                            else raise InvalidReferenceInStack 


type frameElem = FuncName of string | LocalVblsList of ((string * answer * exptype) list) | RetAddr of int 
                 | StaticLink of int | ArgsList of ((string * answer * exptype) list)

type stack = frameElem list

type  exptree =
                V of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
                | Integer of int      (* Integer constant *)
                | Bool of bool     (* Boolean constant *);;

type commands = Assign of string * exptree | FuncCall of (string * (exptree list)) | Return

type exptype = Tint | Tbool

type answer = NumVal of int | BoolVal of bool

exception InvalidFunctionName
exception MainFunctionHasNoParent

let getParent s = match s with
                "P" -> "main"
              | "Q" -> "main"
              | "R" -> "P"
              | "S" -> "P"
              | "T" -> "Q"
              | "U" -> "Q"
              | "V" -> "R"
              | "W" -> "T"
              | "main" -> raise MainFunctionHasNoParent
              | _   -> raise InvalidFunctionName

exception MainCannotBeCalledDirectly
let callable a b = match a with
                | "P" -> b="P" || b="R" || b="S" || b="Q"
                | "Q" -> b="Q" || b="T" || b="U" || b="P"
                | "R" -> b="R" || b="V" || b="P" || b="S" || b="Q"
                | "S" -> b="S" || b="P" || b="R" || b="Q"
                | "T" -> b="T" || b="W" || b="Q" || b="U" || b="P"
                | "U" -> b="U" || b="Q" || b="T" || b="P"
                | "V" -> b="V" || b="R" || b="P" || b="S" || b="Q"
                | "W" -> b="W" || b="T" || b="Q" || b="U" || b="P"
                | "main" -> raise MainCannotBeCalledDirectly
                | _   -> raise InvalidFunctionName


let rec find_in_list x l = match l with
                          [] -> raise NotFound
                        | hd::tl -> let (s,a,e) = hd in
                                    if (s=x) then a else (find_in_list x tl)
