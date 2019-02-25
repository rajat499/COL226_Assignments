open Signature_a0
module A0 : BigInt = struct
  

(* Type Declaration*)

type sign = Neg | NonNeg;;

type bigint = (sign * int list);;


(*Converting an integer to BigInt. First Convert Ocaml's Int to a list, where
head contains the most significant digit*)

let rec convert_to_list n = match n with 
                        0 -> []
                    |   x -> (convert_to_list (x/10)) @ ((x mod 10)::[]);;

let mk_big n = if n<0 then let sign = Neg in
                    (sign, (convert_to_list (n*(-1))))
               else let sign = NonNeg in
                    (sign, (convert_to_list (n)))
                    ;;


(*Convert a Big int to String for printing by concatenating all the elements of the list in order.*)

let rec convert_to_string l = match l with
                         [] -> ""
                    |    hd :: tl -> (string_of_int hd) ^ (convert_to_string tl);;

let print_num bigN = match bigN with
                    (_, []) -> "0"
                  | (Neg, l) -> "-" ^ (convert_to_string l) 
                  | (NonNeg, l) -> (convert_to_string l);;



(*Converting Bigint to Ocaml's int type. Firstly evaluate the list and then add the sign
===== Warning - It may lead to loss of data if bigint is out of range of Ocaml's int =====
*)
let rec eval_list l c = match l with
                    [] -> c
               |    hd :: tl -> eval_list tl (c*10 + hd);;


let eval b = match b with
             (NonNeg, l) -> (eval_list l 0)
          |  (Neg, l) -> (-1)*(eval_list l 0);;


(*Adding n zeroes in front of the list*)
let rec add_zeroes n l = match n with
                    0 -> l
               |    n -> 0 :: (add_zeroes (n-1) l);;


(*Removing all the leading zeroes in a list*)
let rec remove_leading_zeroes l = match l with 
                              [] -> []
                         |    hd :: tl -> if (hd=0) then (remove_leading_zeroes tl)
                                          else l;;


(*Exception declaration*)
exception ListsNotOfSameLength;;


(*Making two lists of same length by adding appropriate amount of zeroes in
front of smaller list to make it of length of larger list*)
let rectify_list l1 l2 = let len1 = List.length l1 in
                         let len2 = List.length l2 in

                         if (len1>len2) then (l1, add_zeroes (len1-len2) l2)
                         else (add_zeroes (len2-len1) l1, l2 );;





(*===========LOGICAL OPERATIONS ON BIGINT============*)

(*Check if two lists of same length are equal or not*)
let rec eq_list l1 l2 = match (l1, l2) with
                       ([], []) -> true
                    |  (h1::t1, h2::t2) -> (h1=h2) && (eq_list t1 t2)
                    |  (_, _) -> raise ListsNotOfSameLength;;

(*Check if two Bigints are equal or not*)
let eq b1 b2 = let (s1, l1) = b1 in
               let (s2, l2) = b2 in
               if ((l1=[]) && (l2=[])) then true
               else
                    let (l1, l2) = rectify_list l1 l2 in
                    match(s1, s2) with
                    (Neg, NonNeg) -> false
               |    (NonNeg, Neg) -> false
               |    (NonNeg, NonNeg) -> eq_list l1 l2
               |    (Neg, Neg) -> eq_list l1 l2;;



(*Check if list1 is greater than list2 or not, both lists should be of same length
and head of the list is the most significant element or digit*)
let rec gt_list l1 l2 = match (l1, l2) with
                    ([], []) -> false
               |    (h1::t1, h2::t2) -> if (h1>h2) then true else if (h1<h2) then false else (gt_list t1 t2)
               |    (_,_) -> raise ListsNotOfSameLength;;

(*Check if Bigint b1 is greater than Bigint b2 or not*)
let gt b1 b2 = let (s1, l1) = b1 in
               let (s2, l2) = b2 in

               if (l1=[]) && (l2=[]) then false
               else
                    let (l1, l2) =  (rectify_list l1 l2) in
                    match (s1, s2) with
                    (NonNeg, Neg) -> true
               |    (Neg, NonNeg) -> false
               |    (NonNeg, NonNeg) -> gt_list l1 l2
               |    (Neg, Neg) -> if ((gt_list l1 l2) = false) then true else false;;

(*Check if Bigint b1 is greater than or equal to Bigint b2 or not*)
let geq b1 b2 = (gt b1 b2) || (eq b1 b2);;




(*Check if list1 is less than list2 or not, both lists of same length and 
head of the list is the most significant element or digit*)
let rec lt_list l1 l2 = match (l1, l2) with
                    ([], []) -> false
               |    (h1::t1, h2::t2) -> if (h1<h2) then true else if (h1>h2) then false else (lt_list t1 t2)
               |    (_,_) -> raise ListsNotOfSameLength;;

(*Check if Bigint b1 is less than Bigint b2 or not*)
let lt b1 b2 = let (s1, l1) = b1 in
               let (s2, l2) = b2 in

               if (l1=[]) && (l2=[]) then false
               else
                    let (l1, l2) =  (rectify_list l1 l2) in

                    match (s1, s2) with
                    (NonNeg, Neg) -> false
               |    (Neg, NonNeg) -> true
               |    (NonNeg, NonNeg) -> lt_list l1 l2
               |    (Neg, Neg) -> if ((lt_list l1 l2) = false) then true else false;;

(*Check if Bigint b1 is less than or equal to Bigint b2 or not*)
let leq b1 b2 = (lt b1 b2) || (eq b1 b2);;




(*===========ARITHMETIC OPERATIONS ON BIGINT============*)
(*Before implementing operations, add_list and sub_list, use rectify_list to make both lists of same length *)



(*Subtracting two lists of same length, starting from head and by propogating the carry forward*)
let rec sub_list l1 l2 c = match (l1, l2) with
                         ([], []) -> if c=1 then (Neg, []) else (NonNeg, [])
                    | (h1::t1, h2::t2) -> let x = h1-h2-c in
                                           let carry = if (x<0) then 1 else 0 in
                                           let (sign, list) = sub_list t1 t2 carry in
                                           (sign, (carry*10 + x)::list)
                    | (_,_) -> raise ListsNotOfSameLength;;


(*add two lists of same length by carry propogation method, starting with head*)
let rec add_list l1 l2 c = match (l1, l2) with
                         ([], []) -> if c=0 then [] else [c]
                    |    (h1::t1, h2::t2) -> let x = h1+h2+c in
                                             (x mod 10) :: (add_list t1 t2 (x/10))
                    | (_, _) -> raise ListsNotOfSameLength ;;




(*Subtracting a bigint b2 from bigint b1
In case b1-b2 is negative, sub function will calculate b2-b1 and return it with a Neg sign
In case one bigint is NonNeg and other is Neg then call the add_list function on both list and
add the corresponding sign*)
let sub b1 b2 = let (s1, l1) = b1 in
                    let (s2, l2) = b2 in
                    let (l1, l2) = rectify_list l1 l2 in

                    match (s1, s2) with
                    (NonNeg, Neg) -> (NonNeg, remove_leading_zeroes (List.rev (add_list (List.rev l1) (List.rev l2) 0)))
               
               |    (Neg, NonNeg) -> (Neg, remove_leading_zeroes (List.rev (add_list (List.rev l1) (List.rev l2) 0)))
               
               
               |    (Neg, Neg) ->       let (sign, list) = (sub_list (List.rev l1) (List.rev l2) 0) in
                                        if (sign=NonNeg) then (Neg, remove_leading_zeroes (List.rev list))
                                        else let (pseudo_sign, correct_list) = (sub_list (List.rev l2) (List.rev l1) 0) in
                                             (pseudo_sign, remove_leading_zeroes (List.rev correct_list) )(*check here*)
               
               |    (NonNeg, NonNeg) -> let (sign, list) = (sub_list (List.rev l1) (List.rev l2) 0) in
                                        if (sign=NonNeg) then (sign, remove_leading_zeroes (List.rev list))
                                        else let (pseudo_sign, correct_list) = (sub_list (List.rev l2) (List.rev l1) 0) in
                                             (sign, remove_leading_zeroes (List.rev correct_list));;
               

(*Adding a bigint b1 to bigint b2
In case one bigint is NonNeg and other is Neg then call the sub function with corresponding argument*)
let add b1 b2 = let (s1, l1) = b1 in
                let (s2, l2) = b2 in
                let (l1, l2) = rectify_list l1 l2 in

                match (s1, s2) with 
                 (NonNeg, NonNeg) -> (NonNeg, remove_leading_zeroes (List.rev (add_list (List.rev l1) (List.rev l2) 0)))
               
               | (Neg, Neg) -> (Neg, remove_leading_zeroes (List.rev (add_list (List.rev l1) (List.rev l2) 0)))
               
               | (NonNeg, Neg) -> sub (NonNeg, l1) (NonNeg, l2)
               | (Neg, NonNeg) -> sub (NonNeg, l2) (NonNeg, l1);;




(*Multiplies a list with a digit between (0-9) and propogates the carry forward
.In the end the carry is added to the end of list.
It starts by multiplying the head of list. Initially while calling the function, pass carry=0*)
let rec mult_list_with_element l1 element carry= match l1 with 
                                    [] -> if (carry=0) then [] else [carry]
                              |    hd :: tl ->  let x = (hd*element) + carry in
                                                (x mod 10) :: (mult_list_with_element tl element (x/10));;


(*Multiplies a list l1 with a list l2 and returns a list of int list, 
in which each list corresponds to multiplication of l1 with each element of l2.*)
let rec mult_list_of_list l1 l2 = match l2 with
                         [] -> []
                    |    hd::tl -> (List.rev (mult_list_with_element (List.rev l1) hd 0)) :: (mult_list_of_list l1 tl);;



(*Takes a list of int list and adds all of them(by adding appropriate amount of trailing zeroes at end of each list) 
and return a single int list.*)
let rec add_multiple_lists l = match l with
                              [] -> []
                         |    hd :: tl -> let n = (List.length l) in
                                          let hd = List.rev (add_zeroes (n-1) (List.rev hd)) in
                                          let list2 = add_multiple_lists tl in
                                          let (hd, list2) = rectify_list hd list2 in
                                          List.rev (add_list (List.rev hd) (List.rev list2) 0);;

(*Multiplies a bigint b1 with bigint b2 and returns a bigint with appropriate sign*)
let mult b1 b2 = let (s1, l1) = b1 in
                 let (s2, l2) = b2 in
                 let list_of_list = (mult_list_of_list l1 l2) in
                 let ans = (remove_leading_zeroes (add_multiple_lists list_of_list) ) in

                 match (s1, s2) with
                 (Neg, Neg) -> (NonNeg, ans)
               | (NonNeg, NonNeg) -> (NonNeg, ans)
               | (Neg, NonNeg) -> (Neg, ans)
               | (NonNeg, Neg) -> (Neg, ans);;




(* Exception Declaration to avoid division of a number by zero *)
exception DivisionByZeroNotAllowed;;

(*Divides a list l1 with list l2.
It doesn't use subtraction method.
Instead checks for quotient in powers of 2. If a quotient lies between 2^(n-1)
and 2^n then it subtracts l2*(2^(n-1)) from l1 and restarts div_list on
(l1 - (l2*2^(n-1))) and adds the result of this recursion to 2^(n-1).
It returns a tuple with (quotient, remainder) *)
let rec div_list l1 l2 prev next = match l1 with 
                                   [] -> (prev, [])
                              |   _ -> let x = (mult (NonNeg, l2) next) in
                                   let (s, l) = sub (NonNeg, l1) x in
                                   
                                   if (l=[]) then (next, l)
                                   else if (s = Neg) then 
                                                            ( if (lt (NonNeg,l) (NonNeg, l2)) then 
                                                                 (    let final_ans = sub next (mk_big 1) in
                                                                      let (useless_sign, rem) = sub (NonNeg, l1) (mult final_ans (NonNeg, l2)) in 
                                                                      (final_ans, rem)
                                                                 ) 
                                                            else 
                                                                 (    let (y, list) = (sub (NonNeg ,l1) (mult (NonNeg, l2) prev) ) in
                                                                      let ( remaining_ans, rem ) = ( div_list list l2 (mk_big 0) (mk_big 1) ) in  
                                                                      ((add prev remaining_ans) , rem)  
                                                                 )
                                                            )
                                   else ( 
                                             if (lt (NonNeg, l) (NonNeg, l2)) then 
                                                  (next, l) 
                                             else 
                                                  ( div_list l1 l2 next (mult next (mk_big 2)))
                                             )
                              ;;

(* Returns the quotient with appropriate sign when Bigint b1 is divided by Bigint b2.*)
let div b1 b2 = let (s1, l1) = b1 in
                let (s2, l2) = b2 in
               
                if (l2=[]) then raise DivisionByZeroNotAllowed
                else
                    let (b, rem_list) = div_list l1 l2 (mk_big 0) (mk_big 1) in 
                    let (s, ans_list) = b in
                    match (s1, s2) with
                    (Neg, Neg) -> (NonNeg, ans_list)
               |     (NonNeg, NonNeg) -> (NonNeg, ans_list)
               |     (Neg,NonNeg) -> (Neg, ans_list)
               |     (NonNeg, Neg) -> (Neg, ans_list);;

(* Returns remainder with appropriate sign when Bigint b1 is divided by Bigint b2*)
let rem b1 b2 = let (s1, l1) = b1 in
                let (s2, l2) = b2 in
               
                if (l2=[]) then raise DivisionByZeroNotAllowed
                else
                    let (b, rem_list) = div_list l1 l2 (mk_big 0) (mk_big 1) in 
                    match (s1, s2) with
                    (Neg, Neg) -> (Neg, rem_list)
               |     (NonNeg, NonNeg) -> (NonNeg, rem_list)
               |     (Neg,NonNeg) -> (Neg, rem_list)
               |     (NonNeg, Neg) -> (NonNeg, rem_list);;

(* Return a tuple (Quotient, Remainder) when a bigint b1 is divided by bigint b2, with corresponding sign 
of both Quotient and Remainder.
It has the same complexity as Quotient and remainder.
It is a useful function in case you require both quotient and remainder. Instead of calling each function 
individually you can call these *)
let quo_with_rem b1 b2 = let (s1, l1) = b1 in
                let (s2, l2) = b2 in
               
                if (l2=[]) then raise DivisionByZeroNotAllowed
                else
                    let (b, rem_list) = div_list l1 l2 (mk_big 0) (mk_big 1) in 
                    let (s, ans_list) = b in
                    match (s1, s2) with
                    (Neg, Neg) -> ( (NonNeg, ans_list), (Neg, rem_list) )
               |     (NonNeg, NonNeg) -> ( (NonNeg, ans_list), (NonNeg, rem_list) )
               |     (Neg,NonNeg) -> ( (Neg, ans_list), (Neg, rem_list) )
               |     (NonNeg, Neg) -> ( (Neg, ans_list), (NonNeg, rem_list) );;



(*Taking Absolute value of Bigint, converting NonNeg to Neg, and leaving NonNeg as it is*)
let abs b = match b with
          (Neg, (l: int list)) -> (NonNeg, l)
     |     _ -> b;;

(*Unary Negation of a bigint, converting NonNeg to Neg and vice-versa*)
let minus b = match b with
              (Neg, (l: int list)) -> (NonNeg, l)
          |    (NonNeg, l) -> (Neg, l);;


end