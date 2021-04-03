(*
Honor code comes here:

First Name: WEI-HENG
Last Name: LIN
BU ID: U79873457

I pledge that this program represents my own
program code and that I have coded on my own. I received
help from no one in designing and debugging my program.
I have read the course syllabus of CS 320 and have read the sections on Collaboration
and Academic Misconduct. I also understand that I may be asked to meet the instructor
or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
may also ask you to solve a related problem.
*)

let rec print_list (ls: int list): unit =
  let rec aux ls = match ls with
    | [] -> print_string ""
    | e::[] -> print_int e
    | e::l -> 
      let _ = print_int e 
      in let _ = print_string "; " 
      in aux l

  in let _ = print_string "[" 
  in let _ = aux ls
  in         print_string "]" 

let rec print_some_list (ls: int list option): unit = 
  match ls with
  | None -> ()
  | Some l -> print_list l


let rec list_of_tuple_as_string list =
  let rec aux list = 
    match list with 
    | None -> "None"
    | Some [] -> ""
    | Some ((a,b)::[])-> "(" ^ (string_of_int a) ^ "," ^ (string_of_int b) ^ ")"
    | Some ((a,b)::t)-> "(" ^ (string_of_int a) ^ "," ^ (string_of_int b) ^ ")" ^ ";" ^ aux (Some t)
  in "[" ^ aux list ^ "]"

let between (n:int) (e:int): int list = 
  let rec aux n e l = 
    if n > e then l
    else aux n (e-1) (e::l) 
  in aux n e []

(*
Write a safe_zip_int function that takes two lists of integers and combines them into a list of pairs of ints
If the two input list are of unequal lengths, return None
your method must be tail recursive.

For example,
safe_zip_int [1;2;3;5] [6;7;8;9] = Some [(1,6);(2,7);(3,8);(5,9)]
safe_zip_int [1] [2;4;6;8] = None
safe_zip_int (between 0 1000000) (between 0 1000000) does not stack overflow

Note: The between function is from the previous programming assignment 1. 
You can use the between function from the previous assignment for testing purposes. 
*)



let rec safe_zip_int (ls1: int list) (ls2: int list) : ((int * int) list) option = 
  let rec aux ls1 ls2 acc = 
    match (ls1,ls2) with
    | [],[] -> acc
    | (h1::t1),[] -> aux t1 [] None
    | [],(h2::t2) -> aux [] t2 None
    | (h1::t1),(h2::t2) -> 
      match acc with
      | None -> None
      | Some tuples -> aux t1 t2 (Some(tuples@[(h1,h2)]))
  in aux ls1 ls2 (Some [])

(* let _= print_string(list_of_tuple_as_string (safe_zip_int [2;3;4;6] [3;8;9;10])) *)

(*
Write a function that produces the ith Pell number:
https://en.wikipedia.org/wiki/Pell_number
https://oeis.org/A000129
your function must be tail recursive, and needs to have the correct output up to integer overflow

pell 0 = 0
pell 1 = 1
pell 7 = 169
pell 1000000  does not stack overflow
*)


let rec pell (i: int) : int = 
  let rec aux i acc1 acc2 = 
    match i with
    | 0 -> acc1
    | _ -> aux (i-1) acc2 (2*acc2+acc1)
  in aux i 0 1


(* let _= print_int(pell 50) *)

(* The nth Tetranacci number T(n) is mathematically defined as follows.
 *
 *      T(0) = 0
 *      T(1) = 1
 *      T(2) = 1
 *      T(3) = 2
 *      T(n) = T(n-1) + T(n-2) + T(n-3) + T(n-4)
 *
 * For more information, you may consult online sources.
 *
 *    https://en.wikipedia.org/wiki/Generalizations_of_Fibonacci_numbers
 *    https://mathworld.wolfram.com/TetranacciNumber.html
 *
 * Write a tail recursive function tetra that computes the nth Tetranacci
 * number efficiently. In particular, large inputs such as (tetra 1000000)
 * should neither cause stackoverflow nor timeout.
*)

let tetra (n : int) : int = 
  let rec aux n acc1 acc2 acc3 acc4 = 
    match n with
    | 0 -> acc1
    | _ -> aux (n-1) acc2 acc3 acc4 (acc1+acc2+acc3+acc4)
  in aux n 0 1 1 2

(* let _ = print_int (tetra 1000000) *)

(*
infinite precision natural numbers can be represented as lists of ints between 0 and 9

Write a function that takes an integer and represents it with a list of integers between 0 and 9 where the head 
of the list holds the least signifigant digit and the very last element of the list represents the most significant digit.
If the input is negative return None. We provide you with some use cases:

For example:
toDec 1234 = Some [4; 3; 2; 1]
toDec 0 = Some []
toDec -1234 = None
*)

(* Hint use 
   mod 10
   / 10
*)

let rec toDec (i : int) : int list option = 
  let rec aux i acc = 
    if i < 0 then None
    else match i with
      | 0 -> acc
      | x -> 
        match acc with
        | None -> None
        | Some ls -> aux (i/10) (Some(ls@[(i mod 10)]))
  in aux i (Some [])

(* let _ = print_some_list (toDec (12341235)) *)


(*
Write a function that sums 2 natrual numbers as represented by a list of integers between 0 and 9 where the head is the least signifigant digit.
Your function should be tail recursive

sum [4; 3; 2; 1] [1;0;1] = [5; 3; 3; 1]
sum [1] [9;9;9] = [0; 0; 0; 1]
sum [] [] = []
sum (nines 1000000) [1] does not stack overflow, when (nines 1000000) provides a list of 1000000 9s
*)

let rec sum (a : int list) (b : int list) : int list =  
  let rec aux a b flag acc = 
    if flag = false 
    then match a,b with
      | [],[] -> acc
      | a,[] -> aux [] [] false (acc@a)
      | [],b -> aux [] [] false (acc@b)
      | [x],[y] -> if x+y <=9 then aux [] [] false (acc@[x+y]) else aux [1] [] false (acc@[(x+y) mod 10])     (*if x+y >=10, where x and y are the 2 only digits left then let a = [1]*)
      | (h1::t1),(h2::t2) -> 
        if h1+h2 <= 9 then aux t1 t2 false (acc@[h1+h2])
        else aux t1 t2 true (acc@[(h1+h2) mod 10])
    else match a,b with
      | [],[] -> acc
      | a,[] -> aux a [1] false (acc)
      | [],b -> aux [1] b false (acc)
      | [x],[y] -> if x+y+1 <=9 then aux [] [] false (acc@[x+y+1]) else aux [1] [] false (acc@[(x+y+1) mod 10])
      |(h1::t1),(h2::t2) -> 
        if (h1+h2+1) <= 9 then aux t1 t2 false (acc@[h1+h2+1])
        else aux t1 t2 true (acc@[(h1+h2+1) mod 10])
  in aux a b false []

(* let rec sum (a : int list) (b : int list) : int list =  
   let rec aux a b acc carry= 
    match a,b with
    | [],[] -> acc
    | a,[] -> aux [] []  *)


(* let _= print_list (sum [0;1] [5;2]) *)

(* let rec sum (a : int list) (b : int list) : int list = 
   let rec aux a b acc = 
    match a,b with
    | [],[] -> acc
    | a,[] -> if x+y <=9 then aux [] [] (acc@a) else aux  *)

(*
Write an infinite precision version of the pel function from before

pell2 0 = []
pell2 1 = [1]
pell2 7 = [9; 6; 1]
pell2 50 = [2; 2; 5; 3; 5; 1; 4; 2; 9; 2; 4; 6; 2; 5; 7; 6; 6; 8; 4]

Hint: You may want to use the sum function from above again inside 
pell2. 

*)


let rec pell2 (i: int) : int list = 
  let rec aux i acc1 acc2 = 
    match i with
    | 0->acc1
    | _ -> aux (i-1) acc2 (sum (sum acc2 acc2) acc1)
  in aux i [] [1]


(* let _ = 
   print_list (pell2 50) *)
