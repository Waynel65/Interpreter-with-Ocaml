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



open Printf

let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
  String.of_seq (List.to_seq ls)

type 'a parser = char list -> ('a * char list) option

(* let parse p c_list = 
   p c_list 
   THIS is how you would run a parser on a c_list
*)


(* p1 takes in a char and outputs ['a * char list] option.
   'a is fed into a function, which can contain lots of parser and make choice which
   one to use. The parser this function outputs can take the char list produced earlier
   and output ['b * char list] option
*)
let (>>=) (p: 'a parser) (f: 'a -> 'b parser): 'b parser = 
  fun c_list -> 
  match p c_list with
  | None -> None
  | Some (x,rest) -> (f x) rest

let (let*) = (>>=)



(* Disj stands for disjunction. It attempts to parse using
 * parser p1. If p1 fails then attempt to parse using p2. 
    This is basically the <|> function
*)
let disj (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls->
  match p1 ls with
  | Some(x,ls) -> Some (x,ls)
  | None -> p2 ls


(* Infix operator for disj.  *)
let (<|>) = disj

(* returnP takes an input a and forms a tuple with the given c_list
   e.g. (returnP 5) "abcd" => (5,"abcd")
*)
let returnP (a: 'a):'a parser = 
  fun c_list -> 
  match c_list with
  | x::rest -> Some(a,c_list)
  | _ -> None


(* TEMPORARY FUNCTION that handles failures of parser*)
let fail: 'a parser = fun c_list -> None


(* Read is a trivial parser. It reads the
 * input and reads the first character. It
 * can only fail if the input is empty. *)
let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls) (* form a tuple of (first_char,rest)  *)
  | _ -> None

(*readn parses out the first n characters and returns a char_list parser*)
let rec readn (n:int): char list parser = 
  (* if n > 0 then
     read >>= fun c -> 
     readn (n-1) >>= fun cs -> 
     returnP (c::cs)

     else
     returnP [] *)
  if n > 0 then
    let* c = read in
    let* cs = readn (n-1) in
    returnP (c::cs)
  else
    returnP [] 


(*satify function: checks if the first character follows the condition outlined by the
  the function f
  if yes, return a parser with x parsed out
  if not, fail
*)
let sat f = 
  read >>= (fun x->if f x then returnP x else fail)


(* If given a parser, we may want to apply it as many times as
 * possible before it fails. For example: parsing of numbers.
 * The many combinators attempts to apply parser p 0 or more times,
 * returning a list of each attempt's result if it does not fail.
 * Since it allows p to be applied 0 times (immediate failure of p),
 * the overall many parser will never fail. *)
let rec many (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | None -> Some ([],ls) (* since many allows 0 time parsing*)
  | Some (x,ls) -> match many p ls with
    |Some(xs,ls)-> Some (x::xs,ls)
    | None -> Some(x::[],ls)



(* Similar to many, but requires that p be parsed 1 or more times.
 * Unlike many, many1 could fail. *)
let rec many1 (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | None -> None
  | Some (x,ls) -> match many p ls with
    (* notice how we match many instead of many1 here
       Because if it doesn't enter the None case, that means we already 
       parse it once. The rest can be parsed just as in many
    *) 
    |Some(xs,ls)-> Some (x::xs,ls)
    | None -> Some(x::[],ls)




(*digit and letter parser
  parse out the first digit or letter of the input
  if parsed successfully, return a parser
  if not, fail
*)

let is_alpha = function
  'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let is_digit = function
  '0' .. '9' -> true | _ -> false



let digit_p = sat is_digit
let letter_p = sat is_alpha

(* charP takes in a character and returns a parser that only parses that character*)
let charP (c : char) : char parser =
  sat (fun x -> x = c)


let whitespace_p = 
  charP ' ' <|> charP '\n' <|> charP '\t' <|> charP '\r'



let natural_numP = 
  many1 digit_p >>= fun xs -> returnP (int_of_string (implode xs))


(*parses both positive and negative natural numbers*)
let intP = 
  natural_numP <|> (charP '-' >>= fun _ -> natural_numP >>= fun n -> returnP (-n))

let stringP (str:string): char list parser =
  let len=String.length str in 
  many whitespace_p >>= fun _ ->   (* get rid of whitespaces before the stuff we want to parse out*)
  readn len >>= fun x->
  many whitespace_p >>= fun _ -> (* get rid of whitespaces after the stuff we want to parse out*)
  if (explode str)=x then returnP x
  else fail

type const = 
    I of int | B of bool | S of string | U of unit  (*still need to define name type*)

type command = 
    Push of const | Pop | Swap | Log | Add | Sub | Mul | Div | Rem | Neg 

(* let pushP: command parser = 
   stringP "Push" >>= fun  *)


(* Seq is a combinator that sequences 2 combinators.
 * Suppose we have a parser p1 and a parser p2. After
 * using p1 to parse some input, we want to parse the
 * remaining list of p1 with p2 but only keep the result
 * of p2. Seq fulfills this functionality. *)
let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

(* Seq' is similar to seq, but instead of keeping the
 * the result of p2, it keeps the result of p1. 
   we still have to parse through p1 first then parse through p2*)
let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) ->
    (match p2 ls with
     | Some (_, ls) -> Some (x, ls)
     | None -> None)
  | None -> None

(* Both is similar to seq and seq', but it keeps results
 * of both p1 and p2. *)
let both (p1 : 'a parser) (p2 : 'b parser) : ('a * 'b) parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) ->
    (match p2 ls with
     | Some (y, ls) -> Some ((x, y), ls)
     | None -> None)
  | None -> None

(* Infix operator for both.  *)
let (+++) = both



let interpreter (s : string) : string list * int = failwith "undefined"


let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

let runfile (file : string) : string list * int =
  let s = readlines file in
  interpreter s