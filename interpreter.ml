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

let flip f y x = f x y

let ($) f g = fun x -> f (g x)


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
  | [] -> Some (a,[])


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
  if n > 0 then
    read >>= fun c -> 
    readn (n-1) >>= fun cs -> 
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

let is_ASCII x= 
  let code = (Char.code x) in
  code != 34

let is_digit = function
  '0' .. '9' -> true | _ -> false


(* this is perhaps not how you would define name*)



type const = 
    I of int | B of bool | S of string | N of string | U of unit 

type command = 
    Push of const | Pop | Swap | Log | Add | Sub | Mul | Div | Rem | Neg 

type prog = 
    Prog of command list



let digit_p = sat is_digit
let letter_p = sat is_alpha
let ascii_p = sat is_ASCII

(* charP takes in a character and returns a parser that only parses that character*)
let charP (c : char) : char parser =
  sat (fun x -> x = c)

let string_match (str:string): char list parser =
  let len=String.length str in 
  readn len >>= fun x->
  if (explode str)=x then returnP x
  else fail

let whitespace_p = 
  charP ' ' <|> charP '\n' <|> charP '\t' <|> charP '\r' <|> charP '\\' <|> charP 'n'

let semicolon_P = 
  charP ';'

let quoteP = 
  charP '"'

let natural_numP = 
  many1 digit_p >>= fun xs -> returnP (int_of_string (implode xs))


(*parses both positive and negative natural numbers*)
let intP = 
  natural_numP <|> (charP '-' >>= fun _ -> natural_numP >>= fun n -> returnP (-n))

let stringP = 
  quoteP >>= fun _ -> 
  many ascii_p >>= fun ls ->
  quoteP >>= fun _ ->
  returnP (implode ls)

let nameP = 
  letter_p >>= fun x -> 
  many (letter_p <|> digit_p <|> charP '_' <|> charP '\'') >>= fun y ->
  returnP (implode (x::y))

let boolP: bool parser = 
  (string_match "<true>" >>= fun _ -> returnP true) 
  <|> 
  (string_match "<false>" >>= fun _ -> returnP false)

let unitP: unit parser = 
  (string_match "<unit>" >>= fun _ -> returnP ()) 

let constP: const parser = 
  (intP >>= fun x -> returnP (I x)) 
  <|>
  (boolP >>= fun x -> returnP (B x))
  <|>
  (stringP >>= fun x -> returnP (S x))
  <|>
  (nameP >>= fun x -> returnP (N x))
  <|>
  (unitP >>= fun x -> returnP (U x))


(* command parsers below*)

let pushP: command parser = 
  string_match "Push" >>= fun _ -> 
  whitespace_p >>= fun _ ->
  constP >>= fun x -> 
  semicolon_P >>= fun _ ->
  many whitespace_p >>= fun _ ->
  returnP (Push x)

let popP: command parser = 
  string_match "Pop" >>= fun _ -> 
  semicolon_P >>= fun _ ->
  many whitespace_p >>= fun _ ->
  returnP (Pop)

let swapP: command parser = 
  string_match "Swap" >>= fun _ -> 
  semicolon_P >>= fun _ ->
  many whitespace_p >>= fun _ ->
  returnP (Swap)

let addP: command parser = 
  string_match "Add" >>= fun _ -> 
  semicolon_P >>= fun _ ->
  many whitespace_p >>= fun _ ->
  returnP (Add)

let subP: command parser = 
  string_match "Sub" >>= fun _ -> 
  semicolon_P >>= fun _ ->
  many whitespace_p >>= fun _ ->
  returnP (Sub)

let mulP: command parser = 
  string_match "Mul" >>= fun _ -> 
  semicolon_P >>= fun _ ->
  many whitespace_p >>= fun _ ->
  returnP (Mul)

let divP: command parser = 
  string_match "Div" >>= fun _ -> 
  semicolon_P >>= fun _ ->
  many whitespace_p >>= fun _ ->
  returnP (Div)

let remP: command parser = 
  string_match "Rem" >>= fun _ -> 
  semicolon_P >>= fun _ ->
  many whitespace_p >>= fun _ ->
  returnP (Rem)

let negP: command parser = 
  string_match "Neg" >>= fun _ -> 
  semicolon_P >>= fun _ ->
  many whitespace_p >>= fun _ ->
  returnP (Neg)

let logP: command parser = 
  string_match "Log" >>= fun _ -> 
  semicolon_P >>= fun _ ->
  many whitespace_p >>= fun _ ->
  returnP (Log)




let parse (c_list:char list) = 
  (pushP <|> popP <|> swapP <|> addP <|> subP <|> mulP <|> divP <|> remP <|> negP <|> logP) c_list

let parseStringIntoCommands (s:string) = 
  match (many parse) (explode s) with
  | Some (command_list, []) -> command_list
  | _ -> []

let const_to_string (c: const):string =
  match c with
  | I x -> string_of_int x
  | B x -> "<" ^ (string_of_bool x) ^ ">"
  | S x -> "\"" ^ x ^ "\""
  | U x -> "<unit>"
  | N x -> x

let rec eval (command_list: command list) (stack: const list)  (log_list:string list): (const list*string list*int) = 
  match command_list,stack with
  | [], _-> (stack,log_list,0)

  | Push v::rest, t -> eval rest (v::t) log_list

  | Pop::rest,x::t -> eval rest t log_list
  | Pop::rest,[] -> ([],log_list,2)

  | Swap::rest,x::y::t -> eval rest (y::x::t) log_list
  | Swap::rest,_ -> (stack,log_list,2)

  | Add::rest, (I x)::(I y)::t -> eval rest (I (x+y)::t) log_list
  | Add::rest, _::_::t -> (stack,log_list,1)
  | Add::rest, _ -> (stack,log_list,2)

  | Sub::rest, (I x)::(I y)::t -> eval rest (I (x-y)::t) log_list
  | Sub::rest, _::_::t -> (stack,log_list,1)
  | Sub::rest, _ -> (stack,log_list,2)

  | Mul::rest, (I x)::(I y)::t -> eval rest (I (x*y)::t) log_list
  | Mul::rest, _::_::t -> (stack,log_list,1)
  | Mul::rest, _ -> (stack,log_list,2)

  | Div::rest, (I x)::(I 0)::t -> (stack,log_list,3)
  | Div::rest, (I x)::(I y)::t -> eval rest (I (x/y)::t) log_list
  | Div::rest, _::_::t -> (stack,log_list,1)
  | Div::rest, _ -> (stack,log_list,2)

  | Rem::rest, (I x)::(I 0)::t -> (stack,log_list,3)
  | Rem::rest, (I x)::(I y)::t -> eval rest (I (x mod y)::t) log_list
  | Rem::rest, _::_::t -> (stack,log_list,1)
  | Rem::rest, _ -> (stack,log_list,2)

  | Neg::rest,(I x)::t -> eval rest ((I (-x))::t) log_list
  | Neg::rest,[] -> ([],log_list,2)
  | Neg::rest,_ -> (stack,log_list,1)

  | Log::rest,x::t -> eval rest t (log_list@[(const_to_string x)])
  | Log::rest,[] -> ([],log_list,2)



let interpreter (s : string) : string list * int = 
  match (eval (parseStringIntoCommands s) [] []) with
  | stack,log_list,x -> log_list,x 



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

let test (file:string) = 
  let testx = readlines file in
  let p = parseStringIntoCommands testx in
  eval p [] []



let runfile (file : string) : string list * int =
  let s = readlines file in
  interpreter s