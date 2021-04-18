open Printf

(* language syntax *)

type const =
  | I of int
  | B of bool
  | S of string
  | N of string
  | U 
  (*U since there is only one value for unit*)

type command =
  (* part 1 *)
  | Push of const
  | Pop
  | Swap
  | Log
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Neg
  | Cat
  | And
  | Or
  | Not
  | Eq
  | Lte
  | Lt
  | Gte
  | Gt
  | Let
  | Ask
  | BeginEnd of (command list)
  | IfElse of (command list * command list)

type prog = command list

(* parser util *)

type 'a parser = char list -> ('a * char list) option

let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
  String.of_seq (List.to_seq ls)

let parse (s : string) (p : 'a parser) : ('a * char list) option =
  p (explode s)

let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None

let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then Some (x, ls)
    else None
  | _ -> None

let char (c : char) : char parser =
  satisfy (fun x -> x = c)

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

let (>>) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) ->
    (match p2 ls with
     | Some (_, ls) -> Some (x, ls)
     | None -> None)
  | None -> None

let (<<) = seq'

let both (p1 : 'a parser) (p2 : 'b parser) : ('a * 'b) parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) ->
    (match p2 ls with
     | Some (y, ls) -> Some ((x, y), ls)
     | None -> None)
  | None -> None

let (+++) = both

let disj (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls)  -> Some (x, ls)
  | None -> p2 ls

let (<|>) = disj

let map (f : 'a -> 'b) (p : 'a parser) : 'b parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> Some (f x, ls)
  | None -> None

let (>|=) = fun p f -> map f p
let (>|) = fun p c -> map (fun _ -> c) p

let rec many (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1 (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

(* requires unit -> parser function as input
    useful when dealing with recursive parser 
*)
let rec many' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let literal (s : string) : unit parser =
  fun ls ->
  let cs = explode s in
  let rec loop cs ls =
    match cs, ls with
    | [], _ -> Some ((), ls)
    | c :: cs, x :: xs ->
      if x = c
      then loop cs xs
      else None
    | _ -> None
  in loop cs ls

let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c
    then Some ((), ls)
    else None
  | _ -> None

let ws : unit parser =
  (many whitespace) >| ()

let ws1 : unit parser =
  (many1 whitespace) >| ()

let bool : bool parser =
  (literal "<true>"  >| true ) <|>
  (literal "<false>" >| false)

let digit : char parser =
  satisfy (fun x -> '0' <= x && x <= '9')

let natural : int parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) ->
    Some (int_of_string (implode xs), ls)
  | _ -> None

let integer : int parser =
  ((char '-') >> ws >> (natural >|= (fun x -> -x))) <|> natural

let alpha : char parser =
  satisfy (fun x -> ('a' <= x && x <= 'z') ||
                    ('A' <= x && x <= 'Z'))

let string : string parser =
  (char '"') >> many (satisfy (fun x -> x <> '"')) << (char '"') >|= (fun cs -> implode cs)

let name : string parser =
  alpha +++ (many (alpha <|>
                   digit <|>
                   (char '_') <|>
                   (char '\''))) >|=
  (fun (c, cs) -> implode (c :: cs))

let unit : unit parser =
  (literal "<unit>") >| ()

(* parser *)

let sep : unit parser =
  char ';' >> ws >| ()

let keyword (s : string) : unit parser =
  (literal s) >> ws >| ()

let const =
  (integer >|= (fun x -> I x)) <|>
  (bool    >|= (fun x -> B x)) <|>
  (string  >|= (fun x -> S x)) <|>
  (name    >|= (fun x -> N x)) <|>
  (unit    >| U)


(* the recusive part is being ready for part2*)
let rec command () = 
  (* part 1 *)
  ((keyword "Push") >> (const) << sep >|= (fun x -> Push x)) <|>
  ((keyword "Pop")             << sep >| Pop)    <|>
  ((keyword "Swap")            << sep >| Swap)   <|>
  ((keyword "Log")             << sep >| Log)    <|>
  ((keyword "Add")             << sep >| Add)    <|>
  ((keyword "Sub")             << sep >| Sub)    <|>
  ((keyword "Mul")             << sep >| Mul)    <|>
  ((keyword "Div")             << sep >| Div)    <|>
  ((keyword "Rem")             << sep >| Rem)    <|>
  ((keyword "Neg")             << sep >| Neg)    <|>
  ((keyword "Cat")             << sep >| Cat)    <|>
  ((keyword "And")             << sep >| And)    <|>
  ((keyword "Or")              << sep >|  Or)    <|>
  ((keyword "Not")             << sep >| Not)    <|>
  ((keyword "Eq")              << sep >|  Eq)    <|>
  ((keyword "Lte")             << sep >| Lte)    <|>
  ((keyword "Lt")              << sep >|  Lt)    <|>
  ((keyword "Gte")             << sep >| Gte)    <|>
  ((keyword "Gt")              << sep >|  Gt)    <|>
  ((keyword "Let")             << sep >| Let)    <|>
  ((keyword "Ask")             << sep >| Ask)    <|>
  ((keyword "Begin") >> (many' command) << (keyword "End") << sep >|= (fun x -> BeginEnd x)) <|>
  (((keyword "If") >> (many' command)) +++ ((keyword "Else") >> (many' command) << (keyword "End") << sep) >|= (fun x -> IfElse x))




let parser = ws >> many' command

(* semantics *)

(* language value *)
type value =
  | I_val of int
  | B_val of bool
  | S_val of string
  | N_val of string
  | U_val

and result =
  | Ok of value list
  | TypeError
  | StackError
  | DivError
  | NotFoundError

and env = (string * value) list

and stack = value list

let lookup (e : env) (name : string) : value option =
  List.assoc_opt name e

let put (e : env) (name : string) (v : value): env =
  (name,v) :: e

let to_string_value v =
  match v with
  | I_val x -> sprintf "%d" x
  | B_val x -> sprintf "<%b>" x
  | S_val x -> sprintf "\"%s\"" x
  | N_val x -> sprintf "%s" x
  | U_val   -> sprintf "<unit>"

let to_int_result (r : result) : int =
  match r with
  | Ok _          -> 0
  | TypeError     -> 1
  | StackError    -> 2
  | DivError      -> 3
  | NotFoundError -> 4



let rec run (p : prog) (st : stack) (log : string list) (mem: env) : string list * result * env =
  match p with
  (* part 1 *)
  | Push cst :: rest ->
    (match cst with
     | I v -> run rest (I_val v :: st) log mem
     | B v -> run rest (B_val v :: st) log mem
     | S v -> run rest (S_val v :: st) log mem
     | N v -> run rest (N_val v :: st) log mem
     | U   -> run rest (U_val   :: st) log mem)
  | Pop :: rest ->
    (match st with
     | _ :: st -> run rest st log mem
     | _ -> log, StackError,mem)
  | Swap :: rest ->
    (match st with
     | x :: y :: st -> run rest (y :: x :: st) log mem
     | _ -> log, StackError,mem)
  | Log :: rest ->
    (match st with
     | x :: st -> run rest st (to_string_value x :: log ) mem
     | _ -> log, StackError,mem)
  | Add :: rest ->
    (match st with
     | I_val x :: I_val y :: st ->
       run rest (I_val (x + y) :: st) log mem
     | _ :: _ :: st -> log, TypeError,mem
     | _ -> log, StackError,mem)
  | Sub :: rest ->
    (match st with
     | I_val x :: I_val y :: st ->
       run rest (I_val (x - y) :: st) log mem
     | _ :: _ :: st -> log, TypeError,mem
     | _ -> log, StackError,mem)
  | Mul :: rest ->
    (match st with
     | I_val x :: I_val y :: st ->
       run rest (I_val (x * y) :: st) log mem
     | _ :: _ :: st -> log, TypeError,mem
     | _ -> log, StackError,mem)
  | Div :: rest ->
    (match st with
     | I_val _ :: I_val 0 :: st -> log, DivError,mem
     | I_val x :: I_val y :: st ->
       run rest (I_val (x / y) :: st) log mem
     | _ :: _ :: st -> log, TypeError,mem
     | _ -> log, StackError,mem)
  | Rem :: rest ->
    (match st with
     | I_val _ :: I_val 0 :: st -> log, DivError,mem
     | I_val x :: I_val y :: st ->
       run rest (I_val (x mod y) :: st) log mem
     | _ :: _ :: st -> log, TypeError,mem
     | _ -> log, StackError,mem)
  | Neg :: rest ->
    (match st with
     | I_val x :: st ->
       run rest (I_val (-x) :: st) log mem
     | _ :: st -> log, TypeError,mem
     | _ -> log, StackError,mem)

  (* Part2*)
  | Cat :: rest -> 
    (match st with
     |S_val x:: S_val y :: st ->
       run rest (S_val (x^y) :: st) log mem
     | _ :: _ :: st -> log, TypeError,mem
     | _ -> log, StackError,mem)
  | And :: rest -> 
    (match st with
     | B_val x::B_val y :: st -> run rest (B_val (x&&y) :: st) log mem
     | _ :: _ :: st -> log, TypeError,mem 
     | _ -> log, StackError,mem) 
  | Or :: rest -> 
    (match st with
     | B_val x::B_val y :: st -> run rest (B_val (x||y) :: st) log mem
     | _ :: _ :: st -> log, TypeError,mem
     | _ -> log, StackError,mem) 
  | Not :: rest -> 
    (match st with
     | B_val x :: st -> run rest (B_val (not x) :: st) log mem
     | _ :: st -> log, TypeError,mem
     | _ -> log, StackError,mem)
  | Eq :: rest ->
    (match st with
     | I_val x :: I_val y :: st -> run rest (B_val (x = y) :: st) log mem
     | _ :: _ :: st -> log, TypeError,mem
     | _ -> log, StackError,mem)
  | Lte :: rest ->
    (match st with
     | I_val x :: I_val y :: st -> run rest (B_val (x <= y) :: st) log mem
     | _ :: _ :: st -> log, TypeError,mem
     | _ -> log, StackError,mem)
  | Lt :: rest ->
    (match st with
     | I_val x :: I_val y :: st -> run rest (B_val (x < y) :: st) log mem
     | _ :: _ :: st -> log, TypeError,mem
     | _ -> log, StackError,mem)
  | Gte :: rest ->
    (match st with
     | I_val x :: I_val y :: st -> run rest (B_val (x >= y) :: st) log mem
     | _ :: _ :: st -> log, TypeError,mem
     | _ -> log, StackError,mem)
  | Gt :: rest ->
    (match st with
     | I_val x :: I_val y :: st -> run rest (B_val (x > y) :: st) log mem
     | _ :: _ :: st -> log, TypeError,mem
     | _ -> log, StackError,mem)
  | Let :: rest ->
    (match st with
     | (N_val n)::v::st -> run rest st log (put mem n v)
     | _ :: _ :: st -> log, TypeError,mem
     | _ -> log, StackError,mem)
  | Ask :: rest ->
    (match st with
     | (N_val n)::st -> 
       (match lookup mem n with
        | Some v -> run rest (v::st) log mem
        | None -> log, NotFoundError, mem)
     | _ :: st -> log, TypeError,mem
     | _ -> log, StackError,mem)
  |BeginEnd cmd_list :: rest -> 
    (match (begin_end_env cmd_list [] [] mem) with
     | (v,Ok _,sub_log) -> run rest (v::st) (sub_log@log) mem   (* sub_log from innerscope of beginEnd concaenated with main log*)
     | (_,error,sub_log) -> (sub_log@log), error, mem)    (* sub_log from innerscope of beginEnd concaenated with main log*)
  (* | IfElse (cmd_list1,cmd_list2) :: rest ->  *)



  | [] -> log, Ok st, mem
  | _ -> failwith "undefined"

and 

  begin_end_env (p: prog) (st: stack) (log: string list) (e: env) : (value * result * string list) = 
  match run p st log e with
  | (log,result,_) -> 
    match result with
    | Ok [] -> (U_val, StackError,log)
    | Ok (x::rest) -> (x, Ok st,log)
    | error -> (U_val, error,log)



(* putting it all together *)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(* let interpreter (s : string) : string list * int =
   match parse s parser with
   | Some (prog, []) ->
    let (log, ret) = run prog [] [] [] in
    (List.rev log, to_int_result ret)
   | _ -> failwith "invalid source"

   let deb prog = let (revlog,stack) = run prog [] [] [] in ((List.rev revlog), stack) *)

(* let debug (s: string) = 
   match parse s parser with
   | Some (prog,[]) -> deb prog
   | _ -> failwith "undefined" *)

let deb_w_mem prog = let (revlog,stack,mem) = run prog [] [] [] in ((List.rev revlog), stack,mem)

let debug_w_mem (s:string) = 
  match parse s parser with
  | Some (prog,[]) -> deb_w_mem prog
  | _ -> failwith "undefined"

(* let runfile (file : string) : string list * int =
   let s = readlines file in
   interpreter s *)

let test = readlines "/home/waynel/cs320/Assignments/input/my_test.txt";;
(* let cmd_result = parser (explode test);; *)
debug_w_mem test;;