open Printf

(* language syntax *)

type const =
  | I of int
  | B of bool
  | S of string
  | N of string
  | U

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
  ((keyword "Neg")             << sep >| Neg)

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

let rec run (p : prog) (st : stack) (log : string list) :
  string list * result =
  match p with
  (* part 1 *)
  | Push cst :: rest ->
    (match cst with
     | I v -> run rest (I_val v :: st) log
     | B v -> run rest (B_val v :: st) log
     | S v -> run rest (S_val v :: st) log
     | N v -> run rest (N_val v :: st) log
     | U   -> run rest (U_val   :: st) log)
  | Pop :: rest ->
    (match st with
     | _ :: st -> run rest st log
     | _ -> log, StackError)
  | Swap :: rest ->
    (match st with
     | x :: y :: st -> run rest (y :: x :: st) log
     | _ -> log, StackError)
  | Log :: rest ->
    (match st with
     | x :: st -> run rest st (to_string_value x :: log)
     | _ -> log, StackError)
  | Add :: rest ->
    (match st with
     | I_val x :: I_val y :: st ->
       run rest (I_val (x + y) :: st) log
     | _ :: _ :: st -> log, TypeError
     | _ -> log, StackError)
  | Sub :: rest ->
    (match st with
     | I_val x :: I_val y :: st ->
       run rest (I_val (x - y) :: st) log
     | _ :: _ :: st -> log, TypeError
     | _ -> log, StackError)
  | Mul :: rest ->
    (match st with
     | I_val x :: I_val y :: st ->
       run rest (I_val (x * y) :: st) log
     | _ :: _ :: st -> log, TypeError
     | _ -> log, StackError)
  | Div :: rest ->
    (match st with
     | I_val _ :: I_val 0 :: st -> log, DivError
     | I_val x :: I_val y :: st ->
       run rest (I_val (x / y) :: st) log
     | _ :: _ :: st -> log, TypeError
     | _ -> log, StackError)
  | Rem :: rest ->
    (match st with
     | I_val _ :: I_val 0 :: st -> log, DivError
     | I_val x :: I_val y :: st ->
       run rest (I_val (x mod y) :: st) log
     | _ :: _ :: st -> log, TypeError
     | _ -> log, StackError)
  | Neg :: rest ->
    (match st with
     | I_val x :: st ->
       run rest (I_val (-x) :: st) log
     | _ :: st -> log, TypeError
     | _ -> log, StackError)
  | [] -> log, Ok st

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

let interpreter (s : string) : string list * int =
  match parse s parser with
  | Some (prog, []) ->
    let (log, ret) = run prog [] [] in
    (List.rev log, to_int_result ret)
  | _ -> failwith "invalid source"

let runfile (file : string) : string list * int =
  let s = readlines file in
  interpreter s
