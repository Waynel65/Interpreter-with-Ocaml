
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


let between (n:int) (e:int): int list = 
  let rec aux n e l = 
    if n > e then l
    else aux n (e-1) (e::l) 
  in aux n e []

let zip_int (a: int list) (b: int list): (int * int) list = 
  let rec aux a b list = 
    match (a,b) with
    | a,[] -> list
    | [],b -> list
    | (h1::t1),(h2::t2) -> aux t1 t2 (list@ [(h1,h2)])
  in aux a b []

let dotProduct (x: int list) (y: int list): int = 
  let rec aux x y accum =
    match (x,y) with
    | [],[] -> accum
    | (h1::t1),[] -> aux t1 [] 0
    | [],(h2::t2) -> aux [] t2 0
    | ((h1::t1),(h2::t2)) -> aux t1 t2 (accum + (h1*h2))
  in aux x y 0


let rec list_of_tuple_as_string (list: (int*int) list): string = 
  let rec aux list = 
    match list with 
    | [] -> ""
    | (a,b)::[]-> "(" ^ (string_of_int a) ^ "," ^ (string_of_int b) ^ ")"
    | (a,b)::t-> "(" ^ (string_of_int a) ^ "," ^ (string_of_int b) ^ ")" ^ ";" ^ aux t
  in "[" ^ aux list ^ "]"

let rec insert (i: int) (list: int list): int list = 
  match list with
  | []->[i]
  | head::tail-> 
    if i<head then i::list 
    else head::(insert i tail)


let rec sort (ls: int list): int list = 
  let rec aux sorted unsorted = 
    match unsorted with
    | []-> sorted
    | head::tail -> aux (insert head sorted) tail
  in aux [] ls
