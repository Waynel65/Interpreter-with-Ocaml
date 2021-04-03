
type 'a tree =
  | Leaf of 'a 
  | Node of 'a tree * 'a tree



let rec map_tree (f: 'a -> 'b) (tree: 'a tree): 'b tree = 
  match tree with
  | Leaf x -> Leaf (f x)
  | Node (left, right) -> Node ((map_tree f left),(map_tree f right))



let rec fold_tree (node: 'b -> 'b -> 'b)  (leaf: 'a -> 'b)  (tree: 'a tree): 'b  = 
  match tree with
  | Leaf x -> leaf x
  | Node (left,right) -> node (fold_tree node leaf left) (fold_tree node leaf right)



let rec sum_ints (tree: int tree): int  = 
  fold_tree (fun l r -> l+r) (fun x -> x) tree




let rec tree_size (tree: 'a tree): int  = 
  fold_tree (fun l r -> l+r+1) (fun x -> 1) tree


let rec tree_height (tree: 'a tree): int  = 
  match tree with
  | Leaf x -> 1
  | Node (left,right) -> 
    let l = tree_height left in
    let r = tree_height right in
    if l > r then l+1 else r+1


let rec tree_contains (tree: 'a tree) (look_for: 'a tree -> bool): bool  = 
  match tree with
  | Leaf x -> look_for (Leaf x)
  | Node (left,right) -> 
    let l = tree_contains left look_for in
    let r = tree_contains right look_for in
    l || r || look_for tree



let rec show_bool_tree (tree: bool tree) : string  = 
  match tree with
  | Leaf x -> string_of_bool x
  | Node (left,right) ->
    let l = show_bool_tree left in
    let r = show_bool_tree right in
    "(" ^ l ^ "^" ^ r ^ ")"


let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let implode l =
  String.of_seq (List.to_seq l)


let read_bool_prefix  (ls: char list): ((bool * (char list)) option) = 
  match ls with
  | 't' :: 'r' :: 'u' :: 'e' :: rest -> Some (true,rest)
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: rest -> Some(false,rest)
  | _ -> None


let rec read_bool_tree_prefix (ls: char list): ((bool tree * (char list)) option) = 
  match ls with
  | [] -> None
  | 't' :: 'r' :: 'u' :: 'e' :: rest -> Some (Leaf true,rest)
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: rest -> Some(Leaf false,rest)
  | '('::rest -> 
    (match read_bool_tree_prefix rest with
     | None -> None
     | Some (left, rest1) -> 
       (match rest1 with
        | '^'::rest_right -> 
          (match read_bool_tree_prefix rest_right with
           | None -> None
           | Some (right,rest2) -> 
             (match rest2 with 
              | ')'::rest_string -> Some (Node (left, right),rest_string)))))
  | _ -> None





let rec read_bool_tree (tree: string) : ((bool tree) option) = 
  match read_bool_tree_prefix (explode tree) with
  | Some (b,ls) -> Some b
  | _ -> None


let rec matching_parens (tree: string) : bool  = 
  let rec test_parens ls acc = 
    match ls,acc with
    | [],[] -> true
    | [],_ -> false
    | '('::t,_ ->  test_parens t ('('::acc)
    | ')'::rest,_ -> 
      (match acc with
       | [] -> false
       | h::t -> if h = '(' then test_parens rest t else false)
    | _ -> false
  in test_parens (explode tree) []
