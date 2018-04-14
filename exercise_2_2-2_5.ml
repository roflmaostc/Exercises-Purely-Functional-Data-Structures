type 'a tree = Empty | Tree of 'a tree * 'a * 'a tree

let empty = Empty

(*tail recursive*)
(*Ex 2.2*)
let rec member x t = 
  let rec aux t prev = 
    match t with
    | Empty -> (match prev with None -> false | Some prev -> x=prev)
    | Tree (l,e,r) -> 
      if x<e then aux l prev
      else aux r (Some e)
  in
  aux t None

exception Already_Exists

(*not tail recursive*)
(*Ex 2.3*)
let insert x t = 
  let rec aux t = match t with
    | Empty -> Tree(Empty, x, Empty)
    | Tree(l,e,r) -> 
      if x<e then Tree(aux l, e, r)
      else if x>e then Tree(l, e, aux r)
      else raise Already_Exists
  in
  try aux t 
  with Already_Exists -> t


(*not tail recursive*)
(*Ex 2.4*)
let insert_2 x t = 
  let rec aux t prev = match t with
    | Empty -> 
      (match prev with 
         | None -> t                   
         | Some prev -> if prev=x then raise Already_Exists else Tree(Empty, x, Empty))
    | Tree(l,e,r) -> 
      if x<e then Tree(aux l prev, e, r)
      else Tree(l, e, aux r (Some e))
  in
  try aux t None 
  with Already_Exists -> t


(*Ex 2.5a*)
let rec complete x d =
  if d>0 then
    let t = complete x (d-1) 
    in
    Tree(t, x, t)
  else Empty 

(*Ex 2.5b is for me a bit unclear*)
