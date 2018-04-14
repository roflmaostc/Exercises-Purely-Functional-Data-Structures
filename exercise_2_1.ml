(*not tail recursive*)
let rec suffixes l = match l with
  | [] -> [[]]
  | x::xs -> l::(suffixes xs)


(*tail recursive*)
let suffixes_tr l =
  let rec aux l acc = match l with
    | [] -> List.rev ([]::acc)
    | x::xs -> aux xs (l::acc)
  in
  aux l []
