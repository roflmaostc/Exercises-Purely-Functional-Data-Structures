module type Heap = sig
  type t
  type heap
  val empty: heap
  val isEmpty: heap -> bool

  val insert: t -> heap -> heap
  val merge: heap -> heap -> heap

  val findMin: heap -> t option
  val deleteMin: heap -> heap
  val from_list: t list -> heap
end

module type Ordered = sig
  type t
  val eq: t -> t -> bool
  val lt: t -> t -> bool
  val leq: t -> t -> bool
end

module WeightBiasedLeftistHeap (E:Ordered) : Heap with type t = E.t = struct 
  type t = E.t
  type heap = E | T of int * t * heap * heap

  
  exception Empty

  let rank x = match x with
    | E -> 0
    | T(w,_,_,_) -> w

  let empty = E

  let isEmpty x = match x with 
    | E -> true
    | _ -> false

  let rec merge t1 t2 = 
    let aux a b i e t= 
      let ra = rank a in
      let rb = rank b in
      if ra<=rb then T(i, e, a, merge b t)
      else T(i, e, b, merge a t)
    in
    match (t1,t2) with
    | (E, E) -> E
    | (E, _) -> t2
    | (_, E) -> t1
    | (T(i1, e1, l1, r1), T(i2, e2, l2, r2)) -> 
      if E.leq e1 e2 then 
        aux l1 r1 (i1+i2) e1 t2
      else 
        aux l2 r2 (i1+i2) e2 t1

  let insert x t = merge (T(1, x, E, E)) t

  let findMin t = match t with
    | E -> None
    | T(_, x, _, _) -> Some x

  let deleteMin t = match t with
    | E -> raise Empty
    | T(i, x, l, r) -> merge l r

  let from_list l = 
    let rec aux l acc = match l with
      | [] -> (match acc with
        | [] -> failwith "List empty"
        | [x] -> x
        | _ -> aux acc []) 
      | h::[] -> if acc = [] then h else aux (h::acc) []
      | h1::h2::tl -> aux tl (merge h1 h2::acc) 
    in
    aux (List.rev_map (fun x -> T(1, x, E, E)) l) []
end


module LeftistHeap (E:Ordered) : Heap with type t = E.t = struct 
  type t = E.t
  type heap = E | T of int * t * heap * heap

  exception Empty

  let rank x = match x with
    | E -> 0
    | T(x,_,_,_) -> x 
 
  let empty = E

  let isEmpty x = match x with 
    | E -> true
    | _ -> false

  let rec merge t1 t2 = 
    let update (T(i, e, l, r)) = 
      let rl, rr = rank l, rank r in
      if rl >= rr then T(rr+1,e, l,r) else T(rl+1, e, r, l)
    in
    match (t1,t2) with
    | (E, E) -> E
    | (E, _) -> t2
    | (_, E) -> t1
    | (T(i1, e1, l1, r1), T(i2, e2, l2, r2)) -> 
      if E.leq e1 e2 then update @@ T (i1, e1, l1, merge r1 t2 )
      else update @@ T (i2, e2, l2, merge r2 t1 )

  let insert_2 x t = merge (T(1, x, E, E)) t

  let rec insert x t = 
    let update (T(i, e, l, r)) = 
      let rl, rr = rank l, rank r in
      if rl >= rr then T(rr+1,e, l,r) else T(rl+1, e, r, l)
    in
    match t with
    | E -> T(1, x, E, E)
    | T(i, e, l, r) -> 
      if E.leq e x then update @@ T(i, e, l, insert x r) 
      else update @@ T(i, x, l, insert e r)

  let findMin t = match t with
    | E -> None
    | T(_, x, _, _) -> Some x

  let deleteMin t = match t with
    | E -> raise Empty
    | T(i, x, l, r) -> merge l r


  let from_list l = 
    let rec aux l acc = match l with
      | [] -> (match acc with
        | [] -> failwith "List empty"
        | [x] -> x
        | _ -> aux acc []) 
      | h::[] -> if acc = [] then h else aux (h::acc) []
      | h1::h2::tl -> aux tl (merge h1 h2::acc) 
    in
    aux (List.rev_map (fun x -> T(1, x, E, E)) l) []
end


module B = struct 
  type t = int 
  let eq = (=) 
  let leq = (<=) 
  let lt = (<) 
end 


module X = LeftistHeap( B)  
