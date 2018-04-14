module type Ordered = sig
  type t
  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val gt : t -> t -> bool
  val leq : t -> t -> bool
end

module UnbalancedMaps (E: Ordered) = struct
  type key = E.t
  type 'a map = Empty | Tree of 'a map * key * 'a * 'a map

  let (=) = E.eq
  let (>) = E.gt
  let (<) = E.lt
  let (<=) = E.leq
  
  let empty = Empty

  let rec lookup x t = match t with
    | Empty -> raise Not_found 
    | Tree(l, k, v, r) -> if x<k then lookup x l
      else if x>k then lookup x r
      else v

  let rec bind k v t = match t with
    | Empty -> Tree(Empty, k, v , Empty)
    | Tree(l, k2, v2, r) -> if k<k2 then Tree(bind k v l, k2, v2, r)
      else if k>k2 then Tree(l, k2, v2, bind k v r)
      else Tree(l, k, v, r)
end
