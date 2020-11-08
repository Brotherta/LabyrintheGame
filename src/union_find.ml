module type UF = 
  sig
    type t 

    val create : int -> t

    val find : t -> int -> int

    val union : t -> int -> int -> unit

  end
;;

module Union_find : UF = 
  struct 
    type t = {parent : int array; rank : int array}

    let create n = {parent = [|n|]; rank = [|0|] }

    let rec find uf n = 
      if n <> uf.
      
    let union uf n m =


  end
;;