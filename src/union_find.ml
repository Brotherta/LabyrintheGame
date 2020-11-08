(* Module qui définie le type Union_Find *)

module type UF = 
  sig
    type t 
    val create : int -> t
    val find : t -> int -> int
    (* val union : t -> int -> int -> unit *)
  end
;;

module  UF = 
  struct 
    type t = {parent : int array; rang : int array}

    let create n = {parent = Array.init n (fun i -> i); rang = Array.init n (fun _-> 0)} 

    let rec find uf n =           (* On cherche le parent le plus haut de n *)
      if uf.parent.(n) = n then   (* On applique la compression des chemins pour applatir l'arbre.*)
        uf.parent.(n)
      else begin
        let parent = find uf uf.parent.(n) in
        uf.parent.(n) <- parent; 
        parent
      end

    
    let union uf n m = 
      let np = find uf n in  (* Parent de n *)
      let mp = find uf m in  (* Parent de n *)

        if np <> mp then   (* Si le parent de n est différent de parent de m alors on fait l'union de leurs racines. *)
          begin
            if uf.rang.(np) < uf.rang.(mp) then   (* On cherche a ratacher le parent du plus petit arbre *)
              uf.parent.(np) <- mp                (* avec le plus grand arbre ici si le rang de n < a m on ratache la racine de n à m.  *)

            else begin
              uf.parent.(mp) <- np;
              if uf.rang.(np) = uf.rang.(mp) then
                uf.rang.(np) <- np + 1
            end
          end

  end
;;



