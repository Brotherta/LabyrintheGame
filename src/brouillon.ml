(***********************************************************************************************)
(***********************************************************************************************)
                (* Module qui définie le type Union_Find *)
(***********************************************************************************************)
(***********************************************************************************************)


module type UF = 
  sig
    type t 
    val create : int -> t
    val find : t -> int -> int
    val union : t -> int -> int -> unit
  end
;;


module  UF = 
  struct 
    type t = {parent : int array; rang : int array}

    let create n = {parent = Array.init n (fun i -> i); rang = Array.init n (fun _-> 0)} 

    (* let rec find uf n =           (* On cherche le parent le plus haut de n *)
      if uf.parent.(n) = n then   (* On applique la compression des chemins pour applatir l'arbre.*)
        uf.parent.(n)
      else begin
        let parent = find uf uf.parent.(n) in
        uf.parent.(n) <- parent; 
        parent
      end *)
    
    let rec find uf n = 
      if uf.parent.(n) <> n then begin
        uf.parent.(n) <- find uf uf.parent.(n);
        uf.parent.(n)
      end
      else
        uf.parent.(n)
    ;;
    
    let union uf n m = 
      let np = find uf n in  (* Parent de n *)
      let mp = find uf m in  (* Parent de n *)

      if np <> mp then begin (* Si le parent de n est différent de parent de m alors on fait l'union de leurs racines. *)
        if uf.rang.(np) < uf.rang.(mp) then   (* On cherche a ratacher le parent du plus petit arbre *)
          uf.parent.(np) <- mp                (* avec le plus grand arbre ici si le rang de n < a m on ratache la racine de n à m.  *)

        else begin
          uf.parent.(mp) <- np;
          if uf.rang.(np) = uf.rang.(mp) then
            uf.rang.(np) <- uf.rang.(np) + 1
        end
      end

  end
;;
(***********************************************************************************************)
(***********************************************************************************************)


(***********************************************************************************************)
(***********************************************************************************************)
                    (* Labyrinthe.ml... *)

open Format

(* Tire un mur aléatoire, et renvoie (d,x y) ou d = 0 ou 1 (0 pour vertical et 1 pour horizontal x colonne et y lignes. *)                    
let mur_au_hasard l h = 
  let n = Random.int ((l-1)*h + l*(h-1)) in (* nombre de mur total*)
  if n < (l-1) * h then
    (0, n mod (l-1), n / (l-1))
  else let n2 = n - (l-1) * h in
    (1, n2 mod l, n2 / l)
;;


let cases_adjacentes l h (d,x,y) = 
  if d = 0 then (* vertical *)
    ((y*l) + x, (y*l) + x +1)
  else (* horizontal *)
    ((y*l) + x, (y*l) + x +l)
;;


let generate_mur_present l h = 
  Array.init 2 (fun _ -> Array.init l (fun _ -> Array.init h (fun _ -> true)))
;;

let generate_lab l h =
  let mur_present = generate_mur_present l h in
  let uf = UF.create (l*h) in
  let acc = ref 1 in
  let test = ref (l*h) in

  while acc < test do
    printf "(acc %d)" !acc;
    let (d,x,y) = mur_au_hasard l h in
    let (i,j) = cases_adjacentes l h (d,x,y) in 

    if UF.find uf i = UF.find uf j then 
      () (* pas d'incrémentation on fait rien *)
    
    else begin
      UF.union uf i j;
      mur_present.(d).(x).(y) <- false;
      acc := !acc + 1;
    end
  done;
  mur_present.(0).(l-1).(h-1) <- false;
  mur_present
;;

#load "graphics.cma";;
Graphics.open_graph " 700x700";;

let trace_pourtour upleftx uplefty taille_case l h = 
  Graphics.moveto upleftx uplefty;
  Graphics.lineto (upleftx + taille_case*l) uplefty;
  Graphics.lineto (upleftx + taille_case*l) ((uplefty - taille_case*h) + taille_case);
  Graphics.moveto (upleftx + taille_case*l) (uplefty - taille_case*h);
  Graphics.lineto (upleftx) (uplefty - taille_case*h);
  Graphics.lineto (upleftx) (uplefty - taille_case)
;;

let trace_mur upleftx uplefty taille_case (d,x,y) = 
  if d = 0 then begin
    Graphics.moveto (upleftx + (x+1)*taille_case) (uplefty - y*taille_case);
    Graphics.lineto (upleftx + (x+1)*taille_case) (uplefty - y*taille_case - taille_case)
  end
  else begin
    (* printf "d:%d x:%d y:%d ?:%d" d x y (uplefty - (y+1)*taille_case); *)
    Graphics.moveto (upleftx + x*taille_case) (uplefty - (y+1)*taille_case);
    Graphics.lineto (upleftx + x*taille_case + taille_case) (uplefty - (y+1)*taille_case)
  end

let trace_lab upleftx uplefty taille_case l h mur_present = 
  printf "\n";
  for d = 0 to 1 do
    for x = 0 to l-1 do
      for y = 0 to h-1 do
        (* printf "(%d,%d,%d) " d x y; *)
        (* print_bool mur_present.(d).(x).(y); *)
        (* printf " "; *)
        if mur_present.(d).(x).(y) then begin
          (* printf "(%d,%d,%d)\n" d x y; *)
          trace_mur upleftx uplefty taille_case (d,x,y)
        end
      done
    done
  done
;;

generate_lab 3 3;;




;;
let (d,x,y) = mur_au_hasard 3 3;;
let (i,j) = cases_adjacentes 3 3(d,x,y);;
;;



let margin = 20;;
let upleftx = margin;;
let uplefty = 700 - margin ;;
let l = 5 ;;
let h = 5 ;;
let mur_present = generate_lab l h ;;

let taille_case = ((700 - 2* margin)/ l) ;;

let a = trace_pourtour upleftx uplefty taille_case l h;;
(* trace_mur upleftx uplefty taille_case (1,2,2); *)
let b = trace_lab upleftx uplefty taille_case l h mur_present;;



let qd = generate_lab 5 5;;

let t = [|[|[|false; true; true; false; false|];
[|false; false; false; true; false|];
[|true; false; false; false; true|]; [|false; true; true; false; true|];
[|true; true; true; true; false|]|];
[|[|false; false; true; false; true|]; [|true; true; true; true; true|];
[|false; true; true; false; true|]; [|false; false; false; false; true|];
[|false; false; true; false; true|]|]|]


let margin = 20;;
let upleftx = margin;;
let uplefty = 700 - margin ;;
let l = 5 ;;
let h = 5 ;;
let mur_present = generate_lab l h ;;
let taille_case = ((700 - 2* margin)/ l) ;;
let b = trace_pourtour upleftx uplefty taille_case l h;;
let a = trace_lab upleftx uplefty taille_case l h mur_present;;

;;
let c = trace_mur upleftx uplefty taille_case (1,1,0);;


true false true    false false false    true true false
false false true   true false true      false true true

let c = Random.init 