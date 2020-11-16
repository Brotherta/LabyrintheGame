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
  let _ = Random.self_init() in
  let n = Random.int ((l-1)*h + l*(h-1)) in (* nombre de mur total*)
  if n < (l-1) * h then
    (0, n mod (l-1), n / (l-1))
  else let n2 = n - (l-1) * h in
    (1, n2 mod l, n2 / l)
;;

mur_au_hasard 2 2;;

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

let trace_pourtour upleftx uplefty taille_case l h = 
  Graphics.set_color Graphics.black;
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
    Graphics.moveto (upleftx + x*taille_case) (uplefty - (y+1)*taille_case);
    Graphics.lineto (upleftx + x*taille_case + taille_case) (uplefty - (y+1)*taille_case)
  end

let trace_lab upleftx uplefty taille_case l h mur_present = 
  for d = 0 to 1 do
    for x = 0 to l-1 do
      for y = 0 to h-1 do
        if mur_present.(d).(x).(y) then begin
          trace_mur upleftx uplefty taille_case (d,x,y)
        end
      done
    done
  done
;;
(*value draw_circle : int -> int -> int -> unit
 Graphics.draw_circle 200 200 (int_of_float taille_pacman)
  draw_circle x y r draws a circle with center x,y and radius r. The current point is unchanged. 
  fill_circle
  value set_color : color -> unit
  draw_arc x y rx ry a1 a2 draws an elliptical arc with center x,y, horizontal radius rx, vertical radius ry, from angle a1 to angle a2 (in degrees). The current point is unchanged.
  Soh Cah Toa = Sinus Opposé Hypon  Cosinus Adjacent Hypon  Tangente opposé Adjacent *)


let draw_pacman upleftx uplefty taille_case (x,y) = 
  let abs = upleftx + ((taille_case / 2) + x * taille_case) in
  let ord = uplefty - ((taille_case / 2) + y * taille_case) in
  let taille_pacman = (float_of_int taille_case -. 0.2 *. float_of_int taille_case) /. 2. in 
  Graphics.set_color Graphics.yellow;
  Graphics.fill_arc abs ord (int_of_float taille_pacman) (int_of_float taille_pacman) 345 15;
  Graphics.set_color Graphics.black;
  Graphics.moveto abs ord;
  Graphics.lineto (int_of_float (float_of_int abs +. (Float.cos 0.261799) *. taille_pacman)) (int_of_float (float_of_int ord +. (Float.sin 0.261799) *. taille_pacman));
  Graphics.moveto abs ord;
  Graphics.lineto (int_of_float (float_of_int abs +. (Float.cos 6.02139) *. taille_pacman)) (int_of_float (float_of_int ord +. (Float.sin 6.02139) *. taille_pacman)); 
  Graphics.draw_arc abs ord (int_of_float (taille_pacman*.1.02)) (int_of_float  (taille_pacman*.1.02)) 345 15;
  Graphics.fill_circle (abs + int_of_float(taille_pacman *. 0.25)) (ord + int_of_float(taille_pacman *. 0.40)) (int_of_float (taille_pacman *. 0.15))
;;

let pacman = 
  while true do
    let s = Graphics.wait_next_event [Graphics.Key_pressed] in

  done;
;;


#load "graphics.cma";;
Graphics.open_graph " 1000x1000";;

let () =
  (* trace_pourtour  *)
  let margin = 20 in
  let upleftx = margin in
  let uplefty = 1000 - margin in
  let l = 25 in
  let h = 25 in
  let mur_present = generate_lab l h in
  
  let taille_case = ((700 - 2* margin)/ l) in
  draw_pacman upleftx uplefty taille_case (1,1);


  trace_pourtour upleftx uplefty taille_case l h;
  (* trace_mur upleftx uplefty taille_case (1,2,2); *)
  trace_lab upleftx uplefty taille_case l h mur_present;
  ignore @@ Graphics.read_key ()
;;



let margin = 20;;
let upleftx = margin;;
let uplefty = 1000 - margin;;
let m = generate_lab 3 3;;
let taille_case = ((700 - 2* margin)/ 3);;
let s = trace_pourtour upleftx uplefty taille_case 3 3;;
  (* trace_mur upleftx uplefty taille_case (1,2,2); *)
let a = trace_lab upleftx uplefty taille_case 3 3 m;;