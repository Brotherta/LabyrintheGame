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


(***********************************************************************************************)
(***********************************************************************************************)
                (* Génération et déclaration de variable *)
(***********************************************************************************************)
(***********************************************************************************************)
open Format
let case_pacman = ref (0,0)
let win = ref false
let win_fantome = ref false
let margin = ref 0 
let upleftx = ref 0 
let uplefty = ref 0 
let l = ref 0
let h = ref 0
let case_fantome = ref (0,0)
let taille_case = ref 0
let mur_voisine = ref [|[| [0,0,true] |] |]
let width = ref 0
let heigh = ref 0
;;

type direction = 
  | Left
  | Right
  | Up
  | Down
  | Wrong
type entity = 
  | Pacman
  | Fantome
;;

(* GENERATION DU TABLEAU DES CASES VOISINES *)
let generate_voisine l h =
  Array.init l (fun _ -> Array.init h (fun _ -> []))

let cases_voisines_first (d,x,y) =
  if d = 0 then [(x+1, y, false)] else [(x, y+1, false)]

let complete_mur_voisine mur_voisine =
  for col = 0 to !h-1 do
    for lig = 0 to !l-1 do
      let l_voisine = mur_voisine.(lig).(col) in
      for i = 0 to (List.length l_voisine)-1 do
        let (x,y,fait) =  List.nth l_voisine i in
        if not fait then begin 
          mur_voisine.(x).(y) <- List.append mur_voisine.(x).(y) [(lig, col, true)]
        end
      done;
    done;
  done;
  mur_voisine
;;


(* GENERATION DU LABYRINTHE *)

let mur_au_hasard l h =  (* Tire un mur aléatoire, et renvoie (d,x y) ou d = 0 ou 1 (0 pour vertical et 1 pour horizontal x colonne et y lignes. *)      
  let _ = Random.self_init() in
  let n = Random.int ((l-1)*h + l*(h-1)) in (* nombre de mur total*)
  if n < (l-1) * h then
    (0, n mod (l-1), n / (l-1))
  else let n2 = n - (l-1) * h in
    (1, n2 mod l, n2 / l)

let cases_adjacentes l h (d,x,y) = 
  if d = 0 then (* vertical *)
    ((y*l) + x, (y*l) + x +1)
  else (* horizontal *)
    ((y*l) + x, (y*l) + x +l)

let generate_mur_present l h = 
  Array.init 2 (fun _ -> Array.init l (fun _ -> Array.init h (fun _ -> true)))

let generate_lab l h =
  let mur_present = generate_mur_present l h in
  let mur_voisine = generate_voisine l h in
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
      mur_voisine.(x).(y) <- List.append mur_voisine.(x).(y) (cases_voisines_first (d,x,y));
      acc := !acc + 1;
    end
  done;
  mur_present.(0).(l-1).(h-1) <- false;
  let mur_voisine_new = complete_mur_voisine mur_voisine in
  (mur_present, mur_voisine_new)
;;
(***********************************************************************************************)
(***********************************************************************************************)

(***********************************************************************************************)
(***********************************************************************************************)


(***********************************************************************************************)
(***********************************************************************************************)
                (* Affichage et dessin *)
(***********************************************************************************************)
(***********************************************************************************************)

(* AFFICHAGE DU LABYRINTHE *)
let trace_pourtour upleftx uplefty taille_case l h = 
  Graphics.set_color Graphics.black;
  Graphics.moveto upleftx uplefty;
  Graphics.lineto (upleftx + taille_case*l) uplefty;
  Graphics.lineto (upleftx + taille_case*l) ((uplefty - taille_case*h) + taille_case);
  Graphics.moveto (upleftx + taille_case*l) (uplefty - taille_case*h);
  Graphics.lineto (upleftx) (uplefty - taille_case*h);
  Graphics.lineto (upleftx) (uplefty - taille_case)

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

(* AFFICHAGE PACMAN ET FANTOME *)

let draw_entity (x,y) entity =  
  if entity = Pacman then begin
    case_pacman := (x,y);
    let abs = !upleftx + ((!taille_case / 2) + x * !taille_case) in
    let ord = !uplefty - ((!taille_case / 2) + y * !taille_case) in
    let taille_pacman = (float_of_int !taille_case -. 0.2 *. float_of_int !taille_case) /. 2. in 

    Graphics.set_color Graphics.yellow;
    Graphics.fill_arc abs ord (int_of_float taille_pacman) (int_of_float taille_pacman) 345 15;
    Graphics.set_color Graphics.black;
    
    Graphics.moveto abs ord;
    Graphics.lineto (int_of_float (float_of_int abs +. (Float.cos 0.261799) *. taille_pacman)) (int_of_float (float_of_int ord +. (Float.sin 0.261799) *. taille_pacman));
    
    Graphics.moveto abs ord;
    Graphics.lineto (int_of_float (float_of_int abs +. (Float.cos 6.02139) *. taille_pacman)) (int_of_float (float_of_int ord +. (Float.sin 6.02139) *. taille_pacman)); 
    
    Graphics.draw_arc abs ord (int_of_float (taille_pacman*.1.02)) (int_of_float  (taille_pacman*.1.02)) 345 15;
    
    Graphics.fill_circle (abs + int_of_float(taille_pacman *. 0.25)) (ord + int_of_float(taille_pacman *. 0.40)) (int_of_float (taille_pacman *. 0.15))
  end
  else begin
    case_fantome := (x,y);
    let abs = !upleftx + ((!taille_case / 2) + x * !taille_case) in
    let ord = !uplefty - ((!taille_case / 2) + y * !taille_case) in
    let taille_pacman = (float_of_int !taille_case -. 0.2 *. float_of_int !taille_case) /. 2. in 

    Graphics.set_color Graphics.blue; 
    Graphics.fill_arc abs ord (int_of_float taille_pacman) (int_of_float taille_pacman) 345 15;
    Graphics.set_color Graphics.black;
    
    Graphics.moveto abs ord;
    Graphics.lineto (int_of_float (float_of_int abs +. (Float.cos 0.261799) *. taille_pacman)) (int_of_float (float_of_int ord +. (Float.sin 0.261799) *. taille_pacman));
    
    Graphics.moveto abs ord;
    Graphics.lineto (int_of_float (float_of_int abs +. (Float.cos 6.02139) *. taille_pacman)) (int_of_float (float_of_int ord +. (Float.sin 6.02139) *. taille_pacman)); 
    
    Graphics.draw_arc abs ord (int_of_float (taille_pacman*.1.02)) (int_of_float  (taille_pacman*.1.02)) 345 15;
    
    Graphics.fill_circle (abs + int_of_float(taille_pacman *. 0.25)) (ord + int_of_float(taille_pacman *. 0.40)) (int_of_float (taille_pacman *. 0.15))
  end

let draw_pas_entity (x,y) =
  let abs = !upleftx + ((!taille_case / 2) + x * !taille_case) in
  let ord = !uplefty - ((!taille_case / 2) + y * !taille_case) in
  let taille_entity = (float_of_int !taille_case -. 0.2 *. float_of_int !taille_case) /. 2. in 
  Graphics.set_color Graphics.white;
  Graphics.fill_circle abs ord (int_of_float(taille_entity *. 1.1));
;;


(* AFFICHAGE VICTOIRE ET DEFAITE *)
let draw_win () =
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 !width !heigh;
  Graphics.moveto (!width / 2) (!heigh / 2);
  Graphics.set_color Graphics.black;
  Graphics.draw_string "YOU WIN";
  Unix.sleep 10;
  exit 0

let draw_lose () =
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 !width !heigh;
  Graphics.moveto (!width / 2) (!heigh / 2);
  Graphics.set_color Graphics.black;
  Graphics.draw_string "GAME OVER !";
  Unix.sleep 10;
  exit 0

let draw_win_or_lose () =
  if !win_fantome then draw_lose ()
  else draw_win () 
;;

(***********************************************************************************************)
(***********************************************************************************************)

(***********************************************************************************************)
(***********************************************************************************************)


(***********************************************************************************************)
(***********************************************************************************************)
                (* Gestion des deplacements*)
(***********************************************************************************************)
(***********************************************************************************************)
(* MOUVEMENT D'UNE ENTITE*)
let wrong_move_out_of_bound mur_present direction (x,y)  = 
  let taille_x = Array.length mur_present.(0) in 
  let taille_y = Array.length mur_present.(0).(0) in
  if (x,y) = (!l-1,!h-1) && direction = Right then begin
    true
  end
  else if x < 0 && direction = Left then false
  else if x > taille_x && direction = Right then false 
  else if y < 0 && direction = Up then false
  else if y > taille_y && direction = Down then false
  else true

let wrong_move_wall mur_present direction (x,y) = match direction with
  | Left -> not mur_present.(0).(x-1).(y)
  | Right -> not mur_present.(0).(x).(y)
  | Up -> not mur_present.(1).(x).(y-1)
  | Down -> not mur_present.(1).(x).(y)
  | Wrong -> false

let move_entity mur_present direction (x,y) entity = match direction with 
    | Left -> if (wrong_move_out_of_bound mur_present direction (x-1, y)) && (wrong_move_wall mur_present direction (x,y) ) 
            then begin  
              draw_pas_entity (x,y);
              draw_entity (x-1,y) entity
            end
    | Right -> if (wrong_move_out_of_bound mur_present direction (x+1, y)) && (wrong_move_wall mur_present direction (x,y) ) 
            then begin
              draw_pas_entity (x,y);
              draw_entity (x+1,y) entity
             
            end
    | Up -> if (wrong_move_out_of_bound mur_present direction (x,y-1)) && (wrong_move_wall mur_present direction (x,y) ) 
            then begin 
              draw_pas_entity (x,y);
              draw_entity (x,y-1) entity
              
            end
    | Down -> if (wrong_move_out_of_bound mur_present direction (x,y+1)) && (wrong_move_wall mur_present direction (x,y) ) 
            then begin 
              draw_pas_entity (x,y);
              draw_entity (x,y+1) entity
              
            end
    | Wrong -> ()
;;
(* MOUVEMENT DU PACMAN *)
let handle_char c mur_present (x,y) = match c with 
   | 'z'  -> move_entity mur_present Up (x,y) Pacman
   | 'q' -> move_entity mur_present Left (x,y) Pacman
   | 's' -> move_entity mur_present Down (x,y) Pacman
   | 'd'  -> move_entity mur_present Right (x,y) Pacman
   | 'p'-> exit 0
   |  _ -> move_entity mur_present Wrong (x,y) Pacman
;;

(* MOUVEMENT DU FANTOME *)

let rec est_reliee src dest evite mur_voisines =
  if src = dest then true
  else begin 
    let (x_src,y_src) = src in
    let voisines = mur_voisines.(x_src).(y_src) in
    let l_voisines = List.length voisines in

    let my_bool = ref false in
    let my_true_bool = ref false in

    for i = 0 to l_voisines -1 do
      let (x_cur, y_cur, mybool) = List.nth voisines i in
      let current = x_cur, y_cur in
      if (current <> evite) then begin
        my_bool := est_reliee current dest (x_src,y_src) mur_voisines;
        if !my_bool then my_true_bool := true
      end
    done;
    !my_true_bool;
  end

let which_direction current =
  let x_cur, y_cur = current in
  let x_fan, y_fan = !case_fantome in
  if x_fan < x_cur then Right
  else if x_fan > x_cur then Left
  else if y_fan < y_cur then Down
  else Up

let where_to_move mur_voisine = 
  let x,y = !case_fantome in 
  let voisines = mur_voisine.(x).(y) in
  let l_voisines = List.length voisines in
  let direction = ref Right in
  for i = 0 to l_voisines - 1 do
    let (x_cur, y_cur, mybool) = List.nth voisines i in
    let current = x_cur, y_cur in
    if est_reliee current !case_pacman !case_fantome mur_voisine then direction := which_direction current
  done;
  !direction
;;

(***********************************************************************************************)
(***********************************************************************************************)

(***********************************************************************************************)
(***********************************************************************************************)


(***********************************************************************************************)
(***********************************************************************************************)
                (* Conditions du jeu *)
(***********************************************************************************************)
(***********************************************************************************************)
let win_or_lose () = 
  let x,y = !case_pacman in
  if !case_fantome = !case_pacman then  win_fantome := true
  else if (!l, !h-1) = (x,y) then win := true

let pacman_win () = 
  let x,y = !case_pacman in
  if (!l, !h-1) = (x,y) then begin 
    win := true;
  end

let pacman mur_present = 
  draw_entity !case_pacman Pacman ;
  while not (!win || !win_fantome) do
    let x,y = !case_pacman in
    let s = Graphics.wait_next_event [Graphics.Key_pressed] in
    handle_char s.Graphics.key mur_present (x,y);
    win_or_lose ();
  done;
  draw_win_or_lose ()

let fantome mur_present = 
  draw_entity !case_fantome Fantome;
  Unix.sleep 2;
  while not (!win || !win_fantome) do
    Unix.sleep 1; 
    let x,y = !case_fantome in
    move_entity mur_present (where_to_move !mur_voisine) (x,y) Fantome;
    
    win_or_lose ();
  done;
  draw_win_or_lose ()

let thread_fantome mur_present = Thread.create fantome mur_present
;;
   
(* INITIALISATION DU JEU *)
let set_value () =
  printf "Initialisation en cours ...\n";
  let colonne = ref (-1) in
  let ligne = ref (-1) in
  let choixTermine = ref false in

  while not (!choixTermine) do

      print_string "Entrez le nombre de colonnes souhaite, ce nombre doit etre entre 4 et 40\n";
      while !colonne=(-1) do
        begin
          try colonne := read_int ();
          with int_of_string -> ();
        end;
          if not(!colonne > 3 && !colonne <= 40) then begin 
             print_string "Mauvaise entree, Entrez le nombre de colonnes souhaite, ce nombre doit etre entre 4 et 40\n";
             colonne := (-1)
          end
      done;

      print_string "Entrez le nombre de lignes souhaite, ce nombre doit etre entre 4 et 40\n";
      while !ligne = (-1) do
        begin
          try ligne := read_int();
          with int_of_string -> ();
        end;
          if not(!ligne > 3 && !ligne <= 40) then begin 
            print_string "Mauvaise entree, Entrez le nombre de ligne souhaite, ce nombre doit etre entre 4 et 40\n";
            ligne := (-1)
          end
      done;

      printf "Etes vous satisfait %d colonnes et  %d lignes oui / non : \n" !colonne !ligne;
      let choix = read_line () in 
      if choix = "oui" then choixTermine := true
      else begin
        choixTermine := false;
        colonne:=(-1);
        ligne:=(-1);
      end
  done;
  !ligne, !colonne


let start_game () = 
  let l_user, h_user = set_value () in
  win := false;
  win_fantome := false;
  l := l_user;
  h := h_user;

  case_pacman := (0,0);
  case_fantome := (!l-1, 0);

  margin := 40;
  if l > h then taille_case := ((700 -2* !margin)/ !l)
  else taille_case := ((700 -2* !margin)/ !h);
  width := (!l * !taille_case) + (2 * !margin) ;
  heigh := (!h * !taille_case) + (2 * !margin) ;

  upleftx := !margin;
  uplefty := !heigh - !margin;

  let my_str = " "^string_of_int !width^"x"^string_of_int !heigh in
  Graphics.open_graph my_str;
;;  
(***********************************************************************************************)
(***********************************************************************************************)

(***********************************************************************************************)
(***********************************************************************************************)


(***********************************************************************************************)
(***********************************************************************************************)
                (* Main du jeu *)
(***********************************************************************************************)
(***********************************************************************************************)
(*ocamlc -thread graphics.cma unix.cma threads.cma projet.ml.*)
let () =
  start_game ();
  let mur_present, mur_vois = generate_lab !l !h in
  mur_voisine := mur_vois;
  trace_pourtour !upleftx !uplefty !taille_case !l !h;
  trace_lab !upleftx !uplefty !taille_case !l !h mur_present;

  thread_fantome mur_present;
  pacman mur_present;
  ignore @@ Graphics.read_key ()
;;
(***********************************************************************************************)
(***********************************************************************************************)

(***********************************************************************************************)
(***********************************************************************************************)

