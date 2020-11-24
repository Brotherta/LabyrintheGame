
open Unix;;

let rec affiche_hello () = 
  Unix.sleep 2;
  print_string "Hello"; print_newline ();
  affiche_hello ()
;;

let _ = Thread.create affiche_hello ();;
;;


let () =
  let s = read_line () in
  print_string s
;;