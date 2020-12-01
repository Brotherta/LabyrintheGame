open Format;;

let set_value () =
  printf "Initialisation en cours ...\n";
  let colonne = ref (-1) in
  let ligne = ref (-1) in
  let choixTermine = ref false in

  while not (!choixTermine) do

      print_string "Entrez le nombre de colonnes souhaite, ce nombre doit etre entre 3 et 40\n";
      while !colonne=(-1) do
        begin
          try colonne := read_int ();
          with int_of_string -> ();
        end;
          if not(!colonne > 3 && !colonne <= 40) then begin 
             print_string "Mauvaise entree, Entrez le nombre de colonnes souhaite, ce nombre doit etre entre 3 et 40\n";
             colonne := (-1)
          end
      done;

      print_string "Entrez le nombre de lignes souhaite, ce nombre doit etre entre 3 et 40\n";
      while !ligne = (-1) do
        begin
          try ligne := read_int();
          with int_of_string -> ();
        end;
          if not(!ligne > 3 && !ligne <= 40) then begin 
            print_string "Mauvaise entree, Entrez le nombre de ligne souhaite, ce nombre doit etre entre 3 et 40\n";
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
;;


let a,b = set_value();;