open Graphics
(* Fonction reprise du cours*)
let print_position outx lexbuf =
  Lexing.(
    let pos = lexbuf.lex_curr_p in
    Printf.fprintf outx "Ligne %d Col %d"
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)
  )


let _ =
(* On verifie si on lance le programme avec ou sans un nom de fichier *)
  let nb_arg = Array.length Sys.argv in
  if (nb_arg = 2) then
	  let c = open_in Sys.argv.(1) in
	  let lb = Lexing.from_channel c
		 in
		 try
		    let ast =
		      Parser.s Lexer.lexeur lb
		    in Interpreter.exec ast;
		with
		| Lexer.Error msg ->
	     Printf.fprintf stderr "%a: Erreur lexeur reading %s\n" print_position lb msg;
	     exit (-1)
	  | Parser.Error ->
	     Printf.fprintf stderr "%a: Erreur de syntaxe \n" print_position lb;
	     exit (-1)
	  | Interpreter.Error s ->
	     Printf.fprintf stderr "%a Erreur  %s\n" print_position lb s;
	     exit (-1)
  else if(nb_arg = 1) then
  begin
  	Graphics.open_graph " 800x800";
  	print_string "Tapez Arret quand on avez fini\n";
	let rec loop i tortue =
		match i with
		| 0 -> print_string "Au revoir\n"
		| _ ->
		let r = read_line() in
		(*Arret tape -> fin *)
			if(r = "Arret") then loop 0 tortue
			else let p = Lexing.from_string r in
			try
				let q = Parser.interp Lexer.lexeur p in loop 1 (Interpreter.exec_interp q tortue)
			with
			| Lexer.Error msg ->
			 Printf.fprintf stderr "%a: Erreur lexical %s\n" print_position p msg;
			 exit (-1)
		  | Parser.Error ->
			 Printf.fprintf stderr "%a: Erreur de syntaxe \n" print_position p;
			 exit (-1)
		  | Interpreter.Error s ->
			 Printf.fprintf stderr "%a Erreur  %s\n" print_position p s;
			 exit (-1)
		in loop 1 (Interpreter.init_tortue []);
		(*Clic sur le fenetre pour fermer le programme *)
		let _ = Graphics.wait_next_event[Button_down] in
		Graphics.close_graph();
		end
	else
		Printf.fprintf stderr "Le programme doit etre lance avec 0 ou 1 arguments";
