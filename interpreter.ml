(*Type pour modéliser la position, l'angle dans le canevas mais aussi si le
pinceau est levé ou non et les variables déclarées avec leur valeurs*)
type tortue = {
	position : int * int;
	angle: int;
	dessine: bool;
	env: (string * int) list;
}
(* Renvoie une instance de tortue avec les identificateurs de envi*)
let init_tortue envi =
	{position = (0,0);angle = 90;dessine=false;env = envi}
(* Type pour les couleurs *)
open Syntax
open Graphics
(* Type pour construire la liste pour évaluer les expressions*)
type app =
	Expr of int | Op of op
exception Error of string
(* Fonctions auxiliaires pour le calcul de la position courante dans le canevas*)
(*Convertit un angle en degré en radiant *)
let deg_to_rad n =
	let a = n mod 360 in
		let a' = if(a < 0) then (float_of_int a) +. 360.
		 		else (float_of_int a)
		in
		a' /. 180. *. 3.1415927
(* Calcule la nouvelle position courante après un mouvement de dist, à partir
de la position (x,y), avec un angle de a degrés*)
let nvx_pos (x,y) a dist=
	let angle = deg_to_rad a in
	 let pos = 	(float_of_int x +. (float_of_int dist) *. (cos angle),
		 		float_of_int y +. (float_of_int dist) *. (sin angle))
	in (int_of_float (fst pos),int_of_float (snd pos))
(* Initialise les variables déclarées à 0 dans l *)
let init_var l = List.map (fun(s) -> (s,0)) l
(* Calcule *)
let calcul_op l =
  	let g = List.hd l in
	let rec aux l1 acc =
	 	match l1 with
		| [] -> acc
		| (Op op)::(Expr e)::ll -> begin match op with
								| Plus -> aux ll (e + acc)
								| Moins -> aux ll (acc - e)
								| _ -> failwith "Impossible_Op" end
		| _ -> failwith "Impossible_aux"
	in

		aux (List.tl l) (match g with Expr r -> r | _ -> failwith "Impossible")
(*Calcule la valeur de expr dans l'environnement env*)
let rec calcul env expr =
 	match expr with
	| Const n -> n
	| Ident s -> begin
					let e = List.assoc_opt s env in
					match e with
					| Some n -> n
					| None -> raise (Error ("identificateur " ^ s ^ " non declare"))
				end
	| Neg e -> let t = 0 - (calcul env e) in t
	| App(_,_) ->
		calcul_app env expr []
(*Calcule la valeur de expr qui est de la forme App().
Pour trouver sa valeur, on contruit une liste constitue uniquement des opérations
Moins et plus qu'on passe à calcul_op*)
and calcul_app env expr l =
	match expr with
	| App(e,(op,es)) -> begin let n = calcul env e in
					(* Si la liste est vide, on ajoute la valeur de l'expression
					et l'opération qui suit si elle est différente de Identite*)
					if ((List.length l) = 0)
				  then
				  	if (op <> Identite )
					then begin calcul_app env es [Op op;Expr n] end
					(*Si l'operation est Identite, alors on renvoie directement
					la valeur de l'expression*)
					else n
				else
					(*La liste étant non vide, on regarde l'opération qui
					est en tete *)
					let f = List.hd l in
					match f with
					(* Si l'opération est Div et que le diviseur est égal à 0,
					on lève une erreur *)
					| Op Div when n = 0 -> raise (Error "Division par 0")
					(* Si l'opération est Div ou Mult, on calcule tout de suite
					l'opération correspondante entre la valeur de e et le nombre
					présent en 2ème position dans la liste.*)
					| Op Div -> begin let n1 = List.hd (List.tl l) in
								match n1 with
								| Expr c -> if(op <> Identite) then calcul_app env es ((Op op)::(Expr (c / n))::(List.tl (List.tl l)))
											else calcul_op (List.rev ((Expr (c / n))::(List.tl (List.tl l))))
								| _ -> failwith("Soucis") end
					| Op Mult -> begin let n1 = List.hd (List.tl l) in
								match n1 with
								| Expr c -> if(op <> Identite) then calcul_app env es ((Op op)::(Expr(n * c))::(List.tl (List.tl l)))
											else calcul_op (List.rev ((Expr (c * n))::(List.tl (List.tl l))))
								| _ -> failwith("Soucis") end
					(* Si l'opération est Plus ou Moins, et que l'opération suivant de
					l'expression n'est pas identité, on ajoute en tete de la liste
					la valeur de e et l'opération suivante*)
					| Op Plus|Op Moins-> if (op <> Identite) then calcul_app env es ((Op op)::(Expr n)::l)
									(*Si l'opération est Identite, on calcule la value de l'expression
									grace à calcul_op*)
										else calcul_op (List.rev ((Expr n)::l))
					| _ -> failwith "Impossible App_f" end
	| _ -> failwith "Impossible App "
(*Execute l'instruction i*)
let rec exec_inst tortue i =
	match i with
	| Avance e ->
		begin
		let n = calcul tortue.env e in
		let npos = nvx_pos tortue.position tortue.angle n in
		if((fst npos) < 0 || (snd npos) < 0) then (raise (Error "Sortie du canevas"));
		if (tortue.dessine) then lineto (fst npos) (snd npos)
		else moveto (fst npos) (snd npos);
		{tortue with position=npos}
		end
	| Tourne e ->
		let n = calcul tortue.env e in
		{ tortue with angle = ((tortue.angle + n) mod 360) }
	| BasPinceau -> {tortue with dessine = true}
	| HautPinceau -> {tortue with dessine = false}
	| ChangeCouleur c -> let col = match c with
						| "noir" -> black
						| "vert" -> green
						| "rouge" -> red
						| "jaune" -> yellow
						| "bleu" -> blue
						| _ -> raise (Error ("Couleur " ^ c ^ "inconnu"))
						in set_color col; tortue
	| ChangeEpaisseur e -> set_line_width (calcul tortue.env e); tortue
	| Affect(s,e) ->
		let ident = List.assoc_opt s tortue.env in
		begin match ident with
		| None -> raise (Error ("identificateur " ^ s ^ " non declare"))
		| Some _ ->
		let n = calcul tortue.env e in
		{ tortue with env =
		(List.map (fun (id,v) -> if(s = id) then (id,n) else (id,v)) tortue.env) }
		end
	| Bloc(l) -> exec_bloc tortue l
	| Cond(e,i1,i2) ->
		let n = calcul tortue.env e in
		if(n <> 0)
		then
			exec_inst tortue i1
		else
			begin
			match i2 with
			| Some si -> exec_inst tortue si
			| None -> tortue end
	| Repet(e,i1) ->
		let n = calcul tortue.env e in
		if (n <> 0)
		then
			let t = exec_inst tortue i1 in
			exec_inst t i;
		else
			tortue

and exec_bloc tortue b =
	match b with
	| [] -> tortue
	| i::ll -> let t = exec_inst tortue i in
			exec_bloc t ll;;
(* Exécute les instructions du programme p*)
let exec p =
	open_graph " 800x800";
	let envi = init_var (fst p) in
	let tortue = {position = (0,0);angle = 90;dessine=false;env = envi} in
	let _ = exec_inst tortue (snd p) in
	let _ = Graphics.wait_next_event[Button_down] in
	close_graph()
(* Exécute les commandes tapées dans le mode interprété du programme *)
let exec_interp e tortue =
	match e with
	| Decl d -> {tortue with env = (d,0)::tortue.env}
	| Inst i -> exec_inst tortue i
