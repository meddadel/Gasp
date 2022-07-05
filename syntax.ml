(*Type représentant les différentes opérations possibles sur les expressions
(Plus,Moins,Div,Mult), et Identite qui marque la fin d'une expression*)
type op =
	Plus | Moins | Identite | Mult | Div
(*Type représentant une expression*)
type suiteexpr =
	op * expression
and expression =
	Const of int
	| Ident of string
	| App of expression * suiteexpr
	| Neg of expression
(*Type représentant les instructions possibles *)
type instruction =
	Avance of expression
	| Tourne of expression
	| BasPinceau
	| HautPinceau
	| ChangeEpaisseur of expression
	| ChangeCouleur of string
	| Affect of string * expression
	| Bloc of instruction list
	| Cond of expression * instruction * instruction option
	| Repet of expression * instruction
(*Type représentant une déclaration*)
type declaration = string
(* Type pour le mode fichier *)
type program = declaration list * instruction
(* Type pour le mode interprété*)
type interpreter = Inst of instruction | Decl of declaration
