{
	open Parser
	open Lexing
	exception Error of string
	let next_line lexbuf =
	   let pos = lexbuf.lex_curr_p in
	   lexbuf.lex_curr_p <-
		 { pos with pos_bol = lexbuf.lex_curr_pos;
					pos_lnum = pos.pos_lnum + 1
		 }
}
let identificateur = ['a'-'z']['a'-'z''A'-'Z''0'-'9']*
let nombre = ['1'-'9']['0'-'9']* | '0'

rule lexeur = parse
 |[' ''\t'] {lexeur lexbuf}
 | '\n' {next_line lexbuf; lexeur lexbuf}
 | nombre as n {NB (int_of_string n)}
 | identificateur as i {ID i}
 | "Var" {VAR}
 | ';'  {COLON}
 | "Debut" {DEBUT}
 | "Fin" {FIN}
 | "Avance" {AVANCE}
 | "Tourne" {TOURNE}
 | "HautPinceau" {HPINCEAU}
 | "BasPinceau" {BPINCEAU}
 | "ChangeCouleur" {COULEUR}
 | "ChangeEpaisseur" {EPAISSEUR}
 | "Si" {SI}
 | "Alors" {ALORS}
 | "Sinon" {SINON}
 | "Tant que" {TANTQUE}
 | "Faire" {FAIRE}
 | '+' {PLUS}
 | '-' {MOINS}
 | '(' {PARG}
 | ')' {PARD}
 | '=' {EGAL}
 | '*' {MULT}
 | '/' {DIV}
 | eof {EOF}
 | _  {raise (Error (Lexing.lexeme lexbuf))}
