%token <int> NB
%token <string> ID
%token PLUS MOINS MULT DIV PARG PARD EGAL EOF
%token COLON VAR
%token DEBUT FIN BPINCEAU HPINCEAU COULEUR EPAISSEUR
%token AVANCE TOURNE
%token SI FAIRE ALORS SINON TANTQUE
%start <Syntax.program> s
%start <Syntax.interpreter> interp
%{ open Syntax %}
%%

s: p=program EOF                                     {p}
interp : d = declaration EOF {Decl d}
	| i = instruction EOF {Inst i}

program: ds=declaration* is=instruction    {(ds,is)}

declaration: VAR i=ID COLON                {i}

instruction:
  | AVANCE e=expression {Avance(e)}
  | TOURNE e=expression {Tourne(e)}
  | BPINCEAU {BasPinceau}
  | HPINCEAU {HautPinceau}
  | COULEUR c=ID {ChangeCouleur c}
  | EPAISSEUR e = expression {ChangeEpaisseur e}
  | i=ID EGAL e=expression {Affect(i,e)}
  | DEBUT b=blocinstruction FIN {Bloc(b)}
  | SI e=expression ALORS i_si=instruction  SINON s = instruction {Cond(e,i_si,Some s)}
  | TANTQUE e=expression FAIRE i=instruction {Repet(e,i)}
blocinstruction:
  | {[]}
  | i=instruction COLON b=blocinstruction {i::b}
expression:
  | s=ID  e=expressionSuite {App(Ident s,e)}
  | MOINS s=ID  e=expressionSuite {App(Neg(Ident s),e)}
  | n=NB  e=expressionSuite {App(Const n,e)}
  | MOINS n=NB  e=expressionSuite {App(Neg(Const n),e)}
  | PARG e=expression PARD es=expressionSuite {App(e,es)}
  | MOINS PARG e=expression PARD es=expressionSuite {App(e,es)}
expressionSuite:
  | PLUS e=expression {(Plus,e)}
  | MOINS e=expression {(Moins,e)}
  | MULT e=expression {(Mult,e)}
  | DIV e=expression {(Div,e)}
  | {(Identite,Const 0)}
/*sinon:
  | {None}
  | SINON i=instruction {Some(i)}
*/
