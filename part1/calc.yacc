(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 
fun getVal((AST.formulaList myList):AST.program) = myList;
%%
(* required declarations *)
%name Calc

%term
  ID of string | NUM of int
| PLUS | TIMES | MINUS | NEGATE | EQUALS | LESSTHAN | GREATERTHAN | RPAREN | LPAREN | EOF
| LET | IN | END | IF | THEN | ELSE | FI | CONST of string | NOT | AND | OR | XOR | IMPLIES | EQ | TERM

%nonterm EXP  of AST.exp | START of AST.program | DECL  of AST.decl | program of AST.program | stmtList of AST.program | statement of AST.exp | FORMULA of AST.exp | intExp of AST.exp|  boolFormula of AST.exp |  gexp of AST.exp

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)


%start START

%right IMPLIES
%left AND OR XOR EQUALS
%right NOT
%left LESSTHAN GREATERTHAN
%left	PLUS MINUS
%left	TIMES
%right	NEGATE
%verbose

%%

	START	:	program (program)
	program	:	stmtList (stmtList)
	stmtList	:	statement stmtList (AST.formulaList([statement])) | statement (AST.formulaList([statement]))
	statement	:	FORMULA TERM (FORMULA)
	FORMULA	:	gexp (gexp)
	gexp		:	LET DECL IN gexp END ( AST.letExp(DECL,gexp)) | IF gexp THEN gexp ELSE gexp FI ( AST.iteExp(gexp1,gexp2,gexp3) ) | EXP ( EXP )
	DECL		:	ID EQ EXP ( AST.ValDecl(ID,EXP))
	EXP		:	intExp	(intExp)| boolFormula (boolFormula) | LPAREN EXP RPAREN (EXP) | ID (AST.varExp(ID))
	intExp		:	gexp EQUALS gexp (AST.binExp(AST.EQUALS,gexp1,gexp2)) | gexp LESSTHAN gexp (AST.binExp(AST.LESSTHAN,gexp1,gexp2))| gexp GREATERTHAN gexp (AST.binExp(AST.GREATERTHAN,gexp1,gexp2)) | gexp PLUS gexp (AST.binExp(AST.PLUS,gexp1,gexp2)) | gexp MINUS gexp (AST.binExp(AST.MINUS,gexp1,gexp2)) | gexp TIMES gexp (AST.binExp(AST.TIMES,gexp1,gexp2)) | NEGATE gexp (AST.unaryExp(AST.NEGATE,gexp)) | NUM (AST.numExp(NUM))
	boolFormula	:	gexp IMPLIES gexp (AST.binExp(AST.IMPLIES,gexp1,gexp2)) | gexp AND gexp (AST.binExp(AST.AND,gexp1,gexp2)) |gexp OR gexp (AST.binExp(AST.OR,gexp1,gexp2)) |gexp XOR gexp (AST.binExp(AST.XOR,gexp1,gexp2)) | NOT gexp (AST.unaryExp(AST.NOT,gexp)) | CONST (AST.constExp(CONST))
	
	
	
	
