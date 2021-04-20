structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val eof = fn () => Tokens.EOF(!pos, !pos)
  val error = fn (e, l:int, _) => TextIO.output(TextIO.stdOut,"line " ^ (Int.toString l) ^ ": " ^ e ^ "\n")

  


  val keywords =
  [
  
   ("end",  Tokens.END),
   ("in",  Tokens.IN),
   ("let",  Tokens.LET),
   ("if",  Tokens.IF),
   ("then",  Tokens.THEN),
   ("else",  Tokens.ELSE),
   ("fi",  Tokens.FI)
  ]

  fun findKeywords (str:string, pos1:pos, pos2:pos) =
  case List.find (fn (s, _) => s = str )  keywords of 
  SOME (_, tk) => tk(pos1, pos2) 
  | NONE => Tokens.ID(str, pos1, pos2)
  %%
%header (functor CalcLexFun(structure Tokens:Calc_TOKENS));

alpha=[A-Za-z];
digit=[0-9];

ws = [\ \t];
%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
"AND" =>  (Tokens.AND(!pos,!pos));
"OR" =>  (Tokens.OR(!pos,!pos));
"XOR" =>  (Tokens.XOR(!pos,!pos));
"IMPLIES" =>  (Tokens.IMPLIES(!pos,!pos));
"NOT" =>  (Tokens.NOT(!pos,!pos));
"PLUS"      => (Tokens.PLUS(!pos,!pos));
"TIMES"      => (Tokens.TIMES(!pos,!pos));
"("      => (Tokens.LPAREN(!pos,!pos));
")"      => (Tokens.RPAREN(!pos,!pos));
"LESSTHAN"	=> (Tokens.LESSTHAN(!pos,!pos));
"GREATERTHAN"	=> (Tokens.GREATERTHAN(!pos,!pos));
"MINUS"      => (Tokens.MINUS(!pos,!pos));
"NEGATE"      => (Tokens.NEGATE(!pos,!pos));
"EQUALS"      => (Tokens.EQUALS(!pos,!pos));
"TRUE"		=> (Tokens.CONST("TRUE",!pos,!pos));
"FALSE"	=> (Tokens.CONST("FALSE",!pos,!pos));
"="      => (Tokens.EQ(!pos,!pos)); 
";"	=>	(Tokens.TERM(!pos,!pos));
{alpha}+ => (findKeywords(yytext,!pos,!pos));
{digit}+ => (Tokens.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),
	      !pos, !pos));
"."      => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());

