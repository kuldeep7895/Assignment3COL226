functor CalcLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Calc_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 
fun getVal((AST.formulaList myList):AST.program) = myList;

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\018\000\002\000\017\000\006\000\016\000\011\000\015\000\
\\013\000\014\000\016\000\013\000\020\000\012\000\021\000\011\000\000\000\
\\001\000\001\000\034\000\000\000\
\\001\000\003\000\068\000\004\000\068\000\005\000\068\000\007\000\068\000\
\\008\000\068\000\009\000\068\000\014\000\069\000\022\000\068\000\
\\023\000\068\000\024\000\068\000\025\000\068\000\000\000\
\\001\000\003\000\028\000\004\000\027\000\005\000\026\000\007\000\025\000\
\\008\000\024\000\009\000\023\000\015\000\056\000\022\000\022\000\
\\023\000\021\000\024\000\020\000\025\000\019\000\000\000\
\\001\000\003\000\028\000\004\000\027\000\005\000\026\000\007\000\025\000\
\\008\000\024\000\009\000\023\000\017\000\048\000\022\000\022\000\
\\023\000\021\000\024\000\020\000\025\000\019\000\000\000\
\\001\000\003\000\028\000\004\000\027\000\005\000\026\000\007\000\025\000\
\\008\000\024\000\009\000\023\000\018\000\055\000\022\000\022\000\
\\023\000\021\000\024\000\020\000\025\000\019\000\000\000\
\\001\000\003\000\028\000\004\000\027\000\005\000\026\000\007\000\025\000\
\\008\000\024\000\009\000\023\000\019\000\058\000\022\000\022\000\
\\023\000\021\000\024\000\020\000\025\000\019\000\000\000\
\\001\000\003\000\028\000\004\000\027\000\005\000\026\000\007\000\025\000\
\\008\000\024\000\009\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\000\000\
\\001\000\012\000\000\000\000\000\
\\001\000\014\000\049\000\000\000\
\\001\000\026\000\050\000\000\000\
\\001\000\027\000\029\000\000\000\
\\060\000\000\000\
\\061\000\000\000\
\\062\000\000\000\
\\063\000\001\000\018\000\002\000\017\000\006\000\016\000\011\000\015\000\
\\013\000\014\000\016\000\013\000\020\000\012\000\021\000\011\000\000\000\
\\064\000\000\000\
\\065\000\003\000\028\000\004\000\027\000\005\000\026\000\007\000\025\000\
\\008\000\024\000\009\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\000\000\
\\066\000\000\000\
\\067\000\000\000\
\\068\000\000\000\
\\068\000\010\000\051\000\000\000\
\\070\000\000\000\
\\071\000\000\000\
\\072\000\000\000\
\\073\000\000\000\
\\074\000\003\000\028\000\004\000\027\000\005\000\026\000\008\000\024\000\
\\009\000\023\000\000\000\
\\075\000\003\000\028\000\004\000\027\000\005\000\026\000\000\000\
\\076\000\003\000\028\000\004\000\027\000\005\000\026\000\000\000\
\\077\000\004\000\027\000\000\000\
\\078\000\004\000\027\000\000\000\
\\079\000\000\000\
\\080\000\000\000\
\\081\000\000\000\
\\082\000\003\000\028\000\004\000\027\000\005\000\026\000\007\000\025\000\
\\008\000\024\000\009\000\023\000\022\000\022\000\023\000\021\000\
\\024\000\020\000\025\000\019\000\000\000\
\\083\000\003\000\028\000\004\000\027\000\005\000\026\000\008\000\024\000\
\\009\000\023\000\000\000\
\\084\000\003\000\028\000\004\000\027\000\005\000\026\000\008\000\024\000\
\\009\000\023\000\000\000\
\\085\000\003\000\028\000\004\000\027\000\005\000\026\000\008\000\024\000\
\\009\000\023\000\000\000\
\\086\000\003\000\028\000\004\000\027\000\005\000\026\000\008\000\024\000\
\\009\000\023\000\000\000\
\\087\000\000\000\
\"
val actionRowNumbers =
"\000\000\017\000\023\000\022\000\
\\011\000\015\000\013\000\012\000\
\\020\000\000\000\039\000\000\000\
\\001\000\000\000\000\000\033\000\
\\025\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\016\000\
\\014\000\038\000\004\000\009\000\
\\010\000\007\000\021\000\032\000\
\\034\000\037\000\036\000\035\000\
\\028\000\027\000\026\000\030\000\
\\031\000\029\000\000\000\000\000\
\\000\000\024\000\005\000\003\000\
\\002\000\000\000\018\000\006\000\
\\019\000\008\000"
val gotoT =
"\
\\001\000\008\000\002\000\057\000\004\000\007\000\005\000\006\000\
\\006\000\005\000\007\000\004\000\008\000\003\000\009\000\002\000\
\\010\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\008\000\005\000\028\000\006\000\005\000\007\000\004\000\
\\008\000\003\000\009\000\002\000\010\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\008\000\008\000\003\000\009\000\002\000\010\000\029\000\000\000\
\\000\000\
\\001\000\008\000\008\000\003\000\009\000\002\000\010\000\030\000\000\000\
\\003\000\031\000\000\000\
\\001\000\034\000\008\000\003\000\009\000\002\000\010\000\033\000\000\000\
\\001\000\008\000\008\000\003\000\009\000\002\000\010\000\035\000\000\000\
\\000\000\
\\000\000\
\\001\000\008\000\008\000\003\000\009\000\002\000\010\000\036\000\000\000\
\\001\000\008\000\008\000\003\000\009\000\002\000\010\000\037\000\000\000\
\\001\000\008\000\008\000\003\000\009\000\002\000\010\000\038\000\000\000\
\\001\000\008\000\008\000\003\000\009\000\002\000\010\000\039\000\000\000\
\\001\000\008\000\008\000\003\000\009\000\002\000\010\000\040\000\000\000\
\\001\000\008\000\008\000\003\000\009\000\002\000\010\000\041\000\000\000\
\\001\000\008\000\008\000\003\000\009\000\002\000\010\000\042\000\000\000\
\\001\000\008\000\008\000\003\000\009\000\002\000\010\000\043\000\000\000\
\\001\000\008\000\008\000\003\000\009\000\002\000\010\000\044\000\000\000\
\\001\000\008\000\008\000\003\000\009\000\002\000\010\000\045\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\008\000\008\000\003\000\009\000\002\000\010\000\050\000\000\000\
\\001\000\008\000\008\000\003\000\009\000\002\000\010\000\051\000\000\000\
\\001\000\052\000\008\000\003\000\009\000\002\000\010\000\033\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\008\000\008\000\003\000\009\000\002\000\010\000\055\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 58
val numrules = 28
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | CONST of unit ->  (string) | NUM of unit ->  (int)
 | ID of unit ->  (string) | gexp of unit ->  (AST.exp)
 | boolFormula of unit ->  (AST.exp) | intExp of unit ->  (AST.exp)
 | FORMULA of unit ->  (AST.exp) | statement of unit ->  (AST.exp)
 | stmtList of unit ->  (AST.program)
 | program of unit ->  (AST.program) | DECL of unit ->  (AST.decl)
 | START of unit ->  (AST.program) | EXP of unit ->  (AST.exp)
end
type svalue = MlyValue.svalue
type result = AST.program
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 11) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "NUM"
  | (T 2) => "PLUS"
  | (T 3) => "TIMES"
  | (T 4) => "MINUS"
  | (T 5) => "NEGATE"
  | (T 6) => "EQUALS"
  | (T 7) => "LESSTHAN"
  | (T 8) => "GREATERTHAN"
  | (T 9) => "RPAREN"
  | (T 10) => "LPAREN"
  | (T 11) => "EOF"
  | (T 12) => "LET"
  | (T 13) => "IN"
  | (T 14) => "END"
  | (T 15) => "IF"
  | (T 16) => "THEN"
  | (T 17) => "ELSE"
  | (T 18) => "FI"
  | (T 19) => "CONST"
  | (T 20) => "NOT"
  | (T 21) => "AND"
  | (T 22) => "OR"
  | (T 23) => "XOR"
  | (T 24) => "IMPLIES"
  | (T 25) => "EQ"
  | (T 26) => "TERM"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.program program1, program1left, 
program1right)) :: rest671)) => let val  result = MlyValue.START (fn _
 => let val  (program as program1) = program1 ()
 in (program)
end)
 in ( LrTable.NT 1, ( result, program1left, program1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.stmtList stmtList1, stmtList1left, 
stmtList1right)) :: rest671)) => let val  result = MlyValue.program
 (fn _ => let val  (stmtList as stmtList1) = stmtList1 ()
 in (stmtList)
end)
 in ( LrTable.NT 3, ( result, stmtList1left, stmtList1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.stmtList stmtList1, _, stmtList1right)) :: (
 _, ( MlyValue.statement statement1, statement1left, _)) :: rest671))
 => let val  result = MlyValue.stmtList (fn _ => let val  (statement
 as statement1) = statement1 ()
 val  stmtList1 = stmtList1 ()
 in (AST.formulaList([statement]))
end)
 in ( LrTable.NT 4, ( result, statement1left, stmtList1right), rest671
)
end
|  ( 3, ( ( _, ( MlyValue.statement statement1, statement1left, 
statement1right)) :: rest671)) => let val  result = MlyValue.stmtList
 (fn _ => let val  (statement as statement1) = statement1 ()
 in (AST.formulaList([statement]))
end)
 in ( LrTable.NT 4, ( result, statement1left, statement1right), 
rest671)
end
|  ( 4, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.FORMULA 
FORMULA1, FORMULA1left, _)) :: rest671)) => let val  result = 
MlyValue.statement (fn _ => let val  (FORMULA as FORMULA1) = FORMULA1
 ()
 in (FORMULA)
end)
 in ( LrTable.NT 5, ( result, FORMULA1left, TERM1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.gexp gexp1, gexp1left, gexp1right)) :: 
rest671)) => let val  result = MlyValue.FORMULA (fn _ => let val  (
gexp as gexp1) = gexp1 ()
 in (gexp)
end)
 in ( LrTable.NT 6, ( result, gexp1left, gexp1right), rest671)
end
|  ( 6, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.gexp gexp1, _, _
)) :: _ :: ( _, ( MlyValue.DECL DECL1, _, _)) :: ( _, ( _, LET1left, _
)) :: rest671)) => let val  result = MlyValue.gexp (fn _ => let val  (
DECL as DECL1) = DECL1 ()
 val  (gexp as gexp1) = gexp1 ()
 in ( AST.letExp(DECL,gexp))
end)
 in ( LrTable.NT 9, ( result, LET1left, END1right), rest671)
end
|  ( 7, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.gexp gexp3, _, _)
) :: _ :: ( _, ( MlyValue.gexp gexp2, _, _)) :: _ :: ( _, ( 
MlyValue.gexp gexp1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) =>
 let val  result = MlyValue.gexp (fn _ => let val  gexp1 = gexp1 ()
 val  gexp2 = gexp2 ()
 val  gexp3 = gexp3 ()
 in ( AST.iteExp(gexp1,gexp2,gexp3) )
end)
 in ( LrTable.NT 9, ( result, IF1left, FI1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP1, EXP1left, EXP1right)) :: rest671))
 => let val  result = MlyValue.gexp (fn _ => let val  (EXP as EXP1) = 
EXP1 ()
 in ( EXP )
end)
 in ( LrTable.NT 9, ( result, EXP1left, EXP1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.DECL (fn _ => let val  (ID as ID1) = ID1 ()
 val  (EXP as EXP1) = EXP1 ()
 in ( AST.ValDecl(ID,EXP))
end)
 in ( LrTable.NT 2, ( result, ID1left, EXP1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.intExp intExp1, intExp1left, intExp1right))
 :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (
intExp as intExp1) = intExp1 ()
 in (intExp)
end)
 in ( LrTable.NT 0, ( result, intExp1left, intExp1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.boolFormula boolFormula1, boolFormula1left,
 boolFormula1right)) :: rest671)) => let val  result = MlyValue.EXP
 (fn _ => let val  (boolFormula as boolFormula1) = boolFormula1 ()
 in (boolFormula)
end)
 in ( LrTable.NT 0, ( result, boolFormula1left, boolFormula1right), 
rest671)
end
|  ( 12, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (EXP)
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 in (AST.varExp(ID))
end)
 in ( LrTable.NT 0, ( result, ID1left, ID1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.gexp gexp2, _, gexp2right)) :: _ :: ( _, ( 
MlyValue.gexp gexp1, gexp1left, _)) :: rest671)) => let val  result = 
MlyValue.intExp (fn _ => let val  gexp1 = gexp1 ()
 val  gexp2 = gexp2 ()
 in (AST.binExp(AST.EQUALS,gexp1,gexp2))
end)
 in ( LrTable.NT 7, ( result, gexp1left, gexp2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.gexp gexp2, _, gexp2right)) :: _ :: ( _, ( 
MlyValue.gexp gexp1, gexp1left, _)) :: rest671)) => let val  result = 
MlyValue.intExp (fn _ => let val  gexp1 = gexp1 ()
 val  gexp2 = gexp2 ()
 in (AST.binExp(AST.LESSTHAN,gexp1,gexp2))
end)
 in ( LrTable.NT 7, ( result, gexp1left, gexp2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.gexp gexp2, _, gexp2right)) :: _ :: ( _, ( 
MlyValue.gexp gexp1, gexp1left, _)) :: rest671)) => let val  result = 
MlyValue.intExp (fn _ => let val  gexp1 = gexp1 ()
 val  gexp2 = gexp2 ()
 in (AST.binExp(AST.GREATERTHAN,gexp1,gexp2))
end)
 in ( LrTable.NT 7, ( result, gexp1left, gexp2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.gexp gexp2, _, gexp2right)) :: _ :: ( _, ( 
MlyValue.gexp gexp1, gexp1left, _)) :: rest671)) => let val  result = 
MlyValue.intExp (fn _ => let val  gexp1 = gexp1 ()
 val  gexp2 = gexp2 ()
 in (AST.binExp(AST.PLUS,gexp1,gexp2))
end)
 in ( LrTable.NT 7, ( result, gexp1left, gexp2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.gexp gexp2, _, gexp2right)) :: _ :: ( _, ( 
MlyValue.gexp gexp1, gexp1left, _)) :: rest671)) => let val  result = 
MlyValue.intExp (fn _ => let val  gexp1 = gexp1 ()
 val  gexp2 = gexp2 ()
 in (AST.binExp(AST.MINUS,gexp1,gexp2))
end)
 in ( LrTable.NT 7, ( result, gexp1left, gexp2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.gexp gexp2, _, gexp2right)) :: _ :: ( _, ( 
MlyValue.gexp gexp1, gexp1left, _)) :: rest671)) => let val  result = 
MlyValue.intExp (fn _ => let val  gexp1 = gexp1 ()
 val  gexp2 = gexp2 ()
 in (AST.binExp(AST.TIMES,gexp1,gexp2))
end)
 in ( LrTable.NT 7, ( result, gexp1left, gexp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.gexp gexp1, _, gexp1right)) :: ( _, ( _, 
NEGATE1left, _)) :: rest671)) => let val  result = MlyValue.intExp (fn
 _ => let val  (gexp as gexp1) = gexp1 ()
 in (AST.unaryExp(AST.NEGATE,gexp))
end)
 in ( LrTable.NT 7, ( result, NEGATE1left, gexp1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.intExp (fn _ => let val  (NUM as NUM1)
 = NUM1 ()
 in (AST.numExp(NUM))
end)
 in ( LrTable.NT 7, ( result, NUM1left, NUM1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.gexp gexp2, _, gexp2right)) :: _ :: ( _, ( 
MlyValue.gexp gexp1, gexp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolFormula (fn _ => let val  gexp1 = gexp1 ()
 val  gexp2 = gexp2 ()
 in (AST.binExp(AST.IMPLIES,gexp1,gexp2))
end)
 in ( LrTable.NT 8, ( result, gexp1left, gexp2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.gexp gexp2, _, gexp2right)) :: _ :: ( _, ( 
MlyValue.gexp gexp1, gexp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolFormula (fn _ => let val  gexp1 = gexp1 ()
 val  gexp2 = gexp2 ()
 in (AST.binExp(AST.AND,gexp1,gexp2))
end)
 in ( LrTable.NT 8, ( result, gexp1left, gexp2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.gexp gexp2, _, gexp2right)) :: _ :: ( _, ( 
MlyValue.gexp gexp1, gexp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolFormula (fn _ => let val  gexp1 = gexp1 ()
 val  gexp2 = gexp2 ()
 in (AST.binExp(AST.OR,gexp1,gexp2))
end)
 in ( LrTable.NT 8, ( result, gexp1left, gexp2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.gexp gexp2, _, gexp2right)) :: _ :: ( _, ( 
MlyValue.gexp gexp1, gexp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolFormula (fn _ => let val  gexp1 = gexp1 ()
 val  gexp2 = gexp2 ()
 in (AST.binExp(AST.XOR,gexp1,gexp2))
end)
 in ( LrTable.NT 8, ( result, gexp1left, gexp2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.gexp gexp1, _, gexp1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.boolFormula
 (fn _ => let val  (gexp as gexp1) = gexp1 ()
 in (AST.unaryExp(AST.NOT,gexp))
end)
 in ( LrTable.NT 8, ( result, NOT1left, gexp1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: 
rest671)) => let val  result = MlyValue.boolFormula (fn _ => let val 
 (CONST as CONST1) = CONST1 ()
 in (AST.constExp(CONST))
end)
 in ( LrTable.NT 8, ( result, CONST1left, CONST1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Calc_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
end
end
