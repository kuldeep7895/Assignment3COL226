structure AST =
struct

type id = string


datatype binop = MINUS | PLUS | GREATERTHAN | LESSTHAN | EQUALS | TIMES | IMPLIES | AND | OR | XOR 
datatype unary = NEGATE | NOT

datatype decl = ValDecl of id * exp
	and exp = iteExp of exp * exp * exp
		|letExp of decl * exp
		|numExp of int
		|constExp of string
		|varExp of id
		|binExp of binop * exp * exp
		|unaryExp of unary * exp

datatype program = formulaList of exp list



datatype value = IntVal of int
               | StringVal of string
	       | BoolVal of bool
				
type environment = (id * value) list

fun envAdd (var:id, v:value, env:environment) =
    (var,v)::env

fun envLookup (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => raise Fail "Environment lookup error"							    
end

(*datatype binop = Add | Sub | Mul | Div | Eq*)



(*and exp = NumExp of int*)
(*    	| StringExp of string*)
(*    	| VarExp of id*)
(*	| BinExp of binop * exp * exp*)
(*	| LetExp of decl * exp*)
(*        | BoolExp of binop * exp * exp*)
(*				       *)
(*datatype value = IntVal of int*)
(*               | StringVal of string*)
(*	       | BoolVal of bool*)
(*				*)
(*type environment = (id * value) list*)

(*fun envAdd (var:id, v:value, env:environment) =*)
(*    (var,v)::env*)

(*fun envLookup (var:id, env:environment) =*)
(*    case List.find(fn (x, _) => x = var) env of*)
(*				       SOME (x, v)   => v*)
(*				    |   NONE => raise Fail "Environment lookup error"							    *)
(*end*)




(*structure AST =*)
(*struct*)

(*type id = string*)

(*datatype binop = Add | Sub | Mul | Div | Eq*)

(*datatype decl = ValDecl of id * exp*)

(*and exp = NumExp of int*)
(*    	| StringExp of string*)
(*    	| VarExp of id*)
(*	| BinExp of binop * exp * exp*)
(*	| LetExp of decl * exp*)
(*        | BoolExp of binop * exp * exp*)
(*				       *)
(*datatype value = IntVal of int*)
(*               | StringVal of string*)
(*	       | BoolVal of bool*)
(*				*)
(*type environment = (id * value) list*)

(*fun envAdd (var:id, v:value, env:environment) =*)
(*    (var,v)::env*)

(*fun envLookup (var:id, env:environment) =*)
(*    case List.find(fn (x, _) => x = var) env of*)
(*				       SOME (x, v)   => v*)
(*				    |   NONE => raise Fail "Environment lookup error"							    *)
(*end*)


