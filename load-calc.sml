structure CalcLrVals = CalcLrValsFun(structure Token = LrParser.Token)
structure CalcLex = CalcLexFun(structure Tokens = CalcLrVals.Tokens);
structure CalcParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = CalcLrVals.ParserData
     	       structure Lex = CalcLex)
     
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,_) =
		    	TextIO.output(TextIO.stdOut, "Error, line " ^ (Int.toString pos) ^ "," ^ s ^ "\n")
		in
		    CalcParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  CalcParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = CalcLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = CalcParser.Stream.get lexer
    in
        if CalcParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val parseString = parse o stringToLexer





fun next_char input =
	Option.valOf (TextIO.input1 input)
	
		
exception emptyInputFile
	

fun cvtstr(file) = 
	let
		val ins = TextIO.openIn file
	in
		let
			fun iterFile(ins,ans) = 
			if(TextIO.endOfStream ins) then ans
			else
				let
					val nextChar = next_char ins; 
				in
					iterFile(ins,ans^str(nextChar))
				end
		in
(*			iterFile(ins)*)
			if(TextIO.endOfStream ins) then
				raise emptyInputFile
			else
				iterFile(ins,"")
(*				handle UnevenFields msg => (print(msg)) *)
		end
	end;
	

(*val parseAr = parseString(cvtstr("f"));*)
(*fun parseList() = myPrint(parseAr);*)


fun run(filename) = 
	let
		val ins = TextIO.openIn filename;
		val lexer = CalcParser.makeLexer(fn _=>TextIO.input ins);
	in
		(parseString(cvtstr(filename)))
	end




