(*

Grammar:

  B ::= A
     | AB
 
  A ::= x
     | [B]
     | {B}

*)

exception Error of string

datatype Token =
  LetterT
| OpenSquareT
| CloseSquareT
| OpenCurlyT
| CloseCurlyT

fun printToken LetterT = "x"
|   printToken OpenSquareT = "["
|   printToken CloseSquareT = "]"
|   printToken OpenCurlyT = "{"
|   printToken CloseCurlyT = "}"

datatype Balanced =
   Single of AtomicBalanced
 | Sequence of AtomicBalanced * Balanced

and AtomicBalanced =
   LetterB
 | Square of Balanced
 | Curly of Balanced

fun lexer [] = []
|   lexer (ch :: chs) =
  case ch of
     #" " => lexer chs
   | #"x" => LetterT :: (lexer chs)
   | #"[" => OpenSquareT :: (lexer chs)
   | #"]" => CloseSquareT :: (lexer chs)
   | #"{" => OpenCurlyT :: (lexer chs)
   | #"}" => CloseCurlyT :: (lexer chs)
   | _ => raise (Error "unknown symbol")

fun expect token tokens = case tokens of
   [] => raise (Error ("'"^(printToken token)^"' expected but input exhausted"))
 | (token1 :: tokens') => 
     if token = token1
     then tokens'
     else raise (Error ("'"^(printToken token)^"' expected but '"^
                        (printToken token1)^"' seen"))

fun may_start_balanced (LetterT :: _) = true
|   may_start_balanced (OpenSquareT :: _) = true
|   may_start_balanced (OpenCurlyT :: _) = true
|   may_start_balanced _ = false

  (* Parse A *)
fun parseA (LetterT :: tks) =  (LetterB ,tks) 
| parseA (OpenCurlyT :: tks) =  
	let val (b, tks1) = parseB tks
	in 
		(Curly b, (expect CloseCurlyT tks1))
	end
| parseA (OpenSquareT :: tks) =
	let val (b, tks1) = parseB tks
	in 
		(Square b, (expect CloseSquareT tks1))
	end		



(*OPTION B*)
and parseB tokens = 
	let val (t , ts) = parseA tokens
	in
		if may_start_balanced ts then 
			let val (t1 , ts1) = parseB ts
			in 
				(Sequence (t, t1), ts1)
			end 
		else
			(Single t , ts)
	end


fun parse tokens = case parseB tokens of
      (b, []) => b
    | (_, (token :: _)) => 
          raise (Error ("unused input, starting with "^
                           (printToken token)^"'"))

fun depth (Single a) = depthA a
|   depth (Sequence (a,b)) = Int.max(depthA a, depth b)
and depthA LetterB = 0
|   depthA (Square b) = 1 + depth b
|   depthA (Curly b) = 1 + depth b

fun run s =
  depth (parse (lexer (explode s)))
  handle (Error msg) => (print ("Error: "^msg^"\n"); 0)

