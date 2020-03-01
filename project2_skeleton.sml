(*
   The Interpret function assumes a "parse" function, 
      written in another file.

This is a skeleton, with 9 places where code needs to be changed/inserted.
Each such place is marked with "CHANGE #k" where k is a number,
indicating the suggested order in which to make the changes.

 #1  how to evaluate a number
 #2  how to execute a sequence of commands
       (make sure it is in the right order, and that each command has an effect)
 #3  how to handle an assignment
       (observe that LeftEval computes a location)
 #4  think about which runtime-errors there can be, 
        and add suitable exception declarations
 #5  handle those exceptions you added in #4
       (you may need to adjust #4 and #5 along the way)
 #6  how to read from the input stream
 #7  how to execute an "if" conditional 
       (remember which integers are considered 'true')
 #8  how to execute a "while" loop
       (hint: the question text mentions an equivalent command)
 #9  how to calculate the 'displacement', that is how far a given array element
        is located from the start of the array
       (the current code works only for 1-dimensional arrays,
         and doesn't catch out-of-bounds errors).

*)


(* ABSTRACT SYNTAX

type Id = string
type Num = int

datatype Exp =
  NumE of Num
| GetE of Left
| AddE of Exp * Exp
| SubE of Exp * Exp
| MulE of Exp * Exp
and Left =
  LocOf of Id * Exp list

datatype Comm =
  SkipC
| SeqC of Comm * Comm
| IfC of Exp * Comm * Comm
| WhileC of Exp * Comm
| AssignC of Left * Exp
| OutputC of Exp
| InputC of Left

type Decl = (Id * Num list) list

datatype Prog = 
  ProgP of Decl * Comm
| ErrorP of string   (* to report errors *)

*)

(* EXCEPTIONS *)

exception NotDeclared of string
exception BoundsError of string
exception ArrayDiscrepancy of string
exception RedefinedError of string
(* CHANGE #4 *)

(* ENVIRONMENTS and STORES *)

type Loc = int    (* locations in stores *)

type Value = int    (* values of expressions *)

type Env = Id -> Loc * Num list 
  (* associates identifiers with locations
      and with their dimensions (null if integers) *)

(* InitEnv: Env *)
fun InitEnv id = raise (NotDeclared id)

(* EnvInsert: Id -> Loc * Num list -> Env -> Env *)
fun EnvInsert id0 (loc,dims) env = 
   fn id => if id = id0 then (loc,dims) else env id

(* EnvLookup: Env -> Id -> Loc * Num list *)
fun EnvLookup env id = env id

(* Stores *)

type Store = Loc -> Value

(* InitSto: Store *)
fun InitSto loc = 0 (* all locations are initially zero *)

(* StoUpdate: Loc -> Value -> Store -> Store)  *)
fun StoUpdate loc0 v sto =
   fn loc => if loc = loc0 then v else sto loc

(* StoLookup: Store -> Loc -> Value *)
fun StoLookup sto loc = sto loc

(* INDEX CALCULATION *)

(* calculate_displacement: Id -> int list -> int list -> int *)
fun calculate_displacement id indices bounds = case (indices, bounds) of
   ([],[]) => 0
 | ((index1 :: indices'), (bound1 :: bounds')) =>
	if (index1 >= bound1) then raise (BoundsError id)
	else 
		index1
		
		
		
		
		
           index1 * bound1 + bound2  (* CHANGE #9 *)   *)
 | _ => raise (ArrayDiscrepancy id)

(* EVALUATION OF EXPRESSIONS
     ExpEval: Exp -> Env -> Store -> Val
*)

fun ExpEval (NumE n) _ _ = n (* CHANGE #1 *)
|   ExpEval (GetE lhs) env sto = StoLookup sto (LeftEval lhs env sto)
|   ExpEval (AddE(exp1,exp2)) env sto =
      let val v1 = ExpEval exp1 env sto
          val v2 = ExpEval exp2 env sto
       in v1 + v2
      end
|   ExpEval (SubE(exp1,exp2)) env sto =
      let val v1 = ExpEval exp1 env sto
          val v2 = ExpEval exp2 env sto
       in v1 - v2
      end
|   ExpEval (MulE(exp1,exp2)) env sto =
      let val v1 = ExpEval exp1 env sto
          val v2 = ExpEval exp2 env sto
       in v1 * v2
      end
(* LeftEval: Exp -> Env -> Store -> Loc   *)
and LeftEval (LocOf (id, exps)) env sto = let
        val (loc,bounds) = EnvLookup env id
        val indices = map (fn exp => ExpEval exp env sto) exps
        val displ = calculate_displacement id indices bounds
       in loc + displ
      end

(* PROCESSING OF DECLARATIONS 
     DeclExec: Decl -> Env * int -> Env * int
*)

fun DeclExec [] (env,next) = (env,next)
|   DeclExec ((id, bounds) :: decl) (env,next) = 
	  if true <> List.all (fn (i,j) => (i <> id)) decl 
	  then raise (RedefinedError id)
	  else if List.all (fn bound => bound > 0) bounds
      then DeclExec decl
              (EnvInsert id (next, bounds) env,
               next + (foldr op* 1 bounds))
      else raise (BoundsError id)

(* EXECUTION OF COMMANDS *)

type InputStream = Num list
type OutputStream = Value list
type RunTimeState = Store * InputStream * OutputStream

(*
CommExec: Comm -> Env -> RunTimeState -> RunTimeState
*)

fun CommExec SkipC env state = state
|   CommExec (SeqC(cmd1,cmd2)) env state = (* CHANGE #2 *)
      let val state1 = CommExec cmd1 env state
          val state2 = CommExec cmd2 env state1
       in state2 end
|   CommExec (IfC(exp,cmd1,cmd2)) env (sto, inp, outp) = (* CHANGE #7 *) 
          if ((ExpEval exp env sto) <= 0) then CommExec cmd2 env (sto, inp, outp)
		  else CommExec cmd1 env (sto, inp, outp)
|   CommExec (WhileC(exp,cmd)) env (sto, inp, outp) = (* CHANGE #8  //might write out recursive functino //or follow hint in question text*)
          if ((ExpEval exp env sto) > 0) then CommExec (SeqC(cmd, WhileC(exp,cmd))) env (sto, inp, outp)
		  else (sto, inp, outp)
|   CommExec (AssignC(lhs, rhs)) env (sto,inp,outp) = (* CHANGE #3 *)
		let val left = LeftEval lhs env sto 
			val result = ExpEval rhs env sto
		in ((StoUpdate left result sto), inp ,outp)
		end
|   CommExec (OutputC exp) env (sto,inp,outp) =
      let val v = ExpEval exp env sto
       in (sto, inp, (v::outp))   (* we eventually reverse the order *)
      end
|   CommExec (InputC lhs) env (sto, i::inp, outp) = (* CHANGE #6 *)
       let val left = LeftEval lhs env sto
	   in ((StoUpdate left i sto), inp, outp)
	   end

(* RUNNING THE PROGRAM *)

fun ProgRun (ProgP(decl,comm)) inp =
       let val (env,_) = DeclExec decl (InitEnv, 0)
           val (_,_,outp) = CommExec comm  env (InitSto, inp, [])
         in rev outp
        end
|   ProgRun(ErrorP s) _ = (print ("*** syntax error: "^s^"\n"); [0])

fun Interpret prog inp = ProgRun (parse prog) inp
   handle (* CHANGE #5 *)
      (NotDeclared x) =>
         (print ("*** error: "^x^" used but not declared\n"); [0])
    | (BoundsError x) =>
         (print ("*** error: "^x^" declared with a zero bound\n"); [0])
    | (ArrayDiscrepancy x) =>
         (print ("*** error: "^x^" not used with same dimensions as declared\n"); [0])
	| (RedefinedError x) =>
		(print ("*** error: "^x^" has already been defines and can not redefined\n"); [0])
    