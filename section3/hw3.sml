(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(*fun only_capitals l =
  let fun f l = Char.isUpper(String.sub(l,0))
  in List.filter f l
  end
No need for fun binding; Unnecessary function wrapping for l.
*)
val only_capitals = List.filter (fn s => Char.isUpper(String.sub(s,0)))

(*fun longest_string1 l =
  let fun f(x1,x2) = if String.size x1 > String.size x2 then x1 else x2
  in foldl f "" l
  end
Same as previous one.
*)
val longest_string1 = foldl (fn (x1,x2) => if String.size x1 > String.size x2 then x1 else x2) ""

(*fun longest_string2 l =
  let fun f(x1,x2) = if String.size x1 >= String.size x2 then x1 else x2
  in foldl f "" l
  end
Same.
*)
val longest_string2 = foldl (fn (x1,x2) => if String.size x1 >= String.size x2 then x1 else x2) ""
      
(*fun longest_string_helper f_int l =
  let fun f_string (x1,x2) = if f_int(String.size x1,String.size x2) then x1 else x2
  in foldl f_string "" l
  end
Unnecessary function binding for l; x1 and x2 is confusing naming but not changing for already figured out.
*)
fun longest_string_helper f_int=
  foldl (fn (x1,x2) => if f_int(String.size x1,String.size x2) then x1 else x2) ""
      
val longest_string3 = longest_string_helper (fn (x1,x2) => x1 > x2)
val longest_string4 = longest_string_helper (fn (x1,x2) => x1 >= x2)

val longest_capitalized = longest_string1 o only_capitals
val rev_string = String.implode o List.rev o String.explode

(*fun first_answer f l =
  case l of
      [] => raise NoAnswer
    | x::xs => let val v = f x
	       in case v of SOME v' => v'
			 | NONE => first_answer f xs
	       end
No need for let expression.
 *)
fun first_answer f l =
  case l of
      [] => raise NoAnswer
    | x::xs => case f x of SOME v' => v'
			 | NONE => first_answer f xs
						
(*fun all_answers f l =
  let fun aux acc xs =
	case xs of [] => (case acc of [] => NONE | x::xs' => SOME acc)
		 | x::xs' => let val v = f x
			     in case v of NONE => aux acc xs'
					| SOME l1 => aux (acc@l1) xs'
			     end
  in case l of [] => SOME [] | x::xs => aux [] l end
No need for pre-processing before calling aux; No need for let expression.
*)
fun all_answers f l =
  let fun aux acc xs =
	case xs of [] => SOME acc
		 | x::xs' => case f x of NONE => NONE
				      | SOME y => aux (acc @ y) xs'
  in aux [] l end
  

val count_wildcards = g (fn () => 1) (fn _ => 0)
			
(*val count_wild_and_variable_lengths = g (fn _ => 1) (fn x => String.size x)
Unnecessary function binding; Use "()" to indicate no function parameter.
 *)
val count_wild_and_variable_lengths = g (fn () => 1) String.size
					
fun count_some_var (s,p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p
			     
fun check_pat p =
  let
      fun aux1 p =
	case p of
	    Variable s => [s]
	  | TupleP ps => List.foldl (fn (p,i) => i@aux1 p) [] ps
	  | ConstructorP(_,p') => aux1 p'
	  | _ => []
      fun aux2 l =
	case l of
	    [] => true
	  | x::xs => (not (List.exists (fn y => y = x) xs andalso aux2 xs(* added andalso, nested patterns also needs examination *)))
  in
      aux2 (aux1 p)
  end
      
(*fun match (v,p) =
  case p of
      Wildcard => SOME []
    | Variable s => SOME [(s,v)]
    | UnitP => (case v of Unit => SOME [] | _ => NONE)
    | ConstP x => (case v of Const y => if x = y then SOME [] else NONE
			   | _ => NONE)
    | TupleP ps => (case v of Tuple vs => let fun f zip v =
						case zip of [] => NONE
							  | (p',v')::zip' => if v = v' then match(v',p') else f zip' v
					  in all_answers (f (ListPair.zip(ps,vs))) vs
					  end
			    | _ => NONE)
    | ConstructorP(s1,pa) => (case v of Constructor(s2,va) => if s1 = s2 then match(va,pa) else NONE 
				      | _ => NONE)
Use Tuple extraction.
 *)
fun match (v,p) =
  case (v,p) of
      (_,Wildcard) => SOME []
    | (_,Variable s) => SOME [(s,v)]
    | (Unit,UnitP) => SOME []
    | (Const i, ConstP j) => if i = j then SOME [] else NONE
    | (Tuple(vs),TupleP(ps)) => if length vs = length ps
				then all_answers match (ListPair.zip (vs,ps))
				else NONE
    | (Constructor(s1,v), ConstructorP(s2,p)) => if s1=s2
						 then match(v,p)
                                                 else NONE
    | _ => NONE
      

fun first_match v ps =
  SOME (first_answer (fn p => match (v,p)) ps)
  handle NoAnswer => NONE

		      (* copy definition for reference
(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string
		       *)

