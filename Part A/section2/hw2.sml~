(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(str, lst) =
  let fun aux(prv, aft)=
	case aft of
	    [] => NONE
	  | x::xs => if same_string(x, str)
		     then SOME(prv@xs)
		     else aux(prv@[x], xs)
  in aux([], lst)
  end

fun get_substitutions1(subs, str) =
  case subs of
      [] => []
    | x::xs => let val found = all_except_option(str, x)
	       in case found of
		      NONE => get_substitutions1(xs, str)
		    | SOME sub => sub@get_substitutions1(xs, str)
	       end

fun get_substitutions2(subs, str) =
  let fun aux(reme, acc) =
	case reme of
	    [] => acc
	  | x::xs => let val found = all_except_option(str, x)
		     in case found of
			    NONE => aux(xs, acc)
			  | SOME sub => aux(xs, acc@sub)
		     end
  in aux(subs, [])
  end
      
fun similar_names(subs, name) =
  case name of
      {first = x, middle = y, last = z} => let val sub = get_substitutions2(subs, x)
					       fun aux(subl, acc) =
						 case subl of
						     [] => acc
			| x'::xs => aux(xs, acc@[{first = x', middle = y, last = z}])
					   in aux(sub, [name])
					   end
						 
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color(x, y) =
  case x of
      Clubs => Black
    | Spades => Black
    | _ => Red

fun card_value(x, y) =
  case y of
      Num n => n
    | Ace => 11
    | _ => 10

fun remove_card(cs, c, e) =
  let fun aux(bef, aft) =
      case aft of
	  [] => raise e
	| x::xs => if x = c
		   then bef@xs
		   else aux(bef@[x], xs)
  in aux([], cs)
  end

fun all_same_color(cs) =
  case cs of
      [] => true
    | (x, y)::rcs => case rcs of
			 [] => true
		       | (x', y')::rcs' => if card_color(x, y) = card_color(x', y')
					   then all_same_color((x', y')::rcs')
					   else false
						    
fun sum_cards(cs) =
  let fun aux((x, y)::rcs, acc) =
	case rcs of
	    [] => acc + card_value(x, y)
	  | next => aux(next, acc + card_value(x, y))
  in case cs of
	 [] => 0
      | _ => aux(cs, 0)
  end

fun score(cs, goal) =
  let val sum = sum_cards(cs)
      val pre = if sum > goal
		then 3 * (sum - goal)
		else goal - sum
  in if all_same_color(cs)
     then pre div 2
     else pre
  end

fun officiate(cs, mvs, goal) =
  let fun aux(cs, mvs, held) =
	case mvs of
	    [] => score(held, goal)
	  | mv::rmvs => case mv of
			    Discard c => aux(cs, rmvs, remove_card(held, c, IllegalMove))
			  | Draw => case cs of
					[] => score(held, goal)
				      | x::xs => if sum_cards(x::held) > goal
						    then score(x::held, goal)
						    else aux(xs, rmvs, x::held)
  in aux(cs, mvs, [])
  end
