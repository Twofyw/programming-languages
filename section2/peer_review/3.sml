(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* 1a *)
 
fun all_except_option (_, []) = NONE  
  | all_except_option (sought, s::tail) =
    let	fun aux (_, []) = ([], false)
	  | aux (res_list, s::tail) =
	        if same_string(sought, s)
	        (* Uses @ instead of :: to get result list 
                    in the same order as input list. *)
		then (res_list @ tail, true)
		else aux(res_list @ [s], tail)
    in
	case aux([], s::tail) of
	    (lst, true) => SOME lst
	  | (_, false) => NONE
    end
	
	    	    
(* 1b *)
			 
fun get_substitutions1 ([], _) = []
  | get_substitutions1 (str_list::tail, s) =
    case all_except_option(s, str_list) of
	NONE => get_substitutions1(tail, s)
      | SOME result => result @ get_substitutions1(tail, s)
						  
	
(* 1c *)

fun get_substitutions2 ([], _) = []
  | get_substitutions2 (str_list::tail, s) =
    let fun aux (res_list, []) = res_list		      
	  | aux (res_list, str_list::tail) =
	    case all_except_option(s, str_list) of
		NONE => aux(res_list, tail)
	      | SOME result => aux(res_list @ result, tail)
    in
	aux([], str_list::tail)
    end
	

(* 1d *)

fun similar_names ([], {first=first, middle=middle, last=last}) = [{first=first, middle=middle, last=last}]  
  | similar_names (name_list::tail, {first=first, middle=middle, last=last}) =
    let	fun aux(res_list, []) = res_list
	  | aux (res_list, substitution::tail) =
	    aux(res_list @ [{first=substitution, middle=middle, last=last}], tail)
    in
	aux([{first=first, middle=middle, last=last}], get_substitutions2(name_list::tail, first) )
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

(* 2a *)

fun card_color (Spades, _) = Black
  | card_color (Clubs, _) = Black
  | card_color (_, _) = Red
			     
  
(* 2b *)

fun card_value (_, Ace) = 11
  | card_value (_, Num i) = i
  | card_value (_, _) = 10
			    
			    
(* 2c *)
			    
fun remove_card (cs, c, e) =
  let fun aux (res_list, []) = raise e
	| aux (res_list, c1::tail) =
	  if c1=c
	  then res_list @ tail
	  else aux(c1::res_list, tail)
  in
      aux([], cs)
  end
      
		  
(* 2d *)

fun all_same_color ([]) = true
  | all_same_color (_::[]) = true
  | all_same_color (c1::c2::tail) = (card_color(c1)=card_color(c2)) andalso all_same_color(c2::tail)
											  

(* 2e *)

fun sum_cards (card_list) =
  let fun aux (res, []) = res
	| aux (res, c1::tail) =
	  aux(res + card_value(c1), tail)
  in
      aux(0, card_list)
  end
      

(* 2f *)

fun score (cards_held, goal) =
  let fun prelim_score (sum) =
	if sum > goal	     
	then 3 * (sum-goal)
	else goal-sum
  in
      if all_same_color(cards_held)
      then prelim_score(sum_cards(cards_held)) div 2
      else prelim_score(sum_cards(cards_held))
  end

      
(* 2g *)

fun officiate (cards, moves, goal) =
  let fun do_move (cards_held, _, []) = score(cards_held, goal) (* Game Over: No more moves. *)
	| do_move (cards_held, card_list, curr_move::rest_moves) =
	  case curr_move of  
	      Draw =>
	      (case card_list of
		   first_card::rest_cards =>
		   (* Move 1st card in card list to held cards. *)
		   if sum_cards(first_card::cards_held) > goal
		   then score(first_card::cards_held, goal) (* Game Over: Sum of cards held exceeds goal. *)
		   else do_move(first_card::cards_held, rest_cards, rest_moves)
		 
	       |   [] => score(cards_held, goal) (* Game over: Drawing from empty card list. *)
              )
		  
	  |   Discard c => do_move(remove_card(cards_held, c, IllegalMove), card_list, rest_moves)

  in
      do_move([], cards, moves)
  end
      
