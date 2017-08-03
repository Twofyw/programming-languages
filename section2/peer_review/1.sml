(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(key,keylist)=
  let   fun all_except_list(result,exists,klist)=
          case (exists,klist)  of
              (_,[]) => (exists,result)
              | (false, el::rest) => if  same_string(el,key)
                                     then all_except_list(result,true, rest)
                                     else all_except_list(result @ [el],false, rest)
              | (true, rest) =>    all_except_list(result @ rest,true, [])

  in
      case all_except_list([],false,keylist) of
          (false,_) => NONE
        | (true,x)  => SOME x
  end
      
fun get_substitutions1(l_lstrs, key)=
  case l_lstrs of
      [] => []
    | xs::rest => case all_except_option(key,xs) of
                      NONE => get_substitutions1(rest,key)
                    | SOME result => result @ get_substitutions1(rest,key)

                                                                      
fun get_substitutions2(l_lstrs, key)=
  let fun get_substs ( result, remlist)=
        case remlist of
            [] => result 
          | xs::rest => case all_except_option(key,xs) of
                            NONE => get_substs(result, rest)
                          | SOME r => get_substs(result @ r , rest)
  in
      get_substs([],l_lstrs)
  end
      

fun similar_names(l_lnames, {first=f,last=l,middle=m})=
  let fun mk_names ( result, fnames)=
        case fnames of
            [] => result 
          | x::rest => mk_names(result @ [{first=x,middle=m,last=l}],rest)
  in
      mk_names([{first=f,middle=m,last=l}],get_substitutions2(l_lnames,f))
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
fun card_color(c)=
  case c of
     (Clubs,_) => Black
   | (Spades,_) => Black
   | (_,_) =>  Red

fun card_value(c)=
  case c of
     (_,Jack) => 10
   | (_,King) => 10
   | (_,Queen) => 10
   | (_,Ace) => 11
   | (_,Num(x)) => x

                       
fun remove_card(cs,c,e)=
  let fun rm_card(result,cards)=
        case cards of
            [] => raise e
          | x::rest => if ( x = c)
                       then result @ rest
                       else rm_card(result @ [x],rest)
  in
      rm_card([],cs)
  end

fun all_same_color(cs)=
  case cs of
      [] => true
    | y::[] =>  true
    | x::y::rest => if card_color(x) <> card_color(y)
                    then false
                    else all_same_color(y::rest)
                        
fun sum_cards(cs)=
  let fun add_cards(acc, cards)=
        case cards of
            [] => acc
          | x::rest => add_cards(acc + card_value(x), rest)
  in
      add_cards(0,cs)
  end
  
fun score(cs,goal)=
  let
      val sum = sum_cards(cs)
      val pscore =  if ( sum > goal )
                    then ( sum - goal ) * 3
                    else ( goal -sum )
  in
      if all_same_color (cs)
          then pscore div 2
          else pscore
              
  end
      

fun officiate(cs,ml,goal)=
  (* make one move and return the state as (held cards, remaning cards) *)
  let fun run_onemove(heldcards,cardlist,mv)=
        case mv of
            Discard(c) => (remove_card(heldcards,c,IllegalMove), cardlist)
          | Draw => case cardlist of
                        [] => (heldcards,[])
                      | c::rest => (c::heldcards,rest)
      fun eval_state(heldcards,cardlist)=
        let
            val cscore = score(heldcards,goal)
        in
        case cardlist of
            [] => (true,cscore)
          | _ => if ( sum_cards(heldcards) > goal )
                 then (true,cscore)
                 else (false, cscore)
        end
      fun run_until(heldcards, cardlist,moves)=
        case moves of
            [] => score(heldcards,goal)
          | mv::rest_of_moves => case run_onemove(heldcards,cardlist,mv) of
                                     (newheld,remcards) => case eval_state(newheld,remcards) of
                                                                 (true,x) => x
                                                              | (false,_)  => run_until(newheld,remcards,
                                                                                        rest_of_moves)

                                                                 
  in
      run_until([],cs,ml)
  end

