(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (str, strs) =
    let fun all_except strs = 
       case strs of
            [] => []
          | x::xs => if same_string(x, str)
                     then all_except(xs)
                     else x::all_except(xs)
        val res = all_except strs
    in
        if res = strs
        then NONE
        else SOME res  
    end

fun get_substitutions1 (subs, str) =
    let fun get_str_subs subs =
          case all_except_option(str, subs) of
               NONE => []
             | SOME x => x
    in
        case subs of
             [] => []
           | x::xs => (get_str_subs x) @ get_substitutions1(xs, str)
    end

fun get_substitutions2 (subs, str) =
    let fun get_str_subs subs =
          case all_except_option(str, subs) of
               NONE => []
             | SOME x => x
        fun get_subs_acc(subs, acc) =
          case subs of
               [] => acc
             | x::xs => get_subs_acc(xs, (get_str_subs x) @ acc)
    in
      get_subs_acc(subs, [])
    end

fun similar_names (subs, {first=f, middle=m, last=l}) =
    let val names = f :: get_substitutions1(subs, f)
        fun similar names =
          case names of
               [] => []
             | x::xs => {first=x, middle=m, last=l}::(similar xs)
    in
      similar names
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
fun card_color (s, _) =
  case s of
       Clubs => Black
     | Spades => Black
     | Diamonds => Red
     | Hearts => Red

fun card_value (_, r) =
  case r of
       Num x => x
     | Ace => 11
     | _ => 10

fun remove_card (cs, c, e) =
    let fun remove_first cs =
        case cs of
             [] => []
           | x::xs => if x = c 
                     then xs
                     else x::(remove_first xs)
        val removed = remove_first cs
    in
      if removed = cs
      then raise e
      else removed
    end

fun all_same_color cs =
  case cs of
       [] => true
     | c::[] => true
     | c1::c2::cs' => ((card_color c1) = (card_color c2)) andalso all_same_color(c2::cs')

fun sum_cards cs =
    let fun sum_cards_acc (cs, acc) =
        case cs of
             [] => acc
           | c::cs' => sum_cards_acc(cs', (card_value c) + acc)
    in
      sum_cards_acc(cs, 0)
    end

fun score (hand, goal) =
    let val sum = sum_cards hand
        val s = if sum > goal
                then (sum - goal) * 3
                else goal - sum
    in
      if all_same_color hand
      then s div 2
      else s
    end

fun officiate (cardlist, movelist, goal) =
    let fun process_moves (cardlist, movelist, hand) =
          if (sum_cards hand) > goal
          then hand
          else case movelist of
               [] => hand
             | (Discard c)::ms => process_moves(cardlist, ms, remove_card(hand, c, IllegalMove))
             | Draw::ms => case cardlist of
                                [] => hand 
                              | c::cs => process_moves(cs, ms, c :: hand)
    in
      score(process_moves(cardlist, movelist, []), goal)
    end

fun possible_sums hand =
  let fun add_to_each (xs, a) =
    case xs of
         [] => []
       | x::xs' => (a+x)::add_to_each(xs', a)
  in
    case hand of
         [] => [0] 
       | c::cs => let val ps = possible_sums cs 
                  in case c of
                       (_, Ace) => add_to_each(ps, 1) @ add_to_each(ps, 11)
                     | _ => add_to_each(ps, card_value c)
                  end
  end

fun score_challenge (hand, goal) =
  let val is_same_color = all_same_color hand
   fun min_score (sums) =
      let fun score sum =
          let val s = if sum > goal
                      then (sum - goal) * 3
                      else goal - sum
          in
            if is_same_color
            then s div 2
            else s
          end
      in
        case sums of
             [] => 10000
           | x::xs => let 
                        val current = score x
                        val rest = min_score xs
                      in 
                        if current < rest
                        then current
                        else rest
                      end
      end
    val sums = possible_sums hand
  in
    min_score sums
  end

fun officiate_challenge (cardlist, movelist, goal) =
    let
      fun all_exceed sums =
        case sums of
             [] => true
           | x::xs => (x > goal) andalso all_exceed xs
      fun process_moves (cardlist, movelist, hand) =
          if all_exceed(possible_sums hand)
          then hand
          else case movelist of
               [] => hand
             | (Discard c)::ms => process_moves(cardlist, ms, remove_card(hand, c, IllegalMove))
             | Draw::ms => case cardlist of
                                [] => hand 
                              | c::cs => process_moves(cs, ms, c :: hand)
    in
      score_challenge(process_moves(cardlist, movelist, []), goal)
    end

fun careful_player (cs, goal) =
  let 
    fun should_discard (hand, next_val, sum) = 
      case hand of
           [] => NONE
         | h::hs => if (sum - (card_value h) + next_val) = goal
                    then SOME h
                    else should_discard(hs, next_val, sum)

    fun play (cs, hand) =
      let val value = sum_cards hand
          val should_draw = (value + 10) < goal
          val should_finish = value = goal
      in
        if should_finish
        then []
        else
          case cs of
               [] => if should_draw then [Draw] else []
             | c::cs' => case should_discard(hand, card_value c, value) of
                              NONE => if should_draw
                                      then Draw::play(cs', c::hand)
                                      else []
                            | SOME x => [Discard x, Draw] 
      end
  in
    play(cs, [])
  end

