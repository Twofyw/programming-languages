(*  1  *)
fun is_older (d1:int*int*int, d2:int*int*int) =
  if #1 d1 > #1 d2
  then false
  else
      if #1 d1 = #1 d2
      then
	  if #2 d1 > #2 d2
	  then false			
	  else
	      if #2 d1 = #2 d2
	      then
		  if #3 d1 >= #3 d2
		  then false
		  else true
	      else true
      else true  
(*  2  *)
fun number_in_month (d: (int*int*int) list, m:int) =
    if null d
    then 0
    else
        let val tl_d = number_in_month(tl d, m)
	in if  #2(hd d) = m 
	    then 1 + tl_d
            else tl_d
	end

(*  3  *)
      
fun number_in_months (d: (int*int*int) list, m: int list) =
  if null m then 0
  else number_in_month(d, hd m) + number_in_months(d, tl m)

(*  4  *)						  
      
fun dates_in_month (d : (int*int*int) list, m: int) =
    if null d
    then []
    else let val tl_d = dates_in_month(tl d, m)
        in if #2(hd d) = m 
	   then hd d::tl_d
	      else tl_d
	 end
(*  5  *)
fun dates_in_months (d :(int*int*int) list, m: int list) =
    if null d
    then []
    else let fun check_months(d: (int*int*int), m: int list) =
	       if null m then []
	       else let val tl_m = check_months(d, tl m)
		    in if hd m = #2 d
		       then d::tl_m
		       else tl_m
		    end
	 in check_months(hd d, m)@dates_in_months(tl d, m)
	 end

      
(*  6  *)
fun get_nth(s: string list, n:int) =
  if null s
  then NONE
  else
      let fun ind(s: string list, m: int, n: int)=
	    if m = n
	    then SOME(hd s)
	    else ind(tl s, m+1, n)
      in  ind(s,1,n)
      end

(*  7  *)	      
fun date_to_string(d: (int*int*int)) =
  let val names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in get_nth(names, #2 d)^" "^Int.toString(#3 d)^", "^Int.toString(#1 d)
  end
(*  8  *)
fun number_before_reaching_sum(sum: int, l: int list) =
  if null l then 0
  else let fun s(l: int list, n: int) =
	     if n = 1
	     then hd l
	     else hd l + s(tl l, n-1)
       in
	   let fun indx(l: int list, sum: int, m: int)=
		 if s(l,m) >= sum
		 then m - 1
		 else indx(l,sum,m+1)
	   in
	       indx(l,sum,1)
	   end
       end

				     
(*  9  *)					     
fun what_month(n: int) =
  if n < 1 orelse n > 365 then 0
  else let val d = [31,28,31,30,31,30,31,31,30,31,30,31]
       in number_before_reaching_sum(n, d) + 1
       end
(* 10  *)	   
fun month_range(day1: int,day2: int) =
  if day1 > day2
  then []
  else if day1 = day2		     
  then [what_month(day2)]
  else what_month(day1)::month_range(day1+1, day2)


(* 11  *)					    
fun oldest(l: (int*int*int) list) =
  if null l then NONE
  else let val d = oldest(tl l)
       in if isSome d andalso is_older(valOf d, hd l)
	  then d
	  else SOME(hd l)
       end

  
