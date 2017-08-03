fun is_older(first : int * int * int, second : int * int * int) =
  if #1 first < #1 second
  then true
  else if #1 first = #1 second
  then if #2 first < #2 second
       then true
       else if #2 first = #2 second
       then if #3 first < #3 second
	    then true
	    else false
       else false
  else false
fun number_in_month(dates : (int * int * int) list, month: int)=
  if null dates
  then 0
  else let val next = number_in_month(tl dates, month)
       in if month = #2 (hd dates) then 1 + next else next
       end
fun number_in_months(dates : (int * int * int) list, months : int list)=
  if null months
  then 0
  else let val next = number_in_months(dates, tl months)
	   val this = number_in_month(dates, hd months)
       in if this = 0
	  then next
	  else next + this
       end
fun dates_in_month(dates : (int * int * int) list, month : int)=
  if null dates
  then []
  else let val next = dates_in_month(tl dates, month)
       in if month = #2 (hd dates)
	  then (#1 (hd dates), #2 (hd dates), #3  (hd dates)) :: next
	  else next
       end
fun dates_in_months(dates : (int * int * int) list, months: int list)=
  if null months
  then []
  else let val date = dates_in_month(dates, hd months)
	   val next = dates_in_months(dates, tl months)
       in if not (null date)
	  then date@next
	  else next
       end
fun get_nth(strings : string list, n : int)=
  if n = 1
  then hd strings
  else get_nth(tl strings, n-1);
fun date_to_string(date : int * int * int)=
  let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end
fun number_before_reaching_sum(sum: int, list: int list)=
  if sum - hd list > 0
  then 1 + number_before_reaching_sum(sum - hd list, tl list)
  else 0
fun what_month(day : int)=
  let val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in number_before_reaching_sum(day, months) + 1
  end
fun month_range(day1 : int, day2 : int)=
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)
fun oldest(dates : (int * int * int) list)=
  if null dates
  then NONE
  else let fun oldest_nonempty(dates : (int * int * int) list)=
	     if null(tl dates)
	     then hd dates
	     else let val next = oldest_nonempty(tl dates)
		  in
		      if is_older(hd dates, next)
		      then hd dates
		      else next
		  end
       in SOME(oldest_nonempty dates)
       end
fun number_in_months_challenge(dates : (int * int * int) list, months : int list)=
  let fun duplicated(month : int, months : int list)=
	if null months
	then false
	else if month = hd months
	then true
	else duplicated(month, tl months)
      fun clean(months : int list)=
	if null months
	then []
	else if duplicated(hd months,tl months)
	then clean(tl months)
	else hd months :: clean(tl months)
  in number_in_months(dates, clean months)
  end
fun dates_in_months_challenge(dates : (int * int * int) list, months: int list)=
  let fun duplicated(month : int, months : int list)=
	if null months
	then false
	else if month = hd months
	then true
	else duplicated(month, tl months)
      fun clean(months : int list)=
	if null months
	then []
	else if duplicated(hd months,tl months)
	then clean(tl months)
	else hd months :: clean(tl months)
  in dates_in_months(dates, clean months)
  end
fun reasonable_date(date : int * int * int)=
  let val year = #1 date
      val month = #2 date
      val day = #3 date
  in let fun leap()=
	   if year mod 4 <> 0
	   then false
	   else if year mod 100 <> 0
	   then true
	   else if year mod 400 <> 0
	   then false
	   else true
     in if year > 0 andalso month > 0 andalso month <13 andalso day > 0
	then let val normal_months = ["31", "28", "31", "30", "31", "30", "31", "31", "30", "31", "30", "31"]
	     in if leap() andalso month = 2
		then if day <=29 then true else false
		else let val month_string = get_nth(normal_months, month)
		     in if day <= valOf(Int.fromString month_string) then true else false
		     end
	     end
	else false
     end
  end
