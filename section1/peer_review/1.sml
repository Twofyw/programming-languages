
fun is_older (date1 : int * int * int, date2 : int * int * int) =
  let val is_older_year = (#1 date1) < (#1 date2)
      val is_same_year = (#1 date1) = (#1 date2)
      val is_older_month = (#2 date1) < (#2 date2)
      val is_same_month = (#2 date1) = (#2 date2)
      val is_older_day = (#3 date1) < (#3 date2)
  in
      if is_older_year
      then true
      else if is_same_year andalso is_older_month
      then true
      else if is_same_year andalso is_same_month andalso is_older_day
      then true
      else false
  end


fun number_in_month (xs : (int * int * int) list, month : int ) =
  if null xs
  then 0
  else if #2 (hd xs) = month
  then 1 + number_in_month(tl xs, month)
  else number_in_month(tl xs, month)

fun number_in_months (xs : (int * int * int) list, months : int list ) =
  if null months
  then 0
  else number_in_month(xs, hd months) + number_in_months(xs, tl months)

fun dates_in_month (xs : (int * int * int) list, month : int ) =
  if null xs
  then []
  else if #2 (hd xs) = month
  then hd xs :: dates_in_month(tl xs, month)
  else dates_in_month(tl xs, month)

fun dates_in_months (xs : (int * int * int) list, months : int list ) =
  if null months
  then []
  else dates_in_month(xs, hd months) @ dates_in_months(xs, tl months)

fun get_nth (strings : string list, n : int) =
  if n = 1
  then hd strings
  else get_nth(tl strings, n - 1)

fun date_to_string (year : int, month : int, day : int) =
  let
      val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(months, month) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)
  end

fun number_before_reaching_sum (sum : int, numbers : int list) =
  if sum <= hd numbers
  then 0
  else 1 +  number_before_reaching_sum(sum - hd numbers, tl numbers)

fun what_month (day : int) =
  let
      val months_length = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
      number_before_reaching_sum(day, months_length) + 1
  end

fun month_range (day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month day1 :: month_range(day1 + 1, day2)

fun oldest (xs : (int * int * int) list) =
  if null xs
  then NONE
  else
      let
	  fun oldest_nonempty (xs : (int * int * int) list) =
	    if null (tl xs)
	    then hd xs
	    else
		let
		    val tl_ans = oldest_nonempty(tl xs)
		in
		    if is_older(tl_ans, hd xs)
		    then tl_ans
		    else hd xs
		end
      in
	  SOME (oldest_nonempty(xs))
      end

