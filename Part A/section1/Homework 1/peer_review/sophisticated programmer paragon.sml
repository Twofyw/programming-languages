(* Returns true if date1 is before date2, where both dates
   are represented as YMD integer triplets. *)
fun is_older(date1: (int * int * int), date2: (int * int * int)) =
  let
    (* Convert to approximate ordinal date; all that matters here
       is that the value we compute is a monotonically increasing function
       of the input date. *)
    fun approx_ordinal_day(date: (int * int * int)) =
      365 * (#1 date) + 31 * (#2 date) + (#3 date)
  in
    approx_ordinal_day(date1) < approx_ordinal_day(date2)
  end

(* Given a list of dates represented by YMD triplets and a month,
   return the number of dates that have the matching month. *)
fun number_in_month(dates: (int * int * int) list, month: int) =
  if null dates
  then 0
  else
    let
      val is_in_int = if ((#2 (hd dates)) = month) then 1 else 0
    in
      is_in_int + number_in_month(tl dates, month)
    end

(* Given a list of dates represented by YMD triplets and a list of months
   as integers, return the number of dates that have a month in months.

   Duplicates in months will result in that month being counted
   more than once. *)
fun number_in_months(dates: (int*int*int) list, months: int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* Given a list of dates represented as YMD triplets and a month, return
   all the dates that have the specified month. *)
fun dates_in_month(dates: (int*int*int) list, month: int) =
  if null dates
  then []
  else
    if (#2 (hd dates)) = month
    then (hd dates)::dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

(* Given a list of dates and a list of months, return all the dates that
   have a month in the specified months.

   Duplicate months in the second argument will result in duplicate dates. *)
fun dates_in_months(dates: (int * int * int) list, months: int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


(* Exception raised when a value that must be positive is not. *)
exception NonPositive

(* Return the nth elements of a list of strings.

  n must be positive, and if n > the length of the list,
    an exception is raised. *)
fun get_nth(strings: string list, n: int) =
  if n <= 0
  then raise NonPositive
  else
    let fun get_nth_inner(xs: string list, n: int) =
      if n = 1
      then hd xs
      else get_nth_inner(tl xs, n - 1)
    in
      get_nth_inner(strings, n)
    end

(* Convert a month number ([1, 12]) to a month name.
   For example: 3 -> March. *)
fun month_num_to_name(month: int) =
  let
    val month_names = ["January", "February", "March", "April", "May",
      "June", "July", "August", "September", "October", "November",
      "December"]
  in
    get_nth(month_names, month)
  end

(* Convert a date to a string representation.

  The date should be represented as a YMD triplet of integers.
  For example (2015, 2, 20) -> "February 20th, 2015".

  The input date is not checked for validity. *)
fun date_to_string(date: (int * int *int)) =
  let
    val year = (#1 date)
    val month = (#2 date)
    val day = (#3 date)
  in
    month_num_to_name(month) ^ " " ^
      Int.toString(day) ^ ", " ^
      Int.toString(year)
  end

(* Given a positive value sum and a list of positive integers,
   return an integer n such that the first n elements of the list
   add up to less then sum, but the first n + 1 elements add up to
   more than sum. *)
fun number_before_reaching_sum(sum: int, xs: int list) =
  if null xs
  then raise Empty
  else
    let
      val hd_xs = hd xs
    in
      if hd_xs >= sum
      then 0
      else 1 + number_before_reaching_sum(sum - hd_xs, tl xs)
    end

exception InvalidDay
(* Given an ordinal day [1, 365] return what month [1, 12] that day is in,
   assuming a standard (non-leap) year. *)
fun what_month(day: int) =
  if day > 365 orelse day < 1
  then raise InvalidDay
  else
    let
      (* Ignoring leap years *)
      val days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
      number_before_reaching_sum(day, days_per_month) + 1
    end

(* Given a start ordinal day (day1) and and end ordinal day (day2)
   return the months [1, 12] for all days in the interval [day1, day2]. *)
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

(* Given a list of dates, return the oldest date d as an option, SOME d,
   or NONE if the list is empty. *)
fun oldest(dates: (int * int * int) list) =
    let
      fun min_date(date1: (int * int * int), date2: (int * int * int)) =
        if is_older(date1, date2)
        then date1
        else date2
    in
      if null dates
      then NONE
      else
        let
          val tl_oldest = oldest(tl dates)
        in
          if isSome tl_oldest
          then SOME (min_date(hd dates, valOf tl_oldest))
          else SOME (hd dates)
        end
    end

(* Removes duplicate entries in a list, leaving the first instance.

  For example: remove_duplicates [1, 1, 2, 1] = [1, 2].

  This uses a n^2 algorithm.*)
fun remove_duplicates(xs: int list) =
  if null xs
  then xs
  else
    let
      (* Removes all occurrences of x from xs' *)
      fun remove_value(x: int, xs': int list) =
        if null xs'
        then xs'
        else
          if x = (hd xs')
          then remove_value(x, tl xs')
          else (hd xs')::remove_value(x, tl xs')
    in
      (hd xs)::remove_duplicates(remove_value(hd xs, tl xs))
    end

(* Given a list of dates represented by YMD triplets and a list of months
   as integers, return the number of dates that have a month in months.

   Duplicates in months will be counted once. *)
fun number_in_months_challenge(dates: (int*int*int) list, months: int list) =
  number_in_months(dates, remove_duplicates months)

(* Given a list of dates and a list of months, return all the dates that
   have a month in the specified list of months.

   Duplicate month values will only be counted once. *)
fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) =
  dates_in_months(dates, remove_duplicates months)

(* Returns true if the provided CE (Common Era) year is a leap year. *)
fun is_leap_year(year: int) =
  year mod 400 = 0 orelse year mod 4 = 0 andalso not (year mod 100 = 0)

(* Get the number of days in a CE month in the specified year. *)
fun get_days_in_month(month: int, year: int) =
  let
    val days_normal =  [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    val days_leap =  [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    fun helper(days: int list, n: int) =
      if n = 1 then hd days else helper(tl days, n - 1)
  in
    if is_leap_year year
    then helper(days_leap, month)
    else helper(days_normal, month)
  end

(* Determine if a date is a valid common era date.

  The input year should be of the form (Y, M, D).  The
  change to the Gregorian calendar is ignored.*)
fun reasonable_date(date: (int * int * int)) =
  let
    val year = #1 date
    val month = #2 date
    val day = #3 date
  in
    if year < 1 orelse month < 1 orelse month > 12 orelse
      day < 1 orelse day > 31
    then false
    else
      if get_days_in_month(month, year) < day
      then false
      else true
  end
