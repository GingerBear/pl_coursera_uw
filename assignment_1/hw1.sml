
fun is_older (d1: int*int*int, d2: int*int*int) =
  (#1 d1 < #1 d2) orelse
  (#1 d1 = #1 d2 andalso #2 d1 < #2 d2) orelse
  (#1 d1 = #1 d2 andalso #2 d1 = #2 d2 andalso #3 d1 < #3 d2)

fun number_in_month (dates: (int*int*int) list, month: int) =
  if not (null dates)
  then
    if #2 (hd dates) = month
    then 1 + number_in_month (tl dates, month)
    else 0 + number_in_month (tl dates, month)
  else 0

fun number_in_months (dates: (int*int*int) list, months: int list) =
  if null months
  then 0
  else number_in_month (dates, hd months) + number_in_months (dates, tl months)

fun dates_in_month (dates: (int*int*int) list, month: int) =
  if null dates
  then []
  else
    if #2 (hd dates) = month
    then hd dates::dates_in_month (tl dates, month)
    else dates_in_month (tl dates, month)

fun append (xs, ys) =
  if null xs
  then ys
  else (hd xs)::append(tl xs, ys)

fun dates_in_months (dates: (int*int*int) list, months: int list) =
  if null months
  then []
  else append (dates_in_month(dates, hd months), dates_in_months(dates, tl months))

fun get_nth (strs: string list, index: int) =
  if index = 1
  then hd strs
  else get_nth (tl strs, index - 1)

fun date_to_string (date: int*int*int) =
  let
    val months = [
      "January",
      "February",
      "March",
      "April",
      "May",
      "June",
      "July",
      "August",
      "September",
      "October",
      "November",
      "December"
    ]
  in
    get_nth(months, #2 date) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
  end

fun number_before_reaching_sum(sum: int, arr: int list) =
  if sum <= 0
  then 0-1
  else 1 + number_before_reaching_sum(sum - (hd arr), tl arr)

fun what_month(days: int) =
  number_before_reaching_sum(
    days,
    [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  ) + 1

fun month_range(day1: int, day2: int) =
  if day1 <= day2
  then what_month(day1)::month_range(day1 + 1, day2)
  else []

fun oldest(dates: (int*int*int) list) =
  if null dates
  then NONE
  else
    let
      fun oldest_nonempty (dates: (int*int*int) list) =
        if null (tl dates)
        then hd dates
        else
          let
            val oldest_date = oldest_nonempty (tl dates)
          in
            if is_older (oldest_date, hd dates)
            then oldest_date
            else hd dates
          end
    in
      SOME (oldest_nonempty dates)
    end
