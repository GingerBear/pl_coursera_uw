(* fun is_older (d1: int*int*int, d2: int*int*int) =
  if #1 d2 > #1 d1
  then true
  else
    if #2 d2 > #2 d1
    then true
    else
      if #3 d2 > #3 d1
      then true
      else false *)


fun is_older (d1: int*int*int, d2: int*int*int) =
  #1 d2 > #1 d1 orelse  #2 d2 > #2 d1 orelse #3 d2 > #3 d1

fun number_in_month (dates: (int*int*int) list, month: int) =
  if not (null dates) andalso #2 (hd dates) = month
  then 1 + number_in_month (tl dates, month)
  else 0