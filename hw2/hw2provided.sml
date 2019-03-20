(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (s1, s2s) =
  case s2s of
    [] => NONE
    | s :: ss =>
      if same_string(s, s1) then SOME ss
      else case all_except_option (s1, ss) of
          NONE => NONE
          | SOME arr => SOME (s::arr)

fun get_substitutions1(strLL, str) =
  case strLL of
    [] => []
    | strL::strLL2 => case all_except_option(str, strL) of
                        NONE => [] @ get_substitutions1(strLL2, str)
                        | SOME arr => arr @ get_substitutions1(strLL2, str)

fun get_substitutions2(strLL, str) =
  let
    fun aux(strLL, str, acc) =
      case strLL of
        [] => acc
        | strL::strLL2 => case all_except_option(str, strL) of
                            NONE => aux(strLL2, str, acc)
                            | SOME arr => aux(strLL2, str, acc@arr)
  in
    aux(strLL, str, [])
  end

fun similar_names(strLL, name) =
  let
    fun mapFullName (strL, last, middle, acc) =
      case strL of
        [] => acc
        | str::strL' => mapFullName(strL', last, middle, acc@[{first=str, last=last, middle=middle}])
  in
    case name of
      {first=f, last=l, middle=m} => mapFullName(get_substitutions2(strLL, f), l, m, [name])
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