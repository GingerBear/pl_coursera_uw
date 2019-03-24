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

fun card_color (suit, rank) =
  case suit of
    Clubs => Black
    | Spades => Black
    | Diamonds => Red
    | Hearts => Red


fun card_value (suit, rank) =
  case rank of
    Num i => i
    | Ace => 11
    | _ => 10

fun remove_card (cs, c, e) =
  case cs of
    [] => raise e
    | c'::cs' => if c' = c then cs' else c'::remove_card(cs', c, e)

fun all_same_color (cards) =
  case cards of
    [] => true
    | _::[] => true
    | card::card'::cards' => card_color(card) = card_color(card') andalso all_same_color(card'::cards')

fun sum_cards (cards) =
  let
    fun sum (cards, acc) =
      case cards of
        [] => acc
        | card::cards' => sum(cards', acc + card_value card)
  in
    sum(cards, 0)
  end

fun score(helds, goal) =
  let
    val sum = sum_cards helds
    val prescore = if sum > goal then (sum - goal) * 3 else goal - sum
  in
    if all_same_color helds then prescore div 2 else prescore
  end

fun officiate(cards, moves, goal) =
  let
    fun play(cards, moves, goal, helds) =
      let
        val score = score(helds, goal)
      in
        if score > goal then score
        else
          case moves of
            [] => score
            | move::moves' =>
              case move of
                Discard card => play(cards, moves', goal, remove_card(helds, card, IllegalMove))
                | Draw =>
                  case cards of
                    [] => score
                    | card::cards' => play(cards', moves', goal, card::helds)
      end

  in
    play(cards, moves, goal, [])
  end