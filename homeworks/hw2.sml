(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
  string), then you avoid several of the functions in problem 1 having
  polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* all_except_option : string * string list ->  string option *)
fun all_except_option(str : string, ss : string list) = 
    let fun f1(xs : string list) = 
	    case xs of
		[] => []
	       |(s::ss') => if same_string(str, s) 
			    then f1(ss') 
			    else s::f1(ss')
    in
	
	if length ss = length (f1(ss))
	then NONE 
	else SOME (f1(ss))
    end

fun get_substitutions1(xss : string list list, str : string) =
    case xss of
	[] => []
      | (ys::yss) => case all_except_option(str, ys) of
			 NONE => get_substitutions1(yss, str)
		       | SOME ls => ls @ get_substitutions1(yss, str)

fun get_substitutions2(xss : string list list, str : string) =
    let fun aux(xss', acc) = 
	    case xss' of
		[] => acc
	      | (ys::yss) => 
		case all_except_option(str, ys) of
		    NONE => aux(yss, acc)
		  | SOME ls => aux(yss, acc @ ls )
				  
    in
	aux(xss,[])
    end



fun similar_names(xss : string list list, thename : {first:string,middle:string,last:string}) =
    let fun nameify(ns : string list) = 
	    case ns of
		[] => []
	      | y::ys =>  {first = y, middle = #middle thename, last = #last thename} :: nameify(ys)
    in 
	thename :: nameify(get_substitutions1(xss, #first thename))
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


fun card_color(s : suit, r : rank) =
    case s of
	Hearts => Red
      | Diamonds  => Red
      | _ => Black

fun card_value(_, r) =
	      case r of
		  Num n => n
		| Ace  => 11 
		| _ =>  10


fun remove_card( cs : card list, c :card, e : exn) =
    let fun aux(ys, acc, is_meet) =
	    case ys of
		[] => (acc, is_meet)
	      | x::xs => if x = c
			 then aux([], acc @ xs, true)
			 else aux(xs, x::acc, is_meet)
	val rs = aux(cs, [], false)
	val nn = length cs
    in
	if #2 rs 
	then #1 rs
	else raise e 
    end

fun all_same_color(cs : card list) =
    case cs of
	[] => true
      | [x] => true 
      | x :: y :: xs  => card_color(x) = card_color(y) andalso all_same_color(y::xs) 

    
fun sum_cards(cs : card list) =
    let fun aux(xs, acc) =
	    case xs of
		[] => acc
	      | y :: ys => aux(ys, acc + card_value(y)) 
    in
	aux(cs, 0)
    end

fun score(cs : card list, goal : int) =
    let 
	val sum = sum_cards(cs);
	val pre_score = if sum > goal
			then 3 * (sum -goal)
			else (goal - sum) 
    in
	if all_same_color(cs)
	then pre_score div 2
	else pre_score
    end
	

fun officiate(cards : card list, ms : move list, goal : int) =
    let 
        fun play(cs : card list, held_cards : card list, xs : move list) = 
	        if sum_cards(held_cards) > goal
	        then score(held_cards, goal)
	        else
		        case xs of
		            [] => score(held_cards, goal)
		          | x::ys => case x of
				                 Draw =>  if sum_cards(hd cs :: held_cards) > goal
					                      then score(hd cs :: held_cards, goal)
					                      else play(tl cs, hd cs :: held_cards,ys)
			                   | Discard c => play(cs, remove_card(held_cards, c, 
                                                                   IllegalMove), ys)
                                                  
    in
	    play(cards, [], ms)
    end
        
