(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)


fun count_wildcards p = g (fn() => 1) (fn(x) => 0) (TupleP [p])

fun count_wild_and_variable_lengths p = 
    g (fn() => 1) (fn(x) => String.size x) (TupleP [p])

fun count_some_var (str : string,  p : pattern) =
    g (fn() => 0) (fn(x) =>  if x = str then 1 else 0) (TupleP [p])

fun all_variables p = 
    case p of
        Variable x => [x]
      | TupleP ps => List.foldl (fn (p, name) => all_variables(p) @ name) [] ps
      | ConstructorP(_, p') => all_variables p'  
      | _ => []
fun has_repeats xs = 
    case xs of
        [] => false
      | [x] => false
      | x::xs' => if List.exists (fn(a) => a = x) xs' 
                  then true 
                  else has_repeats xs' 
                             
val check_pat = not o  has_repeats o all_variables



fun fold (f,acc,xs) =
    case xs of
        [] => acc
      | x::xs => fold(f, f(acc,x), xs)

val  only_capitals =  List.filter (fn s => Char.isUpper(String.sub(s, 0)))

fun longest_string1 xs =
    case xs of
        [] => ""
      | y::_ =>  fold ((fn (acc, x) => if String.size acc < String.size x 
                                       then x 
                                       else acc), y, xs)


fun longest_string2(xs :string list) =
    case xs of
        [] => ""
      | y::_ =>  fold ((fn (acc, x) => if String.size acc <= String.size x 
                                       then x
                                       else acc), y, xs)

fun longest_string_helper f xs = 
    case xs of
        [] => ""
      | y::_ =>  fold ((fn (acc, x) => if f (String.size acc, String.size x) 
                                       then x 
                                       else acc), y, xs)
    
val longest_string3 = longest_string_helper (fn (a, b) => a < b) 
val longest_string4 = longest_string_helper (fn (a, b) => a <= b) 
                                            

val longest_capitalized =  longest_string3 o  only_capitals

val rev_string = implode o List.rev o explode

fun first_answer f xs = 
    case xs of
        [] => raise NoAnswer
      | x::xs' => case (f x) of
                      SOME v => v
                    | NONE => first_answer f xs'                                          

fun all_answers f xs = 
    let fun aux(acc, f, ys) = 
            case ys of
                [] => SOME acc
              | x::xs' => case f x of
                              NONE => NONE
                            | SOME ls =>aux( ls @ acc, f,  xs')

    in
        aux([], f, xs)
    end


(* 11. *)
fun match (v: valu, p: pattern) = 
    case (v, p) of
        (_, Wildcard) => SOME []
      | (v1, Variable x) => SOME [(x,v1)]
      | (Unit, UnitP) => SOME []
      | (Const n, ConstP m) => if n = m then  SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps 
                                 then  all_answers match (ListPair.zip(vs, ps))
                                 else NONE 
      | (Constructor(s2,v'), ConstructorP(s1,p')) => if s1 = s2 
                                                     then match(v',p') 
                                                     else NONE
      | ( _ , _) => NONE 
  
 
(* 112. *)

fun first_match v ps =
    SOME (first_answer (fn p => match(v, p)) ps) handle NoAnswer => NONE
