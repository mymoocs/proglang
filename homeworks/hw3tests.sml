(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)


use "hw3provided.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]


val test2 = longest_string1 ["A","bc","C"] = "bc"


val test3 = longest_string2 ["A","bc","C"] = "bc"

val test4a= longest_string3 ["A","bc","C"] = "bc"


val test4b= longest_string4 ["A","B","C"] = "C"


val test5 = longest_capitalized ["A","bc","C"] = "A";


val test6 = rev_string "abc" = "cba";


val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4


val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE



val test9a = count_wildcards Wildcard = 1

val test9a' = count_wildcards (TupleP [Wildcard, ConstP 1])

val test9a'' = count_wildcards (TupleP [Wildcard, Variable "A", Wildcard])


val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9b' = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "A", Wildcard])

val test9c = count_some_var ("x", Variable("x")) = 1;

val test9c1 = count_some_var ("wild",ConstructorP ("wild",Wildcard)) = 0;
val test9c2 = count_some_var ("x",TupleP[Wildcard,ConstP 17,Variable "x",
                                       UnitP,TupleP[UnitP,UnitP,UnitP],
                                       ConstructorP ("",UnitP),TupleP[],
                                       ConstructorP ("wild",Wildcard),
                                       TupleP[Wildcard,ConstP 17,
                                              Variable "x",UnitP,
                                              TupleP[UnitP,UnitP,UnitP],
                                              ConstructorP ("",UnitP),TupleP[],
                                              ConstructorP ("wild",Wildcard)]]) = 2;


val test9c3 = count_some_var ("x", Variable("x")) = 1;
val test9c4 = count_some_var ("x", Variable("x")) = 1;
val test9c5 = count_some_var ("x", Variable("x")) = 1;


val test10_0 = all_variables (Variable("x")) 


val test10 = check_pat (Variable("x")) = true

val test10a = check_pat (TupleP[ConstP 4,Wildcard,Variable "ba",
                                TupleP[Variable "ab"]]) = true

val test10b = check_pat (ConstructorP ("hi",TupleP[Variable "x",Variable "x"])) = false
val test10c = check_pat
                  (ConstructorP 
                       ("hi", TupleP[Variable "x", 
                                     ConstructorP ("yo",
                                                   TupleP[Variable "x",
                                                          UnitP])])) = false 
                                                                                                                                     
val test11 = match (Const(1), UnitP) = NONE

val test11a = match (Const 17,ConstP 17) = SOME ([])

val test11b = match (Unit,Wildcard) = SOME ([]) 

val test11c = match (Constructor ("egg",Const 4),ConstructorP ("egg",ConstP 4)) = SOME ([])

val test11d = match (Constructor ("egg",Constructor ("egg",Const 4)),
                     ConstructorP ("egg",ConstructorP ("egg",ConstP 4))) = SOME ([])

val test11e = match (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),
                           Constructor ("egg",Constructor ("egg",Const 4)),
                           Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),
                                 Constructor ("egg",Constructor ("egg",Const 4))],
                           Tuple[Unit,Unit],Tuple[Const 17,Const 4],
                           Tuple[Constructor ("egg",Const 4),
                                 Constructor ("egg",Const 4)]],
                     TupleP[ConstP 17,Wildcard,ConstP 4,
                            ConstructorP ("egg",ConstP 4),
                            ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),
                            TupleP[ConstP 17,Wildcard,ConstP 4,
                                   ConstructorP ("egg",ConstP 4),
                                   ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],
                            TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],
                            TupleP[ConstructorP ("egg",ConstP 4),
                                   ConstructorP ("egg",ConstP 4)]]) = SOME ([])


val test11f = match(Unit, Variable "a")  =  SOME [("a",Unit)] 

val test12 = first_match Unit [UnitP] = SOME []


