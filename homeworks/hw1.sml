
fun is_older(a : int*int*int, b : int*int*int) = 
    let
	    val y1 = #1 a
        val m1 = #2 a
        val d1 = #3 a
        val y2 = #1 b
        val m2 = #2 b
        val d2 = #3 b		    
    in	
	    if(y1 = y2) 
	    then if(m1 = m2) 
	         then if(d1 = d2)
		          then false
		          else if(d1 < d2) then true else false 	    
	         else if(m1 < m2) then true else false
        else if(y1 < y2) then true else false
    end




fun number_in_month(ls : (int*int*int) list, m : int)=
    if null ls
    then 0
    else if(#2 (hd ls) = m)
	then 1 + number_in_month(tl ls, m)
    else number_in_month(tl ls, m) 	     
                        

(* 3 *)
fun number_in_months(ls : (int*int*int) list, ms : int list)=
    if null ms 
    then 0 
    else number_in_month(ls, hd ms) + number_in_months(ls, tl ms)		    


(* 4 *)

fun dates_in_month(ls : (int*int*int) list, m : int)=
    if null ls
    then []
    else if(#2 (hd ls) = m)
	then (hd ls)  ::  dates_in_month(tl ls, m)
    else dates_in_month(tl ls, m) 	     
    
fun dates_in_months(ls : (int*int*int) list, ms : int list)=
    if null ms 
    then []
    else dates_in_month(ls, hd ms) @ dates_in_months(ls, tl ms)		    

fun get_nth(ls : string list, n : int) = 
    if n=1 then hd ls else get_nth(tl ls, n-1)

fun date_to_string(d : int*int*int) = 
    let 
	    val ms = ["January","February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "Decembe"]
	    val year = Int.toString(#1 d)
	    val day = Int.toString(#3 d)
	    val month = get_nth(ms, (#2 d)) 			
    in  month ^" "^ day  ^ ", " ^ year  
    end	      


(* 8 *)
fun number_before_reaching_sum(sum : int, ls : int list) = 
    let
	    fun summer(xs : int list) =
	        if null xs 
	        then 0
	        else hd xs +  summer(tl xs)
	    fun partialList(n : int, cs : int list) = 
	        if n = 1
	        then [hd cs]
	        else (hd cs) :: partialList(n-1, tl cs)
			                           
	    fun checkRechingSum(m : int) = 
	        if sum <= summer(partialList(m, ls))
	        then m-1
	        else checkRechingSum(m+1)
    in
	    checkRechingSum(1)
    end
        
(* 9 *)
fun what_month(d : int) =
    let 
	    val ms = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	    number_before_reaching_sum(d, ms) + 1
    end
        
fun month_range(d1 : int, d2 : int) =
    let 
	    fun fromTo(from : int, to : int) =
	        if from = to
	        then [from]
	        else from :: fromTo(from+1, to)
	    fun processing(xs : int list) = 
	        if null xs
	        then []
	        else what_month(hd xs)::processing(tl xs)
    in
	    if d1 > d2
	    then []
	    else  processing(fromTo(d1,d2))
    end
        
fun oldest (xs : (int *int*int) list) =
    if null xs
    then NONE
    else let 
	    fun max_nonempty (xs : (int*int*int) list) =
	        if null (tl xs)
	        then hd xs
	        else
		        let val tl_ans = max_nonempty(tl xs)
		        in
		            if is_older( hd xs , tl_ans)
		            then hd xs
		            else tl_ans
		        end
    in
	    SOME (max_nonempty xs)
    end
             
             
