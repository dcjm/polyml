(* Bug in conversion of tests to cases.  If there were more than
   two tests with the same constant the duplicates weren't properly
   filtered. *)

fun f x =
    if x = 0
    then 1
    else if x = 1
    then 2
    else if x = 0
    then 3
    else if x = 1
    then 4
    else if x = 0
    then 5
    else if x = 2
    then 6
    else if x = 3
    then 7
    else if x = 4
    then 8
    else 9;

