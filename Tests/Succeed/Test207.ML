(* These all worked but were extremely slow.  Issue #203. *)
type ('a1, 'a2) test0 = unit * unit;
type ('a1, 'a2) test1 = (('a1, 'a2) test0, ('a1, 'a2) test0) test0;
type ('a1, 'a2) test2 = (('a1, 'a2) test1, ('a1, 'a2) test1) test1;
type ('a1, 'a2) test3 = (('a1, 'a2) test2, ('a1, 'a2) test2) test2;
type ('a1, 'a2) test4 = (('a1, 'a2) test3, ('a1, 'a2) test3) test3;
type ('a1, 'a2) test5 = (('a1, 'a2) test4, ('a1, 'a2) test4) test4;
type ('a1, 'a2) test6 = (('a1, 'a2) test5, ('a1, 'a2) test5) test5;

fun foo (x : ('a1, 'a2) test6) : ('a1, 'a2) test6 = x;
fun foo (x : ('a1, 'a2) test6) : ('a1, 'a2) test1 = x;
fun foo (x : ('a1, 'a2) test1) : ('a1, 'a2) test6 = x;
fun foo (x : ('a1, 'a2) test1) : unit * unit = x;
