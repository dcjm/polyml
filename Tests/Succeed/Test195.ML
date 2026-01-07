(* Check that the print function is passed into a structure. *)

functor Test1(type T) =
struct
datatype d = Data of T
end;

structure T1 = Test1(type T = int * int);

PolyML.makestring (T1.Data (1, 2)) = "Data (1, 2)" orelse raise Fail "wrong";


functor Test2(type 'a T) =
struct
datatype 'a d = Data of 'a T
end;

structure T2 = Test2(type 'a T = 'a * int);

PolyML.makestring (T2.Data (1, 2)) = "Data (1, 2)" orelse raise Fail "wrong";
