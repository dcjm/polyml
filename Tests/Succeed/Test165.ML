(* Bug with unused loop variable register being reused. *)

fun g r =
let
    val x = ref 0

    fun f () =
    let
        val () = x := !x+1
    in
        if !x = 1 then f() else ()
    end;
in
    f ()
end;

g();
