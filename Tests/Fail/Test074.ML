(* Flexible records do not inherit the correct "level". *)

local
    fun f x =
        let
            val {a, ...} = x
        in
            a
        end;
in
    val q: int = f {a=true}
end;
