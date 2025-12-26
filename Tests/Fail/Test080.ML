(* This should fail since #c r is bool *)


local
    fun f (x as {a, b, ...}) = x
in
    val q = f{a=1,b=2, c=3}
    val r = f{a=true, b=false, c=true}
    
    val p: int = #c r
end;
