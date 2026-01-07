(* Phil Clayton's example in issue 138.  This caused exponential blow-up because of inlining. *)

fun C f =
  fn
    SOME n => f n
  | NONE   => f 0

infixr 1 &&&>
fun (f &&&> g) h (a, b) = f (fn a' => g (fn b' => h (a', b')) b) a;

fun test f =
  (
    C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
     &&&> C
  )
    f
;
