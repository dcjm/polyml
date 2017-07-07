(*
    Title:      Weak references
    Author:     David Matthews
    Copyright   David Matthews 2008, 2015-16

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

(* A weak reference or array contains option values.  The SOME variant of
   the option must contain a reference.  This restriction is imposed because
   they require pointer equality.  A weak reference or array behaves just like
   a normal ref or array with one difference.  The garbage collector may set
   a weak ref or the field of weak array to NONE if it currently contains
   SOME r but r is not reachable other than through weak references.  The
   one proviso is that if r is contained in the executable it is always
   reachable.
*)

signature WEAK =
sig
    val weak: 'a ref option -> 'a ref option ref
    val weakArray: int * 'a ref option -> 'a ref option array
    val weakLock: Thread.Mutex.mutex
    and weakSignal: Thread.ConditionVar.conditionVar
    val touch : 'a ref -> unit
end;

structure Weak: WEAK =
struct
    fun weak (v: 'a ref option): 'a ref option ref = RunCall.allocateWordMemory(0w1, 0wx60, v)
    
    fun weakArray(n: int, v: 'a ref option): 'a ref option array =
    let
        val () = if n < 0 orelse n >= Array.maxLen then raise Size else ()
        val arr = RunCall.allocateWordMemory(Word.fromInt n, 0wx60, v)
    in
        arr
    end

    val weakLock = Thread.Mutex.mutex()
    and weakSignal = Thread.ConditionVar.conditionVar()

    (* touch is considered by the compiler as an access to the ref but doesn't
       actually do anything with it.  The idea is that it ensures that when a ref
       is used as a token that this will access the ref and avoid the weak
       reference becoming set to NONE.  It's primarily there for long-term
       security in the event that the compiler is sufficiently clever to
       work out that something is no longer referenced. *)
    fun touch v = v := !v
end;
