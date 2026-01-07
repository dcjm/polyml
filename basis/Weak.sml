(*
    Title:      Weak references
    Author:     David Matthews
    Copyright   David Matthews 2008, 2015-16, 2019, 2020

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

(**
  The `Weak` structure contains functions for constructing 
  *weak* references and arrays. A weak reference is a way of detecting 
  when a resource is no longer in use and can be recovered. It is, in effect, 
  a way of extending the concept of garbage-collection to user code.
**)
signature WEAK =
sig
    (** Constructs a weak reference.  **)
    val weak: 'a ref option -> 'a ref option ref
    (** Constructs an array containing weak references. **)
    val weakArray: int * 'a ref option -> 'a ref option array
    (** A lock and a condition variable that is broadcast when the garbage collector has recovered a *token*. **)
    val weakLock: Thread.Mutex.mutex
    and weakSignal: Thread.ConditionVar.conditionVar
    (** Uses the reference without changing it, ensuring that it is reachable at that point. **)
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
    val touch: 'a ref -> unit = RunCall.touch
end;
(**
   The idea behind weak references is to allow user library code to recover resources 
  when they are no longer in use. This is only relevant for resources, such as 
  file descriptors, that exist outside the Poly/ML memory and need to be recovered.
  
  The garbage-collector recovers space in the heap by identifying cells that 
  are reachable from *roots*, generally the stacks of threads, and treating 
  everything else as garbage. This can be extended to external resources by associating 
  a *token* with the resource. While the token is reachable the resource 
  is considered to be in use. Once the token ceases to be reachable the resource 
  can be recovered.
  
  A weak reference is used to detect when the token is no longer accessible. 
  To make use of this the library code must allocate a normal reference value, 
  the token, whenever it constructs or links to the external resource and include 
  the token within the data it returns to the client code. The contents of the 
  reference are not relevant; it can be a `unit ref`, 
  what matters is the identity of the reference. When the library creates a token 
  it makes an entry in its own data structure within a weak reference or array. 
  That entry is set to `SOME token`. Note that the 
  type of a weak reference is `'a ref option ref` 
  i.e. it can only contain an option type holding a reference value.
  
  Provided the client code continues to use the resource and has a reachable 
  pointer to the token there will be no change to the state. If, though, it discards 
  the data associated with the resource and hence the pointer to the token the 
  resource is considered to be released and the library may recover the resource. 
  If the garbage collector detects that there are no other pointers to the token 
  except the weak reference it will change the weak reference from `SOME token` to
  `NONE`, so there are no longer  any pointers at all.
  
  To actually release the external resource the library must check the weak references 
  or arrays within its own data structures and look for entries that have been 
  set to `NONE`. Depending how the library code 
  works it may be appropriate to do this synchronously whenever a request is made 
  to allocate a new resource. An alternative would be to create a new thread to 
  manage the process asynchronously. To aid this the thread should lock the `weakLock` 
  mutex and suspend itself by calling `Thread.ConditionVar.wait` 
  or `Thread.ConditionVar.waitUntil`,  passing `weakLock` and `weakSignal` 
  as arguments. The `weakSignal` condition variable 
  is broadcast after a garbage-collection if the garbage collector has modified 
  a weak reference. Because there may be several libraries using weak references 
  the receipt of the signal does not guarantee that a resource associated with 
  any particular library has been released.
  
  The garbage-collector is only run when necessary and detection of released 
  resources may happen very infrequently, depending on factors such as the size 
  of the heap. To force a collection the library can call `PolyML.fullGC`
**)
