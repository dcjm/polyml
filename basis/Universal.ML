(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

(**
  The `Universal` structure provides a universal 
  union type. It allows value of any type to be stored in a single table without 
  knowing in advance the types to be stored. Note that this is not the same as 
  a dynamic type. The values are discriminated by the tag, not by the type. There 
  may be more than one tag that can be used with values of a particular type and 
  these are treated as completely different. `Universal` 
  is built in for efficiency reasons but it is perfectly feasible to implement 
  it in Standard ML using `exception` bindings.
**)

structure Universal :>

sig
    (*!The type of the universal union.*)
    type universal
    (*!The type of a tag that can be used to mark a value of the argument type.*)
    type 'a tag

    (*!Create a tag that can be used to identify a value of a particular type.*)
    val tag : unit -> 'a tag
    (*!Inject a value into the union. This marks the value with the tag.*)
    val tagInject  : 'a tag -> 'a -> universal
    (*!Test whether the value was marked with the tag.*)
    val tagIs      : 'a tag -> universal -> bool
    (*!Project a value from the union. The tag must match the tag that was used 
      to create union value otherwise a `Match` 
      exception will be raised.*)
    val tagProject : 'a tag -> universal -> 'a
  
end =


struct

    (* The universal type is based on exn which provides a tagged union.
       We use opaque signature matching to create a different type. *)
    type universal = exn

    type 'a tag =
      { 
        is:      universal -> bool,
        inject: 'a -> universal,
        project: universal -> 'a
      };

    (* The Match exception is created in the General structure in the basis
       library which hasn't been built yet. *)  
    fun tag () : 'a tag =
    let
      exception E of 'a;
    in
      { 
        inject  = fn x => E x,
        project = fn E x => x    | _ => raise RunCall.Match,
        is      = fn E _ => true | _ => false
      }
    end ;

    val tagIs      : 'a tag -> universal -> bool  = #is
    val tagInject  : 'a tag -> 'a -> universal    = #inject
    val tagProject : 'a tag -> universal -> 'a    = #project
  
end;

(*
This code will test the above structure


datatype t = T of int ;
datatype x = X of string ;

val {is=ist,inject=injectT:t->universal,project=projectT} = tag();
val {is=isx,inject=injectX:x->universal,project=projectX} = tag();

val a = injectT (T 42) ;
val b = injectT (T 24) ;
val c = injectX (X "hello") ;
val d = injectX (X "mike") ;

map ist [a,b,c,d] ;
map isx [a,b,c,d] ;

projectT a ;
projectT b ;
projectT c ;
projectT d ;

projectX a ;
projectX b ;
projectX c ;
projectX d ;
*)

