(*
    Title:      References that allow a single assignment
    Author:     David Matthews
    Copyright   David Matthews 2010, 2016

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

(*!The `SingleAssignment` structure provides a reference 
  that can be assigned a value only once.*)

structure SingleAssignment:>
sig
    (*!The type of a single-assignment reference. It is similar to the standard 
      `ref` type constructor.*)
    type 'a saref (* Equality not allowed *)
    (*!This exception is raised if an attempt is made to assign a value twice 
      to the same reference.*)
    exception Locked
    (*!Construct a single-assignment reference.*)
    val saref: unit -> 'a saref
    (*!Assign a value to the reference. If it has already been assigned a value 
      this will raise `Locked`. Note that this function is not thread-safe. A `mutex`
      must be associated with reference if there is the possibility that two different 
      threads may attempt to assign to the same reference.*)
    val saset: 'a saref * 'a -> unit
    (*!Extract the current value of the reference. If it has not yet been assigned 
      a value it will return `NONE`. If it has, 
      it will return `SOME v` where `v`
      is the value that was assigned.*)
    val savalue: 'a saref -> 'a option
end
=
struct
    exception Locked

    type 'a saref = 'a option ref

    fun saref () = ref NONE
    
    val savalue = !

    fun saset(saVar as ref NONE, newValue) =
    (
        saVar := SOME newValue;
        RunCall.clearMutableBit saVar
    )
    |   saset _ = raise Locked
end;
(*!The reason behind the `SingleAssignment` structure 
  has to do with the way the Poly/ML storage management system deals with *mutable* 
  and *immutable* data. Immutable memory cells are given a value when they 
  are created and once created never change. They are used for lists, tuples, 
  vectors and other datatypes. In contrast, refs and arrays are mutable data. 
  They are given a value when they are created in the same way as immutable data 
  but their contents can change by assignment. In addition Standard ML also distinguishes 
  between mutable and immutable data in the treatment of equality. Immutable data 
  structures are considered equal if their contents are the same, mutable cells 
  are considered equal only if they are the pointers to the same cell.
  
  Because of these differences mutable data has to be handled separately from 
  immutable data by the garbage collector. Using mutable cells imposes an extra 
  cost on each collection when compared with immutable data. In addition it is 
  possible to reduce the heap size by merging immutable cells that have the same 
  contents. In some circumstances the garbage collector may do this automatically; 
  more often it is done explicitly using `PolyML.shareCommonData`. 

  The `SingleAssignment` structure allows for a 
  combination of mutable and immutable data. A value of type `saref` 
  is initially mutable but once it has been assigned a value it is marked as immutable. 
  This allows the garbage-collector and sharing code to treat it as purely immutable 
  once it has been locked.
  
  A typical use for a single-assignment reference is when a data structure is 
  being built by multiple threads. A `saref` can 
  be used within the data structure to represent a portion of the structure to 
  be built and a thread created to build it. When the thread completes it assigns 
  the `saref` with the results of its work. The 
  full structure is now immutable with all the advantages of immutable data.*)
