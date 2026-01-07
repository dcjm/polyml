(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Modified David C.J. Matthews 2008, 2014, 2015

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

(** Hash table type - Creates a hash table of specified initial size. This
   version expands the hash table and rehashes when the table gets too full
**)

structure HashArray:>
sig
    (** The hashtable type **)
    type 'a hash
    (** Create a hash table with a given initial size. **)
    val hash: int -> 'a hash
    (** Add a new entry to the table or update an existing one.  The table will
        grow if necessary. **)
    val update: 'a hash * string * 'a -> unit
    (** Search for an entry.  Returns SOME if the entry exists or NONE if it does not. **)
    val sub: 'a hash * string -> 'a option
    (** Delete an entry from the table. **)
    val delete: 'a hash * string -> unit
    (** Fold a function over all the entries in the table. **)
    val fold: (string * 'a * 'b -> 'b) -> 'b -> 'a hash -> 'b
end =

struct

local
    infix 8 sub

    (* Each entry in the table is a pair containing the key and the value. *)

    (* The type of each entry in the array.  Putting the tuple in here
       allows the compiler to use an optimised representation.
       We have to distinguish empty entries, which stop the search,
       from deleted entries that don't. *)
    datatype 'a namedOption = Empty | Deleted | Used of string * 'a
in

    fun hashValue vecLen str =
        Word.toInt(
            Word.mod(
                CharVector.foldr
                    (fn (ch, n) => Word.fromInt(Char.ord ch) + 0w7*n)
                    0w0 str,
                (Word.fromInt vecLen)))

    (* The above function is the quickest and simplest way of computing the
       hash value now that we have Word.* and Word.mod compiled inline.
       They aren't implemented in all code-generators so it could be worth
       retaining the old code.  DCJM 26/2/01. *)

    datatype 'a hash =
    Hash of
    { 
        used: int ref,
        entries: 'a namedOption array ref
    }

    (* Create an empty table. *)
    fun hash size =
        Hash
        {
            used    = ref 0,
            entries = ref (Array.array (size, Empty))
        }
     
    fun op sub (Hash {entries = ref arr, ...}, name : string) : 'a option =
      (* Searches the table starting from the position given by the hash value. *)
    let
        open Array
        val vecLen = length arr
        (* Loops until it either finds an empty entry - in which case it
           returns NONE, or it finds the name it is looking for.
           There will always be several empty entries because we rehash
           if it gets too full. *)
        fun find i =
        let
            val h = arr sub i
        in
            case h of
                Empty => NONE
            |   Deleted => find ((if i = 0 then vecLen else i) - 1)
            |   Used (n,v) => 
                    if name = n then SOME v else find ((if i = 0 then vecLen else i) - 1)
        end
    in
        find (hashValue vecLen name)
    end

    fun update (Hash {entries as ref currentArray, used}, name, value) =
    let
        open Array

        fun enter a i (entry as (name, _)) =
        (* Loops until it either finds an empty entry - in which case it
           enters the value in there, or it finds the string.
           If it finds a deleted entry it can reuse that but it must check
           that we haven't also got the same string further along. *)
        case a sub i of
            Empty =>  (* Empty.  Add the entry and increment "used". *)
                (update (a, i, Used entry); true)
        |   Deleted => (* Deleted.  Use this entry. *)
            let
                fun checkEntry i =
                    case a sub i of
                        Empty => ()
                    |   Deleted => checkEntry((if i = 0 then length a else i) - 1)
                    |   Used(n, _) =>
                            if n = name
                            then update(a, i, Deleted)
                            else checkEntry((if i = 0 then length a else i) - 1)
            in
                checkEntry i;
                update (a, i, Used entry);
                false
            end
        |   Used (n, _) =>  (* In use.  Overwrite if it's the same name. *)
                if n = name
                then (update (a, i, Used entry); false) (* Same name as previous - overwrite it *)
                else enter a ((if i = 0 then length a else i) - 1) entry;
 
        val () =
            if enter currentArray (hashValue (length currentArray) name) (name, value)
            then used := !used + 1
            else ()

        val currentSize = length currentArray
    in
        (* Do we need to rehash ? *)
        if !used * 5 > currentSize * 4 (* More than 80% full so rehash *)
        then
        let
            val newN     = currentSize * 2 (* Double the size *)
            val newA     = array (newN, Empty)
            val hashNewN = hashValue newN

            fun copyOver(Used(entry as (name, _))) =
                if enter newA (hashNewN name) entry
                then used := !used+1
                else ()
            |   copyOver _ = ()
            
        in
            (* Reset the count to include only non-deleted entries. *)
            used := 0;
            (* Copy into the new array *)
            Array.app copyOver currentArray;
            entries := newA
        end
        else ()
    end

    fun fold f init (Hash { entries = ref e, ...}) =
    let
        fun getEntry(Used(name,alpha), acc) = f (name, alpha, acc)
        |   getEntry(_, acc) = acc
    in
        Array.foldl getEntry init e
    end

    fun delete(Hash {entries = ref arr, ...}, name) =
    let
        open Array
        val vecLen = length arr
        (* Similar to "sub" except that it overwrites the entry if it finds it. *)
        fun find i =
        let
            val h = arr sub i
        in
            case h of
                Empty => () (* Not there *)
            |   Deleted => find ((if i = 0 then vecLen else i) - 1)
            |   Used (n, _) => 
                    if name = n
                    then update(arr, i, Deleted)
                    else find ((if i = 0 then vecLen else i) - 1)
        end
    in
        find (hashValue vecLen name)
    end

end (* local *);

end (* HashArray *);
