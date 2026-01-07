(* Example code for the updated foreign-function interface.

    Copyright David C.J. Matthews 2015

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

open Foreign;

val mylib = loadLibrary "Foreign";

(* Example of creating a conversion for a datatype. *)
datatype intTree = NullTree | Node of {left: intTree, right: intTree, valu: int};

(* The corresponding C structure is
typedef struct _tree {
    struct _tree *left, *right;
    int nValue;
} *tree;
*)

local
    (* Start with the C structure. *)
    val treeNode = cStruct3(cPointer, cPointer, cInt) 
    val {store=storeStruct, load=loadStruct, ctype = {size = sizeStruct, ...}, ... } =
        breakConversion treeNode
in
(* The following function builds a C data structure from an ML datatype. *)
    fun treeMake NullTree = Memory.null
    |   treeMake(Node{left, right, valu}) =
        let
            val mem = Memory.malloc sizeStruct
        in
            ignore(storeStruct(mem, (treeMake left, treeMake right, valu)));
            mem
        end
    
    fun treeClear a =
        if a = Memory.null
        then ()
        else
        let
            val (left, right, _) = loadStruct a
        in
            treeClear left; treeClear right; Memory.free a
        end

    fun treeStore(addr, tree) =
    let
        val mem = treeMake tree
    in
        Memory.setAddress(addr, 0w0, mem);
        (* The store function returns a function that frees the memory. *)
        fn () => treeClear mem
    end
           
    (* The inverse of treeStore. We don't actually use this in this example. *)
    fun treeGet a = 
        if a = Memory.null
        then NullTree
        else
        let
            val (left, right, valu) = loadStruct a
        in
            Node{left=treeGet left, right=treeGet right, valu = valu }
        end

    fun treeLoad v = treeGet(Memory.getAddress(v, 0w0))

end;

(* Build a conversion out of this. *)
val cTree: intTree conversion =
    makeConversion { load = treeLoad, store = treeStore, ctype = LowLevel.cTypePointer };

val sumTree = buildCall1 (getSymbol mylib "SumTree", cTree, cInt);

val aTree = Node{left=Node{left=NullTree, right=NullTree, valu=4},
               right=Node{
                    left= Node{left=NullTree, right=NullTree, valu=3},
                    right=NullTree, valu=5},
               valu = 7};
sumTree aTree;


(* Example of returning a structure. *)
val returnR2 = buildCall2 (getSymbol mylib "ReturnR2", (cInt, cInt), cStruct2(cInt, cInt));
returnR2(5,6);

val returnR4 = buildCall2 (getSymbol mylib "ReturnR4", (cInt, cInt), cStruct4(cInt, cInt, cFloat, cFloat));
returnR4(5,6);


val returnR6 =
    buildCall2 (getSymbol mylib "ReturnR6", (cInt, cInt), cStruct6(cInt, cInt, cInt, cInt, cInt, cInt));
returnR6(5,6);

(* Example of passing and returning strings. *)
val dupNString = buildCall2 (getSymbol mylib "DupNString", (cInt, cString), cString);

dupNString (4, "hi");

(* Example of a callback function. *)

fun f (i, j) = (PolyML.print(i, j); i+j);

val fAsCFunction = buildClosure2(f, (cInt, cInt), cInt);

val doAdd =
	buildCall2 (getSymbol mylib "MakeCallback", (cInt, cFunction: (int*int->int) closure conversion), cInt);
doAdd(4, fAsCFunction);

fun myCallback(a: int, b: char, c: real, d: real, e: int, f: Memory.voidStar) =
(
    PolyML.print(a, b, c, d, e);
    99.0
);

local
	val myCallbackC =
		buildClosure6(myCallback, (cInt, cChar, cDouble, cFloat, cShort, cPointer), cDouble)

	val MakeCallback2 =
	    buildCall1(getSymbol mylib "MakeCallback2", cFunction, cDouble)
in
	val returnR3 = MakeCallback2 myCallbackC
end;

let
	val f = buildClosure1(fn i => print(Int.toString i), cInt, cVoid)
	val doit = buildCall2(getSymbol mylib "MakeCallback3", (cFunction, cInt), cVoid)
in
	doit(f, 2)
end;

(* Call-by-reference. *)

val r = ref 6;

val updateArg =
    buildCall2 (getSymbol mylib "UpdateArg", (cInt, cStar cInt), cVoid);

updateArg(5, r); (* Adds its first argument to the ref. *)

!r;

val manyArg =
    buildCall10 (getSymbol mylib "ManyArguments", (cInt, cInt, cInt, cInt, cInt, cInt, cInt, cInt, cInt, cInt), cInt);


manyArg(1, 0, 0, 0, 0, 0, 2, 3, 4, 5); (* Result should be 5431. *)

