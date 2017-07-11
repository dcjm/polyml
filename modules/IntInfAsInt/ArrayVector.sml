(*
    Title:      Rebuild the basis library: Arrays and Vectors
    Copyright   David C.J. Matthews 2016

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

useBasis "MONO_VECTOR";

functor MapMonoVector(
    structure Old:
    sig
        type  vector
        type  elem
        val maxLen : FixedInt.int
        val fromList : elem list -> vector
        val tabulate : (FixedInt.int * (FixedInt.int -> elem)) -> vector
        val length : vector -> FixedInt.int
        val sub : (vector * FixedInt.int) -> elem
        val update: vector * FixedInt.int * elem -> vector
        val concat : vector list -> vector
        val mapi : ((FixedInt.int * elem) -> elem) -> vector -> vector
        val map : (elem -> elem) -> vector -> vector
        val appi : ((FixedInt.int * elem) -> unit) -> vector -> unit
        val app : (elem -> unit) -> vector -> unit
        val foldli : ((FixedInt.int * elem * 'a) -> 'a) -> 'a -> vector -> 'a
        val foldri : ((FixedInt.int * elem * 'a) -> 'a) -> 'a -> vector -> 'a
        val foldl : ((elem * 'a) -> 'a) -> 'a -> vector -> 'a
        val foldr : ((elem * 'a) -> 'a) -> 'a -> vector -> 'a
        val findi: (FixedInt.int * elem -> bool) -> vector -> (FixedInt.int * elem) option
        val find: (elem -> bool) -> vector -> elem option
        val exists: (elem -> bool) -> vector -> bool
        val all: (elem -> bool) -> vector -> bool
        val collate: (elem * elem -> order) -> vector * vector -> order
    end
): MONO_VECTOR =
struct
    open Old
    val maxLen = FixedInt.toLarge maxLen
    val sub = fn (v, i) => sub(v, FixedInt.fromLarge i)
    val length = FixedInt.toLarge o length
    val update = fn (v, i, e) => update(v, FixedInt.fromLarge i, e)
    val foldri = fn f => foldri (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val foldli = fn f => foldli (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val tabulate = fn (n, f) => tabulate(FixedInt.fromInt n, fn q => f(FixedInt.toInt q))
    val appi = fn f => appi(fn (i, e) => f(FixedInt.toLarge i, e))
    val mapi = fn f => mapi(fn (i, e) => f(FixedInt.toLarge i, e))
    val findi = fn f => fn v => 
        Option.map (fn(i, e) => (FixedInt.toLarge i, e)) (findi(fn (i, e) => f(FixedInt.toLarge i, e)) v)
end;

structure CharVector: MONO_VECTOR = MapMonoVector(structure Old = CharVector);
structure BoolVector: MONO_VECTOR = MapMonoVector(structure Old = BoolVector);
structure RealVector: MONO_VECTOR = MapMonoVector(structure Old = RealVector);
structure Word8Vector: MONO_VECTOR = MapMonoVector(structure Old = Word8Vector);


useBasis "VectorSignature.sml";

structure Vector: VECTOR =
struct
    open Vector
    val maxLen = FixedInt.toLarge maxLen
    val sub = fn (v, i) => sub(v, FixedInt.fromLarge i)
    val length = fn l => FixedInt.toLarge(length l)
    val update = fn (v, i, e) => update(v, FixedInt.fromLarge i, e)
    val foldri = fn f => foldri (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val foldli = fn f => foldli (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val tabulate = fn (n, f) => tabulate(FixedInt.fromInt n, fn q => f(FixedInt.toInt q))
    val appi = fn f => appi(fn (i, e) => f(FixedInt.toLarge i, e))
    val mapi = fn f => mapi(fn (i, e) => f(FixedInt.toLarge i, e))
    val findi = fn f => fn v => 
        Option.map (fn(i, e) => (FixedInt.toLarge i, e)) (findi(fn (i, e) => f(FixedInt.toLarge i, e)) v)
end;

useBasis "MONO_ARRAY";

functor MapMonoArray(
    structure Old:
    sig
        eqtype array
        type elem
        type vector
        val maxLen : FixedInt.int
        val array : (FixedInt.int * elem) -> array
        val fromList : elem list -> array
        val vector: array -> vector
        val tabulate : (FixedInt.int * (FixedInt.int -> elem)) -> array
        val length : array -> FixedInt.int
        val sub : (array * FixedInt.int) -> elem
        val update : (array * FixedInt.int * elem) -> unit
        val copy : {src : array, dst : array, di : FixedInt.int} -> unit
        val copyVec : {src : vector, dst : array, di : FixedInt.int} -> unit
        val appi : ((FixedInt.int * elem) -> unit) -> array -> unit
        val app : (elem -> unit) -> array -> unit
        val foldli : ((FixedInt.int * elem * 'b) -> 'b) -> 'b -> array -> 'b
        val foldri : ((FixedInt.int * elem * 'b) -> 'b) -> 'b -> array -> 'b
        val foldl : ((elem * 'b) -> 'b) -> 'b -> array -> 'b
        val foldr : ((elem * 'b) -> 'b) -> 'b -> array -> 'b
        val modifyi : ((FixedInt.int * elem) -> elem) -> array -> unit
        val modify : (elem -> elem) -> array -> unit
        val findi: (FixedInt.int * elem -> bool) -> array -> (FixedInt.int * elem) option
        val find: (elem -> bool) -> array -> elem option
        val exists: (elem -> bool) -> array -> bool
        val all: (elem -> bool) -> array -> bool
        val collate: (elem * elem -> order) -> array * array -> order
    end
) : MONO_ARRAY =
struct
    open Old
    val maxLen = FixedInt.toLarge maxLen
    val array = fn (i, a) => array(FixedInt.fromLarge i, a)
    val sub = fn (v, i) => sub(v, FixedInt.fromLarge i)
    val length = fn l => FixedInt.toLarge(length l)
    val update = fn (v, i, e) => update(v, FixedInt.fromLarge i, e)
    val copy = fn {di, dst, src} => copy {di=FixedInt.fromLarge di, dst=dst, src=src}
    val copyVec = fn {di, dst, src} => copyVec {di=FixedInt.fromLarge di, dst=dst, src=src}
    val foldri = fn f => foldri (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val foldli = fn f => foldli (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val tabulate = fn (n, f) => tabulate(FixedInt.fromInt n, fn q => f(FixedInt.toInt q))
    val appi = fn f => appi(fn (i, e) => f(FixedInt.toLarge i, e))
    val findi = fn f => fn v => 
        Option.map (fn(i, e) => (FixedInt.toLarge i, e)) (findi(fn (i, e) => f(FixedInt.toLarge i, e)) v)
    val modifyi = fn f => modifyi(fn (i, e) => f(FixedInt.toLarge i, e))
end;

structure CharArray: MONO_ARRAY = MapMonoArray(structure Old = CharArray);
structure BoolArray: MONO_ARRAY = MapMonoArray(structure Old = BoolArray);
structure RealArray: MONO_ARRAY = MapMonoArray(structure Old = RealArray);
structure Word8Array: MONO_ARRAY = MapMonoArray(structure Old = Word8Array);

useBasis "ArraySignature.sml";

structure Array: ARRAY =
struct
    open Array
    val maxLen = FixedInt.toLarge maxLen
    val array = fn (i, a) => array(FixedInt.fromLarge i, a)
    val sub = fn (v, i) => sub(v, FixedInt.fromLarge i)
    val length = fn l => FixedInt.toLarge(length l)
    val update = fn (v, i, e) => update(v, FixedInt.fromLarge i, e)
    val copy = fn {di, dst, src} => copy {di=FixedInt.fromLarge di, dst=dst, src=src}
    val copyVec = fn {di, dst, src} => copyVec {di=FixedInt.fromLarge di, dst=dst, src=src}
    val foldri = fn f => foldri (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val foldli = fn f => foldli (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val tabulate = fn (n, f) => tabulate(FixedInt.fromInt n, fn q => f(FixedInt.toInt q))
    val appi = fn f => appi(fn (i, e) => f(FixedInt.toLarge i, e))
    val findi = fn f => fn v => 
        Option.map (fn(i, e) => (FixedInt.toLarge i, e)) (findi(fn (i, e) => f(FixedInt.toLarge i, e)) v)
    val modifyi = fn f => modifyi(fn (i, e) => f(FixedInt.toLarge i, e))
end;

useBasis "MONO_VECTOR_SLICE";

functor MapMonoVectorSlice(
    structure Old:
    sig
        type vector
        type elem
        type slice
    
        val length : slice -> FixedInt.int
        val sub : (slice * FixedInt.int) -> elem
        val full: vector -> slice
        val slice: vector * FixedInt.int * FixedInt.int option -> slice
        val subslice: slice * FixedInt.int * FixedInt.int option -> slice
        val base: slice -> vector * FixedInt.int * FixedInt.int
        val vector: slice -> vector
        val concat: slice list -> vector
        val isEmpty: slice -> bool
        val getItem: slice -> (elem * slice) option
        val appi : ((FixedInt.int * elem) -> unit) -> slice -> unit
        val app : (elem -> unit) -> slice -> unit
        val mapi : ((FixedInt.int * elem) -> elem) -> slice -> vector
        val map : (elem -> elem) -> slice -> vector
        val foldli : ((FixedInt.int * elem * 'a) -> 'a) -> 'a -> slice -> 'a
        val foldri : ((FixedInt.int * elem * 'a) -> 'a) -> 'a -> slice -> 'a
        val foldl : ((elem * 'a) -> 'a) -> 'a -> slice -> 'a
        val foldr : ((elem * 'a) -> 'a) -> 'a -> slice -> 'a
        val findi: (FixedInt.int * elem -> bool) -> slice -> (FixedInt.int * elem) option
        val find: (elem -> bool) -> slice -> elem option
        val exists: (elem -> bool) -> slice -> bool
        val all: (elem -> bool) -> slice -> bool
        val collate: (elem * elem -> order) -> slice * slice -> order
    end
): MONO_VECTOR_SLICE =
struct
    open Old
    val sub = fn (v, i) => sub(v, FixedInt.fromLarge i)
    val length = FixedInt.toLarge o length
    val base = fn s => let val (a, i, j) = base s in (a, FixedInt.toInt i, FixedInt.toInt j) end
    val slice = fn (v, i, l) => slice(v, FixedInt.fromLarge i, Option.map FixedInt.fromInt l)
    val subslice = fn (v, i, l) => subslice(v, FixedInt.fromLarge i, Option.map FixedInt.fromInt l)
    val foldri = fn f => foldri (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val foldli = fn f => foldli (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val appi = fn f => appi(fn (i, e) => f(FixedInt.toLarge i, e))
    val mapi = fn f => mapi(fn (i, e) => f(FixedInt.toLarge i, e))
    val findi = fn f => fn v => 
        Option.map (fn(i, e) => (FixedInt.toLarge i, e)) (findi(fn (i, e) => f(FixedInt.toLarge i, e)) v)
end;

structure CharVectorSlice: MONO_VECTOR_SLICE = MapMonoVectorSlice(structure Old = CharVectorSlice);
structure RealVectorSlice: MONO_VECTOR_SLICE = MapMonoVectorSlice(structure Old = RealVectorSlice);
structure Word8VectorSlice: MONO_VECTOR_SLICE = MapMonoVectorSlice(structure Old = Word8VectorSlice);

useBasis "VectorSliceSignature.sml";

structure VectorSlice: VECTOR_SLICE =
struct
    open VectorSlice
    val sub = fn (v, i) => sub(v, FixedInt.fromLarge i)
    val length = fn l => FixedInt.toLarge(length l)
    val base = fn s => let val (a, i, j) = base s in (a, FixedInt.toInt i, FixedInt.toInt j) end
    val slice = fn (v, i, l) => slice(v, FixedInt.fromLarge i, Option.map FixedInt.fromInt l)
    val subslice = fn (v, i, l) => subslice(v, FixedInt.fromLarge i, Option.map FixedInt.fromInt l)
    val foldri = fn f => foldri (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val foldli = fn f => foldli (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val appi = fn f => appi(fn (i, e) => f(FixedInt.toLarge i, e))
    val mapi = fn f => mapi(fn (i, e) => f(FixedInt.toLarge i, e))
    val findi = fn f => fn v => 
        Option.map (fn(i, e) => (FixedInt.toLarge i, e)) (findi(fn (i, e) => f(FixedInt.toLarge i, e)) v)
end;


useBasis "MONO_ARRAY_SLICE";

functor MapMonoArraySlice(
    structure Old:
    sig
        type elem
        type array
        type slice
        type vector
        type vector_slice
    
        val length : slice -> FixedInt.int
        val sub : (slice * FixedInt.int) -> elem
        val update: slice * FixedInt.int * elem -> unit
        val full: array -> slice
        val slice: array * FixedInt.int * FixedInt.int option -> slice
        val subslice: slice * FixedInt.int * FixedInt.int option -> slice
        val base: slice -> array * FixedInt.int * FixedInt.int
        val vector: slice -> vector
        val copy : {src : slice, dst : array, di : FixedInt.int} -> unit
        val copyVec : {src : vector_slice, dst : array, di : FixedInt.int} -> unit
        val isEmpty: slice -> bool
        val getItem: slice -> (elem * slice) option
        val appi : ((FixedInt.int * elem) -> unit) -> slice -> unit
        val app : (elem -> unit) -> slice -> unit
        val modifyi : (FixedInt.int * elem -> elem) -> slice -> unit
        val modify : (elem -> elem) -> slice -> unit
        val foldli : ((FixedInt.int * elem * 'a) -> 'a) -> 'a -> slice -> 'a
        val foldri : ((FixedInt.int * elem * 'a) -> 'a) -> 'a -> slice -> 'a
        val foldl : ((elem * 'a) -> 'a) -> 'a -> slice -> 'a
        val foldr : ((elem * 'a) -> 'a) -> 'a -> slice -> 'a
        val findi: (FixedInt.int * elem -> bool) -> slice -> (FixedInt.int * elem) option
        val find: (elem -> bool) -> slice -> elem option
        val exists: (elem -> bool) -> slice -> bool
        val all: (elem -> bool) -> slice -> bool
        val collate: (elem * elem -> order) -> slice * slice -> order
    end
): MONO_ARRAY_SLICE =
struct
    open Old
    val sub = fn (v, i) => sub(v, FixedInt.fromLarge i)
    val length = FixedInt.toLarge o length
    val update = fn (v, i, e) => update(v, FixedInt.fromLarge i, e)
    val copy = fn {di, dst, src} => copy {di=FixedInt.fromLarge di, dst=dst, src=src}
    val copyVec = fn {di, dst, src} => copyVec {di=FixedInt.fromLarge di, dst=dst, src=src}
    val base = fn s => let val (a, i, j) = base s in (a, FixedInt.toInt i, FixedInt.toInt j) end
    val slice = fn (v, i, l) => slice(v, FixedInt.fromLarge i, Option.map FixedInt.fromInt l)
    val subslice = fn (v, i, l) => subslice(v, FixedInt.fromLarge i, Option.map FixedInt.fromInt l)
    val foldri = fn f => foldri (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val foldli = fn f => foldli (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val appi = fn f => appi(fn (i, e) => f(FixedInt.toLarge i, e))
    val findi = fn f => fn v => 
        Option.map (fn(i, e) => (FixedInt.toLarge i, e)) (findi(fn (i, e) => f(FixedInt.toLarge i, e)) v)
    val modifyi = fn f => modifyi(fn (i, e) => f(FixedInt.toLarge i, e))
end;

structure CharArraySlice: MONO_ARRAY_SLICE = MapMonoArraySlice(structure Old = CharArraySlice);
structure RealArraySlice: MONO_ARRAY_SLICE = MapMonoArraySlice(structure Old = RealArraySlice);
structure Word8ArraySlice: MONO_ARRAY_SLICE = MapMonoArraySlice(structure Old = Word8ArraySlice);

useBasis "ArraySliceSignature.sml"; (* Depends on VectorSlice. *)

structure ArraySlice: ARRAY_SLICE =
struct
    open ArraySlice
    val sub = fn (v, i) => sub(v, FixedInt.fromLarge i)
    val length = fn l => FixedInt.toLarge(length l)
    val update = fn (v, i, e) => update(v, FixedInt.fromLarge i, e)
    val copy = fn {di, dst, src} => copy {di=FixedInt.fromLarge di, dst=dst, src=src}
    val copyVec = fn {di, dst, src} => copyVec {di=FixedInt.fromLarge di, dst=dst, src=src}
    val base = fn s => let val (a, i, j) = base s in (a, FixedInt.toInt i, FixedInt.toInt j) end
    val slice = fn (v, i, l) => slice(v, FixedInt.fromLarge i, Option.map FixedInt.fromInt l)
    val subslice = fn (v, i, l) => subslice(v, FixedInt.fromLarge i, Option.map FixedInt.fromInt l)
    val foldri = fn f => foldri (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val foldli = fn f => foldli (fn(i, e, a) => f(FixedInt.toLarge i, e, a))
    val appi = fn f => appi(fn (i, e) => f(FixedInt.toLarge i, e))
    val findi = fn f => fn v => 
        Option.map (fn(i, e) => (FixedInt.toLarge i, e)) (findi(fn (i, e) => f(FixedInt.toLarge i, e)) v)
    val modifyi = fn f => modifyi(fn (i, e) => f(FixedInt.toLarge i, e))
end;

(* Rebuild IntVector etc.  They are defined in terms of polymorphic vector, array etc
   and also we want the "Int" to be arbitrary precision. *)
useBasis "IntArray";

useBasis "Text"; (* Rebuild the Text structure that includes Char, String etc. *)

(* Array2 *)
useBasis "Array2Signature.sml";

structure Array2: ARRAY2 =
struct
    open Array2
    type 'a region =
    {
        base : 'a array,
        row : int,
        col : int,
        nrows : int option,
        ncols : int option
    }
    val array = fn(i, j, a) => array(FixedInt.fromLarge i, FixedInt.fromLarge j, a)
    val sub = fn (a, i, j) => sub(a, FixedInt.fromLarge i, FixedInt.fromLarge j)
    val update = fn (a, i, j, v) => update(a, FixedInt.fromLarge i, FixedInt.fromLarge j, v)
    val tabulate =
        fn t =>
            fn (i, j, f) =>
                tabulate t (FixedInt.fromLarge i, FixedInt.fromLarge j, fn (q, r) => f(FixedInt.toLarge q, FixedInt.toLarge r) )
    val row = fn (a, i) => row(a, FixedInt.fromLarge i)
    and column = fn (a, i) => row(a, FixedInt.fromLarge i)
    val dimensions = fn a => let val (x, y) = dimensions a in (FixedInt.toLarge x, FixedInt.toLarge y) end
    val nCols = fn l => FixedInt.toLarge(nCols l)
    val nRows = fn l => FixedInt.toLarge(nRows l)
    
    local
        fun mapRegion{base, row, col, nrows, ncols} =
            {base = base, row = FixedInt.fromLarge row, col = FixedInt.fromLarge col,
             nrows = Option.map FixedInt.fromLarge nrows,
             ncols = Option.map FixedInt.fromLarge ncols }

        fun modifyi' t f r =
            modifyi t
                (fn (i, j, v) => f(FixedInt.toLarge i, FixedInt.toLarge j, v))
                (mapRegion r)
        and foldi' t f b r =
            foldi t
                (fn (i, j, a, b) => f(FixedInt.toLarge i, FixedInt.toLarge j, a, b))
                b (mapRegion r)
        and appi' t f r =
            appi t
                (fn (i, j, v) => f(FixedInt.toLarge i, FixedInt.toLarge j, v))
                (mapRegion r)
        and copy' {src, dst, dst_row, dst_col} =
            copy {src = mapRegion src, dst = dst, dst_row = FixedInt.fromLarge dst_row, dst_col = FixedInt.fromLarge dst_col }
    in
        val modifyi = modifyi'
        and foldi = foldi'
        and appi = appi'
        and copy = copy'
    end
end;

(* Monomorphic two dimensional arrays.  They are defined in terms of Array2 and also
   for IntArray2 we want the base type to be arbitrary precision. *)
useBasis "IntArray2.sml";
