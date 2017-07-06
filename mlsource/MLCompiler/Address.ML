(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited
 
    Further development copyright David C.J. Matthews 2000-2016

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

signature AddressSig =
sig
    type machineWord
    type address
    type short = Word.word

    val stringOfWord: machineWord -> string
    val rtsFunctionName: int -> string

    val wordEq : machineWord * machineWord -> bool

    val isShort   : machineWord -> bool

    exception Cast of string
    val toMachineWord: 'a    -> machineWord
    val toShort:         machineWord -> Word.word
    val toAddress:       machineWord -> address

    val loadByte:   (address * Word.word) -> Word8.word 
    val loadWord:   (address * Word.word) -> machineWord

    val assignByte: (address * Word.word * Word8.word) -> unit
    val assignWord: (address * Word.word * machineWord)  -> unit

    val allocWordData: (short * Word8.word * machineWord) -> address

    val maxAllocation: word

    val lock:   address -> unit
    val length: address -> short
    val flags:  address -> Word8.word

    val wordSize: int

    val F_words        : Word8.word
    val F_bytes        : Word8.word
    val F_code         : Word8.word
    val F_negative     : Word8.word
    val F_mutable      : Word8.word
    val F_gc           : Word8.word
    val F_noOverwrite  : Word8.word
    val F_weak         : Word8.word
    val F_profile      : Word8.word

    val isWords : address -> bool
    val isBytes : address -> bool
    val isCode  : address -> bool
    val isMutable:address -> bool

    val isIoAddress : address -> bool
end

structure Address :> AddressSig =

struct
    (* These want to be abstract. *)
    local
        structure M:> sig type machineWord and address end =
        struct 
            type machineWord = word (* a legal ML object (tag = 0 or 1) *)
            and address  = word (* a normal pointer (tag = 0) *)
        end
    in
        open M
    end

    (* This is the same as word *)
    type short    = word (* a 31/63-bit int  (tag = 1) *)

    open RuntimeCalls

    (* pointer equality *)
    val wordEq: machineWord * machineWord -> bool = PolyML.pointerEq

  val unsafeCast : 'a -> 'b = RunCall.unsafeCast

  val isShort : machineWord->bool = RunCall.isShort

  (* The following cast is always safe *)
  val toMachineWord : 'a    -> machineWord = unsafeCast

  (* The following casts need checking *)
  exception Cast of string
   
  fun toAddress (w: machineWord) : address =
    if isShort w then raise Cast "toAddress" else unsafeCast w

    fun toShort (w: machineWord) : Word.word =
        if isShort w then unsafeCast w else raise Cast "toShort"

  
  (* Note: 
       assignByte should *not* be used with word-objects
     (we might copy half a pointer into the object,
      then call the garbage collector)
       
       loadWord should *not* be used with byte-objects
     (we might load something that's not a valid ML value,
      then call the garbage collector)
      
    Violating these assertions may corrupt the heap and cause unpredictable
    behaviour.
    
    It's safe to use assignWord with a byte-object or loadByte
    with a word-object but it may not do what you expect.
    
      One difference is that loadWord / assignWord leave the tag bits
      unchanged but loadByte / assignByte strip and replace them.
    
      Another difference is that the offset for the
      "Word" functions is in words, whereas the offset for the
      "Byte" functions is in bytes.
  *)
  
    val loadByte:   address * Word.word -> Word8.word = RunCall.loadByte
    and loadWord:   address * Word.word -> machineWord  = RunCall.loadWord
    and assignByte: address * Word.word * Word8.word -> unit = RunCall.storeByte
    and assignWord: address * Word.word * machineWord -> unit = RunCall.storeWord
    and lock:       address -> unit = RunCall.clearMutableBit
    and wordSize:   int = Word.toInt RunCall.bytesPerWord
    and length:     address -> Word.word = RunCall.memoryCellLength
    and flags:      address -> Word8.word  = Word8.fromLargeWord o Word.toLargeWord o RunCall.memoryCellFlags

    val maxAllocation: word = Compat560.processEnvGeneral (100, ())

    fun allocWordData(len: word, flags: Word8.word, initial: machineWord): address =
        (* Check that the size is within the acceptable range. *)
        if len >= maxAllocation
        then raise Size
        else RunCall.allocateWordMemory(len, Word.fromLargeWord(Word8.toLargeWord flags), initial)

    val F_words        : Word8.word = 0wx00 (* word object - contains pointers and/or tagged values. *)
    val F_bytes        : Word8.word = 0wx01 (* byte object (contains no pointers) *)
    val F_code         : Word8.word = 0wx02 (* code object (mixed bytes and words) *)
    val F_noOverwrite  : Word8.word = 0wx08 (* don't overwrite when loading - mutables only. *)
    val F_negative     : Word8.word = 0wx10 (* sign bit for arbitrary precision ints (byte objects) *)
    val F_profile      : Word8.word = 0wx10 (* object has a profile pointer (word objects) *)
    val F_weak         : Word8.word = 0wx20 (* object contains weak references to option values. *)
    val F_mutable      : Word8.word = 0wx40 (* object is mutable *)
    val F_gc           : Word8.word = 0wx80 (* object is (pointer or depth) tombstone *)

  local
        val doCall: int*address -> bool
             = Compat560.processEnvGeneral
  in
        fun isIoAddress (a: address) : bool = doCall(102, a)
  end
  
  local
    val typeMask : Word8.word = 0wx03

    fun isType (t: Word8.word) (a: address):bool = 
        Word8.andb(flags a, typeMask) = t

  in
    val isWords = isType F_words
    val isBytes = isType F_bytes
    val isCode  = isType F_code

    (* The mutable flag may be used with any of the others. *)
    fun isMutable a = Word8.andb(flags a, F_mutable) = F_mutable
  end

    local
        val rtsTable =
            [
            (POLY_SYS_str_compare,"POLY_SYS_str_compare"),
            (POLY_SYS_teststrgtr,"POLY_SYS_teststrgtr"),
            (POLY_SYS_teststrlss,"POLY_SYS_teststrlss"),
            (POLY_SYS_teststrgeq,"POLY_SYS_teststrgeq"),
            (POLY_SYS_teststrleq,"POLY_SYS_teststrleq"),
            (POLY_SYS_eq_longword,"POLY_SYS_eq_longword"),
            (POLY_SYS_geq_longword,"POLY_SYS_geq_longword"),
            (POLY_SYS_leq_longword,"POLY_SYS_leq_longword"),
            (POLY_SYS_gt_longword,"POLY_SYS_gt_longword"),
            (POLY_SYS_lt_longword,"POLY_SYS_lt_longword"),
            (POLY_SYS_plus_longword,"POLY_SYS_plus_longword"),
            (POLY_SYS_minus_longword,"POLY_SYS_minus_longword"),
            (POLY_SYS_mul_longword,"POLY_SYS_mul_longword"),
            (POLY_SYS_div_longword,"POLY_SYS_div_longword"),
            (POLY_SYS_mod_longword,"POLY_SYS_mod_longword"),
            (POLY_SYS_andb_longword,"POLY_SYS_andb_longword"),
            (POLY_SYS_orb_longword,"POLY_SYS_orb_longword"),
            (POLY_SYS_xorb_longword,"POLY_SYS_xorb_longword"),
            (POLY_SYS_shift_left_longword,"POLY_SYS_shift_left_longword"),
            (POLY_SYS_shift_right_longword,"POLY_SYS_shift_right_longword"),
            (POLY_SYS_shift_right_arith_longword,"POLY_SYS_shift_right_arith_longword"),
            (POLY_SYS_xora,"POLY_SYS_xora"),
            (POLY_SYS_equala,"POLY_SYS_equala"),
            (POLY_SYS_ora,"POLY_SYS_ora"),
            (POLY_SYS_anda,"POLY_SYS_anda"),
            (POLY_SYS_Real_str,"POLY_SYS_Real_str"),
            (POLY_SYS_Real_geq,"POLY_SYS_Real_geq"),
            (POLY_SYS_Real_leq,"POLY_SYS_Real_leq"),
            (POLY_SYS_Real_gtr,"POLY_SYS_Real_gtr"),
            (POLY_SYS_Real_lss,"POLY_SYS_Real_lss"),
            (POLY_SYS_Real_eq,"POLY_SYS_Real_eq"),
            (POLY_SYS_Real_neq,"POLY_SYS_Real_neq"),
            (POLY_SYS_Add_real,"POLY_SYS_Add_real"),
            (POLY_SYS_Sub_real,"POLY_SYS_Sub_real"),
            (POLY_SYS_Mul_real,"POLY_SYS_Mul_real"),
            (POLY_SYS_Div_real,"POLY_SYS_Div_real"),
            (POLY_SYS_Abs_real,"POLY_SYS_Abs_real"),
            (POLY_SYS_Neg_real,"POLY_SYS_Neg_real"),
            (POLY_SYS_conv_real,"POLY_SYS_conv_real"),
            (POLY_SYS_real_to_int,"POLY_SYS_real_to_int"),
            (POLY_SYS_int_to_real,"POLY_SYS_int_to_real"),
            (POLY_SYS_fixed_to_real,"POLY_SYS_fixed_to_real"),
            (POLY_SYS_get_first_long_word,"POLY_SYS_get_first_long_word"),
            (POLY_SYS_bytevec_eq,"POLY_SYS_bytevec_eq"),
            (POLY_SYS_cmem_load_8,"POLY_SYS_cmem_load_8"),
            (POLY_SYS_cmem_load_16, "POLY_SYS_cmem_load_16"),
            (POLY_SYS_cmem_load_32,"POLY_SYS_cmem_load_32"),
            (POLY_SYS_cmem_load_64,"POLY_SYS_cmem_load_64"),
            (POLY_SYS_cmem_load_float,"POLY_SYS_cmem_load_float"),
            (POLY_SYS_cmem_load_double, "POLY_SYS_cmem_load_double"),
            (POLY_SYS_cmem_store_8, "POLY_SYS_cmem_store_8"),
            (POLY_SYS_cmem_store_16, "POLY_SYS_cmem_store_16"),
            (POLY_SYS_cmem_store_32, "POLY_SYS_cmem_store_32"),
            (POLY_SYS_cmem_store_64, "POLY_SYS_cmem_store_64"),
            (POLY_SYS_cmem_store_float, "POLY_SYS_cmem_store_float"),
            (POLY_SYS_cmem_store_double, "POLY_SYS_cmem_store_double"),
            (POLY_SYS_io_operation,"POLY_SYS_io_operation"),
            (POLY_SYS_move_words,"POLY_SYS_move_words"),
            (POLY_SYS_move_bytes,"POLY_SYS_move_bytes"),
            (POLY_SYS_touch_final, "POLY_SYS_touch_final")
            ]

        val rtsNames =
            Vector.tabulate(256,
                fn n => case List.find(fn (rtsNo, _) => rtsNo=n) rtsTable of
                    SOME(_, name) => name | _ => " RTS" ^ Int.toString n)
    in
        val rtsNames = rtsNames
        fun rtsFunctionName n = Vector.sub(rtsNames, n)
     end

    local
        val doCall: int*machineWord -> string
            = Compat560.processEnvGeneral
    in
        fun functionName w = doCall(105, w)
    end

    fun stringOfWord w =
    if isShort w
    then "LIT" ^ Word.toString (unsafeCast w)

    else
    let
        val v = toAddress w
    in
        if isIoAddress v
        then (* RTS call - print the number. *)
            let
                val ioOp : int -> machineWord = RunCall.run_call1 RuntimeCalls.POLY_SYS_io_operation

                fun matchIo n =
                    if n = 256 then raise Fail "Unknown RTS entry"
                    else if wordEq (w, ioOp n)
                    then Vector.sub(rtsNames, n)
                    else matchIo (n+1)
            in
                matchIo 0
            end

        else if isCode v
        then "CODE \"" ^ functionName w ^ "\""
        
        else if isBytes v
        then
        let
            val length = Int.min(Word.toInt(length v) * wordSize, 16)
            val data = Word8Vector.tabulate(length, fn n => loadByte(v, Word.fromInt n))
        in
            "BYTE data" ^ String.toString(Byte.bytesToString data)
        end
    
        else if isWords(toAddress w) andalso Word.toInt(length(toAddress w)) >= 1
        then (* If it's the closure of a function try to print that. *)
            let
                val firstWord = loadWord(toAddress w, 0w0)
            in
                if not (isShort firstWord) andalso isCode(toAddress firstWord)
                then "FUN \"" ^ functionName firstWord ^ "\"" (* Get the function name. *)
                else "LIT <long word data>"
            end
            
        else "LIT <long word data>"
    end

end;

(* Add a print function for machineWord.  This is really only for
   the debugger but prevents addresses being printed as Word.word values. *)
local
    open PolyML Address
    fun printMachineWord _ _ w = PrettyString(stringOfWord w)
in
    val () = addPrettyPrinter printMachineWord
end;

