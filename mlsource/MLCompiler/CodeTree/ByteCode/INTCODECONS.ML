(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited
    
    Further development Copyright (c) 2015, 2016 David C.J. Matthews


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

(*
    Title:      Code Generator Routines.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

(*
 This module contains the code vector and operations to insert code into
  it. Each procedure is compiled into a separate segment. Initially it is
  compiled into a fixed size segment, and then copied into a segment of the
  correct size at the end.
*)

functor INTCODECONS (
structure DEBUG: DEBUGSIG

(*****************************************************************************)
(*                  PRETTY for compilerOutTag                                *)
(*****************************************************************************)
structure PRETTY: PRETTYSIG


(*****************************************************************************)
(*                  MISC                                                     *)
(*****************************************************************************)
structure MISC :
  sig
    exception InternalError of string
  end

) : INTCODECONSSIG =

let

(*****************************************************************************)
(*                  CODESEG                                                  *)
(*****************************************************************************)
structure CODESEG :
sig
    type machineWord
    type address
    type cseg

    val csegMake:          word  -> cseg
    val csegConvertToCode: cseg -> unit
    val csegGet:           cseg * word -> Word8.word
    val csegSet:           cseg * word * Word8.word -> unit
    val csegGetWord:       cseg * word -> machineWord
    val csegPutWord:       cseg * word * machineWord -> unit
    val csegCopySeg:       cseg * cseg * word * word -> unit
    val csegAddr:          cseg -> address
    val csegLockAndGetExecutable: cseg -> address
    val csegPutConstant:   cseg * word * machineWord * word -> unit
end = CODE_ARRAY

(*****************************************************************************)
(*                  ADDRESS                                                  *)
(*****************************************************************************)
structure ADDRESS : AddressSig = Address

in

(*****************************************************************************)
(*                  CODECONS functor body                                    *)
(*****************************************************************************)
struct
  open CODESEG;
  open DEBUG;
  open ADDRESS;
  open MISC;

  (*
      The original way of dealing with constants was to store the offset (in bytes)
      of the constant from the end of the instruction.  That has a problem when
      the database is ported to a different word-length machine because while the
      byte count to the end of the interpreted code does not change the marker word
      and other constants will all have a different length.  I've changed it to use
      new instructions which take an extra argument which is the number of the
      constant.  The byte offset is then always the number of bytes to the end of
      the code.  DCJM 25/9/00.
  *)
  val usePortableConstantOffset = false;
  
  (* The "enterInt" instruction is only used when porting to a new architecture.  Since
     we only use the interpreted version to support non-X86 architectures we don't need
     to generate them. *)
  val generateEnterInt = false

  (* To make the code portable these are both functions.  Note that
     similar changes are needed in Code_seg.ML and in the basis library.
     DCJM 28/9/00. *)
  (* Unfortunately the code-generator always evaluates these at compile time
     if it can.  We need to use a ref to force it not to.  It's too
     complicated making this completely word-length independent but it's
     useful to have it byte-order independent. *)
  local
      val bigEndian = ref RuntimeCalls.POLY_SYS_is_big_endian
  in
      fun littleEndian () : bool = not (RunCall.run_call0 (! bigEndian) ())
  end;

  val wordLength = RunCall.bytesPerWord
 
  val MAXINTARGS = (* 31 *) 126;

  fun forLoop f i n = if i > n then () else (f i; forLoop f (i + 1) n);

  fun applyList (_, [])   = ()
    | applyList (f, h::t) =
    let
      val () = f h;
    in
      applyList (f, t)
    end;
  

(*****************************************************************************)
(*                  Abstype for instruction addresses                        *)
(*****************************************************************************)
  infix 6 addrPlus addrMinus;
  infix 4 (* ? *) addrLt;
  
    (* All indexes into the code vector have type "addrs" *)
  (* This should be an abstype, but it's exported as an eqtype *)
  datatype addrs = Addr of int
  
  (* + is defined to add an integer to an address *)
  fun (Addr a) addrPlus b = Addr (a + b);
    
  (* The difference between two addresses is an integer *)
  fun (Addr a) addrMinus (Addr b) = a - b; 
  
  fun (Addr a) addrLt (Addr b) = a < b; 
  
  fun mkAddr n = Addr n;    (* addr.up   *)
  
  fun getAddr (Addr a) = a; (* addr.down *)
  
  val addrZero = mkAddr 0;
  val addrLast = mkAddr (Word.toInt (Word.<<(0w1, 0w29)) - 1); (* A big number. *)

(*****************************************************************************)
(*                  Opcodes                                                  *)
(*****************************************************************************)
      
   (* These instructions are only needed during porting between
       interpreted and machine-code versions. The first should be the
       interrupt or break-point instruction of the host machine-code and
       causes the machine to enter the interpreter. It is ignored by the
       interpreter except immediately after the interpreter has been
       entered when result registers may be pushed depending on the
       argument. The second instruction should be a no-op in the machine
       code instruction set and has the reverse effect. It is never
       generated by this code-generator but it is needed in machine-code
       code-generators. 
       Note: indirect forms of jumps are assumed to have the opcode 4
       more than the corresponding direct form.
  *)
  local
    (* Not an abstype, because we we require the equality attribute *)
    datatype opcode = Opcode of int;
  in
    type opcode = opcode;
    fun opcode_down (Opcode n) : int = n;
    fun opcode_up (n : int) : opcode = Opcode n;
    
    val opcode_enterInt          = Opcode 0;
    (* Opcode 1 reserved for Interpreter's internal use *)
    val opcode_jump              = Opcode 2;
    val opcode_jumpFalse         = Opcode 3;
    val opcode_containerW        = Opcode 4; (* Added DCJM 5/10/05. *)
    val opcode_delHandler        = Opcode 5;
    val opcode_jumpIFalse        = Opcode 7;
    (*val opcode_set_containerW    = Opcode 8;*) (* Added DCJM 5/10/05. *)
    val opcode_delHandlerI       = Opcode 9;
    val opcode_caseSwitch        = Opcode 10;
    val opcode_callClosure       = Opcode 12;
    val opcode_returnW           = Opcode 13;
    val opcode_pad               = Opcode 14;
    val opcode_jumpI             = Opcode 15 (* Moved from 6 because this is unsigned *)
    val opcode_raiseEx           = Opcode 16;
    val opcode_getStoreW         = Opcode 17;
    val opcode_nonLocal          = Opcode 18;
    val opcode_localW            = Opcode 19;
    val opcode_indirectW         = Opcode 20;
    val opcode_moveToVecW        = Opcode 21;
    val opcode_setStackValW      = Opcode 23;
    val opcode_resetW            = Opcode 24;
    val opcode_resetR_w          = Opcode 25;
    val opcode_constAddr         = Opcode 26;
    val opcode_constIntW         = Opcode 27;
    val opcode_jumpBack8         = Opcode 30;
    val opcode_returnB           = Opcode 31;
    val opcode_jumpBack16        = Opcode 32
    val opcode_getStoreB         = Opcode 33;
    val opcode_localB            = Opcode 34;
    val opcode_indirectB         = Opcode 35;
    val opcode_moveToVecB        = Opcode 36;
    val opcode_setStackValB      = Opcode 37;
    val opcode_resetB            = Opcode 38;
    val opcode_resetRB           = Opcode 39;
    val opcode_constIntB         = Opcode 40;
    val opcode_local_0           = Opcode 41;
    val opcode_local_1           = Opcode 42;
    val opcode_local_2           = Opcode 43;
    val opcode_local_3           = Opcode 44;
    val opcode_local_4           = Opcode 45;
    val opcode_local_5           = Opcode 46;
    val opcode_local_6           = Opcode 47;
    val opcode_local_7           = Opcode 48;
    val opcode_local_8           = Opcode 49;
    val opcode_local_9           = Opcode 50;
    val opcode_local_10          = Opcode 51;
    val opcode_local_11          = Opcode 52;
    val opcode_indirect_0        = Opcode 53;
    val opcode_indirect_1        = Opcode 54;
    val opcode_indirect_2        = Opcode 55;
    val opcode_indirect_3        = Opcode 56;
    val opcode_indirect_4        = Opcode 57;
    val opcode_indirect_5        = Opcode 58;
    val opcode_const_0           = Opcode 59;
    val opcode_const_1           = Opcode 60;
    val opcode_const_2           = Opcode 61;
    val opcode_const_3           = Opcode 62;
    val opcode_const_4           = Opcode 63;
    val opcode_const_10          = Opcode 64;
    val opcode_return_0          = Opcode 65;
    val opcode_return_1          = Opcode 66;
    val opcode_return_2          = Opcode 67;
    val opcode_return_3          = Opcode 68;
    val opcode_moveToVec_0       = Opcode 69;
    val opcode_moveToVec_1       = Opcode 70;
    val opcode_moveToVec_2       = Opcode 71;
    val opcode_moveToVec_3       = Opcode 72;
    val opcode_moveToVec_4       = Opcode 73;
    val opcode_moveToVec_5       = Opcode 74;
    val opcode_moveToVec_6       = Opcode 75;
    val opcode_moveToVec_7       = Opcode 76;
    val opcode_constAddrX_b      = Opcode 77; (* Added DCJM 25/9/00. *)
    val opcode_constAddrX_w      = Opcode 78; (* Added DCJM 25/9/00. *)
    val opcode_reset_1           = Opcode 80;
    val opcode_reset_2           = Opcode 81;
    val opcode_getStore_2        = Opcode 82;
    val opcode_getStore_3        = Opcode 83;
    val opcode_getStore_4        = Opcode 84;
    val opcode_tuple_containerW  = Opcode 85; (* Added DCJM 5/10/05. *)
    val opcode_nonLocalL_1       = Opcode 86;
    val opcode_nonLocalL_2       = Opcode 87;
    val opcode_nonLocalL_3       = Opcode 88;
    val opcode_resetR_1          = Opcode 100;
    val opcode_resetR_2          = Opcode 101;
    val opcode_resetR_3          = Opcode 102;
    val opcode_tupleW            = Opcode 103;
    val opcode_tupleB            = Opcode 104;
    val opcode_tuple_2           = Opcode 105;
    val opcode_tuple_3           = Opcode 106;
    val opcode_tuple_4           = Opcode 107;
    val opcode_lock              = Opcode 108;
    val opcode_ldexc             = Opcode 109;
    val opcode_pushHandler       = Opcode 120;
    val opcode_tailbb            = Opcode 123;
    val opcode_tail              = Opcode 124;
    val opcode_tail3b            = Opcode 125;
    val opcode_tail4b            = Opcode 126;
    val opcode_tail3_2           = Opcode 127;
    val opcode_tail3_3           = Opcode 128;
    (* These have been moved since the old versions were for
       the old exception mechanism. *)
    val opcode_setHandler        = Opcode 129
    val opcode_setHandlerI       = Opcode 130

    val opcode_callFastRTS0      = Opcode 131
    and opcode_callFastRTS1      = Opcode 132
    and opcode_callFastRTS2      = Opcode 133
    and opcode_callFastRTS3      = Opcode 134
    and opcode_callFastRTS4      = Opcode 135
    and opcode_callFastRTS5      = Opcode 136

    val opcode_callFullRTS0      = Opcode 0x89
    and opcode_callFullRTS1      = Opcode 0x8a
    and opcode_callFullRTS2      = Opcode 0x8b
    and opcode_callFullRTS3      = Opcode 0x8c
    and opcode_callFullRTS4      = Opcode 0x8d
    and opcode_callFullRTS5      = Opcode 0x8e

    val opcode_callFastRTSFtoF   = Opcode 0x8f
    and opcode_callFastRTSGtoF   = Opcode 0x90
    
    val opcode_notBoolean        = Opcode 0x91
    and opcode_isTagged          = Opcode 0x92
    and opcode_cellLength        = Opcode 0x93
    and opcode_cellFlags         = Opcode 0x94
    and opcode_clearMutable      = Opcode 0x95
    and opcode_stringLength      = Opcode 0x96
    and opcode_atomicIncr        = Opcode 0x97
    and opcode_atomicDecr        = Opcode 0x98
    and opcode_atomicReset       = Opcode 0x99
    and opcode_longWToTagged     = Opcode 0x9a
    and opcode_signedToLongW     = Opcode 0x9b
    and opcode_unsignedToLongW   = Opcode 0x9c
    and opcode_realAbs           = Opcode 0x9d
    and opcode_realNeg           = Opcode 0x9e
    and opcode_floatFixedInt     = Opcode 0x9f
    
    val opcode_equalWord         = Opcode 0xa0
    and opcode_notequalWord      = Opcode 0xa1
    and opcode_lessSigned        = Opcode 0xa2
    and opcode_lessUnsigned      = Opcode 0xa3
    and opcode_lessEqSigned      = Opcode 0xa4
    and opcode_lessEqUnsigned    = Opcode 0xa5
    and opcode_greaterSigned     = Opcode 0xa6
    and opcode_greaterUnsigned   = Opcode 0xa7
    and opcode_greaterEqSigned   = Opcode 0xa8
    and opcode_greaterEqUnsigned = Opcode 0xa9

    val opcode_fixedAdd          = Opcode 0xaa
    val opcode_fixedSub          = Opcode 0xab
    val opcode_fixedMult         = Opcode 0xac
    val opcode_fixedQuot         = Opcode 0xad
    val opcode_fixedRem          = Opcode 0xae
    val opcode_fixedDiv          = Opcode 0xaf
    val opcode_fixedMod          = Opcode 0xb0
    val opcode_wordAdd           = Opcode 0xb1
    val opcode_wordSub           = Opcode 0xb2
    val opcode_wordMult          = Opcode 0xb3
    val opcode_wordDiv           = Opcode 0xb4
    val opcode_wordMod           = Opcode 0xb5
    val opcode_setStringLength   = Opcode 0xb6
    val opcode_wordAnd           = Opcode 0xb7
    val opcode_wordOr            = Opcode 0xb8
    val opcode_wordXor           = Opcode 0xb9
    val opcode_wordShiftLeft     = Opcode 0xba
    val opcode_wordShiftRLog     = Opcode 0xbb
    val opcode_wordShiftRArith   = Opcode 0xbc
    val opcode_allocByteMem      = Opcode 0xbd
    val opcode_lgWordEqual       = Opcode 0xbe
    val opcode_lgWordNotequal    = Opcode 0xbf
    val opcode_lgWordLess        = Opcode 0xc0
    val opcode_lgWordLessEq      = Opcode 0xc1
    val opcode_lgWordGreater     = Opcode 0xc2
    val opcode_lgWordGreaterEq   = Opcode 0xc3
    val opcode_lgWordAdd         = Opcode 0xc4
    val opcode_lgWordSub         = Opcode 0xc5
    val opcode_lgWordMult        = Opcode 0xc6
    val opcode_lgWordDiv         = Opcode 0xc7
    val opcode_lgWordMod         = Opcode 0xc8
    val opcode_lgWordAnd         = Opcode 0xc9
    val opcode_lgWordOr          = Opcode 0xca
    val opcode_lgWordXor         = Opcode 0xcb
    val opcode_lgWordShiftLeft   = Opcode 0xcc
    val opcode_lgWordShiftRLog   = Opcode 0xcd
    val opcode_lgWordShiftRArith = Opcode 0xce
    val opcode_realEqual         = Opcode 0xcf
    val opcode_realNotequal      = Opcode 0xd0
    val opcode_realLess          = Opcode 0xd1
    val opcode_realLessEq        = Opcode 0xd2
    val opcode_realGreater       = Opcode 0xd3
    val opcode_realGreaterEq     = Opcode 0xd4
    val opcode_realAdd           = Opcode 0xd5
    val opcode_realSub           = Opcode 0xd6
    val opcode_realMult          = Opcode 0xd7
    val opcode_realDiv           = Opcode 0xd8
    val opcode_getThreadId       = Opcode 0xd9
    val opcode_allocWordMemory   = Opcode 0xda
    val opcode_loadMLWord        = Opcode 0xdb
    val opcode_loadMLByte        = Opcode 0xdc
    val opcode_loadC8            = Opcode 0xdd
    val opcode_loadC16           = Opcode 0xde
    val opcode_loadC32           = Opcode 0xdf
    val opcode_loadC64           = Opcode 0xe0
    val opcode_loadCFloat        = Opcode 0xe1
    val opcode_loadCDouble       = Opcode 0xe2
    val opcode_storeMLByte       = Opcode 0xe3
    val opcode_storeC8           = Opcode 0xe4
    val opcode_storeC16          = Opcode 0xe5
    val opcode_storeC32          = Opcode 0xe6
    val opcode_storeC64          = Opcode 0xe7
    val opcode_storeCFloat       = Opcode 0xe8
    val opcode_storeCDouble      = Opcode 0xe9
    val opcode_blockMoveWord     = Opcode 0xea
    val opcode_blockMoveByte     = Opcode 0xeb
    val opcode_blockEqualByte    = Opcode 0xec
    val opcode_blockCompareByte  = Opcode 0xed
 
    (* val opcode_last              = opcode_ioVec_225; *)

    local
      val repArray : string Array.array = 
        Array.tabulate (256, fn (i : int) => "<UNKNOWN " ^ Int.toString i ^ ">");
      
      fun repUpdate (Opcode n, s) = Array.update (repArray, n, s);

      val () = repUpdate(opcode_enterInt,     "enterInt");
      val () = repUpdate(opcode_jump,         "jump");
      val () = repUpdate(opcode_jumpFalse,    "jumpFalse");
      val () = repUpdate(opcode_delHandler,   "delHandler");
      val () = repUpdate(opcode_jumpI,        "jumpI");
      val () = repUpdate(opcode_jumpIFalse,   "jumpIFalse");
      val () = repUpdate(opcode_delHandlerI,  "delHandlerI");
      val () = repUpdate(opcode_caseSwitch,   "caseSwitch");
      val () = repUpdate(opcode_callClosure,  "callClosure");
      val () = repUpdate(opcode_returnW,      "returnW");
      val () = repUpdate(opcode_pad,          "pad");
      val () = repUpdate(opcode_raiseEx,      "raiseEx");
      val () = repUpdate(opcode_getStoreW,    "getStoreW");
      val () = repUpdate(opcode_nonLocal,     "nonLocal");
      val () = repUpdate(opcode_localW,       "localW");
      val () = repUpdate(opcode_indirectW,    "indirectW");
      val () = repUpdate(opcode_moveToVecW,   "moveToVecW");
      val () = repUpdate(opcode_setStackValW, "setStackValW");
      val () = repUpdate(opcode_resetW,        "resetW");
      val () = repUpdate(opcode_resetR_w,      "resetR_w");
      val () = repUpdate(opcode_constAddr,     "constAddr");
      val () = repUpdate(opcode_constAddrX_b,  "constAddrX_b");
      val () = repUpdate(opcode_constAddrX_w,  "constAddrX_w");
      val () = repUpdate(opcode_constIntW,     "constIntW");
      val () = repUpdate(opcode_jumpBack8,     "jumpBack8");
      val () = repUpdate(opcode_returnB,       "returnB");
      val () = repUpdate(opcode_jumpBack16,    "jumpBack16");
      val () = repUpdate(opcode_getStoreB,     "getStoreB");
      val () = repUpdate(opcode_localB,        "localB");
      val () = repUpdate(opcode_indirectB,     "indirectB");
      val () = repUpdate(opcode_moveToVecB,    "moveToVecB");
      val () = repUpdate(opcode_setStackValB,  "setStackValB");
      val () = repUpdate(opcode_resetB,        "resetB");
      val () = repUpdate(opcode_resetRB,       "resetRB");
      val () = repUpdate(opcode_constIntB,     "constIntB");
      val () = repUpdate(opcode_local_0,       "local_0");
      val () = repUpdate(opcode_local_1,       "local_1");
      val () = repUpdate(opcode_local_2,       "local_2");
      val () = repUpdate(opcode_local_3,       "local_3");
      val () = repUpdate(opcode_local_4,       "local_4");
      val () = repUpdate(opcode_local_5,       "local_5");
      val () = repUpdate(opcode_local_6,       "local_6");
      val () = repUpdate(opcode_local_7,       "local_7");
      val () = repUpdate(opcode_local_8,       "local_8");
      val () = repUpdate(opcode_local_9,       "local_9");
      val () = repUpdate(opcode_local_10,      "local_10");
      val () = repUpdate(opcode_local_11,      "local_11");
      val () = repUpdate(opcode_indirect_0,    "indirect_0");
      val () = repUpdate(opcode_indirect_1,    "indirect_1");
      val () = repUpdate(opcode_indirect_2,    "indirect_2");
      val () = repUpdate(opcode_indirect_3,    "indirect_3");
      val () = repUpdate(opcode_indirect_4,    "indirect_4");
      val () = repUpdate(opcode_indirect_5,    "indirect_5");
      val () = repUpdate(opcode_const_0,       "const_0");
      val () = repUpdate(opcode_const_1,       "const_1");
      val () = repUpdate(opcode_const_2,       "const_2");
      val () = repUpdate(opcode_const_3,       "const_3");
      val () = repUpdate(opcode_const_4,       "const_4");
      val () = repUpdate(opcode_const_10,      "const_10");
      val () = repUpdate(opcode_return_0,      "return_0");
      val () = repUpdate(opcode_return_1,      "return_1");
      val () = repUpdate(opcode_return_2,      "return_2");
      val () = repUpdate(opcode_return_3,      "return_3");
      val () = repUpdate(opcode_moveToVec_0,   "moveToVec_0");
      val () = repUpdate(opcode_moveToVec_1,   "moveToVec_1");
      val () = repUpdate(opcode_moveToVec_2,   "moveToVec_2");
      val () = repUpdate(opcode_moveToVec_3,   "moveToVec_3");
      val () = repUpdate(opcode_moveToVec_4,   "moveToVec_4");
      val () = repUpdate(opcode_moveToVec_5,   "moveToVec_5");
      val () = repUpdate(opcode_moveToVec_6,   "moveToVec_6");
      val () = repUpdate(opcode_moveToVec_7,   "moveToVec_7");
      val () = repUpdate(opcode_reset_1,       "reset_1");
      val () = repUpdate(opcode_reset_2,       "reset_2");
      val () = repUpdate(opcode_getStore_2,    "getStore_2");
      val () = repUpdate(opcode_getStore_3,    "getStore_3");
      val () = repUpdate(opcode_getStore_4,    "getStore_4");
      val () = repUpdate(opcode_nonLocalL_1,   "nonLocalL_1");
      val () = repUpdate(opcode_nonLocalL_2,   "nonLocalL_2");
      val () = repUpdate(opcode_nonLocalL_3,   "nonLocalL_3");
      val () = repUpdate(opcode_resetR_1,      "resetR_1");
      val () = repUpdate(opcode_resetR_2,      "resetR_2");
      val () = repUpdate(opcode_resetR_3,      "resetR_3");
      val () = repUpdate(opcode_tupleW,        "tupleW");
      val () = repUpdate(opcode_tupleB,        "tupleB");
      val () = repUpdate(opcode_tuple_2,       "tuple_2");
      val () = repUpdate(opcode_tuple_3,       "tuple_3");
      val () = repUpdate(opcode_tuple_4,       "tuple_4");
      val () = repUpdate(opcode_lock,          "lock");
      val () = repUpdate(opcode_ldexc,         "ldexc");
      val () = repUpdate(opcode_setHandler,    "setHandler");
      val () = repUpdate(opcode_pushHandler,   "pushHandler");
      val () = repUpdate(opcode_setHandlerI,   "setHandlerI");
      val () = repUpdate(opcode_tailbb,        "tailbb");
      val () = repUpdate(opcode_tail,          "tail");
      val () = repUpdate(opcode_tail3b,        "tail3b");
      val () = repUpdate(opcode_tail4b,        "tail4b");
      val () = repUpdate(opcode_tail3_2,       "tail3_2");
      val () = repUpdate(opcode_tail3_3,       "tail3_3");
      val () = repUpdate(opcode_callFastRTS0,  "callFastRTS0")
      val () = repUpdate(opcode_callFastRTS1,  "callFastRTS1")
      val () = repUpdate(opcode_callFastRTS2,  "callFastRTS2")
      val () = repUpdate(opcode_callFastRTS3,  "callFastRTS3")
      val () = repUpdate(opcode_callFastRTS4,  "callFastRTS4")
      val () = repUpdate(opcode_callFastRTS5,  "callFastRTS5")
      val () = repUpdate(opcode_callFullRTS0,  "callFullRTS0")
      val () = repUpdate(opcode_callFullRTS1,  "callFullRTS1")
      val () = repUpdate(opcode_callFullRTS2,  "callFullRTS2")
      val () = repUpdate(opcode_callFullRTS3,  "callFullRTS3")
      val () = repUpdate(opcode_callFullRTS4,  "callFullRTS4")
      val () = repUpdate(opcode_callFullRTS5,  "callFullRTS5")
      val () = repUpdate(opcode_callFastRTSFtoF,  "callFullRTSFtoF")
      val () = repUpdate(opcode_callFastRTSGtoF,  "callFullRTSGtoF")
        val () = repUpdate(opcode_notBoolean, "notBoolean")
        val () = repUpdate(opcode_isTagged, "isTagged")
        val () = repUpdate(opcode_cellLength, "cellLength")
        val () = repUpdate(opcode_cellFlags, "cellFlags")
        val () = repUpdate(opcode_clearMutable, "clearMutable")
        val () = repUpdate(opcode_stringLength, "stringLength")
        val () = repUpdate(opcode_atomicIncr, "atomicIncr")
        val () = repUpdate(opcode_atomicDecr, "atomicDecr")
        val () = repUpdate(opcode_atomicReset, "atomicReset")
        val () = repUpdate(opcode_longWToTagged, "longWToTagged")
        val () = repUpdate(opcode_signedToLongW, "signedToLongW")
        val () = repUpdate(opcode_unsignedToLongW, "unsignedToLongW")
        val () = repUpdate(opcode_realAbs, "realAbs")
        val () = repUpdate(opcode_realNeg, "realNeg")
        val () = repUpdate(opcode_floatFixedInt, "floatFixedInt")

        val () = repUpdate(opcode_equalWord, "equalWord")
        val () = repUpdate(opcode_notequalWord, "notequalWord")
        val () = repUpdate(opcode_lessSigned, "lessSigned")
        val () = repUpdate(opcode_lessUnsigned, "lessUnsigned")
        val () = repUpdate(opcode_lessEqSigned, "lessEqSigned")
        val () = repUpdate(opcode_lessEqUnsigned, "lessEqUnsigned")
        val () = repUpdate(opcode_greaterSigned, "greaterSigned")
        val () = repUpdate(opcode_greaterUnsigned, "greaterUnsigned")
        val () = repUpdate(opcode_greaterEqSigned, "greaterEqSigned")
        val () = repUpdate(opcode_greaterEqUnsigned, "greaterEqUnsigned")

        val () = repUpdate(opcode_fixedAdd, "fixedAdd")
        val () = repUpdate(opcode_fixedSub, "fixedSub")
        val () = repUpdate(opcode_fixedMult, "fixedMult")
        val () = repUpdate(opcode_fixedQuot, "fixedQuot")
        val () = repUpdate(opcode_fixedRem, "fixedRem")
        val () = repUpdate(opcode_fixedDiv, "fixedDiv")
        val () = repUpdate(opcode_fixedMod, "fixedMod")
        val () = repUpdate(opcode_wordAdd, "wordAdd")
        val () = repUpdate(opcode_wordSub, "wordSub")
        val () = repUpdate(opcode_wordMult, "wordMult")
        val () = repUpdate(opcode_wordDiv, "wordDiv")
        val () = repUpdate(opcode_wordMod, "wordMod")
        val () = repUpdate(opcode_setStringLength, "setStringLength")
        val () = repUpdate(opcode_wordAnd, "wordAnd")
        val () = repUpdate(opcode_wordOr, "wordOr")
        val () = repUpdate(opcode_wordXor, "wordXor")
        val () = repUpdate(opcode_wordShiftLeft, "wordShiftLeft")
        val () = repUpdate(opcode_wordShiftRLog, "wordShiftRLog")
        val () = repUpdate(opcode_wordShiftRArith, "wordShiftRArith")
        val () = repUpdate(opcode_allocByteMem, "allocByteMem")
        val () = repUpdate(opcode_lgWordEqual, "lgWordEqual")
        val () = repUpdate(opcode_lgWordNotequal, "lgWordNotequal")
        val () = repUpdate(opcode_lgWordLess, "lgWordLess")
        val () = repUpdate(opcode_lgWordLessEq, "lgWordLessEq")
        val () = repUpdate(opcode_lgWordGreater, "lgWordGreater")
        val () = repUpdate(opcode_lgWordGreaterEq, "lgWordGreaterEq")
        val () = repUpdate(opcode_lgWordAdd, "lgWordAdd")
        val () = repUpdate(opcode_lgWordSub, "lgWordSub")
        val () = repUpdate(opcode_lgWordMult, "lgWordMult")
        val () = repUpdate(opcode_lgWordDiv, "lgWordDiv")
        val () = repUpdate(opcode_lgWordMod, "lgWordMod")
        val () = repUpdate(opcode_lgWordAnd, "lgWordAnd")
        val () = repUpdate(opcode_lgWordOr, "lgWordOr")
        val () = repUpdate(opcode_lgWordXor, "lgWordXor")
        val () = repUpdate(opcode_lgWordShiftLeft, "lgWordShiftLeft")
        val () = repUpdate(opcode_lgWordShiftRLog, "lgWordShiftRLog")
        val () = repUpdate(opcode_lgWordShiftRArith, "lgWordShiftRArith")
        val () = repUpdate(opcode_realEqual, "realEqual")
        val () = repUpdate(opcode_realNotequal, "realNotequal")
        val () = repUpdate(opcode_realLess, "realLess")
        val () = repUpdate(opcode_realLessEq, "realLessEq")
        val () = repUpdate(opcode_realGreater, "realGreater")
        val () = repUpdate(opcode_realGreaterEq, "realGreaterEq")
        val () = repUpdate(opcode_realAdd, "realAdd")
        val () = repUpdate(opcode_realSub, "realSub")
        val () = repUpdate(opcode_realMult, "realMult")
        val () = repUpdate(opcode_realDiv, "realDiv")
        val () = repUpdate(opcode_getThreadId, "getThreadId")
        val () = repUpdate(opcode_allocWordMemory, "allocWordMemory")
        val () = repUpdate(opcode_loadMLWord, "loadMLWord")
        val () = repUpdate(opcode_loadMLByte, "loadMLByte")
        val () = repUpdate(opcode_loadC8, "loadC8")
        val () = repUpdate(opcode_loadC16, "loadC16")
        val () = repUpdate(opcode_loadC32, "loadC32")
        val () = repUpdate(opcode_loadC64, "loadC64")
        val () = repUpdate(opcode_loadCFloat, "loadCFloat")
        val () = repUpdate(opcode_loadCDouble, "loadCDouble")
        val () = repUpdate(opcode_storeMLByte, "storeMLByte")
        val () = repUpdate(opcode_storeC8, "storeC8")
        val () = repUpdate(opcode_storeC16, "storeC16")
        val () = repUpdate(opcode_storeC32, "storeC32")
        val () = repUpdate(opcode_storeC64, "storeC64")
        val () = repUpdate(opcode_storeCFloat, "storeCFloat")
        val () = repUpdate(opcode_storeCDouble, "storeCDouble")
        val () = repUpdate(opcode_blockMoveWord, "blockMoveWord")
        val () = repUpdate(opcode_blockMoveByte, "blockMoveByte")
        val () = repUpdate(opcode_blockEqualByte, "blockEqualByte")
        val () = repUpdate(opcode_blockCompareByte, "blockCompareByte")
    in
      fun repr (Opcode n) : string = Array.sub (repArray, n);
    end;


    local
      val sizeArray : int Array.array = Array.array (256, 1);

      fun sizeUpdate (Opcode n, s) = Array.update (sizeArray, n, s);
      
      val () = sizeUpdate(opcode_enterInt    , 2); (* Restored DCJM 22/9/00. *)
(*      val () = sizeUpdate(opcode_enterInt    , 4);  *)(* SPF 30/1/97 *)
      val () = sizeUpdate(opcode_jump        , 2);
      val () = sizeUpdate(opcode_jumpFalse   , 2);
      val () = sizeUpdate(opcode_delHandler  , 2);
      val () = sizeUpdate(opcode_jumpIFalse  , 2);
      val () = sizeUpdate(opcode_delHandlerI , 2);
      val () = sizeUpdate(opcode_caseSwitch  , 3);
      val () = sizeUpdate(opcode_returnW     , 3);
      val () = sizeUpdate(opcode_getStoreW   , 3);
      val () = sizeUpdate(opcode_nonLocal    , 7);
      val () = sizeUpdate(opcode_localW      , 3);
      val () = sizeUpdate(opcode_indirectW   , 3);
      val () = sizeUpdate(opcode_moveToVecW  , 3);
      val () = sizeUpdate(opcode_setStackValW, 3);
      val () = sizeUpdate(opcode_resetW      , 3);
      val () = sizeUpdate(opcode_resetR_w    , 3);
      val () = sizeUpdate(opcode_constAddr   , 3);
      val () = sizeUpdate(opcode_constAddrX_b , 4);
      val () = sizeUpdate(opcode_constAddrX_w , 5);
      val () = sizeUpdate(opcode_constIntW   , 3);
      val () = sizeUpdate(opcode_jumpBack8   , 2);
      val () = sizeUpdate(opcode_returnB     , 2);
      val () = sizeUpdate(opcode_jumpBack16  , 3);
      val () = sizeUpdate(opcode_getStoreB   , 2);
      val () = sizeUpdate(opcode_localB      , 2);
      val () = sizeUpdate(opcode_indirectB   , 2);
      val () = sizeUpdate(opcode_moveToVecB  , 2);
      val () = sizeUpdate(opcode_setStackValB, 2);
      val () = sizeUpdate(opcode_resetB      , 2);
      val () = sizeUpdate(opcode_resetRB     , 2);
      val () = sizeUpdate(opcode_constIntB   , 2);
      val () = sizeUpdate(opcode_nonLocalL_1 , 2);
      val () = sizeUpdate(opcode_nonLocalL_2 , 2);
      val () = sizeUpdate(opcode_nonLocalL_3 , 2);
      val () = sizeUpdate(opcode_tupleW      , 3);
      val () = sizeUpdate(opcode_tupleB      , 2);
      val () = sizeUpdate(opcode_setHandler  , 2);
      val () = sizeUpdate(opcode_setHandlerI , 2);
      val () = sizeUpdate(opcode_tailbb      , 3);
      val () = sizeUpdate(opcode_tail        , 5);
      val () = sizeUpdate(opcode_tail3b      , 2);
      val () = sizeUpdate(opcode_tail4b      , 2);
    in
      fun size (Opcode n) : int = Array.sub (sizeArray, n);
    end;
  end; (* opcode abstype *)

(*****************************************************************************)
(*                  Types for branch labels                                  *)
(*****************************************************************************)

  (* The addrs is the address of the branch instruction, so we can fix up
     the branch later, NOT the address we're branching to, which we
     don't know when we generate the label. The cacheState indicates whether
     what was cached at the source of the jump.
   *)
  datatype jumpFrom =
    Jump8From  of addrs  (* branch instruction has  8-bit offset field *)
  | Jump16From of addrs; (* branch instruction has 16-bit offset field *)

  (* We need a jumpFrom ref, because we may have to indirect short branches
     via long branches if the offset won't fit into 14 bits *)
  type labels = (jumpFrom ref) list;
  
  val noJump : labels = []; 
  
  (* This is the list of outstanding labels.  Use a separate type from
      "labels" for extra security. *)
  type labList = (jumpFrom ref) list;

(*****************************************************************************)
(*                  The main "code" datatype                                 *)
(*****************************************************************************)

  datatype const =
     WVal of machineWord        (* an existing constant *)

  and setCodeseg =
     Unset
   | Set of cseg   (* Used for completing forward references. *)

  and code = Code of 
    { codeVec:        cseg,           (* This segment is used as a buffer. When the
                                         procedure has been code generated it is
                                         copied into a new segment of the correct size *)
      ic:             addrs ref,      (* Pointer to first free location in "codevec" *)
      constVec:       const list ref, (* Vector of words to be put at end *)
      numOfConsts:    int ref,        (* size of constVec *)
      stackReset:     int ref,        (* Distance to reset the stack before the next instr. *)
      carry:          bool ref,       (* Should a value be moved down if stackReset <> 0? *)
      labelList:      labList ref,    (* List of outstanding short branches. *)
      longestBranch:  addrs ref,      (* Address of the earliest short branch.*)

      procName:       string,         (* Name of the procedure. *)
      resultSeg:      setCodeseg ref, (* The segment as the final result. *)
      constLoads:     (addrs * int) list ref, (* where do we load constants? *)
      printAssemblyCode:bool,            (* Whether to print the code when we finish. *)
      printStream:    string->unit    (* The stream to use *)
    };

(*****************************************************************************)
(*                  Auxiliary functions on "code"                            *)
(*****************************************************************************)

  fun codeVec        (Code {codeVec,...})          = codeVec;
  fun ic             (Code {ic,...})               = ic;
  fun constVec       (Code {constVec,...})         = constVec;
  fun numOfConsts    (Code {numOfConsts,...})      = numOfConsts;
  fun stackReset     (Code {stackReset ,...})      = stackReset;
  fun carry          (Code {carry,...})            = carry;
  fun labelList      (Code {labelList,...})        = labelList;
  fun longestBranch  (Code {longestBranch,...})    = longestBranch;
  fun procName       (Code {procName,...})         = procName;
  fun resultSeg      (Code {resultSeg,...})        = resultSeg;
  fun constLoads     (Code {constLoads,...})       = constLoads;

  fun scSet (Set x) = x | scSet _ = raise Match;

  val codesize = 32; (* bytes. Initial size of segment. *)

  fun sameConst (WVal w1, WVal w2) = wordEq (w1, w2)

  (* create and initialise a code segment *)
  fun codeCreate (name : string, parameters) : code = 
  let
    val words : int = codesize div wordLength
    val printStream = PRETTY.getSimplePrinter parameters;
  in
    Code
      { 
         codeVec          = csegMake(Word.fromInt words), (* a byte array *)
         ic               = ref addrZero,
         constVec         = ref [],
         numOfConsts      = ref 0,
         stackReset       = ref 0, (* stack adjustment in WORDs *)
         carry            = ref false,
         labelList        = ref [],
         longestBranch    = ref addrLast, (* None so far *)
         procName         = name,
         resultSeg        = ref Unset,   (* Not yet done *)
         constLoads       = ref [],
         printAssemblyCode = DEBUG.getParameter DEBUG.assemblyCodeTag parameters,
         printStream    = printStream
      }
  end;

  fun setLong (value : int, Addr a : addrs, seg : cseg) : unit =
  let
    fun putBytes(value, a, seg, i) =
    if i = wordLength then ()
    else
        (
        csegSet(seg,
            Word.fromInt(if littleEndian() then a+i else a+wordLength-i-1),
            Word8.fromInt(value mod 256));
        putBytes(value div 256, a, seg, i+1)
        )
  in
    putBytes(value, a, seg, 0)
  end;

  fun putByte (ival: int, Addr a, cvec: code) : unit =
    csegSet(codeVec cvec, Word.fromInt a, Word8.fromInt (if ival < 0 then 256 + ival else ival));

  fun genByte (ival: int, cvec: code) : unit = 
  let
    val icVal : addrs = ! (ic cvec);
    val () = putByte (ival, icVal, cvec);
  in
    ic cvec := icVal addrPlus 1
  end;
   
  fun genBytes (ival: int, length: int, cvec: code) : unit =
  let
    val () = genByte (ival mod 256, cvec);
  in
    if length = 1 then ()
    else genBytes (ival div 256, length - 1, cvec)
  end;

  fun genWord (ival : int, cvec : code) : unit =
    genBytes (ival, 2, cvec);

  (* puts "length" bytes of "val" into locations "addr", "addr"+1... *)
  fun putBytes (ival : int, length : int, addr : addrs, cvec : code) : unit =
  let
    val () = putByte (ival mod 256, addr, cvec);
  in
    if length = 1 then ()
    else putBytes (ival div 256, length - 1, addr addrPlus 1, cvec)
  end;

  fun getByte (Addr a, cvec : code) : int =
    Word8.toInt (csegGet(codeVec cvec, Word.fromInt a));

  (* Gets "length" bytes from locations "addr", "addr"+1...
     Returns an unsigned number. *)
  fun getB (length : int, addr : int, seg: cseg) : int =
  let
    val byte = Word8.toInt (csegGet (seg, Word.fromInt addr));
  in
    if length = 1 (* Top byte *)
    then byte
    else let
      val rest = getB (length - 1, addr + 1, seg);
    in
      rest * 256 + byte
    end
  end;

  fun getBytes (length: int, Addr a, cvec : code) : int =
    getB (length, a, codeVec cvec);

  fun resetSp (cvec: code) : unit =
  let 
    val offset = !(stackReset cvec);

    val () =
      if offset < 0
        then raise InternalError ("resetSp: bad reset value " ^ Int.toString offset)
      
      else if offset = 0
        then ()
     
      else if 255 <= offset
        then let
          val opc = if !(carry cvec) then opcode_resetR_w else opcode_resetW;
          val () = genByte (opcode_down opc, cvec);
        in
          genWord (offset, cvec)
        end
         
      else if !(carry cvec)
    then if 3 < offset
      then let
        val () = genByte (opcode_down opcode_resetRB, cvec);
      in
        genByte (offset, cvec)
      end
      else let
        val opc : int = opcode_down opcode_resetR_1 + offset - 1;
      in
        genByte(opc, cvec)
      end
    
      else if 2 < offset
    then let
      val () = genByte (opcode_down opcode_resetB, cvec);
    in
      genByte (offset, cvec)
    end
    else let
      val opc : int = opcode_down opcode_reset_1 + offset - 1;
    in
      genByte(opc, cvec)
    end
  in
    stackReset cvec := 0
  end; (* resetSp *)


(* 
   The cvec holds a list of short branches so that they can be extended
   to long branches before they go out of range. If we fix up a
   short branch, we must call "removeLabel" to purge it from this list.
   To keep things simple, we call "removeLabel" whenever we fix up
   a jump - if the label is long, or if it doesn't appear in the list
   (which is the case for branches backwards), we just won't find it
   in the list. SPF 21/9/95
*)
  fun removeLabel (lab : addrs, cvec : code) : unit = 
  let
    fun removeEntry ([]: labList) : labList = []
      | removeEntry ((ref (Jump16From _)) :: t) =
          removeEntry t (* we discard all long jumps *)
        
      | removeEntry ((entry as ref (Jump8From addr)) :: t) =
        if lab = addr
        then removeEntry t
        else let
          val () =
            if addr addrLt !(longestBranch cvec)
            then longestBranch cvec := addr
            else ();
        in    
          entry :: removeEntry t
        end;
  in
    (* We recompute the longest 14-bit branch. *)
    longestBranch cvec := addrLast;
    labelList cvec     := removeEntry (! (labelList cvec))
  end;

    fun fixupOffset (Jump8From addr, target, cvec) =
        let
            (* Offsets are calculated from the END of the instruction, which explains the "+ 1" *)
            val newOffset : int = target addrMinus (addr addrPlus 1);
    
            val () = 
                if 0 <= newOffset andalso newOffset < 256 then ()
                else raise InternalError "fixupOffset: jump too far (8-bit offset)"
    
            val oldOffset : int = getByte (addr, cvec);
    
            val () = 
                if oldOffset = 0 then ()
                else raise InternalError "fixupOffset: 8-bit branch already fixed up"

            (* 
               We're about to fix up the jump, so remove it from the
               list of pending short jumps.
             *)
            val () = removeLabel (addr, cvec);
        in
            putByte (newOffset, addr, cvec)
        end

    |   fixupOffset (Jump16From addr, target, cvec) =
        let
            (* Offsets are calculated from the END of the instruction, which explains the "+ 2" *)
            val newOffset = target addrMinus (addr addrPlus 2);
    
            val () =
                (* Jumps are unsigned. *)
                if 0 <= newOffset andalso newOffset < 65536 then ()
                else raise InternalError "fixupOffset: jump too far (16-bit offset)"
    
            val oldOffset = getBytes (2, addr, cvec);

            val () = 
                if oldOffset = 0 then ()
                else raise InternalError "fixupOffset: 16-bit branch already fixed up"
        in
            putBytes (newOffset, 2, addr, cvec)
        end


  fun fixup ([]  : labels, _: code) : unit = ()
    | fixup (lab : labels, cvec: code) : unit =
  let
    (* Deal with any pending resets. *)
    val () = resetSp cvec;
    val target : addrs = ! (ic cvec);
  in
    applyList (fn (ref jf) => fixupOffset (jf, target, cvec), lab)
  end;

  (* Makes a new label and puts it in the list. *)
  fun makeLabel (cvec: code, addr: addrs) : labels =
  let
    (* All labels are created as short jumps *)
    val lab : jumpFrom ref = ref (Jump8From addr);
    
    val () =
      if addr addrLt !(longestBranch cvec)
      then longestBranch cvec := addr
      else ();
      
    (* Add to the list of pending fixups *)
    val () = labelList cvec := lab :: !(labelList cvec);
  in
    [lab]
  end;

  (* If the longest branch is close to going out of range it must
     be converted into a long form.
     If the size is large (e.g. casel/casew) then all labels should be
     converted. If we have just made an unconditional branch then we
     make the distance shorter. 220 is just a fairly conservative
     number. (Dave used a clever calculation, but I don't like too much
     cleverness.) 
     
     This code isn't very clever because it uses a separate 16-bit extension
     for each original 8-bit jump. I think Dave's original code tried
     to use a single 16-bit extension per target (not per jump). Since
     this code is only for use in bootstrapping, simplicity is more
     important than efficiency (KISS!).
     SPF 7/1/97
   *)
  fun checkBranchList (cvec: code, branched: bool, size: int): unit =
  let
    val maxDiff = 220 - size;

    fun convertLabels ([]:labList) : labList = []
      | convertLabels (lab::labs) =
    let
      (* Process the list starting at the end. The reason for this
     is that more recent labels appear before earlier ones.
     We must put the earliest labels in first because they may
     be about to go out of range. *)
       val convertRest = convertLabels labs;
    in
      (* Now do this entry. *)
      case !lab of
    Jump16From _ => (* shouldn't happen? *)
      convertRest
    
      | Jump8From addr =>
    let
      val here : addrs = !(ic cvec);
    in
      if maxDiff < here addrMinus addr
      then let (* Getting close -  fix up the short branch to indirect via here *)
            (* Offsets are calculated from the END of the instruction, which explains the "+ 1" *)
            val newOffset : int = here addrMinus (addr addrPlus 1);

            val () = 
              if 0 <= newOffset andalso newOffset < 256 then ()
              else raise InternalError "checkBranchList: offset too large to convert"

            val oldOffset : int = getByte (addr, cvec);
    
            val () = 
              if oldOffset = 0 then ()
              else raise InternalError "checkBranchList: 8-bit offset already fixed up";
              
            (* Convert the instruction to the "indirect" form *)
            val instrAddr    : addrs = addr addrPlus ~1;
            val oldInstr = opcode_up(getByte (instrAddr, cvec))
            
            val newInstr =
                if oldInstr = opcode_jump
                then opcode_jumpI
                else if oldInstr = opcode_jumpFalse
                then opcode_jumpIFalse
                else if oldInstr = opcode_delHandler
                then opcode_delHandlerI
                else if oldInstr = opcode_setHandler
                then opcode_setHandlerI
                else raise InternalError "Extending unknown branch instruction"
              
            (* Fix up the instruction and offset *)
            val () = putByte (opcode_down newInstr, instrAddr, cvec);
            val () = putByte (newOffset, addr, cvec);

        (* Generate the indirection itself, and alter the jump state *)
        val () = genWord (0, cvec);
        val () = lab := Jump16From here;
      in
        convertRest
      end
      else let
        (* Not ready to remove this. Just find out if
           this is an earlier branch and continue. *)
        val () =
          if addr addrLt !(longestBranch cvec)
          then longestBranch cvec := addr
          else ();
      in
        lab :: convertRest
      end
       end
    end; (* convertLabels *)
  in
    if !(ic cvec) addrMinus !(longestBranch cvec) <= maxDiff then ()
    else let
      (* Must save the stack-reset, otherwise "fixup" will try
         to reset it. *)
      val sr       = ! (stackReset cvec);
      val () = stackReset cvec := 0;
        
      (* Must skip round the branches unless we have just
     taken an unconditional branch. *)
      val lab : labels = 
    if branched then noJump
    else let
      val () = genByte(opcode_down opcode_jump, cvec);
      val () = genByte(0, cvec);
    in
      makeLabel(cvec, !(ic cvec) addrPlus ~1)
    end

      (* Find the new longest branch while converting the labels *)
      val () = longestBranch cvec := addrLast;
      val () = labelList cvec := convertLabels (! (labelList cvec));
      val () = fixup (lab, cvec); (* Continue with normal processing. *)
    in
      stackReset cvec := sr (* Restore old value. *)
    end
  end; (* checkBranchList *)

  (* Dave had some complicated scheme here - with the new representation of
     labels, everything gets much simpler. *)
  fun linkLabels (lab1 : labels, lab2 : labels, _ : code) : labels =
    lab1 @ lab2;

  (* Put in the opcode for an instruction. *)
  fun genOpc (opc: opcode, size: int, cvec: code) : unit =
  let
    val opn : int = opcode_down opc;
  
(* ...
    val () =
      if 0 <= opn andalso opn < 256 andalso opn <> opcode_down opcode_booleanOr
      then ()
      else raise InternalError ("genOpc: bad opcode: " ^ Int.toString opn);
... *)
  
    val () = checkBranchList (cvec, false, size);
    val () = resetSp cvec;
  in
    genByte (opn, cvec)
  end
  
    fun genOpcode (oper, code) = genOpc(oper, 1, code)

  fun genRaiseEx (cvec: code) : unit =
    genOpc (opcode_raiseEx, 1, cvec);
  
  fun genLock(cvec: code) : unit =
    genOpc (opcode_lock, 1, cvec);
  
  fun genLdexc (cvec: code) : unit =
    genOpc (opcode_ldexc, 1, cvec);

  fun genPushHandler (cvec: code) : unit =
    genOpc (opcode_pushHandler, 1, cvec);

  (* Generate word, byte or single opcodes. The values from ``f''  to ``l''
     are packed into the opcode by generating opF, opF+1, ... opF+(l-f).
     Other arguments which will fit into a byte generate opB followed by
     the argument. The rest require opW and a word argument. *)
  fun gen1 (opW: opcode, opB: opcode, opF: opcode,
        first : int, last : int, arg1: int, cvec: code) : unit =
        
    if (first <= arg1 andalso arg1 <= last)
    then genOpc (opcode_up (opcode_down opF + arg1 - first), 1, cvec)

    else if 0 <= arg1 andalso arg1 <= 254 (* why not 255? *)
    then let
      val () = genOpc(opB, 2, cvec);
    in
      genByte(arg1, cvec)
    end

    else let
      val () = genOpc(opW, 3, cvec);
    in
      genWord(arg1, cvec)
    end;

  fun genReturn (arg1 : int, cvec : code) : unit =
    gen1 (opcode_returnW,
      opcode_returnB,
      opcode_return_0,
      0, 3, arg1, cvec);

  fun genGetStore (arg1 : int, cvec : code) : unit =
    gen1 (opcode_getStoreW,
      opcode_getStoreB,
      opcode_getStore_2,
      2, 4, arg1, cvec);

  fun genLocal (arg1 : int, cvec : code) : unit =
    gen1 (opcode_localW, 
      opcode_localB, 
      opcode_local_0,
      0, 11, arg1, cvec);

  fun genIndirect (arg1 : int, cvec : code) : unit =
    gen1 (opcode_indirectW, 
      opcode_indirectB,
      opcode_indirect_0,
      0, 5, arg1, cvec);

  fun genMoveToVec (arg1 : int, cvec : code) : unit =
    gen1 (opcode_moveToVecW,
      opcode_moveToVecB,
      opcode_moveToVec_0,
      0, 7, arg1, cvec);

  fun genSetStackVal (arg1 : int, cvec : code) : unit =
    gen1 (opcode_setStackValW,
      opcode_setStackValB,
      opcode_setStackValB, (* Don't care - no "implied" form exists *)
      1, 0, arg1, cvec);

  fun genCase (arg1 : int, cvec : code) : unit =
  let
    (* The destination addresses immediately follow the case instruction
       so we must make sure there is enough room. *)
    val () = genOpc (opcode_caseSwitch, 3 + arg1 * 2, cvec);
  in
    genWord (arg1, cvec)
  end;

  fun genTuple (arg1: int, cvec: code) : unit =
    gen1 (opcode_tupleW,
      opcode_tupleB,
      opcode_tuple_2,
      2, 4, arg1, cvec)

  fun genNonLocal (arg1 : int, arg2 : int, arg3 : int, cvec: code) : unit =
    if arg1 <= 0 orelse arg2 <= 0
      then raise InternalError "genNonLocal: invalid parameters"
  
    else if arg1 <= 16 andalso arg2 <= 3 andalso ~6 <= arg3 andalso arg3 <= 9
    then let (* use a coded representation *)
      val opc = opcode_up(opcode_down opcode_nonLocalL_1 + arg2 - 1);
      val () = genOpc (opc, 1, cvec);
    in
      genByte((arg1 - 1) * 16 + arg3 + 6, cvec)
    end

    else let
      val () = genOpc (opcode_nonLocal, 5, cvec);
      val () = genWord (arg1, cvec);
      val () = genWord (arg2, cvec);
    in
      genWord (arg3, cvec)
    end;

    fun genEnterInt (cvec: code, args: int) : unit =
    if generateEnterInt
    then
    (
        genByte(opcode_down opcode_enterInt, cvec);
        genByte(args + 1, cvec)
    )
    else ()

  fun genEnterIntCall (cvec: code, args: int) : unit =
  let
    val () =
      if args < MAXINTARGS then ()
      else raise InternalError "genEnterIntCall: too many arguments";
  in
    genEnterInt(cvec, args)
  end;

  local
    val enterHandlerCode = (*2 * MAXINTARGS *) 254;
  in
    fun genEnterIntCatch (cvec: code) : unit =
      genEnterInt(cvec, enterHandlerCode);
  end;

  fun genEnterIntProc (cvec: code, args: int) : unit =
  let
    val () =
      if args < MAXINTARGS then ()
      else raise InternalError "genEnterIntProc: too many arguments";
      
    val argCode : int = MAXINTARGS + args; 
  
    (* Primary entry point (address 0) *)
    val () = genEnterInt(cvec, argCode);
  in
    ()
  end;

  (* Used for jump, jumpFalse, setHandler and delHandler. *)
  fun putBranchInstruction (opc: opcode, cvec: code) : labels =
    if opc = opcode_setHandler orelse
       opc = opcode_jumpFalse
    then let (* The next instruction may or will be executed. *)
      val () = genOpc (opc, 3, cvec); (* why not 2? *)
      val () = genByte (0, cvec);
    in
      makeLabel (cvec, !(ic cvec) addrPlus ~1)
    end
    
    else let (* Unconditional branches. *)
      val () = resetSp cvec;
      val () = genByte (opcode_down opc, cvec);
      val () = genByte (0, cvec);
      val lab : labels = makeLabel (cvec, !(ic cvec) addrPlus ~1);
      
      (* Having just generated an unconditional branch we can extend
     branches without the overhead of an extra branch. That's
     why we did a genByte, rather than a genOpc, just now. *)
      val () = checkBranchList (cvec, true, 0);
    in
      lab
    end;

    (* Generate either a short or long jump. *)
    fun jumpback (lab: addrs, cvec: code) : unit =
    let
        val () = resetSp cvec;
  
        (* Don't use genOpc(opcode_jump) because we want to check the branch
           list afterwards, and also because it might generate some code if
           we try to use a short branch and take it over the limit. *)
        val newOffset : int = !(ic cvec) addrMinus lab;
    
        val () =
            if newOffset < 256
            then
            let (* short *)
                (* For opcode_jumpBack, exceptionally, the offset is relative
                   to the START of the instruction. Also, the offset is
                   backwards, as opposed to the usual forwards convention. *)
                val () = genByte (opcode_down opcode_jumpBack8, cvec);
            in
                genByte (newOffset, cvec)
            end
            else
            let (* must use 16-bit jump *)
                val () = genByte (opcode_down opcode_jumpBack16, cvec);
            in
                genWord (newOffset, cvec)
            end;
    in
        (* Having just generated an unconditional branch we can extend
           branches without the overhead of an extra branch. *)
        checkBranchList(cvec, true, 0)
    end

    fun genRTSCallFast(0, cvec) = genOpc (opcode_callFastRTS0, 1, cvec)
    |   genRTSCallFast(1, cvec) = genOpc (opcode_callFastRTS1, 1, cvec)
    |   genRTSCallFast(2, cvec) = genOpc (opcode_callFastRTS2, 1, cvec)
    |   genRTSCallFast(3, cvec) = genOpc (opcode_callFastRTS3, 1, cvec)
    |   genRTSCallFast(4, cvec) = genOpc (opcode_callFastRTS4, 1, cvec)
    |   genRTSCallFast(5, cvec) = genOpc (opcode_callFastRTS5, 1, cvec)
    |   genRTSCallFast(_, _) = raise InternalError "genRTSFastCall"

    fun genRTSCallFull(0, cvec) = genOpc (opcode_callFullRTS0, 1, cvec)
    |   genRTSCallFull(1, cvec) = genOpc (opcode_callFullRTS1, 1, cvec)
    |   genRTSCallFull(2, cvec) = genOpc (opcode_callFullRTS2, 1, cvec)
    |   genRTSCallFull(3, cvec) = genOpc (opcode_callFullRTS3, 1, cvec)
    |   genRTSCallFull(4, cvec) = genOpc (opcode_callFullRTS4, 1, cvec)
    |   genRTSCallFull(5, cvec) = genOpc (opcode_callFullRTS5, 1, cvec)
    |   genRTSCallFull(_, _) = raise InternalError "genRTSCallFull"
    
    fun genRTSCallFastFloatFloat cvec = genOpc (opcode_callFastRTSFtoF, 1, cvec)
    and genRTSCallFastGeneralFloat cvec = genOpc (opcode_callFastRTSGtoF, 1, cvec)

  local
    fun fixupConstantLoad (constStartAddrs : addrs, cvec : code) =
      fn (fixupAddr : addrs, constNum : int) =>
      let
        val oldOffset : int = getBytes (2, fixupAddr, cvec);
        val () =
          if oldOffset = 0 then ()
          else raise InternalError "fixupConstantLoad: already fixed-up";

        val constAddr : addrs =
           if usePortableConstantOffset
           then constStartAddrs
           else constStartAddrs addrPlus (wordLength * (constNum+4));
          
        (* Offsets are calculated from the END of the instruction, which explains the "+ 2" *)
        val newOffset : int = constAddr addrMinus (fixupAddr addrPlus 2);
        
        val () = 
          if 0 <= newOffset andalso newOffset < 65536 then ()
          else raise InternalError "fixupConstantLoad: constant too distant (16-bit offset)"
      in
        putBytes (newOffset, 2, fixupAddr, cvec)
      end
  in
    fun fixupConstantLoads (cvec, constStartAddrs, loadInfo) : unit =
      applyList (fixupConstantLoad (constStartAddrs, cvec), loadInfo);
  end;


  (* Find the offset in the constant area of a constant. *)
  (* The first has offset 0.                             *)
  fun addConstToVec (valu : const, cvec : code) : int =
  let
     (* Search the list to see if the constant is already there. *)
    fun findConst valu [] num =
      (* Add to the list *)
        (
         numOfConsts cvec := ! (numOfConsts cvec) + 1;
         constVec cvec    := ! (constVec cvec) @ [valu];
         num
        )
      | findConst valu (h :: t) num =
          if sameConst (valu, h)
          then num
          else findConst valu t (num + 1) (* Not equal *);
  in
    findConst valu (! (constVec cvec)) 0
  end;

  fun genConstRef (constNum : int, cvec : code) : unit =
  let
    (* Remember address of the indirection so we can fix it up later *)
    val fixupAddrs : addrs = !(ic cvec);
    val () = genWord (0, cvec);
  in
    constLoads cvec := (fixupAddrs, constNum) :: !(constLoads cvec)
  end;

  fun pushConst (value : machineWord, cvec : code) : unit =
    if isShort value andalso toShort value < 0w32768
    then let
      val iVal: int = Word.toInt (toShort value);
    in
      if iVal = 10
        then genOpc (opcode_const_10, 1, cvec)
      
      else if iVal <= 4
        then genOpc (opcode_up (opcode_down opcode_const_0 + iVal), 1, cvec)
  
      else if iVal < 256
      then let
        val () = genOpc (opcode_constIntB, 2, cvec);
      in
        genByte (iVal, cvec)
      end
      
      else let
        val () = genOpc (opcode_constIntW, 3, cvec);
      in
        genWord (iVal, cvec)
      end
    end

    else let (* address or large short *)
      val constNum : int = addConstToVec (WVal value, cvec);
      val () =
        if not usePortableConstantOffset
        then genOpc (opcode_constAddr, 3, cvec)
        else if constNum < 256
        then (genOpc (opcode_constAddrX_b, 4, cvec); genByte (constNum, cvec))
        else (genOpc (opcode_constAddrX_w, 5, cvec); genWord (constNum, cvec));
    in
      genConstRef (constNum, cvec)
    end;

  fun genCallClosure (cvec: code) : unit = genOpc (opcode_callClosure, 1, cvec)
    
    fun genTailCall (toslide : int, slideby: int, cvec: code) : unit =
    if toslide < 256 andalso slideby < 256
    then case (toslide, slideby) of
        (3, 2) =>  genOpc (opcode_tail3_2, 1, cvec)
           
    |   (3, 3) => genOpc (opcode_tail3_3, 1, cvec)
           
    |   (3, _) => 
        let
            val () = genOpc (opcode_tail3b, 2, cvec);
        in
            genByte (slideby, cvec)
        end
           
    |   (4, _) => 
        let
            val () = genOpc (opcode_tail4b, 2, cvec);
        in
            genByte (slideby, cvec)
        end
           

    |   (_, _) => 
        let (* General byte case *)
            val () = genOpc (opcode_tailbb, 3, cvec);
            val () = genByte (toslide, cvec);
        in
            genByte (slideby, cvec)
        end
           
    else
    let (* General case. *)
        val () = genOpc (opcode_tail, 5, cvec);
        val () = genWord (toslide, cvec);
    in
         genWord(slideby, cvec)
    end; (* genTailCall *)

  fun genContainer (size : int, cvec: code) : unit =
    (genOpc(opcode_containerW, 3, cvec); genWord(size, cvec));

  fun genTupleFromContainer (size : int, cvec: code) : unit =
    (genOpc(opcode_tuple_containerW, 3, cvec); genWord(size, cvec));


  (* Adds in the reset. *)
  fun resetStack (offset : int, carryValue : bool, cvec : code) : unit =
  let
    val () =
      if 0 < offset then ()
      else raise InternalError ("resetStack: bad offset " ^ Int.toString offset);
  
    val () = stackReset cvec := !(stackReset cvec) + offset;
  in
     carry cvec := carryValue
  end;

  fun printCode (seg: cseg, procName: string, endcode : int, printStream) : unit =
  let
    val () = printStream "\n";
    val () =
     if procName = "" (* No name *) then printStream "?" else printStream procName;
    val () = printStream ":\n";

    (* prints a string representation of a number *)
    fun printHex (v : int) : unit = printStream(Int.fmt StringCvt.HEX v);
 
    val ptr = ref 0;
 
    (* To make sure we do not print branch extensions as though they
       were instructions we keep a list of all indirect forward references
       and print values at those addresses as addresses.
       This list is sorted with the lowest address first. *)
 
    val indirections : int list ref = ref [];
 
    local
      fun addL (n, [] : int list) : int list = [n]
        | addL (n, l as (x :: xs)) =
          if n < x then n :: l else
          if n = x then l else
             x :: addL (n, xs)
    in
      fun addInd (ind : int) : unit =
        indirections := addL (ind, !indirections)
    end;
 
    (* Prints a relative address. *)
    fun printDisp (len: int, spacer: string, addToList: bool) : unit =
    let
      val ad : int = getB(len, !ptr, seg) + !ptr + len;
      val () = if addToList then addInd ad else ();
      val () = printStream spacer;
      val () = printHex ad;
    in
      ptr := !ptr + len
    end;

    (* Prints an operand of an instruction *)
    fun printOp (len: int, spacer : string) : unit =
    let
      val () = printStream spacer;
      val () = printHex (getB (len, !ptr, seg));
    in
      ptr := !ptr + len
    end;

    val () =     
      while !ptr < endcode
      do let
        val addr : int = !ptr;
        val () = printHex addr; (* The address. *)
  
        val () = 
          if (case !indirections of v :: _ => v = addr | [] => false)
          then let (* It's an address. *)
            val () = printDisp (2, "\t", false);
          in
            case !indirections of
              _ :: vs => indirections := vs
            | _       => raise InternalError "printCode: indirection list confused"
          end
              
          else let (* It's an instruction. *)
            val ()  = printStream "\t";
            val opc : opcode = opcode_up (Word8.toInt (csegGet (seg, Word.fromInt (!ptr)))); (* opcode *)
            val ()  = ptr := !ptr + 1;
            val ()  = printStream (repr opc);
    
            val sz : int = size opc;
          in
            if sz = 1 then ()
            
             else if opc = opcode_jump orelse
                     opc = opcode_jumpFalse orelse
                     opc = opcode_setHandler orelse
                     opc = opcode_delHandler orelse
                     opc = opcode_constAddr
                then printDisp (sz - 1, "\t", false)
            
            else if opc = opcode_jumpI orelse
                    opc = opcode_jumpIFalse orelse
                    opc = opcode_setHandlerI orelse
                    opc = opcode_delHandlerI
              then printDisp (1, "\t", true)
              
            else if opc = opcode_jumpBack8 (* Should be negative *)
              then let
                val () = printStream "\t";
                val () = printHex((!ptr - 1) - getB(1,!ptr,seg));
              in
                ptr := !ptr + 1
              end

             else if opc = opcode_jumpBack16 (* Should be negative *)
              then let
                val () = printStream "\t";
                val () = printHex((!ptr - 1) - getB(2,!ptr,seg));
              in
                ptr := !ptr + 2
              end
             
            else if opc = opcode_nonLocal
              then let
                val () = printOp (2, "\t");
                val () = printOp (2, ",");
              in          
                printOp(2, ",")
              end
 
             else if opc = opcode_caseSwitch
              then let
                (* Have to find out how many items there are. *)
                val limit : int = getB (2, !ptr, seg);
                val () = printOp (2, "\t");
                val base : int = !ptr;
                
                fun printEntry (_ : int) =
                let
                  val () = printStream "\n\t";
                  val () = printHex(base + getB(2, !ptr, seg));
                in
                  ptr := !ptr + 2
                end;
              in
                forLoop printEntry 0 limit
              end
                 
            else if opc = opcode_tail
              then let
                val () = printOp (2, "\t");
              in
                printOp (2, ",")
              end
                 
            else if opc = opcode_tailbb
              then let
                val () = printOp (1, "\t");
              in
                printOp (1, ",")
              end
                 
             else if opc = opcode_constAddrX_b
                then ( printOp (1, "\t"); printDisp (sz - 1, ",", false) )

             else if opc = opcode_constAddrX_w
                then ( printOp (2, "\t"); printDisp (sz - 1, ",", false) )

             else printOp (sz - 1, "\t")
          end; (* an instruction. *)
      in
        printStream "\n"
      end (* main loop *)  
  in (* body of printCode *)
    ()
  end; (* printCode *)

  (* The count of the number of constants is an untagged value so we
     can't use loadWord. *)
  fun loadConstCount (a : address, offset : int) : int =
  let
    val byteOffset : int = wordLength * offset
    fun loadBytes (i: int) (acc: int) : int =
        if i = wordLength then acc
        else
        let
            val addr: int =
                if littleEndian() then byteOffset + wordLength - i - 1
                else byteOffset + i;
            val b = loadByte (a, Word.fromInt addr);
            val acc' = acc*256 + Word8.toInt b
        in
            loadBytes (i+1) acc'
        end
  in
    loadBytes 0 0
   end;
  
  (* Bootstrapping problems currently prevent us from using Address.nameOfCode *)
  fun nameOfCode (a : address) =
    let
      val objLength  : int  = Word.toInt (ADDRESS.length a);
      val lastWord   : int  = objLength - 1;
      val constCount : int  = loadConstCount (a, lastWord);
      val codeName   : machineWord = loadWord (a, Word.fromInt (lastWord - constCount));
    in
      RunCall.unsafeCast codeName
    end;

  (* prints a string representation of a number *)
  fun printHex (v : int, printStream) : unit = printStream(Int.fmt StringCvt.HEX v);

  fun printConstCode (a : address, printStream) : unit =
    printStream ("code:\t" ^ nameOfCode a);
  
  fun printConstClosure (a : address, printStream) : unit =
    printStream ("clos:\t" ^ nameOfCode a);
  
  fun printWords (a : address, printStream) : unit =
    let
      val objLength : int = Word.toInt (ADDRESS.length a)
    in
      if objLength = 1
      then printStream ("long:\t1 word")
      else printStream ("long:\t" ^ Int.toString objLength ^ " words")
    end;
  
  fun printBytes (a : address, printStream) : unit =
    let
      val objLength  : int = Word.toInt (ADDRESS.length a)
    in
      if objLength = 1
      then printStream ("bytes:\t1 word")
      else printStream ("bytes:\t" ^ Int.toString objLength ^ " words")
    end;

  fun printConst (WVal w : const, printStream) : unit =
    if isShort w
    then let
      val value : int = Word.toInt (toShort w);
      val () = printStream "short:\t";
      val () = printHex(value, printStream);
      val () = printStream " (";
      val () = printStream (Int.toString value);
    in
      printStream ")"
    end
    else let
      val a : address = toAddress w;
    in
      if isIoAddress a
        then printStream "RTS entry"
      else if isCode a
        then printConstCode(a, printStream)
      else if isBytes a
        then printBytes(a, printStream)
      else if isWords a andalso 0w1 <= ADDRESS.length a
        then let
          val w' : machineWord = loadWord (a, 0w0)
        in
          if not (isShort w')
          then let
            val a' : address = toAddress w';
          in
            if not (isIoAddress a') andalso isCode a'
            then printConstClosure(a', printStream)
            else printWords(a, printStream) (* First element of tuple is not a code segment *) 
          end
          else printWords(a, printStream) (* First element of tuple is a short *)
        end
        else printWords(a, printStream) (* Not a proper tuple (shouldn't occur) *)
    end;
           
  fun printConstants (_    : int, [] : const list, _) : unit = ()
    | printConstants (addr : int, h :: t, printStream) : unit =
  let
    val () = printHex(addr, printStream);
    val () = printStream "\t";
    val () = printConst(h, printStream);
    val () = printStream "\n";
  in
    printConstants (addr + wordLength, t, printStream)
  end;

  (* set the num'th constant in cvec to be value *)
  fun constLabels (cvec : code, num : int, value : machineWord) : unit =
  let
    val seg       = scSet (!(resultSeg cvec));
    (* The +2 in the next instruction is because ic is always the byte count of
       the word after the marker word.  We need to skip over the function name
       and the profile count. *)
    val constAddr = (getAddr (!(ic cvec))) div wordLength + num + 2;
  in
    csegPutWord (seg, Word.fromInt constAddr, value)
  end;

   (* Adds the constants onto the code, and copies the code into a new segment *)
  fun copyCode (cvec: code as Code{ printAssemblyCode, printStream, ...}) : address =
  let
    (* Pad out to long word boundary. Don't just leave as zero because, if
       the last instruction (return) had a zero argument, this could give
       a whole word of zero, which would mess up the garbage-collector. 
    *)
    (* Now round up to 8 byte boundary.  This makes porting to a 64 bit
       machine much simpler. DCJM 22/9/00. *)
    val alignTo = if wordLength < 8 then 8 else wordLength;
    val () = 
       while (getAddr (! (ic cvec)) mod alignTo) <> 0 do
          genByte (opcode_down opcode_pad, cvec);

    (* This also aligns ic onto a fullword boundary. *)
    val endIC    = !(ic cvec); (* Remember end *)
    val () = genBytes (0, wordLength, cvec); (* Marker *)

    (* +4 for code size, profile count, function name and constants count *)
    val numOfConst = !(numOfConsts cvec);
    val endOfCode : int = getAddr (! (ic cvec)) div wordLength;
    val segSize   : int = endOfCode + numOfConst + 4;

    (* fix-up all the constant loads (or indirections) *)
    val () = fixupConstantLoads (cvec, endIC, !(constLoads cvec));

    (* Now make the byte segment that we'll turn into the code segment *)
    val seg : cseg = csegMake(Word.fromInt segSize)
    val ()   = resultSeg cvec := Set seg;
    
    (* Copy the code into the new segment. *)
    val () = csegCopySeg (codeVec cvec, seg, Word.fromInt(getAddr (!(ic cvec))), 0w0);

    (* Byte offset of start of code. *)
    local
      val byteEndOfCode = endOfCode * wordLength;
      val addr = mkAddr byteEndOfCode;
    in
      val () = setLong (byteEndOfCode, addr, seg); 
    end;
    
    (* Put in the number of constants. This must go in before
       we actually put in any constants. *)
    local
      val addr = mkAddr ((segSize - 1) * wordLength);
    in
      val () = setLong (numOfConst + 1, addr, seg) 
    end;
    
    (* Next the profile count. *)
    local
      val addr = mkAddr ((endOfCode + 1) * wordLength);
    in
      val () = setLong (0, addr, seg) 
    end;

    (* Now we've filled in all the C integers; now we need to convert the segment
      into a proper code segment before it's safe to put in any ML values.
      SPF 13/2/97
    *)
    val () = csegConvertToCode seg;

    local
      (* why do we treat the empty string as a special case? SPF 15/7/94 *)
      (* This is so that profiling can print "<anon>". Note that a
         tagged zero *is* a legal string (it's "\000"). SPF 14/10/94 *)
      val name     : string = procName cvec;
      val nameWord : machineWord = if name = "" then toMachineWord 0 else toMachineWord name;
    in
      val () = csegPutWord (seg, Word.fromInt endOfCode + 0w2, nameWord)
    end;


    (* and then copy the objects from the constant list. *)
    fun putLocalConsts []      _ = ()
      | putLocalConsts (WVal w::cs) num =
        (constLabels (cvec, num, w); putLocalConsts cs (num + 1))
    
    val () = putLocalConsts (! (constVec cvec)) 1;

    val () = 
      if printAssemblyCode
      then let (* print out the code *)
    val () = printCode (seg, procName cvec, getAddr endIC, printStream);
    (* Skip: byte offset of start of code segment, 
         number of constants,
         profiling word,
         name of code segment
    *)
    val constants : const list = ! (constVec cvec);
    val () = printConstants (getAddr endIC + 4*wordLength, constants, printStream);
      in
         printStream"\n"
      end
      else ();
  in
    csegLockAndGetExecutable seg
  end (* copyCode *)

  (* ic function exported to GCODE *)
  val ic : code -> addrs = 
    fn (cvec : code) =>
    let
      (* Make sure any pending stack resets are done. *)
      val () = resetSp cvec
    in
      ! (ic cvec)
    end;

  (* For export from the functor *)
  val jump       : opcode = opcode_jump;
  val jumpFalse  : opcode = opcode_jumpFalse;
  val setHandler : opcode = opcode_setHandler;
  val delHandler : opcode = opcode_delHandler;
end (* CODECONS functor body *)

end; (* structure-level let *)


