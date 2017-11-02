(*
    Copyright (c) 2015-17 David C.J. Matthews
    
    Copyright (c) 2000
        Cambridge University Technical Services Limited

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

functor INTCODECONS (
structure DEBUG: DEBUGSIG

structure PRETTY: PRETTYSIG

) : INTCODECONSSIG =

struct
    open CODE_ARRAY
    open DEBUG
    open Address
    open Misc

    infix 5 << <<+ <<- >> >>+ >>- ~>> ~>>+ ~>>- (* Shift operators *)
    infix 3 andb orb xorb andbL orbL xorbL andb8 orb8 xorb8
    
    val op << = Word.<< and op >> = Word.>> and op ~>> = Word.~>>

    val wordToWord8 = Word8.fromLargeWord o Word.toLargeWord
    and word8ToWord = Word.fromLargeWord o Word8.toLargeWord

    (* Typically the compiler is built on a little-endian machine but it could
       be run on a machine with either endian-ness.  We have to find out the
       endian-ness when we run.  There are separate versions of the compiler
       for 32-bit and 64-bit so that can be a constant.  *)
    local
        val isBigEndian: unit -> bool = RunCall.rtsCallFast1 "PolyIsBigEndian"
    in
        val littleEndian = not o isBigEndian
    end

    val wordLength = RunCall.bytesPerWord
    
    val opcode_enterInt          = 0wx00
    and opcode_jump              = 0wx02    (* 8-bit unsigned jump forward. *)
    and opcode_jumpFalse         = 0wx03    (* Test top of stack. Take 8-bit unsigned jump if false. *)
    and opcode_delHandler        = 0wx05
    and opcode_alloc_ref         = 0wx06
    and opcode_case16            = 0wx0a
    and opcode_containerW        = 0wx0b
    and opcode_callClosure       = 0wx0c
    and opcode_returnW           = 0wx0d
    and opcode_pad               = 0wx0e
    and opcode_raiseEx           = 0wx10
    and opcode_getStoreW         = 0wx11
    and opcode_nonLocal          = 0wx12
    and opcode_localW            = 0wx13
    and opcode_indirectW         = 0wx14
    and opcode_moveToVecW        = 0wx15
    and opcode_setStackValW      = 0wx17
    and opcode_resetW            = 0wx18
    and opcode_resetR_w          = 0wx19
    and opcode_constAddr16       = 0wx1a
    and opcode_constIntW         = 0wx1b
    and opcode_jumpBack8         = 0wx1e   (* 8-bit unsigned jump backwards - relative to end of instr. *)
    and opcode_returnB           = 0wx1f
    and opcode_jumpBack16        = 0wx20    (* 16-bit unsigned jump backwards - relative to end of instr. *)
    and opcode_getStoreB         = 0wx21
    and opcode_localB            = 0wx22
    and opcode_indirectB         = 0wx23
    and opcode_moveToVecB        = 0wx24
    and opcode_setStackValB      = 0wx25
    and opcode_resetB            = 0wx26
    and opcode_resetRB           = 0wx27
    and opcode_constIntB         = 0wx28
    and opcode_local_0           = 0wx29
    and opcode_local_1           = 0wx2a
    and opcode_local_2           = 0wx2b
    and opcode_local_3           = 0wx2c
    and opcode_local_4           = 0wx2d
    and opcode_local_5           = 0wx2e
    and opcode_local_6           = 0wx2f
    and opcode_local_7           = 0wx30
    and opcode_local_8           = 0wx31
    and opcode_local_9           = 0wx32
    and opcode_local_10          = 0wx33
    and opcode_local_11          = 0wx34
    and opcode_indirect_0        = 0wx35
    and opcode_indirect_1        = 0wx36
    and opcode_indirect_2        = 0wx37
    and opcode_indirect_3        = 0wx38
    and opcode_indirect_4        = 0wx39
    and opcode_indirect_5        = 0wx3a
    and opcode_const_0           = 0wx3b
    and opcode_const_1           = 0wx3c
    and opcode_const_2           = 0wx3d
    and opcode_const_3           = 0wx3e
    and opcode_const_4           = 0wx3f
    and opcode_const_10          = 0wx40
    and opcode_return_0          = 0wx41
    and opcode_return_1          = 0wx42
    and opcode_return_2          = 0wx43
    and opcode_return_3          = 0wx44
    and opcode_moveToVec_0       = 0wx45
    and opcode_moveToVec_1       = 0wx46
    and opcode_moveToVec_2       = 0wx47
    and opcode_moveToVec_3       = 0wx48
    and opcode_moveToVec_4       = 0wx49
    and opcode_moveToVec_5       = 0wx4a
    and opcode_moveToVec_6       = 0wx4b
    and opcode_moveToVec_7       = 0wx4c
    and opcode_reset_1           = 0wx50
    and opcode_reset_2           = 0wx51
    and opcode_getStore_2        = 0wx52
    and opcode_getStore_3        = 0wx53
    and opcode_getStore_4        = 0wx54
    and opcode_tuple_containerW  = 0wx55
    and opcode_nonLocalL_1       = 0wx56
    and opcode_nonLocalL_2       = 0wx57
    and opcode_nonLocalL_3       = 0wx58
    and opcode_resetR_1          = 0wx64
    and opcode_resetR_2          = 0wx65
    and opcode_resetR_3          = 0wx66
    and opcode_tupleW            = 0wx67
    and opcode_tupleB            = 0wx68
    and opcode_tuple_2           = 0wx69
    and opcode_tuple_3           = 0wx6a
    and opcode_tuple_4           = 0wx6b
    and opcode_lock              = 0wx6c
    and opcode_ldexc             = 0wx6d
    and opcode_pushHandler       = 0wx78
    and opcode_tailbb            = 0wx7b
    and opcode_tail              = 0wx7c
    and opcode_tail3b            = 0wx7d
    and opcode_tail4b            = 0wx7e
    and opcode_tail3_2           = 0wx7f
    and opcode_tail3_3           = 0wx80
    and opcode_setHandler        = 0wx81

    and opcode_callFastRTS0      = 0wx83
    and opcode_callFastRTS1      = 0wx84
    and opcode_callFastRTS2      = 0wx85
    and opcode_callFastRTS3      = 0wx86
    and opcode_callFastRTS4      = 0wx87
    and opcode_callFastRTS5      = 0wx88

    and opcode_callFullRTS0      = 0wx89
    and opcode_callFullRTS1      = 0wx8a
    and opcode_callFullRTS2      = 0wx8b
    and opcode_callFullRTS3      = 0wx8c
    and opcode_callFullRTS4      = 0wx8d
    and opcode_callFullRTS5      = 0wx8e

    and opcode_callFastRTSFtoF   = 0wx8f
    and opcode_callFastRTSGtoF   = 0wx90
    
    and opcode_notBoolean        = 0wx91
    and opcode_isTagged          = 0wx92
    and opcode_cellLength        = 0wx93
    and opcode_cellFlags         = 0wx94
    and opcode_clearMutable      = 0wx95
    and opcode_atomicIncr        = 0wx97
    and opcode_atomicDecr        = 0wx98
    and opcode_atomicReset       = 0wx99
    and opcode_longWToTagged     = 0wx9a
    and opcode_signedToLongW     = 0wx9b
    and opcode_unsignedToLongW   = 0wx9c
    and opcode_realAbs           = 0wx9d
    and opcode_realNeg           = 0wx9e
    and opcode_floatFixedInt     = 0wx9f
    
    and opcode_equalWord         = 0wxa0
    and opcode_notequalWord      = 0wxa1
    and opcode_lessSigned        = 0wxa2
    and opcode_lessUnsigned      = 0wxa3
    and opcode_lessEqSigned      = 0wxa4
    and opcode_lessEqUnsigned    = 0wxa5
    and opcode_greaterSigned     = 0wxa6
    and opcode_greaterUnsigned   = 0wxa7
    and opcode_greaterEqSigned   = 0wxa8
    and opcode_greaterEqUnsigned = 0wxa9

    and opcode_fixedAdd          = 0wxaa
    and opcode_fixedSub          = 0wxab
    and opcode_fixedMult         = 0wxac
    and opcode_fixedQuot         = 0wxad
    and opcode_fixedRem          = 0wxae
    and opcode_fixedDiv          = 0wxaf
    and opcode_fixedMod          = 0wxb0
    and opcode_wordAdd           = 0wxb1
    and opcode_wordSub           = 0wxb2
    and opcode_wordMult          = 0wxb3
    and opcode_wordDiv           = 0wxb4
    and opcode_wordMod           = 0wxb5
    and opcode_wordAnd           = 0wxb7
    and opcode_wordOr            = 0wxb8
    and opcode_wordXor           = 0wxb9
    and opcode_wordShiftLeft     = 0wxba
    and opcode_wordShiftRLog     = 0wxbb
    and opcode_wordShiftRArith   = 0wxbc
    and opcode_allocByteMem      = 0wxbd
    and opcode_lgWordEqual       = 0wxbe
    and opcode_lgWordNotequal    = 0wxbf
    and opcode_lgWordLess        = 0wxc0
    and opcode_lgWordLessEq      = 0wxc1
    and opcode_lgWordGreater     = 0wxc2
    and opcode_lgWordGreaterEq   = 0wxc3
    and opcode_lgWordAdd         = 0wxc4
    and opcode_lgWordSub         = 0wxc5
    and opcode_lgWordMult        = 0wxc6
    and opcode_lgWordDiv         = 0wxc7
    and opcode_lgWordMod         = 0wxc8
    and opcode_lgWordAnd         = 0wxc9
    and opcode_lgWordOr          = 0wxca
    and opcode_lgWordXor         = 0wxcb
    and opcode_lgWordShiftLeft   = 0wxcc
    and opcode_lgWordShiftRLog   = 0wxcd
    and opcode_lgWordShiftRArith = 0wxce
    and opcode_realEqual         = 0wxcf
    and opcode_realNotequal      = 0wxd0
    and opcode_realLess          = 0wxd1
    and opcode_realLessEq        = 0wxd2
    and opcode_realGreater       = 0wxd3
    and opcode_realGreaterEq     = 0wxd4
    and opcode_realAdd           = 0wxd5
    and opcode_realSub           = 0wxd6
    and opcode_realMult          = 0wxd7
    and opcode_realDiv           = 0wxd8
    and opcode_getThreadId       = 0wxd9
    and opcode_allocWordMemory   = 0wxda
    and opcode_loadMLWord        = 0wxdb
    and opcode_loadMLByte        = 0wxdc
    and opcode_loadC8            = 0wxdd
    and opcode_loadC16           = 0wxde
    and opcode_loadC32           = 0wxdf
    and opcode_loadC64           = 0wxe0
    and opcode_loadCFloat        = 0wxe1
    and opcode_loadCDouble       = 0wxe2
    and opcode_storeMLWord       = 0wxe3
    and opcode_storeMLByte       = 0wxe4
    and opcode_storeC8           = 0wxe5
    and opcode_storeC16          = 0wxe6
    and opcode_storeC32          = 0wxe7
    and opcode_storeC64          = 0wxe8
    and opcode_storeCFloat       = 0wxe9
    and opcode_storeCDouble      = 0wxea
    and opcode_blockMoveWord     = 0wxeb
    and opcode_blockMoveByte     = 0wxec
    and opcode_blockEqualByte    = 0wxed
    and opcode_blockCompareByte  = 0wxee
    and opcode_loadUntagged      = 0wxef
    and opcode_storeUntagged     = 0wxf0
    and opcode_deleteHandler     = 0wxf1 (* Just deletes the handler - no jump. *)
    and opcode_jump32            = 0wxf2 (* 32-bit signed jump, forwards or backwards. *)
    and opcode_jump32False       = 0wxf3 (* Test top item. Take 32-bit signed jump if false. *)
    and opcode_constAddr32       = 0wxf4 (* Followed by a 32-bit offset. Load a constant at that address. *)
    and opcode_setHandler32      = 0wxf5 (* Setup a handler whose address is given by the 32-bit signed offset. *)
    and opcode_case32            = 0wxf6 (* Indexed case with 32-bit offsets *)
    and opcode_jump16            = 0wxf7
    and opcode_jump16False       = 0wxf8
    and opcode_setHandler16      = 0wxf9
    and opcode_constAddr8        = 0wxfa
    and opcode_stackSize8        = 0wxfb
    and opcode_stackSize16       = 0wxfc

 
    local
        val repArray : string Array.array = 
            Array.tabulate (256, fn (i) => "<UNKNOWN " ^ Int.toString i ^ ">");
      
        fun repUpdate (n, s) = Array.update (repArray, Word8.toInt n, s);

        val () = repUpdate(opcode_enterInt,     "enterInt");
        val () = repUpdate(opcode_jump,         "jump");
        val () = repUpdate(opcode_jumpFalse,    "jumpFalse");
        val () = repUpdate(opcode_delHandler,   "delHandler");
        val () = repUpdate(opcode_alloc_ref,    "alloc_ref");
        val () = repUpdate(opcode_case16,       "case16");
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
        val () = repUpdate(opcode_constAddr16,   "constAddr16");
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
        val () = repUpdate(opcode_storeMLWord, "storeMLWord")
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
        val () = repUpdate(opcode_loadUntagged, "loadUntagged")
        val () = repUpdate(opcode_deleteHandler, "deleteHandler")
        val () = repUpdate(opcode_jump32,       "jump32")
        val () = repUpdate(opcode_jump32False,  "jump32False")
        val () = repUpdate(opcode_constAddr32,  "constAddr32")
        val () = repUpdate(opcode_setHandler32, "setHandler32")
        val () = repUpdate(opcode_jump16,       "jump16")
        val () = repUpdate(opcode_case32,       "case32")
        val () = repUpdate(opcode_jump16False,  "jump16false")
        val () = repUpdate(opcode_setHandler16, "setHandler16")
        val () = repUpdate(opcode_constAddr8,   "constAddr8")
        val () = repUpdate(opcode_stackSize8,   "stackSize8")
        val () = repUpdate(opcode_stackSize16,  "stackSize16")
    in
        fun repr n : string = Array.sub (repArray, Word8.toInt n);
    end;


    local
        val sizeArray : int Array.array = Array.array (256, 1);

        fun sizeUpdate (n, s) = Array.update (sizeArray, Word8.toInt n, s);

        val () = sizeUpdate(opcode_enterInt    , 2);
        val () = sizeUpdate(opcode_jump        , 2);
        val () = sizeUpdate(opcode_jumpFalse   , 2);
        val () = sizeUpdate(opcode_delHandler  , 2);
        val () = sizeUpdate(opcode_case16      , 3);
        val () = sizeUpdate(opcode_returnW     , 3);
        val () = sizeUpdate(opcode_getStoreW   , 3);
        val () = sizeUpdate(opcode_nonLocal    , 7);
        val () = sizeUpdate(opcode_localW      , 3);
        val () = sizeUpdate(opcode_indirectW   , 3);
        val () = sizeUpdate(opcode_moveToVecW  , 3);
        val () = sizeUpdate(opcode_setStackValW, 3);
        val () = sizeUpdate(opcode_resetW      , 3);
        val () = sizeUpdate(opcode_resetR_w    , 3);
        val () = sizeUpdate(opcode_constAddr16  , 3);
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
        val () = sizeUpdate(opcode_tailbb      , 3);
        val () = sizeUpdate(opcode_tail        , 5);
        val () = sizeUpdate(opcode_tail3b      , 2);
        val () = sizeUpdate(opcode_tail4b      , 2);
        val () = sizeUpdate(opcode_case32      , 3);
        val () = sizeUpdate(opcode_jump32,       5)
        val () = sizeUpdate(opcode_jump32False,  5)
        val () = sizeUpdate(opcode_constAddr32,  5)
        val () = sizeUpdate(opcode_setHandler32, 5)
        val () = sizeUpdate(opcode_constAddr8  , 2);
        val () = sizeUpdate(opcode_stackSize8  , 2);
        val () = sizeUpdate(opcode_stackSize16 , 3);
    in
        fun size n = Array.sub (sizeArray, Word8.toInt n);
    end

    (* A Label is a ref that is later set to the location. *)
    type labels = {destination: Word.word ref }

    (* Used for jump, jumpFalse, setHandler and delHandler. *)
    datatype jumpTypes = Jump | JumpFalse | SetHandler

    datatype opcode =
        SimpleCode of Word8.word list           (* Bytes that don't need any special treatment *)
    |   LabelCode of labels                     (* A label - forwards or backwards. *)
    |   JumpInstruction of { label: labels, jumpType: jumpTypes, size : jumpSize ref }   (* Jumps or SetHandler. *)
    |   PushConstant of { constNum: int, size : jumpSize ref }
    |   IndexedCase of { labels: labels list, size : jumpSize ref }
    
    and jumpSize = Size8 | Size16 | Size32

    and code = Code of 
    {
        constVec:       machineWord list ref, (* Vector of words to be put at end *)
        procName:       string,         (* Name of the procedure. *)
        printAssemblyCode:bool,            (* Whether to print the code when we finish. *)
        printStream:    string->unit,    (* The stream to use *)
        stage1Code:     opcode list ref
    }

    (* create and initialise a code segment *)
    fun codeCreate (name : string, parameters) = 
    let
        val printStream = PRETTY.getSimplePrinter(parameters, []);
    in
        Code
        { 
            constVec         = ref [],
            procName         = name,
            printAssemblyCode = DEBUG.getParameter DEBUG.assemblyCodeTag parameters,
            printStream    = printStream,
            stage1Code       = ref []
        }
    end

    (* Find the offset in the constant area of a constant. *)
    (* The first has offset 0.                             *)
    fun addConstToVec (valu, Code{constVec, ...}) =
    let
        (* Search the list to see if the constant is already there. *)
        fun findConst valu [] num =
            (* Add to the list *)
            (
                constVec    := ! constVec @ [valu];
                num
            )
        |   findConst valu (h :: t) num =
                if wordEq (valu, h)
                then num
                else findConst valu t (num + 1) (* Not equal *)
    in
        findConst valu (! constVec) 0
    end

    fun printCode (seg: codeVec, procName: string, endcode, printStream) =
    let
        val () = printStream "\n";
        val () = if procName = "" (* No name *) then printStream "?" else printStream procName;
        val () = printStream ":\n";

        (* prints a string representation of a number *)
        fun printHex (v) = printStream(Word.fmt StringCvt.HEX v);
 
        val ptr = ref 0w0;
 
        (* To make sure we do not print branch extensions as though they
           were instructions we keep a list of all indirect forward references
           and print values at those addresses as addresses.
           This list is sorted with the lowest address first. *)
 
        val indirections = ref [];
 
        local
            fun addL (n, []) = [n]
            |   addL (n, l as (x :: xs)) =
                    if n < x then n :: l
                    else if n = x then l
                    else x :: addL (n, xs)
        in
            fun addInd (ind) = indirections := addL (ind, !indirections)
        end
        
        (* Gets "length" bytes from locations "addr", "addr"+1...
           Returns an unsigned number. *)
        fun getB (0, _, _) = 0w0
        |   getB (length, addr, seg) =
                (getB (length - 1, addr + 0w1, seg) << 0w8) + word8ToWord (codeVecGet (seg, addr))

        (* Prints a relative address. *)
        fun printDisp (len, spacer: string, addToList: bool) =
        let
            val ad = getB(len, !ptr, seg) + !ptr + Word.fromInt len
            val () = if addToList then addInd ad else ();
            val () = printStream spacer;
            val () = printHex ad;
        in
            ptr := !ptr + Word.fromInt len
        end

        (* Prints an operand of an instruction *)
        fun printOp (len, spacer : string) =
        let
            val () = printStream spacer;
            val () = printHex (getB (len, !ptr, seg))
        in
            ptr := !ptr + Word.fromInt len
        end;

    in
        while !ptr < endcode do
        let
            val addr = !ptr
        in
            printHex addr; (* The address. *)

            if (case !indirections of v :: _ => v = addr | [] => false)
            then
            let (* It's an address. *)
                val () = printDisp (2, "\t", false);
            in
                case !indirections of
                    _ :: vs => indirections := vs
                |   _       => raise InternalError "printCode: indirection list confused"
            end
      
            else
            let (* It's an instruction. *)
                val ()  = printStream "\t";
                val opc = codeVecGet (seg, !ptr) (* opcode *)
                val ()  = ptr := !ptr + 0w1;
                val ()  = printStream (repr opc);

                val sz = size opc;
            in
                if sz = 1 then ()
    
                else if opc = opcode_jump orelse
                     opc = opcode_jumpFalse orelse
                     opc = opcode_setHandler orelse
                     opc = opcode_delHandler orelse
                     opc = opcode_constAddr16 orelse
                     opc = opcode_jump32 orelse
                     opc = opcode_jump32False orelse
                     opc = opcode_setHandler32 orelse
                     opc = opcode_constAddr8 orelse
                     opc = opcode_constAddr32
                then printDisp (sz - 1, "\t", false)
      
                else if opc = opcode_jumpBack8 (* Should be negative *)
                then
                (
                    printStream "\t";
                    printHex((!ptr - 0w1) - getB(1, !ptr, seg));
                    ptr := !ptr + 0w1
                )

                else if opc = opcode_jumpBack16 (* Should be negative *)
                then
                (
                    printStream "\t";
                    printHex((!ptr - 0w1) - getB(2, !ptr, seg));
                    ptr := !ptr + 0w2
                )
     
                else if opc = opcode_nonLocal
                then ( printOp (2, "\t"); printOp (2, ","); printOp(2, ","))

                else if opc = opcode_case16
                then
                let
                    (* Have to find out how many items there are. *)
                    val limit = getB (2, !ptr, seg);
                    val () = printOp (2, "\t");
                    val base = !ptr;
        
                    fun printEntry _ = (printStream "\n\t"; printHex(base + getB(2, !ptr, seg)); ptr := !ptr + 0w2)
        
                    fun forLoop f i n = if i > n then () else (f i; forLoop f (i + 0w1) n)
                in
                    forLoop printEntry 0w0 limit
                end
         
                else if opc = opcode_tail
                then (printOp (2, "\t"); printOp (2, ","))
         
                else if opc = opcode_tailbb
                then (printOp (1, "\t"); printOp (1, ","))
                
                else printOp (sz - 1, "\t")
            end; (* an instruction. *)

            printStream "\n"
        end (* main loop *)
    end (* printCode *)
    
    fun codeSize (SimpleCode l) = List.length l
    |   codeSize (LabelCode _) = 0
    |   codeSize (JumpInstruction{size=ref Size8, ...}) = 2
    |   codeSize (JumpInstruction{size=ref Size16, ...}) = 3
    |   codeSize (JumpInstruction{size=ref Size32, ...}) = 5
    |   codeSize (PushConstant{size=ref Size8, ...}) = 2
    |   codeSize (PushConstant{size=ref Size16, ...}) = 3
    |   codeSize (PushConstant{size=ref Size32, ...}) = 5
    |   codeSize (IndexedCase{labels, size=ref Size32, ...}) = 3 + List.length labels * 4 
    |   codeSize (IndexedCase{labels, size=ref Size16, ...}) = 3 + List.length labels * 2 
    |   codeSize (IndexedCase{labels=_, size=ref Size8, ...}) = raise InternalError "codeSize"
   
    (* General function to process the code.  ic is the byte counter within the original code. *)
    fun foldCode foldFn n ops =
    let
        fun doFold(oper :: operList, ic, acc) =
            doFold(operList, ic + Word.fromInt(codeSize oper),
                foldFn(oper, ic, acc))
        |   doFold(_, _, n) = n
    in
        doFold(ops, 0w0, n)
    end

    (* Process the code, setting the destination of any labels.  Return the length of the code. *)    
    fun setLabels(LabelCode{destination, ...} :: ops, ic) = (destination := ic; setLabels(ops, ic))
    |   setLabels(oper :: ops, ic) = setLabels(ops, ic + Word.fromInt(codeSize oper))
    |   setLabels([], ic) = ic
    
    (* Set the sizes of branches depending on the distance to the destination. *)
    fun setLabelsAndSizes ops =
    let
        (* Set the labels and adjust the sizes, repeating until it never gets smaller*)
        fun setLabAndSize(ops, lastSize) =
        let
            (* Calculate offsets for constants. *)
            val endIC = Word.andb(lastSize + wordLength - 0w1, ~ wordLength)
            val firstConstant = endIC + wordLength * 0w3
            (* Because the constant area is word aligned we have to allow for
               the possibility that the distance between a "load constant"
               instruction and the target could actually increase. *)
            val alignment = wordLength - 0w1
        
            fun adjust(JumpInstruction{size as ref Size32, label={destination=ref dest}, ...}, ic, _) =
                let
                    val diff =
                        if dest <= ic (* N.B. Include infinite loops as backwards. *)
                        then ic - dest (* Backwards - Counts from start of instruction. *)
                        else dest - (ic + 0w5) (* Forwards - Relative to the current end. *)
                in
                    if diff < 0wx100
                    then size := Size8
                    else if diff < 0wx10000
                    then size := Size16
                    else ()
                end

            |   adjust(JumpInstruction{size as ref Size16, label={destination=ref dest}, ...}, ic, _) =
                if dest <= ic
                then if ic - dest < 0wx100 then size := Size8 else ()
                else if dest - (ic + 0w3)  < 0wx100 then size := Size8 else ()
        
            |   adjust(IndexedCase{size as ref Size32, labels}, ic, _) =
                let
                    val startAddr = ic+0w3
                    (* Use 16-bit case if all the offsets are 16-bits. *)
                    fun is16bit{destination=ref dest} =
                        dest > startAddr andalso dest < startAddr+0wx10000
                in
                    if List.all is16bit labels
                    then size := Size16
                    else ()
                end

            |   adjust(PushConstant{size as ref Size32, constNum, ...}, ic, _) =
                let
                    val constAddr = firstConstant + Word.fromInt constNum * wordLength
                    val offset = constAddr - (ic + 0w5)
                in
                    if offset < 0wx100-alignment then size := Size8
                    else if offset < 0wx10000-alignment then size := Size16
                    else ()
                end

            |   adjust(PushConstant{size as ref Size16, constNum, ...}, ic, _) =
                let
                    val constAddr = firstConstant + Word.fromInt constNum * wordLength
                    val offset = constAddr - (ic + 0w3)
                in
                    if offset < 0wx100-alignment then size := Size8
                    else ()
                end

            |   adjust _ = ()

            val () = foldCode adjust () ops
            val nextSize = setLabels(ops, 0w0)
        in
            if nextSize < lastSize then setLabAndSize(ops, nextSize)
            else if nextSize = lastSize then lastSize
            else raise InternalError "setLabAndSize - size increased"
        end
    in
        setLabAndSize(ops, setLabels(ops, 0w0))
    end
    
    fun genCode(ops, Code {constVec, ...}) =
    let
        (* First pass - set the labels. *)
        val codeSize = setLabelsAndSizes ops
        (* Align to wordLength. *)
        val endIC = Word.andb(codeSize + wordLength - 0w1, ~ wordLength)
        val endOfCode = endIC div wordLength
        val firstConstant = endIC + wordLength * 0w3 (* Add 3 for fn name, unused and profile count. *)
        val segSize   = endOfCode + Word.fromInt(List.length(! constVec)) + 0w4
        val codeVec = byteVecMake segSize

        val ic = ref 0w0
        
        fun genByte b = byteVecSet(codeVec, !ic, b) before ic := !ic + 0w1

        fun genByteCode(SimpleCode bytes, _, _) =
            (* Simple code - just generate the bytes. *)
                List.app genByte bytes

        |   genByteCode(LabelCode _, _, _) = ()

        |   genByteCode(JumpInstruction{label={destination=ref dest}, jumpType, size=ref Size32, ...}, ic, _) =
            let
                val opc =
                    case jumpType of
                        SetHandler => opcode_setHandler32
                    |   JumpFalse => opcode_jump32False
                    |   Jump => opcode_jump32
                val diff = dest - (ic + 0w5)
            in
                genByte opc;
                genByte(wordToWord8 diff);
                (* This may be negative so we must use an arithmetic shift. *)
                genByte(wordToWord8(diff ~>> 0w8));
                genByte(wordToWord8(diff ~>> 0w16));
                genByte(wordToWord8(diff ~>> 0w24))
            end

        |   genByteCode(JumpInstruction{label={destination=ref dest}, jumpType, size=ref Size16, ...}, ic, _) =
            if dest <= ic
            then (* Jump back. *)
            let
                val _ = jumpType = Jump orelse raise InternalError "genByteCode - back jump"
                val diff = ic - dest
                val _ = diff < 0wx10000 orelse raise InternalError "genByteCode - jump range"
            in
                genByte opcode_jumpBack16;
                genByte(wordToWord8 diff);
                genByte(wordToWord8(diff >> 0w8))
            end
            else
            let
                val opc =
                    case jumpType of
                        SetHandler => opcode_setHandler16
                    |   JumpFalse => opcode_jump16False
                    |   Jump => opcode_jump16
                val diff = dest - (ic + 0w3)
                val _ = diff < 0wx10000 orelse raise InternalError "genByteCode - jump range"
            in
                genByte opc;
                genByte(wordToWord8 diff);
                genByte(wordToWord8(diff >> 0w8))
            end

        |   genByteCode(JumpInstruction{label={destination=ref dest}, jumpType, size=ref Size8, ...}, ic, _) =
            if dest <= ic
            then (* Jump back. *)
            let
                val _ = jumpType = Jump orelse raise InternalError "genByteCode - back jump"
                val diff = ic - dest
                val _ = diff < 0wx100 orelse raise InternalError "genByteCode - jump range"
            in
                genByte opcode_jumpBack8;
                genByte(wordToWord8 diff)
            end
            else
            let
                val opc =
                    case jumpType of
                        SetHandler => opcode_setHandler
                    |   JumpFalse => opcode_jumpFalse
                    |   Jump => opcode_jump
                val diff = dest - (ic + 0w2)
                val _ = diff < 0wx100 orelse raise InternalError "genByteCode - jump range"
            in
                genByte opc;
                genByte(wordToWord8 diff)
            end

        |   genByteCode(PushConstant{ constNum, size=ref Size32, ... }, ic, _) =
            let
                val constAddr = firstConstant + Word.fromInt constNum * wordLength
                (* Offsets are calculated from the END of the instruction *)
                val offset = constAddr - (ic + 0w5)
            in
                genByte(opcode_constAddr32);
                genByte(wordToWord8 offset);
                genByte(wordToWord8(offset >> 0w8));
                genByte(wordToWord8(offset >> 0w16));
                genByte(wordToWord8(offset >> 0w24))
            end

        |   genByteCode(PushConstant{ constNum, size=ref Size16, ... }, ic, _) =
            let
                val constAddr = firstConstant + Word.fromInt constNum * wordLength
                val offset = constAddr - (ic + 0w3)
                val _ = offset < 0wx10000 orelse raise InternalError "genByteCode - constant range"
            in
                genByte(opcode_constAddr16);
                genByte(wordToWord8 offset);
                genByte(wordToWord8(offset >> 0w8))
            end

        |   genByteCode(PushConstant{ constNum, size=ref Size8, ... }, ic, _) =
            let
                val constAddr = firstConstant + Word.fromInt constNum * wordLength
                val offset = constAddr - (ic + 0w2)
                val _ = offset < 0wx100 orelse raise InternalError "genByteCode - constant range"
            in
                genByte(opcode_constAddr8);
                genByte(wordToWord8 offset)
            end
        
        |   genByteCode(IndexedCase{labels, size=ref Size32, ...}, ic, _) =
            let
                val nCases = List.length labels
                val () = genByte(opcode_case32)
                val () = genByte(Word8.fromInt nCases)
                val () = genByte(Word8.fromInt (nCases div 256))
                val startOffset = ic+0w3 (* Offsets are relative to here. *)

                fun putLabel{destination=ref dest} =
                let
                    val diff = dest - startOffset
                    val _ = dest > startOffset orelse raise InternalError "genByteCode - indexed case"
                in
                    genByte(wordToWord8 diff);
                    genByte(wordToWord8(diff >> 0w8));
                    genByte(wordToWord8(diff >> 0w16));
                    genByte(wordToWord8(diff >> 0w24))
                end
            in
                List.app putLabel labels
            end
        
        |   genByteCode(IndexedCase{labels, size=ref Size16, ...}, ic, _) =
            let
                val nCases = List.length labels
                val () = genByte(opcode_case16)
                val () = genByte(Word8.fromInt nCases)
                val () = genByte(Word8.fromInt (nCases div 256))
                val startOffset = ic+0w3 (* Offsets are relative to here. *)

                fun putLabel{destination=ref dest} =
                let
                    val diff = dest - startOffset
                    val _ = dest > startOffset orelse raise InternalError "genByteCode - indexed case"
                    val _ = diff < 0wx10000 orelse raise InternalError "genByteCode - indexed case"
                in
                    genByte(wordToWord8 diff);
                    genByte(wordToWord8(diff >> 0w8))
                end
            in
                List.app putLabel labels
            end
        
        |   genByteCode(IndexedCase{size=ref Size8, ...}, _, _) = raise InternalError "genByteCode - IndexedCase byte"
    in
        foldCode genByteCode () ops;
        (codeVec (* Return the completed code. *), endIC (* And the size. *))
    end

    fun setLong (value, addrs, seg) =
    let
        fun putBytes(value, a, seg, i) =
        if i = wordLength then ()
        else
        (
            byteVecSet(seg,
                if littleEndian() then a+i else a+wordLength-i-0w1,
                Word8.fromInt(value mod 256));
            putBytes(value div 256, a, seg, i+0w1)
        )
    in
        putBytes(value, addrs, seg, 0w0)
    end

    (* Adds the constants onto the code, and copies the code into a new segment *)
    fun copyCode (cvec as
                    Code{ printAssemblyCode, printStream,
                           procName, constVec, stage1Code, ...}, maxStack) : address =
    let
        local
            val revCode = List.rev(!stage1Code)
            (* Add a stack check. *)
            val stackCheck =
                if maxStack < 256
                then SimpleCode[opcode_stackSize8, Word8.fromInt maxStack]
                else SimpleCode[opcode_stackSize16, Word8.fromInt maxStack, Word8.fromInt(maxStack div 256)]
        in
            val codeList = stackCheck :: revCode
        end
        val (byteVec, endIC) = genCode(codeList, cvec)
  
        (* +3 for profile count, function name and constants count *)
        val numOfConst = List.length(! constVec)
        val endOfCode = endIC div wordLength
        val segSize   = endOfCode + Word.fromInt numOfConst + 0w4
        val firstConstant = endIC + wordLength * 0w3 (* Add 3 for fn name, unused and profile count. *)
    
        (* Put in the number of constants. This must go in before
           we actually put in any constants. *)
        local
            val addr = ((segSize - 0w1) * wordLength)
        in
            val () = setLong (numOfConst + 3, addr, byteVec) 
        end

        (* Now we've filled in all the size info we need to convert the segment
           into a proper code segment before it's safe to put in any ML values. *)
        val codeVec = byteVecToCodeVec byteVec

        local
            val name     : string = procName
            val nameWord : machineWord = toMachineWord name
        in
            val () = codeVecPutWord (codeVec, endOfCode, nameWord)
        end
        (* This used to be used on X86 for the register mask. *)
        val () = codeVecPutWord (codeVec, endOfCode+0w1, toMachineWord 1)
        (* Profile ref.  A byte ref used by the profiler in the RTS. *)
        local
            val v = RunCall.allocateByteMemory(0w1, Word.fromLargeWord(Word8.toLargeWord(Word8.orb(F_mutable, F_bytes))))
            fun clear 0w0 = ()
            |   clear i = (assignByte(v, i-0w1, 0w0); clear (i-0w1))
            val () = clear(Word.fromInt wordSize)
        in
            val () = codeVecPutWord (codeVec, endOfCode+0w2, toMachineWord v)
        end

        (* and then copy the constants from the constant list. *)
        local
            fun setConstant(value, num) =
            let
                val constAddr = (firstConstant div wordLength) + num
            in
                codeVecPutWord (codeVec, constAddr, value);
                num+0w1
            end
        in
            val _ = List.foldl setConstant 0w0 (!constVec)
        end
    in
        if printAssemblyCode
        then (* print out the code *)
            (printCode (codeVec, procName, endIC, printStream); printStream"\n")
        else ();
        codeVecLockAndGetExecutable codeVec
    end (* copyCode *)
    
    fun addItemToList(item, Code{stage1Code, ...}) = stage1Code := item :: !stage1Code

    val genOpcode = addItemToList
    
    fun putBranchInstruction(brOp, label, cvec) =
        addItemToList(JumpInstruction{label=label, jumpType=brOp, size = ref Size32}, cvec)

    fun setLabel(label, cvec) = addItemToList(LabelCode label, cvec)
    
    fun createLabel () = { destination=ref 0w0 }
    
    local
        fun genOpc(opc, cvec) = addItemToList(SimpleCode [opc], cvec)
        and genOpcByte(opc, arg1, cvec) = addItemToList(SimpleCode [opc, Word8.fromInt arg1], cvec)
        and genOpcWord(opc, arg1, cvec) =
            addItemToList(SimpleCode[opc, Word8.fromInt arg1, Word8.fromInt (arg1 div 256)], cvec)
    in
        fun genRaiseEx cvec = genOpc (opcode_raiseEx, cvec)
        fun genLock cvec = genOpc (opcode_lock, cvec)
        fun genLdexc cvec = genOpc (opcode_ldexc, cvec)
        fun genPushHandler cvec = genOpc (opcode_pushHandler, cvec)
    
        fun genRTSCallFast(0, cvec) = genOpc (opcode_callFastRTS0, cvec)
        |   genRTSCallFast(1, cvec) = genOpc (opcode_callFastRTS1, cvec)
        |   genRTSCallFast(2, cvec) = genOpc (opcode_callFastRTS2, cvec)
        |   genRTSCallFast(3, cvec) = genOpc (opcode_callFastRTS3, cvec)
        |   genRTSCallFast(4, cvec) = genOpc (opcode_callFastRTS4, cvec)
        |   genRTSCallFast(5, cvec) = genOpc (opcode_callFastRTS5, cvec)
        |   genRTSCallFast(_, _) = raise InternalError "genRTSFastCall"

        fun genRTSCallFull(0, cvec) = genOpc (opcode_callFullRTS0, cvec)
        |   genRTSCallFull(1, cvec) = genOpc (opcode_callFullRTS1, cvec)
        |   genRTSCallFull(2, cvec) = genOpc (opcode_callFullRTS2, cvec)
        |   genRTSCallFull(3, cvec) = genOpc (opcode_callFullRTS3, cvec)
        |   genRTSCallFull(4, cvec) = genOpc (opcode_callFullRTS4, cvec)
        |   genRTSCallFull(5, cvec) = genOpc (opcode_callFullRTS5, cvec)
        |   genRTSCallFull(_, _) = raise InternalError "genRTSCallFull"

        fun genContainer (size, cvec) = genOpcWord(opcode_containerW, size, cvec)
        and genTupleFromContainer (size, cvec) = genOpcWord(opcode_tuple_containerW, size, cvec)

        fun genCase (nCases, cvec) =
        let
            val labels = List.tabulate(nCases, fn _ => createLabel())
        in
            addItemToList(IndexedCase{labels=labels, size=ref Size32}, cvec);
            labels
        end
        
        (* For the moment don't try to merge stack resets. *)
        fun resetStack(0, _, _) = ()

        |   resetStack(offset, true, cvec) =
            if offset < 0 then raise InternalError "resetStack"
            else if offset > 255
            then genOpcWord(opcode_resetR_w, offset, cvec)
            else if offset > 3 then genOpcByte(opcode_resetRB, offset, cvec)
            else addItemToList(SimpleCode[opcode_resetR_1 + Word8.fromInt(offset - 1)], cvec)
            
        |   resetStack(offset, false, cvec) =
            if offset < 0 then raise InternalError "resetStack"
            else if offset > 255
            then genOpcWord(opcode_resetW, offset, cvec)
            else if offset > 2 then genOpcByte(opcode_resetB, offset, cvec)
            else addItemToList(SimpleCode[opcode_reset_1 + Word8.fromInt(offset - 1)], cvec)

        fun genCallClosure cvec = genOpc (opcode_callClosure, cvec)
    
        fun genTailCall (toslide, slideby, cvec) =
        if toslide < 256 andalso slideby < 256
        then case (toslide, slideby) of
            (3, 2) => genOpc (opcode_tail3_2, cvec)
        |   (3, 3) => genOpc (opcode_tail3_3, cvec)
        |   (3, _) => genOpcByte(opcode_tail3b, slideby, cvec)
        |   (4, _) => genOpcByte(opcode_tail4b, slideby, cvec)
        |   (_, _) => (* General byte case *)
                addItemToList(SimpleCode[opcode_tailbb, Word8.fromInt toslide, Word8.fromInt slideby], cvec)          
        else (* General case. *)
                addItemToList(
                    SimpleCode[opcode_tail, Word8.fromInt toslide, Word8.fromInt(toslide div 256),
                               Word8.fromInt slideby, Word8.fromInt (slideby div 256)], cvec)

        fun pushConst (value : machineWord, cvec) =
            if isShort value andalso toShort value < 0w32768
            then
            let
                val iVal = Word.toInt (toShort value);
            in
                if iVal = 10
                then genOpc (opcode_const_10, cvec)
      
                else if iVal <= 4
                then genOpc (opcode_const_0 + Word8.fromInt iVal, cvec)
  
                else if iVal < 256
                then genOpcByte (opcode_constIntB, iVal, cvec)
      
                else genOpcWord (opcode_constIntW, iVal, cvec)
            end

            else (* address or large short *)
                addItemToList(PushConstant{constNum = addConstToVec(value, cvec), size=ref Size32}, cvec)

        fun genRTSCallFastFloatFloat cvec = genOpc (opcode_callFastRTSFtoF, cvec)
        and genRTSCallFastGeneralFloat cvec = genOpc (opcode_callFastRTSGtoF, cvec)
    end
    
    local
        fun gen1 (opW, opB, opF, first, last, arg1, cvec) =
            if (first <= arg1 andalso arg1 <= last)
            then addItemToList(SimpleCode[opF + Word8.fromInt(arg1 - first)], cvec)

            else if 0 <= arg1 andalso arg1 <= 255
            then addItemToList(SimpleCode [opB, Word8.fromInt arg1], cvec)

            else addItemToList(
                SimpleCode [opW, Word8.fromInt arg1, Word8.fromInt(arg1 div 256)], cvec)
    in
        fun genReturn (arg1, cvec) =
            gen1 (opcode_returnW, opcode_returnB, opcode_return_0, 0, 3, arg1, cvec)

        fun genLocal (arg1, cvec) =
            gen1 (opcode_localW, opcode_localB, opcode_local_0, 0, 11, arg1, cvec);

        fun genIndirect (arg1, cvec) =
            gen1 (opcode_indirectW, opcode_indirectB, opcode_indirect_0, 0, 5, arg1, cvec);

        fun genMoveToVec (arg1, cvec) =
            gen1 (opcode_moveToVecW, opcode_moveToVecB, opcode_moveToVec_0, 0, 7, arg1, cvec);

        fun genSetStackVal (arg1, cvec) =
            gen1 (opcode_setStackValW, opcode_setStackValB,
                    opcode_setStackValB, (* Don't care - no "implied" form exists *) 1, 0, arg1, cvec);

        fun genTuple (arg1, cvec) =
            gen1 (opcode_tupleW, opcode_tupleB, opcode_tuple_2, 2, 4, arg1, cvec)
    end
    
    (* Unused - only used for bootstrapping. *)
    fun genEnterIntCatch _ = ()
    and genEnterIntProc _ = ()
    and genEnterIntCall _ = ()
    
    val opcode_notBoolean       = SimpleCode [opcode_notBoolean]
    val opcode_isTagged         = SimpleCode [opcode_isTagged]
    and opcode_cellLength       = SimpleCode [opcode_cellLength]
    and opcode_cellFlags        = SimpleCode [opcode_cellFlags]
    and opcode_clearMutable     = SimpleCode [opcode_clearMutable]
    and opcode_atomicIncr       = SimpleCode [opcode_atomicIncr]
    and opcode_atomicDecr       = SimpleCode [opcode_atomicDecr]
    and opcode_atomicReset      = SimpleCode [opcode_atomicReset]
    and opcode_longWToTagged    = SimpleCode [opcode_longWToTagged]
    and opcode_signedToLongW    = SimpleCode [opcode_signedToLongW]
    and opcode_unsignedToLongW  = SimpleCode [opcode_unsignedToLongW]
    and opcode_realAbs          = SimpleCode [opcode_realAbs]
    and opcode_realNeg          = SimpleCode [opcode_realNeg]
    and opcode_floatFixedInt    = SimpleCode [opcode_floatFixedInt]
    
    val opcode_equalWord        = SimpleCode [opcode_equalWord]
    val opcode_notequalWord     = SimpleCode [opcode_notequalWord] (* Not currently generated. *)
    and opcode_lessSigned       = SimpleCode [opcode_lessSigned]
    and opcode_lessUnsigned     = SimpleCode [opcode_lessUnsigned]
    and opcode_lessEqSigned     = SimpleCode [opcode_lessEqSigned]
    and opcode_lessEqUnsigned   = SimpleCode [opcode_lessEqUnsigned]
    and opcode_greaterSigned    = SimpleCode [opcode_greaterSigned]
    and opcode_greaterUnsigned  = SimpleCode [opcode_greaterUnsigned]
    and opcode_greaterEqSigned  = SimpleCode [opcode_greaterEqSigned]
    and opcode_greaterEqUnsigned = SimpleCode [opcode_greaterEqUnsigned]

    val opcode_fixedAdd         = SimpleCode [opcode_fixedAdd]
    val opcode_fixedSub         = SimpleCode [opcode_fixedSub]
    val opcode_fixedMult        = SimpleCode [opcode_fixedMult]
    val opcode_fixedQuot        = SimpleCode [opcode_fixedQuot]
    val opcode_fixedRem         = SimpleCode [opcode_fixedRem]
    val opcode_fixedDiv         = SimpleCode [opcode_fixedDiv]
    val opcode_fixedMod         = SimpleCode [opcode_fixedMod]
    val opcode_wordAdd          = SimpleCode [opcode_wordAdd]
    val opcode_wordSub          = SimpleCode [opcode_wordSub]
    val opcode_wordMult         = SimpleCode [opcode_wordMult]
    val opcode_wordDiv          = SimpleCode [opcode_wordDiv]
    val opcode_wordMod          = SimpleCode [opcode_wordMod]
    val opcode_wordAnd          = SimpleCode [opcode_wordAnd]
    val opcode_wordOr           = SimpleCode [opcode_wordOr]
    val opcode_wordXor          = SimpleCode [opcode_wordXor]
    val opcode_wordShiftLeft    = SimpleCode [opcode_wordShiftLeft]
    val opcode_wordShiftRLog    = SimpleCode [opcode_wordShiftRLog]
    val opcode_wordShiftRArith  = SimpleCode [opcode_wordShiftRArith]
    val opcode_allocByteMem     = SimpleCode [opcode_allocByteMem]
    val opcode_lgWordEqual      = SimpleCode [opcode_lgWordEqual]
    val opcode_lgWordNotequal   = SimpleCode [opcode_lgWordNotequal]
    val opcode_lgWordLess       = SimpleCode [opcode_lgWordLess]
    val opcode_lgWordLessEq     = SimpleCode [opcode_lgWordLessEq]
    val opcode_lgWordGreater    = SimpleCode [opcode_lgWordGreater]
    val opcode_lgWordGreaterEq  = SimpleCode [opcode_lgWordGreaterEq]
    val opcode_lgWordAdd        = SimpleCode [opcode_lgWordAdd]
    val opcode_lgWordSub        = SimpleCode [opcode_lgWordSub]
    val opcode_lgWordMult       = SimpleCode [opcode_lgWordMult]
    val opcode_lgWordDiv        = SimpleCode [opcode_lgWordDiv]
    val opcode_lgWordMod        = SimpleCode [opcode_lgWordMod]
    val opcode_lgWordAnd        = SimpleCode [opcode_lgWordAnd]
    val opcode_lgWordOr         = SimpleCode [opcode_lgWordOr]
    val opcode_lgWordXor        = SimpleCode [opcode_lgWordXor]
    val opcode_lgWordShiftLeft  = SimpleCode [opcode_lgWordShiftLeft]
    val opcode_lgWordShiftRLog  = SimpleCode [opcode_lgWordShiftRLog]
    val opcode_lgWordShiftRArith = SimpleCode [opcode_lgWordShiftRArith]
    val opcode_realEqual        = SimpleCode [opcode_realEqual]
    val opcode_realNotequal     = SimpleCode [opcode_realNotequal]
    val opcode_realLess         = SimpleCode [opcode_realLess]
    val opcode_realLessEq       = SimpleCode [opcode_realLessEq]
    val opcode_realGreater      = SimpleCode [opcode_realGreater]
    val opcode_realGreaterEq    = SimpleCode [opcode_realGreaterEq]
    val opcode_realAdd          = SimpleCode [opcode_realAdd]
    val opcode_realSub          = SimpleCode [opcode_realSub]
    val opcode_realMult         = SimpleCode [opcode_realMult]
    val opcode_realDiv          = SimpleCode [opcode_realDiv]
    val opcode_getThreadId      = SimpleCode [opcode_getThreadId]
    val opcode_allocWordMemory  = SimpleCode [opcode_allocWordMemory]
    val opcode_alloc_ref        = SimpleCode [opcode_alloc_ref]
    val opcode_loadMLWord       = SimpleCode [opcode_loadMLWord]
    val opcode_loadMLByte       = SimpleCode [opcode_loadMLByte]
    val opcode_loadC8           = SimpleCode [opcode_loadC8]
    val opcode_loadC16          = SimpleCode [opcode_loadC16]
    val opcode_loadC32          = SimpleCode [opcode_loadC32]
    val opcode_loadC64          = SimpleCode [opcode_loadC64]
    val opcode_loadCFloat       = SimpleCode [opcode_loadCFloat]
    val opcode_loadCDouble      = SimpleCode [opcode_loadCDouble]
    val opcode_loadUntagged     = SimpleCode [opcode_loadUntagged]
    val opcode_storeMLWord      = SimpleCode [opcode_storeMLWord]
    val opcode_storeMLByte      = SimpleCode [opcode_storeMLByte]
    val opcode_storeC8          = SimpleCode [opcode_storeC8]
    val opcode_storeC16         = SimpleCode [opcode_storeC16]
    val opcode_storeC32         = SimpleCode [opcode_storeC32]
    val opcode_storeC64         = SimpleCode [opcode_storeC64]
    val opcode_storeCFloat      = SimpleCode [opcode_storeCFloat]
    val opcode_storeCDouble     = SimpleCode [opcode_storeCDouble]
    val opcode_storeUntagged    = SimpleCode [opcode_storeUntagged]
    val opcode_blockMoveWord    = SimpleCode [opcode_blockMoveWord]
    val opcode_blockMoveByte    = SimpleCode [opcode_blockMoveByte]
    val opcode_blockEqualByte   = SimpleCode [opcode_blockEqualByte]
    val opcode_blockCompareByte = SimpleCode [opcode_blockCompareByte]
    val opcode_deleteHandler    = SimpleCode [opcode_deleteHandler]

end;

