/*
    Title:  Definitions for the code-tree instructions.
    Author:     Dave Matthews, Cambridge University Computer Laboratory

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further development Copyright David C.J. Matthews 2015-18, 2020-23.

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

*/
#define INSTR_jump8                 0x02
#define INSTR_jump8false            0x03
#define INSTR_loadMLWord            0x04
#define INSTR_storeMLWord           0x05
#define INSTR_alloc_ref             0x06
#define INSTR_blockMoveWord         0x07
#define INSTR_loadUntagged          0x08
#define INSTR_storeUntagged         0x09
#define INSTR_case16                0x0a
#define INSTR_call_closure          0x0c
#define INSTR_return_w              0x0d
#define INSTR_stack_containerB      0x0e
#define INSTR_raise_ex              0x10
#define INSTR_callConstAddr16       0x11 // Legacy
#define INSTR_callConstAddr8        0x12 // Legacy
#define INSTR_local_w               0x13
#define INSTR_constAddr16_8         0x14
#define INSTR_constAddr8_8          0x15
#define INSTR_callLocalB            0x16
#define INSTR_callConstAddr8_8      0x17
#define INSTR_callConstAddr16_8     0x18
#define INSTR_constAddr16           0x1a // Legacy
#define INSTR_const_int_w           0x1b
#define INSTR_jump_back8            0x1e
#define INSTR_return_b              0x1f
#define INSTR_jump_back16           0x20
#define INSTR_indirectLocalBB       0x21
#define INSTR_local_b               0x22
#define INSTR_indirect_b            0x23
#define INSTR_moveToContainerB      0x24
#define INSTR_set_stack_val_b       0x25
#define INSTR_reset_b               0x26
#define INSTR_reset_r_b             0x27
#define INSTR_const_int_b           0x28
#define INSTR_local_0               0x29
#define INSTR_local_1               0x2a
#define INSTR_local_2               0x2b
#define INSTR_local_3       0x2c
#define INSTR_local_4       0x2d
#define INSTR_local_5       0x2e
#define INSTR_local_6       0x2f
#define INSTR_local_7       0x30
#define INSTR_local_8       0x31
#define INSTR_local_9       0x32
#define INSTR_local_10      0x33
#define INSTR_local_11      0x34
#define INSTR_indirect_0    0x35
#define INSTR_indirect_1    0x36
#define INSTR_indirect_2    0x37
#define INSTR_indirect_3    0x38
#define INSTR_indirect_4    0x39
#define INSTR_indirect_5    0x3a
#define INSTR_const_0       0x3b
#define INSTR_const_1       0x3c
#define INSTR_const_2       0x3d
#define INSTR_const_3       0x3e
#define INSTR_const_4       0x3f
#define INSTR_const_10      0x40
#define INSTR_return_1      0x42
#define INSTR_return_2      0x43
#define INSTR_return_3      0x44
#define INSTR_local_12      0x45
#define INSTR_jump8True     0x46
#define INSTR_jump16True    0x47
#define INSTR_local_13              0x49
#define INSTR_local_14              0x4a
#define INSTR_local_15              0x4b
#define INSTR_arbAdd                0x4c
#define INSTR_arbSubtract           0x4d
#define INSTR_arbMultiply           0x4e
#define INSTR_reset_1               0x50
#define INSTR_reset_2               0x51
#define INSTR_no_op                 0x52
#define INSTR_indirectClosureBB     0x54
#define INSTR_constAddr8_0          0x55
#define INSTR_constAddr8_1          0x56
#define INSTR_callConstAddr8_0      0x57
#define INSTR_callConstAddr8_1      0x58
#define INSTR_reset_r_1     0x64
#define INSTR_reset_r_2     0x65
#define INSTR_reset_r_3     0x66
#define INSTR_tuple_b       0x68
#define INSTR_tuple_2       0x69
#define INSTR_tuple_3       0x6a
#define INSTR_tuple_4       0x6b
#define INSTR_lock          0x6c
#define INSTR_ldexc         0x6d
#define INSTR_indirectContainerB    0x74
#define INSTR_moveToMutClosureB     0x75
#define INSTR_allocMutClosureB      0x76
#define INSTR_indirectClosureB0     0x77
#define INSTR_push_handler  0x78
#define INSTR_indirectClosureB1     0x7a
#define INSTR_tail_b_b      0x7b
#define INSTR_indirectClosureB2     0x7c
#define INSTR_setHandler8   0x81
#define INSTR_callFastRTS0      0x83
#define INSTR_callFastRTS1      0x84
#define INSTR_callFastRTS2      0x85
#define INSTR_callFastRTS3      0x86
#define INSTR_callFastRTS4      0x87
#define INSTR_callFastRTS5      0x88
#define INSTR_notBoolean        0x91
#define INSTR_isTagged          0x92
#define INSTR_cellLength        0x93
#define INSTR_cellFlags         0x94
#define INSTR_clearMutable      0x95
#define INSTR_atomicIncr        0x97 // Legacy
#define INSTR_atomicDecr        0x98 // Legacy
#define INSTR_equalWord         0xa0
#define INSTR_lessSigned        0xa2
#define INSTR_lessUnsigned      0xa3
#define INSTR_lessEqSigned      0xa4
#define INSTR_lessEqUnsigned    0xa5
#define INSTR_greaterSigned     0xa6
#define INSTR_greaterUnsigned   0xa7
#define INSTR_greaterEqSigned   0xa8
#define INSTR_greaterEqUnsigned 0xa9
#define INSTR_fixedAdd          0xaa
#define INSTR_fixedSub          0xab
#define INSTR_fixedMult         0xac
#define INSTR_fixedQuot         0xad
#define INSTR_fixedRem          0xae
#define INSTR_wordAdd           0xb1
#define INSTR_wordSub           0xb2
#define INSTR_wordMult          0xb3
#define INSTR_wordDiv           0xb4
#define INSTR_wordMod           0xb5
#define INSTR_wordAnd           0xb7
#define INSTR_wordOr            0xb8
#define INSTR_wordXor           0xb9
#define INSTR_wordShiftLeft     0xba
#define INSTR_wordShiftRLog     0xbb
#define INSTR_allocByteMem      0xbd
#define INSTR_indirectLocalB1       0xc1
#define INSTR_isTaggedLocalB        0xc2
#define INSTR_jumpNEqLocalInd       0xc3
#define INSTR_jumpTaggedLocal       0xc4
#define INSTR_jumpNEqLocal          0xc5
#define INSTR_indirect0Local0       0xc6
#define INSTR_indirectLocalB0       0xc7
#define INSTR_closureB              0xd0
#define INSTR_getThreadId       0xd9
#define INSTR_allocWordMemory   0xda
#define INSTR_loadMLByte        0xdc
#define INSTR_storeMLByte       0xe4
#define INSTR_enterIntArm64         0xe9
#define INSTR_blockMoveByte     0xec
#define INSTR_blockEqualByte    0xed
#define INSTR_blockCompareByte  0xee
#define INSTR_deleteHandler     0xf1
#define INSTR_jump16            0xf7
#define INSTR_jump16false       0xf8
#define INSTR_setHandler16      0xf9
#define INSTR_constAddr8        0xfa    // Legacy
#define INSTR_stackSize16       0xfc
#define INSTR_escape            0xfe
#define INSTR_enterIntX86       0xff

// Extended opcodes - preceded by escape
#define EXTINSTR_stack_containerW   0x0b
#define EXTINSTR_allocMutClosureW   0x0f
#define EXTINSTR_indirectClosureW   0x10
#define EXTINSTR_indirectContainerW 0x11
#define EXTINSTR_indirect_w         0x14
#define EXTINSTR_moveToContainerW   0x15
#define EXTINSTR_moveToMutClosureW  0x16
#define EXTINSTR_set_stack_val_w    0x17
#define EXTINSTR_reset_w            0x18
#define EXTINSTR_reset_r_w          0x19
#define EXTINSTR_callFastRRtoR      0x1c
#define EXTINSTR_callFastRGtoR      0x1d
#define EXTINSTR_loadPolyWord       0x20
#define EXTINSTR_loadNativeWord     0x21
#define EXTINSTR_storePolyWord      0x22
#define EXTINSTR_storeNativeWord    0x23
#define EXTINSTR_jump32True         0x48
#define EXTINSTR_floatAbs           0x56
#define EXTINSTR_floatNeg           0x57
#define EXTINSTR_fixedIntToFloat    0x58
#define EXTINSTR_floatToReal        0x59
#define EXTINSTR_realToFloat        0x5a
#define EXTINSTR_floatEqual         0x5b
#define EXTINSTR_floatLess          0x5c
#define EXTINSTR_floatLessEq        0x5d
#define EXTINSTR_floatGreater       0x5e
#define EXTINSTR_floatGreaterEq     0x5f
#define EXTINSTR_floatAdd           0x60
#define EXTINSTR_floatSub           0x61
#define EXTINSTR_floatMult          0x62
#define EXTINSTR_floatDiv           0x63
#define EXTINSTR_realToInt          0x6e
#define EXTINSTR_tuple_w            0x67
#define EXTINSTR_floatToInt         0x6f
#define EXTINSTR_callFastFtoF       0x70
#define EXTINSTR_callFastGtoF       0x71
#define EXTINSTR_callFastFFtoF      0x72
#define EXTINSTR_callFastFGtoF      0x73
#define EXTINSTR_realUnordered      0x79
#define EXTINSTR_floatUnordered     0x7a
#define EXTINSTR_tail               0x7c
#define EXTINSTR_arbCompare         0x7d
#define EXTINSTR_callFastRtoR       0x8f
#define EXTINSTR_callFastGtoR       0x90
#define EXTINSTR_createMutex        0x91
#define EXTINSTR_lockMutex          0x92
#define EXTINSTR_tryLockMutex       0x93
#define EXTINSTR_atomicExchAdd      0x96 // Legacy
#define EXTINSTR_atomicReset        0x99
#define EXTINSTR_longWToTagged      0x9a
#define EXTINSTR_signedToLongW      0x9b
#define EXTINSTR_unsignedToLongW    0x9c
#define EXTINSTR_realAbs            0x9d
#define EXTINSTR_realNeg            0x9e
#define EXTINSTR_fixedIntToReal     0x9f
#define EXTINSTR_fixedDiv           0xaf
#define EXTINSTR_fixedMod           0xb0
#define EXTINSTR_wordShiftRArith    0xbc
#define EXTINSTR_lgWordEqual        0xbe
#define EXTINSTR_lgWordLess         0xc0
#define EXTINSTR_lgWordLessEq       0xc1
#define EXTINSTR_lgWordGreater      0xc2
#define EXTINSTR_lgWordGreaterEq    0xc3
#define EXTINSTR_lgWordAdd          0xc4
#define EXTINSTR_lgWordSub          0xc5
#define EXTINSTR_lgWordMult         0xc6
#define EXTINSTR_lgWordDiv          0xc7
#define EXTINSTR_lgWordMod          0xc8
#define EXTINSTR_lgWordAnd          0xc9
#define EXTINSTR_lgWordOr           0xca
#define EXTINSTR_lgWordXor          0xcb
#define EXTINSTR_lgWordShiftLeft    0xcc
#define EXTINSTR_lgWordShiftRLog    0xcd
#define EXTINSTR_lgWordShiftRArith  0xce
#define EXTINSTR_realEqual          0xcf
#define EXTINSTR_closureW           0xd0
#define EXTINSTR_realLess           0xd1
#define EXTINSTR_realLessEq         0xd2
#define EXTINSTR_realGreater        0xd3
#define EXTINSTR_realGreaterEq      0xd4
#define EXTINSTR_realAdd            0xd5
#define EXTINSTR_realSub            0xd6
#define EXTINSTR_realMult           0xd7
#define EXTINSTR_realDiv            0xd8
#define EXTINSTR_loadC8             0xdd
#define EXTINSTR_loadC16            0xde
#define EXTINSTR_loadC32            0xdf
#define EXTINSTR_loadC64            0xe0
#define EXTINSTR_loadCFloat         0xe1
#define EXTINSTR_loadCDouble        0xe2
#define EXTINSTR_storeC8            0xe5
#define EXTINSTR_storeC16           0xe6
#define EXTINSTR_storeC32           0xe7
#define EXTINSTR_storeC64           0xe8
#define EXTINSTR_storeCFloat        0xe9
#define EXTINSTR_storeCDouble       0xea
#define EXTINSTR_log2Word           0xef
#define EXTINSTR_constAddr32_16     0xf0
#define EXTINSTR_jump32            0xf2
#define EXTINSTR_jump32False       0xf3
#define EXTINSTR_constAddr32       0xf4 // Legacy
#define EXTINSTR_setHandler32      0xf5
#define EXTINSTR_case32            0xf6
#define EXTINSTR_allocCSpace       0xfd
#define EXTINSTR_freeCSpace        0xfe
