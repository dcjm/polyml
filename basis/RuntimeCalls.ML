(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further work copyright David C. J. Matthews 2012, 2015-16

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


(* This file should reflect the RTS entries in libpolyml/sys.h.
   When adding entries don't forget to update the table in Address.ML. *)
structure RuntimeCalls =
struct  
  (* 0 not used (unusable?) *)
  val POLY_SYS_alloc_store         = 11
  val POLY_SYS_alloc_uninit        = 12
  val POLY_SYS_raisex              = 14
  val POLY_SYS_str_compare         = 23 (* DCJM 21/3/06 *)
  val POLY_SYS_teststrgtr          = 26
  val POLY_SYS_teststrlss          = 27
  val POLY_SYS_teststrgeq          = 28
  val POLY_SYS_teststrleq          = 29
  val POLY_SYS_lockseg             = 47
  val POLY_SYS_emptystring         = 48
  val POLY_SYS_nullvector          = 49
  val POLY_SYS_eq_longword         = 53
  val POLY_SYS_geq_longword        = 55
  val POLY_SYS_leq_longword        = 56
  val POLY_SYS_gt_longword         = 57
  val POLY_SYS_lt_longword         = 58
  val POLY_SYS_atomic_reset        = 69 (* DCJM 18/9/12 *)
  val POLY_SYS_atomic_incr         = 70 (* DCJM 12/3/07 *)
  val POLY_SYS_atomic_decr         = 71 (* DCJM 12/3/07 *)
  val POLY_SYS_thread_self         = 72 (* DCJM 12/3/07 *)
  val POLY_SYS_plus_longword       = 74
  val POLY_SYS_minus_longword      = 75
  val POLY_SYS_mul_longword        = 76
  val POLY_SYS_div_longword        = 77
  val POLY_SYS_mod_longword        = 78
  val POLY_SYS_andb_longword       = 79
  val POLY_SYS_orb_longword        = 80
  val POLY_SYS_xorb_longword       = 81
  (*val POLY_SYS_kill_self           = 84*)
  val POLY_SYS_shift_left_longword         = 85
  val POLY_SYS_shift_right_longword        = 86
  val POLY_SYS_shift_right_arith_longword  = 87
  val POLY_SYS_longword_to_tagged          = 89
  val POLY_SYS_signed_to_longword          = 90
  val POLY_SYS_unsigned_to_longword        = 91
  val POLY_SYS_equal_short_arb     = 101
  val POLY_SYS_quotrem             = 104 (* DCJM 05/03/10 *)
  val POLY_SYS_xora                = 112 (* DCJM 21/6/00 *)
  val POLY_SYS_equala              = 113
  val POLY_SYS_ora                 = 114 (* DCJM 21/6/00 *)
  val POLY_SYS_anda                = 115 (* DCJM 21/6/00 *)
  val POLY_SYS_Real_str            = 117 (* DCJM 6/4/00 *)
  val POLY_SYS_Real_geq            = 118 (* DCJM 28/3/00 *)
  val POLY_SYS_Real_leq            = 119 (* DCJM 28/3/00 *)
  val POLY_SYS_Real_gtr            = 120 (* DCJM 28/3/00 *)
  val POLY_SYS_Real_lss            = 121 (* DCJM 28/3/00 *)
  val POLY_SYS_Real_eq             = 122 (* DCJM 28/3/00 *)
  val POLY_SYS_Real_neq            = 123 (* DCJM 28/3/00 *)
  val POLY_SYS_Add_real            = 125
  val POLY_SYS_Sub_real            = 126
  val POLY_SYS_Mul_real            = 127
  val POLY_SYS_Div_real            = 128
  val POLY_SYS_Abs_real            = 129
  val POLY_SYS_Neg_real            = 130
  val POLY_SYS_conv_real           = 133
  val POLY_SYS_real_to_int         = 134
  val POLY_SYS_int_to_real         = 135
  val POLY_SYS_fixed_to_real       = 142
  val POLY_SYS_stdin               = 148
  val POLY_SYS_stdout              = 149
  val POLY_SYS_set_string_length   = 151  (* DCJM 28/2/01 *)
  val POLY_SYS_get_first_long_word = 152  (* DCJM 28/2/01 *)
  val POLY_SYS_bytevec_eq          = 154  (* DCJM 13/1/10 *)
  val POLY_SYS_cmem_load_8         = 160
  val POLY_SYS_cmem_load_16        = 161
  val POLY_SYS_cmem_load_32        = 162
  val POLY_SYS_cmem_load_64        = 163
  val POLY_SYS_cmem_load_float     = 164
  val POLY_SYS_cmem_load_double    = 165
  val POLY_SYS_cmem_store_8        = 166
  val POLY_SYS_cmem_store_16       = 167
  val POLY_SYS_cmem_store_32       = 168
  val POLY_SYS_cmem_store_64       = 169
  val POLY_SYS_cmem_store_float    = 170
  val POLY_SYS_cmem_store_double   = 171
  val POLY_SYS_fixed_mul           = 182
  val POLY_SYS_fixed_quot          = 183
  val POLY_SYS_fixed_rem           = 184
  val POLY_SYS_fixed_div           = 185
  val POLY_SYS_fixed_mod           = 186
  val POLY_SYS_io_operation        = 189
  val POLY_SYS_move_words_overlap  = 193
  val POLY_SYS_move_words          = 195  (* DCJM 9/10/99 *)
  val POLY_SYS_shift_right_arith_word   = 196  (* DCJM 9/10/99 *)
  val POLY_SYS_move_bytes          = 198  (* DCJM 9/10/99 *)
  val POLY_SYS_move_bytes_overlap  = 199
  val POLY_SYS_stderr              = 202  (* SPF 10/11/1998 *)
  val POLY_SYS_is_big_endian       = 213  (* added 30/3/95 (DCJM) *)
  (*val POLY_SYS_bytes_per_word      = 214*)  (* added 30/3/95 (DCJM) *)
  val POLY_SYS_shift_right_word    = 216
  val POLY_SYS_string_length       = 223
  val POLY_SYS_touch_final         = 228
  val POLY_SYS_mul_word            = 238  (* Reinstated DCJM 9/10/99 *)
  val POLY_SYS_plus_word           = 239  (* Reinstated DCJM 9/10/99 *)
  val POLY_SYS_minus_word          = 240  (* Reinstated DCJM 9/10/99 *)
  val POLY_SYS_div_word            = 241  (* Reinstated DCJM 9/10/99 *)
  val POLY_SYS_or_word             = 242
  val POLY_SYS_and_word            = 243
  val POLY_SYS_xor_word            = 244
  val POLY_SYS_shift_left_word     = 245
  val POLY_SYS_mod_word            = 246  (* Reinstated DCJM 9/10/99 *)
  val POLY_SYS_assign_byte         = 254
  val POLY_SYS_assign_word         = 255

  (* val POLY_SYS_vecsize = 256 *)

(* Exception identifiers *)
  val EXC_interrupt   = 1
  val EXC_syserr      = 2
  val EXC_size        = 4
  val EXC_overflow    = 5
  val EXC_underflow   = 6
  val EXC_divide      = 7
  val EXC_conversion  = 8
  val EXC_XWindows    = 10
  val EXC_subscript   = 11
  val EXC_thread      = 12 (* DCJM 13/3/07. *)

  val EXC_foreign     = 23  (* nic 4/5/94 *)
  
  (* The following numbers are hard-coded into the ML compiler
     rather than the run-time system, but they're still hard to change. *)
  val EXC_Bind        = 100; (* SPF 17/11/94 *)
  val EXC_Match       = 101; (* SPF 17/11/94 *)
  val EXC_Fail        = 103; (* Added DCJM 23/7/00 *) (* Now also in the RTS. *)
end;
