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


(* This file should reflect the RTS entries in libpolyml/sys.h. *)
structure RuntimeCalls =
struct  
  (* 0 not used (unusable?) *)
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
  val POLY_SYS_io_operation        = 189

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
