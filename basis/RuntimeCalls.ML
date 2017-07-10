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
  val POLY_SYS_cmem_load_float     = 164
  val POLY_SYS_cmem_load_double    = 165
  val POLY_SYS_cmem_store_8        = 166
  val POLY_SYS_cmem_store_16       = 167
  val POLY_SYS_cmem_store_32       = 168
  val POLY_SYS_cmem_store_64       = 169
  val POLY_SYS_cmem_store_float    = 170
  val POLY_SYS_cmem_store_double   = 171
  val POLY_SYS_io_operation        = 189

end;
