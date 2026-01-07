(*
    Title:      Standard Basis Library: Math Signature.
    Author:     David Matthews
    Copyright   David Matthews 2000

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

(* G&R 2004 status: checked, no change required. *)

signature MATH =
sig
    type real
    val pi : real 
    val e : real 
    val sqrt : real -> real 
    val sin : real -> real 
    val cos : real -> real 
    val tan : real -> real 
    val asin : real -> real 
    val acos : real -> real 
    val atan : real -> real 
    val atan2 : (real * real) -> real 
    val exp : real -> real 
    val pow : (real * real) -> real 
    val ln : real -> real 
    val log10 : real -> real 
    val sinh : real -> real 
    val cosh : real -> real 
    val tanh : real -> real 
end;
