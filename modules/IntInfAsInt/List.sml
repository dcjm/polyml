(*
    Title:      Rebuild the basis library: list
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

useBasis "ListSignature";

structure List: LIST =
struct
    open List
    val take = fn (l, i) => take(l, FixedInt.fromInt i)
    val tabulate = fn (n, f) => tabulate(FixedInt.fromInt n, fn q => f(FixedInt.toInt q))
    val length = fn l => FixedInt.toInt(length l)
    val nth = fn (l, i) => nth(l, FixedInt.fromInt i)
    val drop = fn (l, i) => drop(l, FixedInt.fromInt i)
end;

val length : 'a list -> int = List.length;
