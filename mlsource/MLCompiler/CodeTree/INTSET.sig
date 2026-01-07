(*
    Copyright (c) 2017, 2021 David C.J. Matthews

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

signature INTSET =
sig
    type intSet
    val emptySet: intSet
    val setToList: intSet -> int list
    val listToSet: int list -> intSet
    val removeFromSet: int * intSet -> intSet
    val union: intSet * intSet -> intSet
    val minus: intSet * intSet -> intSet
    val cardinality: intSet -> int
    val filterSet: (int->bool) -> intSet -> intSet
    val member: int * intSet -> bool
    val partition: (int -> bool) -> intSet -> intSet * intSet
end;
