(*
    Title:      Standard Basis Library: Pack Real structures for 32-bit platforms
    Author:     David Matthews
    Copyright   David Matthews 2021

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    Licence version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    Licence along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)


structure PackReal32Big: PACK_REAL =
    PackRealBoxed(type realType = Real32.real val isBigEndian = true val realSize = 0w4)
and PackReal32Little: PACK_REAL =
    PackRealBoxed(type realType = Real32.real val isBigEndian = false val realSize = 0w4);
