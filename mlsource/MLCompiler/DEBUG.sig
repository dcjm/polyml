(*
    Copyright (c) 2013-2016, 2020-21 David C.J. Matthews

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

(* Signature for debugging flags *)
signature DEBUG =
sig
    val assemblyCodeTag : bool Universal.tag
    val bindingCounterTag : (unit -> FixedInt.int) Universal.tag
    val codetreeAfterOptTag : bool Universal.tag
    val codetreeTag : bool Universal.tag
    val compilerDebugTag: int Universal.tag
    val createPrintFunctionsTag : bool Universal.tag
    val debugTag : bool Universal.tag
    val defaults : Universal.universal list
    val errorDepthTag : FixedInt.int Universal.tag
    val fileNameTag : string Universal.tag
    val getParameter : 'a Universal.tag -> Universal.universal list -> 'a
    val icodeTag : bool Universal.tag
    val inlineFunctorsTag : bool Universal.tag
    val lineLengthTag : FixedInt.int Universal.tag
    val lineNumberTag : (unit -> FixedInt.int) Universal.tag
    val lowlevelOptimiseTag : bool Universal.tag
    val maxInlineSizeTag : FixedInt.int Universal.tag
    val narrowOverloadFlexRecordTag : bool Universal.tag
    val languageExtensionsTag : bool Universal.tag
    val offsetTag : (unit -> FixedInt.int) Universal.tag
    val parsetreeTag : bool Universal.tag
    val printDepthFunTag : (unit -> FixedInt.int) Universal.tag
    val profileAllocationTag : FixedInt.int Universal.tag
    val reportExhaustiveHandlersTag : bool Universal.tag
    val reportUnreferencedIdsTag : bool Universal.tag
    val reportDiscardedValuesTag: FixedInt.int Universal.tag
    val reportDiscardNone: FixedInt.int (* No reports *)
    and reportDiscardFunction: FixedInt.int (* Only report discarded functions *)
    and reportDiscardNonUnit: FixedInt.int (* Report discarding any non unit values *)
end;
