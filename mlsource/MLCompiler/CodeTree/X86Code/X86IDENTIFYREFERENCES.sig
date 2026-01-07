(*
    Copyright (c) 2016-18 David C.J. Matthews

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

signature X86IDENTIFYREFERENCES =
sig
    type x86ICode and reg and preg and basicBlock and controlFlow
    and argument and memoryIndex and regProperty and ccRef

    datatype outCCState = CCSet of ccRef | CCIndeterminate | CCUnchanged


    type intSet

    type regState =
    { 
        active: int, refs: int, pushState: bool, prop: regProperty
    }
    
    (* Simple basic block with register usage information. *)
    datatype extendedBasicBlock =
        ExtendedBasicBlock of
        {
            block: {instr: x86ICode, current: intSet, active: intSet, kill: intSet } list,
            flow: controlFlow,
            locals: intSet, (* Defined and used entirely within the block. *)
            imports: intSet, (* Defined outside the block, used inside it, but not needed afterwards. *)
            exports: intSet, (* Defined within the block, possibly used inside, but used outside. *)
            passThrough: intSet, (* Active throughout the block. May be referred to by it but needed afterwards. *)
            loopRegs: intSet, (* Destination registers for a loop.  They will be updated by this block. *)
            initialStacks: intSet, (* Stack items required at the start i.e. imports+passThrough for stack items. *)
            inCCState: ccRef option, (* The state this block assumes.  If SOME _ all predecessors must set it. *)
            outCCState: ccRef option (* The condition code set by this block.  SOME _ if at least one successor needs it. *)
        }

    val identifyRegisters: basicBlock vector * regProperty vector -> extendedBasicBlock vector * regState vector
    
    (* Get the registers for an instruction.  *)
    val getInstructionRegisters: x86ICode -> { sources: preg list, dests: preg list }
    val getInstructionCC: x86ICode -> outCCState
    val argRegs: argument -> preg list
    and argIndex: memoryIndex -> preg list

    structure Sharing:
    sig
        type x86ICode           = x86ICode
        and reg                 = reg
        and preg                = preg
        and intSet              = intSet
        and basicBlock          = basicBlock
        and extendedBasicBlock  = extendedBasicBlock
        and controlFlow         = controlFlow
        and argument            = argument
        and memoryIndex         = memoryIndex
        and regProperty         = regProperty
        and ccRef               = ccRef
        and outCCState          = outCCState
    end;
end;
