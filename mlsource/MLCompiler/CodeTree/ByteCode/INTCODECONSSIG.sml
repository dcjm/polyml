(*
    Copyright (c) 2016 David C.J. Matthews

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

signature INTCODECONSSIG =
sig
    type machineWord = Address.machineWord
    type address = Address.address
    type code
    type opcode
    type addrs
    type labels

    val noJump: labels

    val jumpFalse  : opcode
    val jump       : opcode
    val setHandler : opcode
    val delHandler : opcode
    
    val opcode_notBoolean: opcode
    val opcode_isTagged: opcode
    val opcode_equalWord: opcode

    val addrPlus  : addrs * int -> addrs
    val addrMinus : addrs * addrs -> int

    val codeCreate: string * Universal.universal list -> code  (* makes the initial segment. *)
  
    (* ic - Address for the next instruction in the segment. *)
    val ic: code -> addrs
  
    (* putBytes : puts "length" bytes of "val" into locations "addr", "addr"+1 *)
    val putBytes: int * int * addrs * code -> unit

   (* GEN- routines all put a value at the instruction counter and add
      an appropriate amount to it. *)

   (* genWord - Puts 2 bytes. *)
   val genWord : int * code -> unit
      
   (* gen... - put instructions and their operands. *)
   val genCallClosure : code -> unit
   val genRaiseEx     : code -> unit
   val genLock        : code -> unit
   val genLdexc       : code -> unit
   val genPushHandler : code -> unit
      
   val genReturn      : int * code -> unit
   val genGetStore    : int * code -> unit
   val genLocal       : int * code -> unit
   val genIndirect    : int * code -> unit
   val genMoveToVec   : int * code -> unit
   val genSetStackVal : int * code -> unit
   val genCase        : int * code -> unit
   val genTuple       : int * code -> unit
   
   val genTailCall    : int * int * code -> unit
   val genNonLocal    : int * int * int * code -> unit
   
   val genRTSCallFast:    int * code -> unit
   val genRTSCallFull:    int * code -> unit
   val genRTSCallFastFloatFloat: code -> unit
   
   val genOpcode: opcode * code -> unit

   (* genEnter instructions are only needed when machine-code routines
      can call interpreted routines or vice-versa. The enterInt instruction
      causes the interpreter to be entered and the argument indicates the
      reason. *)
      
   val genEnterIntCatch : code -> unit
   val genEnterIntProc  : code * int -> unit
   val genEnterIntCall  : code * int -> unit
      
   (* pushConst - Generates code to push a constant. *)
   val pushConst        : machineWord * code -> unit

   (* Create a container on the stack *)
   val genContainer : int * code -> unit

   (* Create a tuple from a container. *)
   val genTupleFromContainer : int * code -> unit
      
   (* copyCode - Finish up after compiling a function. *)
   val copyCode : code -> address
   
   (* getBytes - gets "length" bytes from locations "addr", "addr"+1...
      Returns a negative number if the first bit was set. *)
   val getBytes: int * addrs * code -> int

   (* putBranchInstruction puts in an instruction which involves
      a forward reference. *)
   val putBranchInstruction: opcode * code -> labels
   
   (* Instruction to delete a handler and skip round it. *)
   val fixup: labels * code -> unit (* Fix up a forward reference. *)
   
   val linkLabels: labels * labels * code -> labels (* Link label lists. *)
   val jumpback: addrs * code -> unit (* Backwards jump. *)
   val resetStack: int * bool * code -> unit (* Set a pending reset *)
   val alignOffWord: code * int -> unit (* Add a pad byte if the value would
                                            be word-aligned. *)
end ;
