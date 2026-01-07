(*
    Title:  Extend the PolyML structure.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright (c) 2000-7
        Cambridge University Technical Services Limited

    Modified David C.J. Matthews 2008, 2015

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

local
in

    structure PolyML =
    (* We must not have a signature on the result otherwise print and makestring
       will be given polymorphic types and will only produce "?" *)

    struct
        open PolyML

        local
            (* Initialise the list with the existing start-up function. *)
            val onEntryList: (unit->unit) list ref = LibrarySupport.onEntryList
            and onEntryMutex = Thread.Mutex.mutex()

            (* Run the list in reverse order. *)
            fun runOnEntry [] = ()
              | runOnEntry (f :: b) = (runOnEntry b; f() handle _ => ());

            (* This wraps the function provided to PolyML.export and PolyML.exportPortable
               so that the library is initialised at start-up and finalised at close-down. *)
            fun runFunction f () =
            let
                val () = runOnEntry(! onEntryList); (* Perform start-up operations. *)
                (* Run the main program.  If it doesn't explicitly call OS.Process.exit then
                   use "success" as the normal result and "failure" if it raises an exception. *)
                val result = (f(); OS.Process.success) handle _ => OS.Process.failure (* Run the main function. *)
            in
                OS.Process.exit result (* Perform close-down actions. *)
            end
            
            val callExport: string * (unit->unit) -> unit = RunCall.rtsCallFull2 "PolyExport"
            and callExportP: string * (unit->unit) -> unit = RunCall.rtsCallFull2 "PolyExportPortable"
        in
            (* The equivalent of atExit except that functions are added to
               the list persistently and of course the functions are executed
               at start-up rather than close-down. *)
            (* Protect this with a mutex in case two threads try to add entries at the
               same time.  Very unlikely since this is really only called when building
               the basis library. *)
            val onEntry : (unit->unit) -> unit =
                ThreadLib.protect onEntryMutex LibrarySupport.addOnEntry
        
            (* Export functions - write out the function and everything reachable from it. *)
            fun export(filename, f) = callExport(filename, runFunction f)
            and exportPortable(filename, f) = callExportP(filename, runFunction f)
        end
        
        local
            (* shareCommonData needs to be able to take a value of any type. *)
            val callShare: word -> unit = RunCall.rtsCallFull1 "PolyShareCommonData"
        in
            fun shareCommonData(root: 'a): unit = callShare(RunCall.unsafeCast root)
        end

        (* ObjSize etc all take values of any type but we can't give the RTS call type 'a->int. *)
        local
            val callObjSize: word -> int = RunCall.rtsCallFull1 "PolyObjSize"
            and callShowSize: word -> int = RunCall.rtsCallFull1 "PolyShowSize"
            and callObjProfile: word -> int = RunCall.rtsCallFull1 "PolyObjProfile"
        in
            fun objSize(x:'a) = callObjSize(RunCall.unsafeCast x)
            and showSize(x:'a) = callShowSize(RunCall.unsafeCast x)
            and objProfile(x:'a) = callObjProfile(RunCall.unsafeCast x)
        end
    
        val fullGC: unit -> unit = RunCall.rtsCallFull0 "PolyFullGC"

        val pointerEq = RunCall.pointerEq

        val rtsVersion: unit -> int = RunCall.rtsCallFast0 "PolyGetPolyVersionNumber"
        
        local
            val doCall: int * unit -> string = RunCall.rtsCallFull2 "PolySpecificGeneral"
        in
            fun architecture(): string = doCall (12, ())
            fun rtsArgumentHelp(): string = doCall (19, ())
        end

        structure IntInf =
        struct
            val gcd: LargeInt.int * LargeInt.int -> LargeInt.int = RunCall.rtsCallFull2 "PolyGCDArbitrary"
            and lcm: LargeInt.int * LargeInt.int -> LargeInt.int = RunCall.rtsCallFull2 "PolyLCMArbitrary"
        end
    end

end;
