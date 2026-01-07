(*
    Title:      Standard Basis Library: Timer Signature and structure.
    Author:     David Matthews
    Copyright   David Matthews 2000, 2005, 2008, 2017

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

(* G&R 2004 status: updated.  Added checkCPUTimes. *)

signature TIMER =
sig
    type cpu_timer
    type real_timer
    val startCPUTimer : unit -> cpu_timer
    val checkCPUTimer : cpu_timer -> {usr : Time.time, sys : Time.time}
    val checkGCTime : cpu_timer -> Time.time
    val totalCPUTimer : unit -> cpu_timer
    val startRealTimer : unit -> real_timer
    val checkRealTimer : real_timer -> Time.time
    val totalRealTimer : unit -> real_timer
    
    val checkCPUTimes : cpu_timer ->
        {
            nongc: { usr : Time.time, sys : Time.time},
            gc: { usr : Time.time, sys : Time.time}
        }
end

structure Timer :> TIMER =
struct
    type cpu_timer = {userTime: Time.time, sysTime: Time.time, gcUTime: Time.time, gcSTime: Time.time }
    type real_timer = Time.time

    local
        open Time
        val getUserTime: unit -> Time.time = RunCall.rtsCallFull0 "PolyTimingGetUser"
        and getSysTime: unit -> Time.time = RunCall.rtsCallFull0 "PolyTimingGetSystem"
        and getGCUTime: unit -> Time.time = RunCall.rtsCallFull0 "PolyTimingGetGCUser"
        and getGCSTime: unit -> Time.time = RunCall.rtsCallFull0 "PolyTimingGetGCSystem"
    in
        fun startCPUTimer () =
            {userTime=getUserTime(),
             sysTime=getSysTime(),
             gcUTime=getGCUTime(),
             gcSTime=getGCSTime() }
        and checkCPUTimer ({ userTime, sysTime, ...}: cpu_timer) =
            { usr = getUserTime() - userTime, sys = getSysTime() - sysTime}
        and checkGCTime ({ gcUTime, ...}: cpu_timer) = getGCUTime() - gcUTime
        and totalCPUTimer () =
            { userTime=Time.zeroTime, sysTime=Time.zeroTime, gcUTime=Time.zeroTime, gcSTime=Time.zeroTime }

        fun checkCPUTimes (timer as { gcUTime, gcSTime, ... }) =
            let
                val { usr, sys } = checkCPUTimer timer
                val gc_usr = getGCUTime() - gcUTime and gc_sys = getGCSTime() - gcSTime 
            in
                { gc = { usr = gc_usr, sys = gc_sys },
                  nongc = { usr = usr-gc_usr, sys = sys-gc_sys } }
            end
        
        val startRealTimer: unit -> Time.time = RunCall.rtsCallFull0 "PolyTimingGetReal"

        fun totalRealTimer() = Time.zeroTime
        and checkRealTimer t = startRealTimer() - t
    end

end;

(* Override the default printer so they're abstract. *)
local
    fun prettyCPUTimer _ _ (_: Timer.cpu_timer) = PolyML.PrettyString "?"
    and prettyRealTimer _ _ (_: Timer.real_timer) = PolyML.PrettyString "?"
in
    val () = PolyML.addPrettyPrinter prettyCPUTimer
    and () = PolyML.addPrettyPrinter prettyRealTimer
end;

