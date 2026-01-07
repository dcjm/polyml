(*
    Title:      Poly/ML Statistics parser.
    Author:     David Matthews
    Copyright   David Matthews 2013, 2015-17, 2019

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

(*
    This is an interface to the statistics provided by the run-time system.
    For machine-independence and backwards compatibility they are encoded
    using ASN1 binary encodeing
*)

local
    open Asn1

    datatype statistic =
        UnknownStat
    |   CounterStat of { identifier: int, name: string, count: int }
    |   SizeStat of { identifier: int, name: string, size: LargeInt.int }
    |   TimeStat of { identifier: int, name: string, time: Time.time }
    |   UserStat of { identifier: int, name: string, count: int }

    datatype component =
        CounterValue of int
    |   ByteCount of LargeInt.int
    |   Time of Time.time
    |   UnknownComponent
    
    datatype gcState = MLCode | MinorGC of int | MajorGC of int | GCSharing of int | OtherState of int

    val emptySlice = Word8VectorSlice.full(Word8Vector.fromList [])

    fun convStats(v: Word8Vector.vector) =
    let
        fun parseStatistic p =
            case decodeItem p of
                SOME {tag = Application(0x1, Constructed), data, remainder} =>
                    (
                        case parseComponents({identifier=0, name="", value=UnknownComponent}, data) of
                            {identifier, name, value=CounterValue v} =>
                                (CounterStat{identifier=identifier, name=name, count=v}, remainder)
                        |   _ => (UnknownStat, remainder)
                    )

            |   SOME {tag = Application(0x2, Constructed), data, remainder} =>
                    (
                        case parseComponents({identifier=0, name="", value=UnknownComponent}, data) of
                            {identifier, name, value=ByteCount s} =>
                                (SizeStat{identifier=identifier, name=name, size=s}, remainder)
                        |   _ => (UnknownStat, remainder)
                    )

            |   SOME {tag = Application(0x3, Constructed), data, remainder} =>
                    (
                        case parseComponents({identifier=0, name="", value=UnknownComponent}, data) of
                            {identifier, name, value=Time t} =>
                                (TimeStat{identifier=identifier, name=name, time=t}, remainder)
                        |   _ => (UnknownStat, remainder)
                    )

            |   SOME {tag = Application(0xb, Constructed), data, remainder} =>
                    (
                        case parseComponents({identifier=0, name="", value=UnknownComponent}, data) of
                            {identifier, name, value=CounterValue c} =>
                                (UserStat{identifier=identifier, name=name, count=c}, remainder)
                        |   _ => (UnknownStat, remainder)
                    )

            |   SOME {remainder, ...} => (UnknownStat, remainder)

            |   NONE => (UnknownStat, emptySlice)


        and parseComponents(result as {identifier, name, value }, p) =
            if Word8VectorSlice.length p = 0
            then result
            else
            (
                case decodeItem p of
                    SOME {tag = Application(0x4, Primitive), data, remainder} =>
                        parseComponents({identifier=decodeInt data, name=name, value=value}, remainder)
                |   SOME {tag = Application(0x5, Primitive), data, remainder} =>
                        parseComponents({name=decodeString data, identifier=identifier, value=value}, remainder)
                |   SOME {tag = Application(0x6, Primitive), data, remainder} =>
                        parseComponents({identifier=identifier, name=name, value=CounterValue(decodeInt data)}, remainder)
                |   SOME {tag = Application(0x7, Primitive), data, remainder} =>
                        parseComponents({identifier=identifier, name=name, value=ByteCount(decodeLargeInt data)}, remainder)
                |   SOME {tag = Application(0x8, Constructed), data, remainder} =>
                        let
                            fun parseTime (t, p) =
                                if Word8VectorSlice.length p = 0
                                then t
                                else
                                (
                                    case decodeItem p of
                                        SOME {tag = Application(0x9, Primitive), data, remainder} =>
                                            parseTime(t + Time.fromSeconds(LargeInt.fromInt(decodeInt data)), remainder)
                                    |   SOME {tag = Application(0xa, Primitive), data, remainder} =>
                                            parseTime(t + Time.fromMicroseconds(LargeInt.fromInt(decodeInt data)), remainder)
                                    |   SOME {remainder, ...} => parseTime(t, remainder) (* Unknown *)
                                    |   NONE => t
                                )
                        in
                            parseComponents({identifier=identifier, name=name,
                                    value=Time(parseTime(Time.zeroTime, data))}, remainder)
                        end
                |   SOME {remainder, ...} => parseComponents(result, remainder)

                |   NONE => result
            )

        fun parseStatistics l =
            if Word8VectorSlice.length l = 0
            then []
            else
            let
                val (item, rest) = parseStatistic l
                val items = parseStatistics rest
            in
                item :: items
            end

        val stats =
            case decodeItem (Word8VectorSlice.full v) of
                SOME {tag = Application(0x0, Constructed), data, ...} => parseStatistics data
            |   _ => raise Fail "Statistics not available"
        
        fun extractCounter(n, l) =
            case List.find (fn CounterStat{identifier, ...} => identifier = n | _ => false) l of
                SOME(CounterStat{ count, ...}) => count
            |   _ => 0
        and extractSize(n, l) =
            case List.find (fn SizeStat{identifier, ...} => identifier = n | _ => false) l of
                SOME(SizeStat{ size, ...}) => size
            |   _ => 0
        and extractTime(n, l) =
            case List.find (fn TimeStat{identifier, ...} => identifier = n | _ => false) l of
                SOME(TimeStat{ time, ...}) => time
            |   _ => Time.zeroTime
        and extractUser(n, l) =
            case List.find (fn UserStat{identifier, ...} => identifier = n | _ => false) l of
                SOME(UserStat{ count, ...}) => count
            |   _ => 0
    in
        {
            threadsTotal = extractCounter(1, stats),
            threadsInML = extractCounter(2, stats),
            threadsWaitIO = extractCounter(3, stats),
            threadsWaitMutex = extractCounter(4, stats),
            threadsWaitCondVar = extractCounter(5, stats),
            threadsWaitSignal = extractCounter(6, stats),
            gcFullGCs = extractCounter(7, stats),
            gcPartialGCs = extractCounter(8, stats),
            sizeHeap = extractSize(9, stats),
            sizeHeapFreeLastGC = extractSize(10, stats),
            sizeHeapFreeLastFullGC = extractSize(11, stats),
            sizeAllocation = extractSize(12, stats),
            sizeAllocationFree = extractSize(13, stats),
            timeNonGCUser = extractTime(14, stats),
            timeNonGCSystem = extractTime(15, stats),
            timeGCUser = extractTime(16, stats),
            timeGCSystem = extractTime(17, stats),
            userCounters = Vector.tabulate(8, fn n => extractUser(n+18, stats)),
            gcSharePasses = extractCounter(28, stats),
            timeNonGCReal = extractTime(26, stats),
            timeGCReal = extractTime(27, stats),
            sizeCode = extractSize(29, stats),
            sizeStacks = extractSize(30, stats),
            gcState =
            let
                val pc = extractCounter(32, stats)
            in
                case extractCounter(31, stats) of
                    0 => MLCode | 1 => MinorGC pc | 2 => MajorGC pc | 3 => GCSharing pc | _ => OtherState pc
            end 
        }
    end
    
    val localStats = RunCall.rtsCallFull0 "PolyGetLocalStats"
    and remoteStats = RunCall.rtsCallFull1 "PolyGetRemoteStats"
in
    structure PolyML =
    struct
        open PolyML
        structure Statistics =
        struct
            datatype gcState = datatype gcState
            fun getLocalStats() = convStats(localStats())
            and getRemoteStats(pid: int) = convStats(remoteStats pid)
            
            val numUserCounters: unit -> int = RunCall.rtsCallFast0 "PolyGetUserStatsCount"
            val setUserCounter: int * int -> unit = RunCall.rtsCallFull2 "PolySetUserStat"
        end
    end
end;
