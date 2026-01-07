(*
    Title:      Standard Basis Library: Date structure.
    Copyright   David Matthews 2000, 2016-17

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

structure Date :> DATE =
struct
    (* There seems to be an assumption, particularly in the "compare"
       function, that Date.date values are records of year, month, day
       etc. *)
    type date = {
        year: int, (* Signed year. *)
        month: int, (* Month as 0..11 *)
        day: int, (* Day as 1..(28, 29, 30, 31) *)
        hour: int, (* Hour as 0..23 *)
        minute: int, (* Minute as 0..59 *)
        second: int, (* Second as 0..59 (maybe 60 or 61 if leap) *)
        offset: Time.time option (* Offset as Time.time -24hrs<t<24hrs *)
        }

    datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    datatype month = Jan | Feb | Mar | Apr | May | Jun | 
                     Jul | Aug | Sep | Oct | Nov | Dec

    exception Date

    val secsPerHour: LargeInt.int = 60*60
    val secsPerDay: LargeInt.int = 24*secsPerHour
    val monthVec =
        Vector.fromList [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec];
    val dayOfWkVec =
        Vector.fromList [Sun, Mon, Tue, Wed, Thu, Fri, Sat]

    (* Vector of days from the beginning of the year. *)
    val dayVec: int Vector.vector =
        Vector.fromList [~1, 30, 58, 89, 119, 150, 180, 211, 242, 272, 303, 333, 365];
    val dayInLeapYearVec: int Vector.vector =
        Vector.fromList [~1, 30, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 366];

    (* Days of the week and months in abbreviated English form. *)
    val dayNames =
        Vector.fromList ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
    val monthNames =
        Vector.fromList ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

    (* Convert a month number to the enumerated type. *)
    fun monthNoToMonth n =
        Vector.sub(monthVec, n) handle Subscript => raise Date (* Should never happen *)

    fun isLeapYear(l: int): bool =
        if l mod 100 = 0 then (l div 100) mod 4 = 0 else l mod 4 = 0

    (* Convert the enumerated type to a month number. *)
    fun monthToMonthNo Jan =  0
     |  monthToMonthNo Feb =  1
     |  monthToMonthNo Mar =  2
     |  monthToMonthNo Apr =  3
     |  monthToMonthNo May =  4
     |  monthToMonthNo Jun =  5
     |  monthToMonthNo Jul =  6
     |  monthToMonthNo Aug =  7
     |  monthToMonthNo Sep =  8
     |  monthToMonthNo Oct =  9
     |  monthToMonthNo Nov = 10
     |  monthToMonthNo Dec = 11

    (* Get the local time offset which applied at the specific time.
       The time is in seconds since the epoch. The result may be the
       current time offset if it is outside the range for which we have
       information.  We use seconds as the argument and result here because
       it avoids having to multiply and divide arbitrary precision values
       in the RTS.  May raise Size if the value is too large (or small).  In
       that case we use the current time offset. *)
    local
        val getLocalOffset: LargeInt.int -> LargeInt.int = RunCall.rtsCallFull1 "PolyTimingLocalOffset"
    in
        fun localOffsetApplying (t: LargeInt.int) : LargeInt.int =
            getLocalOffset t 
                handle General.Size => getLocalOffset (Time.toSeconds(Time.now()))
    end

    (* Get the current local time offset. *)
    fun localOffset (): Time.time =
        Time.fromSeconds(localOffsetApplying(Time.toSeconds(Time.now())))

    local
        (* Time values are since 1st January of this year. *)
        val baseYear: int = RunCall.rtsCallFull0 "PolyTimingBaseYear" () (* 1601 or 1970 *)
        val yearOffset: int = RunCall.rtsCallFull0 "PolyTimingYearOffset" ()  (* The offset of zeroTime within that year. 0 on both Unix and Windows *)

        (* Get the day in the year.  Either of day or year may be unnormalised
           but that shouldn't affect the result (except if year is negative???) *)
        fun dayInYear (day, month, year) = 
            if isLeapYear year then Vector.sub(dayInLeapYearVec, month) + day
            else Vector.sub(dayVec, month) + day

        (* Compute the number of days since the start. *)
        fun yearToDays y =
        let
            fun ytod dys yr =
                if yr = baseYear then dys
                (* If the year is before the base year we subtract the number of
                   days in this year and recurse. *)
                else if yr < baseYear
                then if isLeapYear yr then ytod (dys-366) (yr+1)
                    else ytod (dys-365) (yr+1)
                (* The year we want is after the base year. *)
                else if yr - baseYear >= 100
                (* If it is more than a century apart then we can add in the
                   number of days in a century.  There are 24 leap years in most
                   centuries except those which are divisible by 400.
                   Note: We're assuming the Gregorian calendar. *)
                then if ((yr-1) div 100) mod 4 = 0 then ytod (dys+36525) (yr-100)
                        else ytod (dys+36524) (yr-100)
                else if isLeapYear(yr-1) then ytod (dys+366) (yr-1)
                else ytod (dys+365) (yr-1)
        in
            ytod 0 y
        end

        (* Convert days to number of years plus the day within the year. *)
        fun daysToYears d =
        let
            fun dtoy dys yr =
                if dys < 0
                then (* Before the base year: have to add in days. *)
                    if isLeapYear (yr-1) then dtoy (dys+366) (yr-1)
                    else dtoy (dys+365) (yr-1)
                (* If we are at least a century away we can subtract the century. *)
                else if dys >= 36525
                then if ((yr+99) div 100) mod 4 = 0 then dtoy (dys-36525) (yr+100)
                    else dtoy (dys-36524) (yr+100)
                else if isLeapYear yr
                then if dys >= 366 then dtoy (dys-366) (yr+1) else (yr, dys)
                else if dys >= 365 then dtoy (dys-365) (yr+1) else (yr, dys)
        in
            dtoy d baseYear
        end
    
        (* Convert a number of seconds to a date. *)
        fun fromSeconds t (tzOffset: Time.time option) : date =
        let
            val tsecs = t - LargeInt.fromInt yearOffset
            val secs = LargeInt.toInt(tsecs mod 60)
            val mins = LargeInt.toInt((tsecs div 60) mod 60)
            val hrs  = LargeInt.toInt((tsecs div secsPerHour) mod 24)
            (* Get the day and year.  The day is a value between 0 and 364/365. *)
            val (year, days) = daysToYears(LargeInt.toInt(tsecs div secsPerDay))
            (* Convert the day into a month+day *)
            val isLeap = isLeapYear year
            fun dayToMonth dy mth =
                if dy <= Vector.sub(if isLeap then dayInLeapYearVec else dayVec, mth+1)
                then mth
                else dayToMonth dy (mth+1)
    
            val month = dayToMonth days 0
            val dayInMonth =
                days - Vector.sub(if isLeap then dayInLeapYearVec else dayVec, month)
        in
            {year=year, month=month, day=dayInMonth, hour=hrs, minute=mins,
             second=secs, offset = tzOffset }
        end
    in
        (* Get the day in the year. *)
        fun yearDay({day, month, year, ...}:date) = dayInYear(day, month, year)

        (* Convert the date into a UTC time value. *)
        fun toTime (date as {year, hour, minute, second, offset, ...}) =
        let
            (* Compute the seconds. *)
            val secs =
                LargeInt.fromInt second + LargeInt.fromInt minute * 60 + LargeInt.fromInt hour * secsPerHour + 
                    LargeInt.fromInt(yearDay date + yearToDays year) * secsPerDay +
                    LargeInt.fromInt yearOffset
        in
            case offset of
                SOME t => Time.+(t, Time.fromSeconds secs)
              | NONE =>
                    Time.fromSeconds(secs + localOffsetApplying secs)
        end

        (* Convert a UTC time to a UTC date. *)
        fun fromTimeUniv t = fromSeconds (Time.toSeconds t) (SOME Time.zeroTime)

        (* Convert a UTC time to a date in the local time zone. *)
        fun fromTimeLocal t =
        let
            val secs = Time.toSeconds t
            val localOffset = localOffsetApplying secs
        in
            fromSeconds (secs-localOffset) NONE
        end
            
        (* Generate a normalised date. *)
        fun date {year, month, day, hour, minute, second, offset} =
        let
            (* Get the time zone information if it is provided.  If it is
               outside +/- 24 hours we get the number of full days. *)
            val (tzDays, normTz) =
                case offset of
                    SOME tz =>
                        let
                            open Time
                            val excess = LargeInt.quot(Time.toSeconds tz, secsPerDay)*secsPerDay;
                        in
                            (excess, SOME(tz-Time.fromSeconds excess))
                        end
                  | NONE => (0, NONE)
            (* Convert it to the number of seconds since the epoch which will
               normalise it. *)
            val secs =
                LargeInt.fromInt second + LargeInt.fromInt minute * 60 + LargeInt.fromInt hour * secsPerHour + 
                    LargeInt.fromInt (dayInYear(day, monthToMonthNo month, year) + yearToDays year) * secsPerDay +
                    LargeInt.fromInt yearOffset + tzDays
        in
            (* Convert it into a date. *)
            fromSeconds secs normTz
        end
            
    end

    val year: date->int = #year
    and day: date->int = #day
    and hour: date->int = #hour
    and minute: date->int = #minute
    and second: date->int = #second
    and offset: date->Time.time option = #offset

    (* Return the month from the enumerated type. *)
    fun month({month, ...}:date) = monthNoToMonth month

    (* Get the day of the week as a number - not exported. *)
    fun dayOfWeek({year, month, day, ...}: date) =
    let
        (* From looking at the code of mktime, which is marked as being in
           the public domain, this formula (Zeller's Congruence?) is used to
           find the day of the week for the first of any month.
           I don't know what range this works for but it seems accurate as
           far as I can test it. *)
        val m0 = month+1 (* Count months from 1 *)
        val m1 = (m0 + 9) mod 12 + 1
        val yy0 = if m0 <= 2 then year-1 else year
        val yy1 = yy0 div 100
        val yy2 = yy0 mod 100
        val dow = ((26*m1 - 2) div 10 + 1 + yy2 + yy2 div 4 + yy1 div 4 - 2*yy1) mod 7
    in
        (* Add on the day within the month. *)
        (dow + day - 1) mod 7
    end

    (* Get day of week as an enumerated type - exported. *)
    fun weekDay date = Vector.sub(dayOfWkVec, dayOfWeek date)

    (* QUESTION: The definition of isDst is very vague. I am assuming that it
       means that, for a local time, did/will Summer Time apply at that time?  *)
    val getSummer: LargeInt.int -> int = RunCall.rtsCallFull1 "PolyTimingSummerApplies"

    fun isDst (d as {offset=NONE, ...} : date): bool option =
    let
        val isSummer =
            getSummer (Time.toSeconds(toTime d)) handle Size => ~1
    in
        if isSummer < 0 then NONE
        else SOME (isSummer > 0)
    end
    |   isDst {offset=SOME _, ...} = SOME false (* ?? *)

    (* Compare the dates ignoring time zone information. *)
    fun compare({year=y1, month=m1, day=d1, hour=h1, minute=n1, second=s1, ...}:date,
                {year=y2, month=m2, day=d2, hour=h2, minute=n2, second=s2, ...}:date) =
        if y1 < y2 then General.LESS
        else if y1 > y2 then General.GREATER
        else if m1 < m2 then General.LESS
        else if m1 > m2 then General.GREATER
        else if d1 < d2 then General.LESS
        else if d1 > d2 then General.GREATER
        else if h1 < h2 then General.LESS
        else if h1 > h2 then General.GREATER
        else if n1 < n2 then General.LESS
        else if n1 > n2 then General.GREATER
        else Int.compare(s1, s2)

    (* Parse a date/time. *)
    fun scan getc str =
    let
        (* Try to extract an n-character string. *)
        fun getChars n str =
        let
            fun getN 0 s str = SOME (String.implode(List.rev s), str)
             |  getN n s str =
                case getc str of
                    NONE => NONE
                |   SOME(ch, str') => getN (n-1) (ch :: s) str'
        in
            getN n [] str
        end

        (* Get the day of the week.  We don't actually use it but we
           need to verify it. *)
        (* QUESTION: What time offset should be used?  I presume NONE. *)
        fun parseDayOfWeek str =
        case getChars 3 str of
            NONE => NONE
        |   SOME(s, str') =>
                if Vector.foldr (fn(s', t) => t orelse s=s') false dayNames
                then SOME(s, str') else NONE
        
        fun parseMonth str =
        case getChars 3 str of
            NONE => NONE
        |   SOME(s, str') =>
                (* Return the month corresponding to the month name
                   otherwise NONE. *)
                Vector.foldri (fn(n:int, s':string, t) =>
                    if s = s' then SOME(Vector.sub(monthVec, n), str') else t) NONE
                    monthNames

        (* Get a two digit number. *)
        fun parse2Digits str =
        case getc str of
            NONE => NONE
        |   SOME(ch0, str1) =>
            if ch0 < #"0" orelse ch0 > #"9" then NONE
            else case getc str1 of
                NONE => NONE
            |   SOME(ch1, str2) =>
                if ch1 < #"0" orelse ch1 > #"9" then NONE
                else SOME((Char.ord ch0 - Char.ord #"0")*10 +
                          (Char.ord ch1 - Char.ord #"0"), str2)

        (* Get two digits as a day of the month.  Don't check the range. *)
        val parseDayOfMonth = parse2Digits

        (* A time is written as hh:mm:ss *)
        fun parseTime str =
        case parse2Digits str of
            NONE => NONE
        |   SOME(hh, str1) =>
            case getc str1 of
                NONE => NONE
            |   SOME(ch, str2) =>
                if ch <> #":" then NONE
                else case parse2Digits str2 of
                    NONE => NONE
                |   SOME(mm, str3) =>
                    case getc str3 of
                        NONE => NONE
                    |   SOME(ch, str4) =>
                        if ch <> #":" then NONE
                        else case parse2Digits str4 of
                            NONE => NONE
                        |   SOME(ss, str5) =>
                                SOME((hh, mm, ss), str5)

        (* A year is represented as four digits. *)
        fun parseYear str =
        case parse2Digits str of
            NONE => NONE
        |   SOME(yy0, str1) =>
            case parse2Digits str1 of
                NONE => NONE
            |   SOME(yy1, str2) => SOME(yy0*100+yy1, str2)


        fun parseDate str =
        case parseDayOfWeek str of
            NONE => NONE
        |   SOME(_, str1) =>
            case getc str1 of (* Get exactly one space. *)
                NONE => NONE
            |   SOME(ch, str2) =>
                if ch <> #" " then NONE
                else case parseMonth str2 of (* Name of month. *)
                    NONE => NONE
                |   SOME(mth, str3) =>
                    case getc str3 of (* Get exactly one space. *)
                        NONE => NONE
                    |   SOME(ch, str4) =>
                        if ch <> #" " then NONE
                        else case parseDayOfMonth str4 of
                            NONE => NONE
                        |   SOME(day, str5) =>
                            case getc str5 of (* Get exactly one space. *)
                                NONE => NONE
                            |   SOME(ch, str6) =>
                                if ch <> #" " then NONE
                                else case parseTime str6 of
                                    NONE => NONE
                                |   SOME((hr,min,sec), str7) =>                 
                                    case getc str7 of (* Get exactly one space. *)
                                        NONE => NONE
                                    |   SOME(ch, str8) =>
                                        if ch <> #" " then NONE
                                        else case parseYear str8 of
                                            NONE => NONE
                                        |   SOME(year, str9) =>
                                            SOME(date{year=year, month=mth, day=day,
                                                      hour=hr, minute=min, second=sec,
                                                      offset=NONE}, str9)               
    in
        case getc str of
            NONE => NONE
        |   SOME (ch, str') =>
                (* Remove initial white space. *)
                if Char.isSpace ch then scan getc str'
                else parseDate str
    end

    val fromString = StringCvt.scanString scan

    (* toString generates an English language, American style date. *)
    fun toString (date as {year, month, day, hour, minute, second, ...}: date) =
    let
        (* Pad a number with zeros up to the required width. Doesn't
           work for negatives which ought to be padded after the
           minus sign, but that's only a problem for years. *)
        fun int2str n i =
        let
            val str = Int.toString i
            fun padZeros n = if n <= 0 then "" else "0" ^  padZeros (n-1)
        in
            padZeros (n-String.size str) ^ str
        end
    in
        String.concat[
            Vector.sub(dayNames, dayOfWeek date), " ",
            Vector.sub(monthNames, month), " ",
            int2str 2 day, " ",
            int2str 2 hour, ":", int2str 2 minute, ":", int2str 2 second, " ",
            int2str 4 year]
    end

    val convertDate = RunCall.rtsCallFull1 "PolyTimingConvertDateStuct"

    fun fmt s (date as {year, month, day, hour, minute, second, offset}) =
    let
        (* Edit the string to remove any undefined escape combinations.
           They shouldn't normally occur. *)
        fun editString s i l =
            if i = l then s (* Done *)
            else if String.sub(s, i) <> #"%"
            then editString s (i+1) l
            else (* Found a % sign. *)
                if i = l-1
            then (* This was the last character.  QUESTION: This isn't defined
                    assume we should remove it. *)
                String.substring(s, 0, i)
            else
            let
                val c = String.sub(s, i+1)
            in
                if Char.contains "aAbBcdHIjmMpSUwWxXyYZ%" c
                then (* OK *) editString s (i+2) l
                else (* Replace %c by c, i.e. remove the %. *)
                    editString (String.substring(s, 0, i) ^ 
                                    String.substring(s, i+1, l-i-1))
                        i (l-1)
            end
        val newFormat = editString s 0 (String.size s)
        val summer =
            case offset of
                SOME _ => ~1
            |   NONE => getSummer (Time.toSeconds(toTime date))
                            handle Size => ~1
    in
        convertDate (newFormat, year, month, day, hour, minute, second,
            dayOfWeek date, yearDay date, summer)
            handle RunCall.Size => raise Date
    end
end;

local
    (* Install the pretty printer for Date.date.  This has to be
       done outside the structure because of the opaque matching. *)
    fun pretty _ _ x = PolyML.PrettyString(Date.toString x)
in
    val () = PolyML.addPrettyPrinter pretty
end
