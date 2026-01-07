(*
    Title:      Standard Basis Library: OS Structures and Signatures
    Author:     David Matthews
    Copyright   David Matthews 2000, 2005, 2015-16, 2019-20, 2022

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

signature OS_FILE_SYS =
  sig
    type dirstream
    val openDir : string -> dirstream
    val readDir : dirstream -> string option
    val rewindDir : dirstream -> unit
    val closeDir : dirstream -> unit
    val chDir : string -> unit
    val getDir : unit -> string
    val mkDir : string -> unit
    val rmDir : string -> unit
    val isDir : string -> bool
    val isLink : string -> bool
    val readLink : string -> string
    val fullPath : string -> string
    val realPath : string -> string
    val modTime : string -> Time.time
    val fileSize : string -> Position.int

    val setTime : (string * Time.time Option.option) -> unit
    val remove : string -> unit
    val rename : {old : string, new : string} -> unit

    datatype access_mode
      = A_READ
      | A_WRITE
      | A_EXEC

    val access : (string * access_mode list) -> bool

    val tmpName : unit -> string

    eqtype  file_id
    val fileId : string -> file_id
    val hash : file_id -> word
    val compare : (file_id * file_id) -> General.order
  end (* OS_FILE_SYS *);


signature OS_PATH =
sig
    exception Path
    exception InvalidArc
    val parentArc : string
    val currentArc : string

    val fromString : string -> { isAbs : bool, vol : string, arcs : string list }
    val toString : { isAbs : bool, vol : string, arcs : string list } -> string

    val validVolume : {isAbs : bool, vol : string} -> bool
    val getVolume : string -> string
    val getParent : string -> string
    val splitDirFile : string -> {dir : string, file : string}
    val joinDirFile : {dir : string, file : string} -> string
    val dir  : string -> string
    val file : string -> string
    val splitBaseExt : string -> {base : string, ext : string option }
    val joinBaseExt : {base : string, ext : string option} -> string
    val base : string -> string
    val ext  : string -> string option
    val mkCanonical : string -> string
    val isCanonical : string -> bool
    val mkAbsolute : {path : string, relativeTo : string} -> string
    val mkRelative : {path : string, relativeTo : string} -> string
    val isAbsolute : string -> bool
    val isRelative : string -> bool
    val isRoot : string -> bool
    val concat : string * string -> string
    val toUnixPath : string -> string
    val fromUnixPath : string -> string
end (* OS_PATH *);


signature OS_PROCESS =
  sig
     type status
     val success : status
     val failure : status
     val isSuccess : status -> bool
     val system : string -> status
     val atExit : (unit -> unit) -> unit
     val exit : status -> 'a
     val terminate : status -> 'a
     val getEnv : string -> string Option.option
     val sleep: Time.time -> unit
  end (* OS_PROCESS *);


signature OS_IO =
  sig
    eqtype iodesc
    val hash : iodesc -> word
    val compare : (iodesc * iodesc) -> General.order
    
    eqtype iodesc_kind
    val kind : iodesc -> iodesc_kind
    
    structure Kind:
      sig
        val file : iodesc_kind
        val dir : iodesc_kind
        val symlink : iodesc_kind
        val tty : iodesc_kind
        val pipe : iodesc_kind
        val socket : iodesc_kind
        val device : iodesc_kind
      end

    eqtype poll_desc
    type  poll_info
    val pollDesc : iodesc -> poll_desc Option.option
    val pollToIODesc : poll_desc -> iodesc

    exception Poll
    val pollIn : poll_desc -> poll_desc
    val pollOut : poll_desc -> poll_desc
    val pollPri : poll_desc -> poll_desc

    val poll : (poll_desc list * Time.time Option.option) -> poll_info list

    val isIn : poll_info -> bool
    val isOut : poll_info -> bool
    val isPri : poll_info -> bool

    val infoToPollDesc : poll_info -> poll_desc

  end (* OS_IO *);


signature OS =
  sig
    eqtype syserror
    exception SysErr of (string * syserror Option.option)
    val errorMsg : syserror -> string
    val errorName : syserror -> string
    val syserror : string -> syserror Option.option

    structure FileSys : OS_FILE_SYS
    structure Path : OS_PATH
    structure Process : OS_PROCESS
    structure IO : OS_IO
  end (* OS *);


structure OS:> OS where type syserror = LibrarySupport.syserror (* Don't make it abstract a second time *) =
struct
    type syserror = LibrarySupport.syserror (* Implemented as a SysWord.word value. *)

    (* The calls themselves raise the SysCall exception.
       That has to be turned into a SysError exception. *)
    exception SysErr = LibrarySupport.SysErr

    (* Convert a numeric system error to a string.
       Note: unlike Posix.Error.errorName and Posix.Error.sysError
       the results are not defined other than that
       SOME e = syserror(errorName e) nor is this defined to
       be the same as the Posix.Error functions.  Those are
       defined to return e.g. "etoobig".  Here we return "E2BIG". *)
    val errorName: syserror -> string = RunCall.rtsCallFull1 "PolyProcessEnvErrorName"
    and errorMsg: syserror -> string = RunCall.rtsCallFull1 "PolyProcessEnvErrorMessage"

    local
        val doCall: string -> syserror = RunCall.rtsCallFull1 "PolyProcessEnvErrorFromString"
    in
        (* Convert a string to an error message if possible. *)
        fun syserror (s: string) : syserror option =
        let
            val n = doCall s
        in
            if LibrarySupport.syserrorToWord n = 0w0 then NONE else SOME n
        end
    end



    structure Path:> OS_PATH =
    struct
        (* Note: The definition of relative and absolute paths are
           somewhat unclear and some of the examples seem contradictory.
           The definition I would prefer to use is that an absolute path
           is one which identifies a given file independent of any setting
           of the current directory.  Hence the examples of "\" and "\A\B" as
           being absolute paths in DOS is in my opinion wrong.  These are
           relative since they depend on the setting of the current volume.
           However this is a mess when it comes to fromString since if
           we don't treat "\A" as an absolute path it looks just like an
           absolute path with an empty arc. *)
        exception Path
        exception InvalidArc
        
        local
            val getOSCall: unit -> int = RunCall.rtsCallFast0 "PolyGetOSType"
            val getOS: int = getOSCall()
        in
            val isWindows =
                case getOS of
                    0 => false (* Posix *)
                |   1 => true
                |   _ => raise Fail "Unknown operating system"
        end
        
        val isCaseSensitive = not isWindows

        val isSeparator =
            if isWindows then fn #"/" => true | #"\\" => true | _ => false
            else fn #"/" => true | _ => false

        val separator =
            if isWindows then "\\" else "/"
        
        
        val parentArc = ".." and currentArc = "."

        val isValidArc =
            if isWindows
            then
            let
                fun invalidChars #"\000" = true
                |   invalidChars #"<"    = true
                |   invalidChars #">"    = true
                |   invalidChars #":"    = true
                |   invalidChars #"\""   = true
                |   invalidChars #"\\"   = true
                |   invalidChars #"/"    = true
                |   invalidChars #"|"    = true
                |   invalidChars #"?"    = true
                |   invalidChars #"*"    = true
                |   invalidChars _       = false
            in
                not o (CharVector.exists invalidChars)
            end
            else
            let
                (* Posix - only null and / are invalid. *)
                fun invalidChars #"\000" = true
                |   invalidChars #"/"    = true
                |   invalidChars _       = false
            in
                not o (CharVector.exists invalidChars)
            end

        local
            (* Given a string it examines the prefix and extracts the volume
               name if there is one.  It returns the volume and also whether
               the name is absolute.  It also returns the number of characters
               which matched so that this can be removed before treating
               the rest as a relative path. *)
            fun matchVolumePrefixPosix s =
                if String.size s > 0 andalso String.sub(s, 0) = #"/"
                then {volLen = 1, vol = "", abs = true, root = true }
                else {volLen = 0, vol = "", abs = false, root = false } 
            
            fun matchVolumePrefixWindows s =
            let
                val slen = String.size s
            in
                if slen = 0 then { volLen = 0, vol = "", abs = false, root = false }
                else if slen >= 2 andalso String.sub(s, 1) = #":" andalso
                        Char.isAlpha(String.sub(s, 0))
                then
                    if slen > 2 andalso isSeparator(String.sub(s, 2))
                    then { volLen = 3, vol = String.substring(s, 0, 2), abs = true, root = true } (* e.g. C:\ or C:\fred *)
                    else { volLen = 2, vol = String.substring(s, 0, 2), abs = false, root = false } (* e.g. C: or C:fred *)
                else if slen > 2 andalso isSeparator(String.sub(s, 0))
                        andalso isSeparator(String.sub(s, 1))
                then (* Looks like a UNC server name. See how big it is. *)
                let
                    val (server, rest) =
                        Substring.splitl(fn c => not (isSeparator c))
                            (Substring.extract(s, 2, NONE))
                    (* TODO: Is the server name actually valid?  Assume yes. *)
                in
                    if Substring.size rest = 0
                    then { volLen = 0, vol = "", abs = false, root = false }
                    else (* Must be room for a share name as well. *)
                    let
                        val shareName =
                            Substring.takel(fn c => not (isSeparator c))
                                    (Substring.triml 1 rest)
                    in
                        { volLen = Substring.size server + Substring.size shareName + 4,
                          vol =
                            separator ^ separator ^ 
                                Substring.string server ^ separator ^
                                Substring.string shareName,
                          abs = true, root = true }
                    end
                end
                (* Leading \ in Windows means the "root" directory on the current drive.  *)
                else if isSeparator(String.sub(s, 0))
                then { volLen = 1, vol = "", abs = false, root = true }

                else { volLen = 0, vol = "", abs = false, root = false }
            end
        in
            val matchVolumePrefix =
                if isWindows then matchVolumePrefixWindows else matchVolumePrefixPosix
        end

        (* Internal - map the strings to the canonical case if they
           are not case sensitive. *)
        val toCanonicalCase =
            if isCaseSensitive then fn s => s
            else String.map Char.toLower

        (* Internal - are the arcs equivalent? *)
        fun equivalent (s, t) = toCanonicalCase s = toCanonicalCase t

        (* See if the volume name is valid for either an absolute or
           relative path.  Windows relative paths may or may not
           have a volume but if they have the volume must look right.
           On Unix relative paths may not specify a volume and
           the only volume for absolute paths is the empty string. *)
        val validVolume =
            if isWindows
            then
                fn  {isAbs, vol = ""} =>
                    not isAbs (* Empty volume is only valid for relative paths. *)
                    
                |   {vol, ...} =>
                    if size vol = 2 andalso String.sub(vol, 1) = #":"
                            andalso Char.isAlpha(String.sub(vol, 0))
                    then true (* Drive letter e.g. C: *)
                    else if size vol > 2 andalso isSeparator(String.sub(vol, 0))
                    then (* UNC name?  \\server\share *)
                        case String.fields isSeparator vol of
                            ["", "", server, share] => server <> "" andalso share <> ""
                        |   _ => false
                    else false

            else (* Posix.  The volume must always be empty. *)
                fn {vol = "", ...} => true | _ => false

        (* We only return an empty arcs list if the argument is the empty string.  *)
        fun fromString "" = {isAbs = false, vol = "", arcs=[]}
        |   fromString (s: string) =
        let
            (* Do we have a volume name? *)
            val {volLen, vol, abs, root, ...} = matchVolumePrefix  s
            (* The remainder forms a set of arcs. *)
            val rest = String.extract(s, volLen, NONE)
            val arcs = String.fields isSeparator rest
            (* If it begins with the Windows \ without a drive we
               need to add an extra empty arc.  Otherwise we can't
               distinguish \a from a. *)
            val allArcs =
                if root andalso not abs then "" :: arcs else arcs
        in
            {isAbs = abs, vol = vol, arcs=allArcs}
        end

        (* Note: This is a mess as well.  For example it says that it should
           raise Path if there is a relative path which begins with an
           empty arc.  That's only true in Unix.  What it should say is
           that it if isAbs is false then it should raise Path if the
           resulting path has the form of an absolute path. In Windows
           we should raise path if given (e.g.)
          {isAbs=false, vol="", arcs=["", "", "a", "b"]} because that
           looks like a UNC name. *)
        fun toString {isAbs : bool, vol : string, arcs : string list} =
            (* Check we have a valid volume. *)
            if not (validVolume{isAbs=isAbs, vol=vol})
            then raise Path
            (* Check that each arc is valid. *)
            else if List.exists (fn s => not (isValidArc s)) arcs
            then raise InvalidArc
            else
            let
                (* Place separators between each arc. *)
                fun arcsToLinks [] = []
                  | arcsToLinks [a] = [a]
                  | arcsToLinks (a::b) =
                    a :: separator :: arcsToLinks b
                fun makePrefix(vol, false) = vol | makePrefix(vol, true) = vol ^ separator
                val r = String.concat(makePrefix(vol, isAbs) :: arcsToLinks arcs)
                (* Check to see whether we have turned a relative path into
                   an absolute one by including empty arcs in the wrong places. *)
                val {abs = nowAbs, ...} = matchVolumePrefix r
            in
                if nowAbs <> isAbs
                then raise Path
                else r
            end
        (* Note: this is just defined to "return the volume portion" but
           doesn't say what to do if there isn't a volume.  Seems simplest
           to define it as below. *)
        fun getVolume s = #vol(fromString s)

        (* Note: Once again this has very much a Unix view of the world,
           most of which almost works in Windows.
           I think the idea is that if possible it replaces the path
           with the path to the containing directory.
           If we're in the root directory we get the root directory.
           If we're in a path that ends with a component
           *)
        fun getParent "" = parentArc
         |  getParent s =
            let
                val len = String.size s
                val {volLen, ...} = matchVolumePrefix s
                (* Split it at the last separator. *)
                val (prefix, suffix) =
                    Substring.splitr (fn c => not (isSeparator c))
                        (Substring.full s) 
            in
                if volLen = len
                then s (* We have a root.  *)
                else if Substring.size suffix = 0
                then
                    (* If the last character is a separator just add on
                       the parent arc (..) to refer to the parent directory.
                       I don't know why we can't just remove the last component
                       in this case but the examples don't do that.  The only
                       special case is where we have reached the root when
                       we just return the root. *)
                    s ^ parentArc
                else if Substring.string suffix = parentArc
                (* If the last component is a parent arc we need to add
                   a separator and another parent arc. *)
                then s ^ separator ^ parentArc
                else if Substring.size prefix = 0
                then (* No separator at all *)
                    (
                    if s = parentArc (* .. => ../.. *)
                    then parentArc ^ (separator) ^ parentArc
                    else if s = currentArc
                    then parentArc (* . => .. *)
                    else currentArc (* abc => . *)
                    )
                else if Substring.size prefix = volLen
                (* ??? If the prefix matches the volume then return
                   the whole of prefix including the separator. *)
                then Substring.string prefix
                else (* Return the prefix with the separator removed. *)
                    Substring.string(Substring.trimr 1 prefix)
            end

        (* Another mess defined in terms of examples for Unix from which
           one is expected to infer a general rule.
           It seems to split the string at the last separator and
           return the two halves without the separator except in the
           case where the directory is a root directory when a full
           volume name and separator are given. *)
        fun splitDirFile s =
        let
            (* Split it at the last separator. *)
            val (prefix, suffix) =
                Substring.splitr (fn c => not (isSeparator c))
                    (Substring.full s) 
            val {volLen, vol, ...} = matchVolumePrefix s
            val dirName =
                case Substring.size prefix of
                    0 => ""
                |   1 => Substring.string prefix (* Special case of Windows \a. *)
                |   _ => Substring.string(Substring.trimr 1 prefix)
            and fileName = Substring.string suffix
        in
            if volLen <> 0 andalso vol = dirName
            then {dir = vol ^ separator, file = fileName}
            else {dir = dirName, file = fileName}
        end

        fun dir s = #dir(splitDirFile s)
        and file s = #file(splitDirFile s)

        (* Question: It seems from the definition of toString that the
           arcs list can include separators.  Is that true here?
           Assume yes. *)
        (* If the last character is already a separator we don't add one,
           e.g. if the directory is "/". *)
        fun joinDirFile{dir, file} =
            if not (isValidArc file) then raise InvalidArc
            else if dir = "" then file (* Return the file name unchanged *)
            else if isSeparator(String.sub(dir, size dir - 1))
            then dir ^ file
            else dir ^ separator ^ file

        fun splitBaseExt s =
        let
            val slen = String.size s
            fun getExt n =
                if n <= 0 then NONE (* If it's at the start ignore it. *)
                else if isSeparator(String.sub(s, n))
                then NONE
                else if String.sub(s, n) = #"."
                then (* Found a dot. *)
                    (
                    if n = slen-1 then NONE (* Dot in last position. *)
                    else if isSeparator(String.sub(s, n-1))
                    then NONE (* Dot immediately after separator. *)
                    else SOME n
                    )
                else getExt (n-1)
            val extPos = getExt(slen - 1)
        in
            case extPos of
                NONE => {base=s, ext=NONE}
            |   SOME n => {base=String.substring(s, 0, n),
                           ext=SOME(String.substring(s, n+1, slen-n-1))}
        end

        fun joinBaseExt {base : string, ext = NONE} = base
         |  joinBaseExt {base : string, ext = SOME ""} = base
         |  joinBaseExt {base : string, ext = SOME ext} = base ^ "." ^ ext
    
        fun base s = #base(splitBaseExt s)
        and ext s = #ext(splitBaseExt s)

        val emptyArcIsRedundant = true

        fun mkCanonical s =
        let
            val {isAbs, vol, arcs} = fromString s
            fun collapse [] = []
              | collapse (a :: b) =
                    (* Work down the list removing currentArc entries and
                       null entries (if the OS treats them as redundant).. *)
                    if a = currentArc orelse (emptyArcIsRedundant andalso a = "")
                    then collapse b
                    (* Then work back up it removing parentArc entries. *)
                    else
                        case collapse b of
                        [] => [a]
                      | b' as (x :: y) =>
                            if x = parentArc andalso not (a = parentArc)
                            then (* Remove "a" and "x". *) y
                            else a :: b'

            val collapsed = collapse arcs

            (* If this is the root we can remove leading occurrences of
               the parent arc since the parent of the root is the root. *)
            fun removeLeadingParent [] = []
              | removeLeadingParent (a::b) =
                    if a = parentArc then removeLeadingParent b else a::b
            val newArcs =
                if isAbs then removeLeadingParent collapsed else collapsed
            val res = toString{isAbs=isAbs, vol=vol, arcs=newArcs}
        in
            (* Finally replace the empty string with "." and map to lower case
               if it's not case sensitive. *)
            if res = "" then currentArc
            else toCanonicalCase res
        end

        fun isCanonical s = mkCanonical s = s handle Path => false

        fun isAbsolute s = #isAbs(fromString s)
        and isRelative s = not(#isAbs(fromString s))

        (* Concatenate two paths.  The second must be relative and, if it
           contains a volume name, refer to the same volume as the first. *)
        fun concat(s, t) =
        let
            val {isAbs=absS, vol=volS, arcs=ArcsS} = fromString s
            val {isAbs=absT, vol=volT, arcs=ArcsT} = fromString t

            (* Concatenate the two lists of arcs except that a trailing
               empty arc on the first path is removed
               (i.e. concat("a/", "b") is the same as concat("a", "b") *)
            fun concatArcs [] p = p
             |  concatArcs [a] p = if a = "" then p else a :: p 
             |  concatArcs (a::b) p = a :: concatArcs b p 
        in
            if absT then raise Path
            else if volT <> "" andalso not(equivalent(volS, volT))
            then raise Path
            else if #root(matchVolumePrefix t)
            (* Special case for Windows. concat("c:\\abc\\def", "\\xyz") is "c:\\xyz". *)
            then
            let
                (* Because this a relative path we have an extra empty arc here. *)
                val ArcsT' = case ArcsT of "" :: a => a | a => a
            in
                toString{isAbs=absS, vol=volS, arcs=ArcsT'}
            end
            else toString{isAbs=absS, vol=volS, arcs=concatArcs ArcsS ArcsT}
        end
        
       (* Make an absolute path by treating a relative path as relative to
           a given path. *)
        fun mkAbsolute {path, relativeTo} =
        let
            val {isAbs=absP, vol=volP, ...} = fromString path
            val {isAbs=absRT, vol=volRT, ...} = fromString relativeTo
        in
            if absP then path
            else if not absRT then raise Path
            (* If the path contained a volume it must be the
               same as the absolute path. *)
            else if volP <> "" andalso not(equivalent(volP, volRT))
            then raise Path
            else mkCanonical(concat(relativeTo, path))
        end

        (* Make a relative path by treating an absolute path as derived
           from a given other absolute path. *)
        fun mkRelative {path, relativeTo} =
            case fromString path of
                {isAbs=false, ...} => path (* Already relative *)
             |  {vol=volP, arcs=arcsP, ...} =>
                let
                    val {isAbs=absRT, vol=volRT, arcs=arcsRT} =
                        fromString (mkCanonical relativeTo)
        
                    (* Add as many parent arcs as there are arcs in the path. *)
                    fun addParents [] p = p
                     |  addParents (_::b) p = parentArc :: addParents b p

                    fun matchPaths [] [] = [currentArc] (* Both equal *)
                     |  matchPaths p  [] = (* Absolute path is finished - return p *) p
                     |  matchPaths [] r = (* Relative paths finished - add parent arcs *)
                                            addParents r []
                     |  matchPaths (p :: p') (r :: r') =
                            (* Are they the same arc?  Note: When arcs are
                               case insensitive I'm doing a case insensitive match
                               here.  *)
                            if equivalent(p, r)
                            then matchPaths p' r'
                            else addParents (r :: r') (p :: p')

                    (* We have a special case with the root directory
                       (/ on Unix or c:\\ on Windows).  In that case fromString returns
                       a single empty arc and we want to remove it here otherwise
                       we can end up with an empty arc in addParents. *) 
                    val arcsP' = case arcsP of [""] => [] | _ => arcsP
                    val arcsRT' = case arcsRT of [""] => [] | _ => arcsRT
                in
                    if not absRT then raise Path
                    (* If the path contained a volume it must be the
                       same as the absolute path. *)
                    else if volP <> "" andalso not(equivalent(volP, volRT))
                    then raise Path
                    else toString{isAbs=false, vol="", arcs=matchPaths arcsP' arcsRT'}
                end

        (* Another badly defined function.  What is a root?  Does it have to specify
           a volume or is \ a root in Windows?  Assume that it must be absolute. *)
        fun isRoot s =
        let
            val {volLen, abs, ...} = matchVolumePrefix  s
        in
            abs andalso volLen = String.size s andalso isCanonical s
        end

        (* Question: there's no definition of what these functions mean.  The crucial
           questions are how to deal with volume names and also how to deal
           with symbols in the paths which may be invalid (e.g. path separators) in
           one or other system.  For instance "a\b" is a valid file name in Unix
           and 31/3/2000 is valid in MacOS.
           Are they supposed to represent the original file system in some way? *)
        fun toUnixPath s =
        let
            (* We may have occurrences of "/" in the arcs if that is not
               a separator on this OS.  Replace them by this machine's separator. *)
            fun mapArc a =
                if a = currentArc then "."
                else if a = parentArc then ".."
                else a

            fun mapArcs [] = []
             |  mapArcs [a] = [mapArc a]
             |  mapArcs (a::b) = mapArc a :: "/" :: mapArcs b

            val {isAbs, vol, arcs} = fromString s
            val volArc = if vol <> "" then vol :: arcs else arcs
            val sl = String.concat(mapArcs volArc)
        in
            if String.size sl = 0 then ""
            else if isAbs then if String.sub(sl, 0) <> #"/" then "/" ^ sl else sl
            else (* not abs *) if String.sub(sl, 0) = #"/" then "." ^ sl else sl
        end

        fun fromUnixPath s =
        let
            val arcs = String.fields (fn ch => ch = #"/") s
            (* Turn any occurrences of this OS's separator into / since
               that can't occur within an arc. *)
            val convArc =
                String.translate (
                    fn ch => if isSeparator ch then "/" else String.str ch)
            val convArcs = List.map convArc arcs
        in
            case convArcs of
                [] => ""
            |   ("" :: a :: rest) =>
                let (* We had a leading / : is the first arc a volume name? *)
                    val {volLen = n, vol, ...} = matchVolumePrefix a
                in
                    if n = String.size a
                    then (* We have a volume name. *)
                        toString{isAbs=true, vol=vol, arcs=rest}
                    else toString{isAbs=true, vol="", arcs=convArcs}
                end
            |   (a :: rest) =>
                let (* May be a relative volume name. *)
                    val {volLen = n, vol, ...} = matchVolumePrefix a
                in
                    if n = String.size a
                    then toString{isAbs=false, vol=vol, arcs=rest}
                    else toString{isAbs=false, vol="", arcs=convArcs}
                end
        end

    end (* Path *)

    structure FileSys:> OS_FILE_SYS =
    struct
        type dirFd = int
        (* The directory stream consists of the stream identifier
           returned by openDir together with the original directory
           name.  We need that for rewind in Windows. *)
        datatype dirstream = DIR of dirFd * string

        local
            val doIo: int*unit*string -> dirFd
                 = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
        in
            fun openDir (s : string):  dirstream = 
                DIR(doIo(50, (), s), s)
        end

        local
            val doIo: int*dirFd*unit -> string
                 = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
        in
            fun readDir (DIR(d, _)):  string option =
            let
                (* This returns the empty string at end-of-stream. *)
                val s = doIo(51, d, ())
            in
                if s = "" then NONE else SOME s
            end
        end

        local
            val doIo: int*dirFd*unit -> unit
                 = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
        in
            fun closeDir(DIR(d, _)) =
                doIo(52, d, ())
        end

        local
            val doIo: int*dirFd*string -> unit
                 = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
        in
            (* We need to pass in the string because Windows
               has to reopen the stream. *)
            fun rewindDir(DIR(d, s)) =
                doIo(53, d, s)
        end

        val chDir: string -> unit = RunCall.rtsCallFull1 "PolyChDir"

        local
            val doIo: int*unit*unit -> string
                 = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
        in
            (* Return current directory. *)
            fun getDir() = doIo(54, (), ())
            (* Get a temporary file name. *)
            fun tmpName() = doIo(67, (), ())
        end

        local
            val doIo: int*unit*string -> unit
                 = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
        in
            (* Create and delete directories and remove a file. *)
            fun mkDir s = doIo(55, (), s)
            and rmDir s = doIo(56, (), s)
            and remove s = doIo(64, (), s)
        end

        local
            val doIo: int*unit*string -> bool
                 = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
        in
            (* Test for directory and symbolic link. *)
            fun isDir s = doIo(57, (), s)
            and isLink s = doIo(58, (), s)
        end

        local
            val doIo: int*unit*string -> string
                 = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
        in
            (* Read a symbolic link. *)
            fun readLink s = doIo(59, (), s)
            (* Get a full canonical path name. *)
            and fullPath s = doIo(60, (), s)
        end

        local
            val doIo: int*unit*string -> Time.time
                 = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
        in
            (* Get file modification time. *)
            fun modTime s = doIo(61, (), s)
        end

        local
            val doIo: int*unit*string -> Position.int (* This can be larger than 32-bits. *)
                 = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
        in
            (* Get file size. *)
            fun fileSize s = doIo(62, (), s)
        end

        local
            val doIo: int*string*Time.time -> unit
                 = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
        in
            (* Get file size. *)
            fun setTime(s, NONE) = doIo(63, s, Time.now())
             | setTime(s, SOME t) = doIo(63, s, t)
        end

        local
            val doIo: int*string*string -> unit
                 = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
        in
            (* Rename a file. *)
            fun rename {old, new} = doIo(65, old, new)
        end

        datatype access_mode
          = A_READ
          | A_WRITE
          | A_EXEC

        local
            val doIo: int*string*word -> bool
                 = RunCall.rtsCallFull3 "PolyBasicIOGeneral"

            fun mapAccess (A_READ, m) = Word.orb(m, 0w1)
             |  mapAccess (A_WRITE, m) = Word.orb(m, 0w2)
             |  mapAccess (A_EXEC, m) = Word.orb(m, 0w4)
        in
            (* Get access rights. *)
            fun access (s, m) = doIo(66, s, List.foldl mapAccess 0w0 m)
        end

        (* file_id seems to be intended to reflect the semantics of
           a Unix inode.  That concept doesn't exist in Windows so
           we use a canonical file name instead. *)
        datatype file_id =
            INODE of LargeInt.int | FILENAME of string

        fun compare(INODE i, INODE j) = LargeInt.compare(i, j)
          | compare(FILENAME s, FILENAME t) = String.compare(s, t)
          | (* These cases shouldn't happen but we'll define them
               anyway. *)
            compare(INODE _, FILENAME _) = General.GREATER
          | compare(FILENAME _, INODE _) = General.LESS

        (* TODO: The hash function is supposed to well distribute the
           the values when taken modulo 2^n for any n.
           I'm sure we can come up with something better than this. *)
        fun hash(INODE i) =
            let
                open Word
                infix xorb <<
                val w = Word.fromLargeInt i
            in
                w xorb (w << 0w8) xorb (w << 0w16) xorb (w << 0w24)
            end

         |  hash(FILENAME s) =
                (* Simple hash function which multiplies the accumulator
                   by 7 and adds in the next character. *)
                CharVector.foldl
                    (fn(c, a) => a * 0w7 + Word.fromInt(Char.ord c)) 0w0 s
        local
            val doIo: int*unit*string -> LargeInt.int
                 = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
        in
            (* Get file id (inode).  Returns negative value if inodes aren't
               supported. *)
            fun fileId s =
            let
                val i = doIo(68, (), s)
            in
                if i < 0
                then FILENAME(fullPath s)
                else INODE i
            end
        end

        fun realPath p =
            if Path.isAbsolute p
            then fullPath p
            else Path.mkRelative{path=fullPath p, relativeTo=fullPath(getDir())}
    end (* FileSys *)

    structure IO :> OS_IO =
    struct
        datatype iodesc = 
            IODESC of word ref (* This is currently a volatile ref.  We MUST use pointer equality. *)
        local
            val doIo: int*iodesc*unit -> int
                 = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
        in
            (* Get underlying index. *)
            fun getIndex f = doIo(69, f, ())
        end

        (* TODO: The hash function is supposed to well distribute the
           the values when taken modulo 2^n for any n.
           I'm sure we can come up with something better than this. *)
        fun hash (i: iodesc) : word =
        let
            open Word
            infix xorb <<
            val w = Word.fromInt(getIndex i)
        in
            w xorb (w << 0w8) xorb (w << 0w16) xorb (w << 0w24)
        end

        fun compare(i, j) = Int.compare(getIndex i, getIndex j)

        (* eq *)type iodesc_kind = int
        
        structure Kind =
        struct
            val file : iodesc_kind = 0
            val dir : iodesc_kind = 1
            val symlink : iodesc_kind = 2
            val tty : iodesc_kind = 3
            val pipe : iodesc_kind = 4
            val socket : iodesc_kind = 5
            val device : iodesc_kind = 6
        end

        local
            val doIo: int*iodesc*int -> int
                 = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
        in
            fun kind (i: iodesc): iodesc_kind =
            let
                val k = doIo(21, i, 0)
            in
                (* Returns a negative number if the call fails,
                   otherwise one of the above numbers.
                   Returns 7 on unknown or something else. *)
                if k < 0 orelse k > 6
                then raise SysErr("Invalid result", NONE)
                else k
            end
        end

        (* The poll descriptor and the result of polling is a
           bit map together with the io descriptor. *)
        val inBit = 0w1 and outBit = 0w2 and priBit = 0w4

        (* N.B. The implementation of poll_desc is hard-wired into
           Socket.pollDesc. *)
        type      poll_desc = word*iodesc
        datatype  poll_info = PI of word*poll_desc

        local
            val doIo: int*iodesc*int -> word
                 = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
        in
            fun sys_poll_test(i: iodesc) = doIo(22, i, 0)
        end

        val sysPoll:iodesc Vector.vector * word Vector.vector * int -> word Vector.vector =
            RunCall.rtsCallFull3 "PolyPollIODescriptors"


        fun pollDesc (i: iodesc): poll_desc option =
        (* If the poll test returns zero then polling is
           not allowed for any mode. *)
            if sys_poll_test i = 0w0
            then NONE
            else SOME(0w0, i)

        fun pollToIODesc(_, i): iodesc = i

        exception Poll

        (* Add the appropriate bit to the set if it is allowed. *)
        local
            fun addBit b ((bm, i)) =
                if Word.andb(sys_poll_test i, b) = 0w0
                then raise Poll
                else (Word.orb(bm, b), i)
        in
            val pollIn = addBit inBit
            and pollOut = addBit outBit
            and pollPri = addBit priBit
        end

        fun poll (l : poll_desc list, t: Time.time Option.option) : poll_info list =
        let
            (* The original poll descriptor list may contain multiple occurrences of
               the same IO descriptor with the same or different flags.  On Cygwin, at
               least, passing this directly produces funny results so we transform the
               request so that we make at most one request for each descriptor. *)
            local
                fun quickSort _                      ([]:'a list)      = []
                |   quickSort _                      ([h]:'a list)     = [h]
                |   quickSort (leq:'a -> 'a -> bool) ((h::t) :'a list) =
                let
                    val (after, befor) = List.partition (leq h) t
                in
                    quickSort leq befor @ (h :: quickSort leq after)
                end;

                fun leqPoll((p1, f1): poll_desc) ((p2, f2): poll_desc) =
                    case compare(f1, f2) of
                        EQUAL => p1 <= p2
                    |   LESS => true
                    |   GREATER => false
                
                fun merge ((p1, f1) :: (p2, f2) :: rest) =
                        if compare(f1, f2) = EQUAL
                        then merge((Word.orb(p1, p2), f1) :: rest)
                        else (p1, f1) :: merge((p2, f2) :: rest)
                |   merge c = c

                val sortedDescs = quickSort leqPoll l
            in
                val mergedDescs = merge sortedDescs
            end

            (* Turn the list into vectors of io descriptors and
               request bits - easier for the RTS to manage. 
               N.B.  This assumes that Vector.vector creates a simple memory vector and
               does not wrap it in any way. *)
            local
                val (bits, ioDescs) = ListPair.unzip mergedDescs
            in
                val bitVector: word Vector.vector = Vector.fromList bits
                and ioVector: iodesc Vector.vector = Vector.fromList ioDescs
            end
            (* Do the actual polling.  Returns a vector with bits set for the results. *)
            val finishTime = case t of NONE => NONE | SOME t => SOME(t + Time.now())
            
            val pollMillSeconds = 1000 (* 1 second *)
            fun doPoll() =
            let
                val timeToGo =
                    case finishTime of
                        NONE => pollMillSeconds
                    |   SOME finish => LargeInt.toInt(LargeInt.min(LargeInt.max(0, Time.toMilliseconds(finish-Time.now())), LargeInt.fromInt pollMillSeconds))
                
                (* Poll the descriptors.  Returns after the timeout whether or not they are ready. *)
                val resV = sysPoll(ioVector, bitVector, timeToGo)
            in
                if timeToGo < pollMillSeconds orelse Vector.exists(fn w => w <> 0w0) resV
                then resV
                else doPoll()
            end
            
            val resV : word Vector.vector = doPoll()

            (* Process the original list to see which items are present, retaining the
               original order. *)
            fun testResults(request as (bits, iod), tl) =
            let
                val (index, _) = (* Find the IO descriptor.  It must be there somewhere. *)
                    valOf(Vector.findi (fn (_, iod1) => compare(iod, iod1) = EQUAL) ioVector)
                (* The result is in the corresponding index position.   We need to AND this
                   with the request because we could have separate requests asking for
                   different bits for the same file descriptor. *)
                val result = Word.andb(bits, Vector.sub(resV, index))
            in
                if result = 0w0
                then tl
                else PI(result, request) :: tl
            end
        in
            List.foldl testResults [] l
        end

        fun isIn(PI(b, _)) = Word.andb(b, inBit) <> 0w0
        and isOut(PI(b, _)) = Word.andb(b, outBit) <> 0w0
        and isPri(PI(b, _)) = Word.andb(b, priBit) <> 0w0

        fun infoToPollDesc (PI(_, pd)) = pd

    end (* IO *)

    structure Process:> OS_PROCESS =
    struct

        type status = int

        val success = RunCall.rtsCallFull0 "PolyProcessEnvSuccessValue" ()
        and failure = RunCall.rtsCallFull0 "PolyProcessEnvFailureValue" ()

        fun isSuccess i = i = success

        (* Run a process and wait for the result. *)
        val system: string -> status = RunCall.rtsCallFull1 "PolyProcessEnvSystem"
        
        local
            val atExitList = LibrarySupport.atExitList
            val atExitMutex = Thread.Mutex.mutex()
            val exitResult = LibrarySupport.volatileOptionRef() (* Set to the exit result. *)
            
            val reallyExit: int -> unit = RunCall.rtsCallFull1 "PolyFinish"
        in
            (* Register a function to be run at exit.  If we are already exiting
               this has no effect. *)
            val atExit = ThreadLib.protect atExitMutex
                (fn f => case exitResult of ref NONE => LibrarySupport.addAtExit f | _ => ())
            
            (* Exit.  Run the atExit functions and then exit with the result code.
               There are a few complications.  If a second thread calls exit after
               the first one it mustn't start the exit process again.  If one of the
               atExit functions calls exit recursively it is defined to never return.
               We just need to pick up the next atExit function and carry on. *)
            fun exit (n: int) =
            let
                open Thread
                open Mutex Thread
                (* Turn off further interrupts *)
                val () = setAttributes[InterruptState InterruptDefer]
                val () = lock atExitMutex
                val () =
                    case !exitResult of
                        SOME threadId =>
                            if threadId = self()
                            then ()
                            else (unlock atExitMutex; Thread.exit())
                    |   NONE => exitResult := SOME(self())
                val () = unlock atExitMutex
                (* This is now the only thread here.
                   Take an item off the list and update the list with the
                   tail in case we recursively call "exit". *)
                fun runExit () =
                    case !atExitList of
                        [] => reallyExit n
                    |   (hd::tl) => (atExitList := tl; hd() handle _ => (); runExit())
            in
                runExit();
                raise Match (* Never reached but gives the 'a result. *)
            end
        end

        (* Terminate without running the atExit list or flushing the
           buffers.  We raise an exception to get the type right. *)
        local
            val doCall: int -> unit = RunCall.rtsCallFull1 "PolyTerminate"
        in
            fun terminate n = (doCall n; raise Fail "never")
        end

        local
            val doCall: string -> string = RunCall.rtsCallFull1 "PolyGetEnv"
        in
            (* Get an environment string.  The underlying call raises an
               exception if the string isn't there. *)
            fun getEnv s = SOME(doCall s) handle RunCall.SysErr _ => NONE
        end

        (* poll is implemented so that an empty list simply waits for
           the time. *)
        fun sleep t = (IO.poll([], SOME t); ())
    end (* Process. *)

end;

local
    (* Install the pretty printer for OS.IO.Kind and OS.syserror. *)
    fun kind_string k =
        if k = OS.IO.Kind.file then "file"
        else if k = OS.IO.Kind.dir then "dir"
        else if k = OS.IO.Kind.symlink then "symlink"
        else if k = OS.IO.Kind.tty then "tty"
        else if k = OS.IO.Kind.pipe then "pipe"
        else if k = OS.IO.Kind.socket then "socket"
        else if k = OS.IO.Kind.device then "device"
        else "unknown"

    fun printKind _ _ x = PolyML.PrettyString(kind_string x)
    fun printSysError _ _ x = PolyML.PrettyString(OS.errorName x)

    (* For the moment just make these opaque. *)
    fun printPollDesc _ _ (_: OS.IO.poll_desc) = PolyML.PrettyString "?"
    and printPollInfo _ _ (_: OS.IO.poll_info) = PolyML.PrettyString "?"
in
    val () = PolyML.addPrettyPrinter printKind
    val () = PolyML.addPrettyPrinter printSysError
    val () = PolyML.addPrettyPrinter printPollDesc
    val () = PolyML.addPrettyPrinter printPollInfo
end
