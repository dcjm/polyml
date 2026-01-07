(*
    Copyright (c) 2009 David C.J. Matthews 2008, 2013, 2015.

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

structure Pretty:> PRETTY =
struct

(*    abstype context =
        AbsContextLocation of
            { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }
    |   AbsContextProperty of string * string (* User property. *)

    and pretty =
        AbsPrettyBlock of int * bool * context list * pretty list
    |   AbsPrettyBreak of int * int
    |   AbsPrettyString of string
    |   AbsPrettyStringAndWidth of string * int
    |   AbsPrettyLineBreak
    
    with
        val ContextLocation = AbsContextLocation
        and ContextProperty = AbsContextProperty
        
        val PrettyBlock = AbsPrettyBlock
        and PrettyBreak = AbsPrettyBreak
        and PrettyString = AbsPrettyString
        
        fun isPrettyBlock(AbsPrettyBlock _) = true | isPrettyBlock _ = false
        and isPrettyBreak(AbsPrettyBreak _) = true | isPrettyBreak _ = false
        and isPrettyString(AbsPrettyString _) = true | isPrettyString _ = false

        fun projPrettyBlock(AbsPrettyBlock b) = b | projPrettyBlock _ = raise Match
        and projPrettyBreak(AbsPrettyBreak b) = b | projPrettyBreak _ = raise Match
        and projPrettyString(AbsPrettyString b) = b | projPrettyString _ = raise Match
    end;*)

    (* This is complicated because the data structures we use here will be exported into
       the code produced by the compiler.  We can't assume that the same representations
       will be used by this version of the compiler as are used by the compiler that is
       compiling this code.  We use an explicit representation here which must be kept in
       synch with the representation used in DATATYPE_REP.ML *)
    local
        open Address
        fun cast p = toAddress(toMachineWord p)
    in
        type context = address
        type loc = { file: string, startLine: FixedInt.int, startPosition: FixedInt.int, endLine: FixedInt.int, endPosition: FixedInt.int }
        (* Because the argument tuple has more than 4 fields the address is used rather than copying the fields. *)
        fun ContextLocation(p: loc): context = cast(0w0, p)
        and ContextProperty(s1: string, s2: string): context = cast(0w1, s1, s2)
    end

    local
        open Address
        fun cast p = toAddress(toMachineWord p)
    in
        type pretty = address
        
        val tagPrettyBlock = 0w0
        and tagPrettyBreak = 0w1
        (*and tagPrettyLineBreak = 0w2*)        (* Not used in the compiler. *)
        and tagPrettyString = 0w3
        (*and tagPrettyStringWithWidth = 0w4*)   (* Not used in the compiler. *)
        
        val maxPrettyTag = 0w4 (* Exported because it is used in TagTest. *)

        fun PrettyBlock(offset: FixedInt.int, consistent: bool, context: context list, items: pretty list): pretty =
            cast(tagPrettyBlock, offset, consistent, context, items)
        and PrettyBreak(breaks: FixedInt.int, offset: FixedInt.int): pretty = cast(tagPrettyBreak, breaks, offset)
        and PrettyString(s: string): pretty = cast(tagPrettyString, s)

        fun isPrettyBlock p = toShort(loadWord(p, 0w0)) = tagPrettyBlock
        and isPrettyBreak p = toShort(loadWord(p, 0w0)) = tagPrettyBreak
        and isPrettyString p = toShort(loadWord(p, 0w0)) = tagPrettyString

        fun projPrettyBlock p =
            if isPrettyBlock p
            then
            let
                val (_: int, offset: int, consistent: bool, context: context list, items: pretty list) =
                    RunCall.unsafeCast p
            in
                (offset, consistent, context, items)
            end
            else raise Match

        and projPrettyBreak p =
            if isPrettyBreak p
            then
            let
                val (_: int, breaks: int, offset: int) = RunCall.unsafeCast p
            in
                (breaks, offset)
            end
            else raise Match

        and projPrettyString p =
            if isPrettyString p
            then
            let
                val (_: int, s: string) = RunCall.unsafeCast p
            in
                s
            end
            else raise Match
    end

    fun uglyPrint p =
        if isPrettyBlock p then String.concat(map uglyPrint(#4 (projPrettyBlock p)))
        else if isPrettyBreak p then String.implode(List.tabulate(#1 (projPrettyBreak p), fn _ => #" "))
        else projPrettyString p

    (* Pretty printer copied directly from basis/PrettyPrinter.  We can't use the
       same code because the "pretty" type is not the same. *)
    fun prettyPrint (stream : string -> unit, lineWidth : int) (pretty: pretty): unit =
    let
        fun printBlanks n =
            if n > 0 then (stream " "; printBlanks(n-1)) else ()

        (* Find out whether the block fits and return the space left if it does.
           Terminates with NONE as soon as it finds the line doesn't fit. *)
        fun getSize(p, spaceLeft) =
            if isPrettyBlock p
            then
            let
                val (_, _, _, entries) = projPrettyBlock p
            in
                List.foldl(fn (p, SOME s) => getSize(p, s) | (_, NONE) => NONE)
                    (SOME spaceLeft) entries
            end
            
            else if isPrettyBreak p
            then
            let
                val (blanks, _) = projPrettyBreak p
            in
                if blanks <= spaceLeft then SOME(spaceLeft-blanks) else NONE
            end
            
            else
            let
                val size = String.size (projPrettyString p)
            in
                if size <= spaceLeft
                then SOME(spaceLeft-size)
                else NONE
            end

        (* Lay out the block and return the space that is left after the line
           has been printed. *)
        fun layOut (p, indent, spaceLeft) =
            if isPrettyBlock p
            then
            let
                val (blockOffset, consistent, _, entries) = projPrettyBlock p
                val blockIndent = indent+blockOffset
            in
                case getSize(p, spaceLeft) of
                    SOME s => (* Fits *)
                    (
                        (* Lay out the contents. This will not need to break. *)
                        List.foldl(fn(p, space) => layOut(p, blockIndent, space)) spaceLeft entries;
                        s
                    )
                |   NONE => (* Doesn't fit - break line somewhere. *)
                    let
                        (* Lay out this block, breaking where necessary. *)
                        fun doPrint([], left) = (* Finished: return what's left. *) left
                        
                        |   doPrint(hd :: rest, left) =
                            if isPrettyBreak hd
                            then if null rest
                            then left (* Ignore trailing breaks. *)
                            else
                            let
                                val (blanks, breakOffset) = projPrettyBreak hd
                                 (* Compute the space of the next item(s) up to the end or the
                                   next space.  Since we only break at spaces if there are
                                   Blocks or Strings without spaces between we need to know
                                   the total size. *)
                                fun getsp([], left) = SOME left
                                |   getsp(next::rest, left) =
                                        if isPrettyBreak next
                                        then SOME left
                                        else case getSize(next, left) of
                                            NONE => NONE
                                        |   SOME sp => getsp(rest, sp)
                            in
                                if consistent orelse left <= blanks orelse
                                    not(isSome(getsp(rest, left-blanks)))
                                then (* Either a consistent break or the next item won't fit. *)
                                (
                                    stream "\n";
                                    printBlanks(blockIndent+breakOffset);
                                    doPrint(rest, lineWidth-blockIndent-breakOffset)
                                )
                                else (* We don't need to break here. *)
                                (
                                    printBlanks blanks;
                                    doPrint(rest, left-blanks)
                                )
                            end
                            
                            else if isPrettyString hd
                            then
                            let
                                val s = projPrettyString hd
                            in
                                stream s;
                                doPrint(rest, left-size s)
                            end

                            else (* Block *) doPrint(rest, layOut(hd, blockIndent, left))

                        val onLine = doPrint(entries, spaceLeft);
                    in
                        onLine
                    end
            end
            
            else if isPrettyBreak p
            then
            let
                val (blanks, _) = projPrettyBreak p
            in
                printBlanks blanks; Int.max(spaceLeft-blanks, 0)
            end
            
            else
            let
                val st = projPrettyString p
            in
                stream st; Int.max(spaceLeft-String.size st, 0)
            end

    in
        if layOut(pretty, 0, lineWidth) <> lineWidth
        then stream "\n" (* End the line unless we haven't written anything. *)
        else ()
    end

    local
        open Universal
    in
        (* Tag for pretty printed out from PolyML.print. *)
        val printOutputTag : (pretty -> unit) tag = tag()
        (* Compiler output.  Used for timing information and compiler debug output. *)
        and compilerOutputTag: (pretty->unit) tag = tag()
    end

    local
        open Universal
        fun getTag (t: (pretty -> unit) tag) (tagList: universal list) : pretty -> unit =
            case List.find (tagIs t) tagList of
                SOME a => tagProject t a
            |   NONE => fn _ => () (* Use the default *)
    in
        val getPrintOutput = getTag printOutputTag
        and getCompilerOutput = getTag compilerOutputTag
        
        (* The low-level code-generators print strings a bit at a time and separate the lines
           with new-line characters.  This provides a simple string printer for backwards
           compatibility.  It has now been extended with explicit tab stops.  By default
           tabs are inserted every eight characters. *)
        fun getSimplePrinter(parameters, tabStops) =
        let
            val compilerOut: pretty -> unit = getTag compilerOutputTag parameters
            val buff = ref ""
            fun printStream (s: string) =
            let
                (* If there's a newline split there. *)
                val (a, b) = Substring.splitl(fn #"\n" => false | _ => true) (Substring.full(!buff ^ s))
            in
                if Substring.size b = 0 (* No newline. *)
                then buff := Substring.string a
                else
                let
                    val str = Substring.string a
                    (* Split at the tabs. *)
                    val fields = String.fields (fn #"\t" => true | _ => false) str
                    fun nSpaces n = if n <= 0 then "" else CharVector.tabulate(n, fn _ => #" ")
                    fun rebuild(f::l, tabs) =
                        let
                            fun findTab(length, []) =
                                (8 - Int.rem(length, 8), [])
                            |   findTab(length, tab::tabs) =
                                if length >= tab
                                then
                                let
                                    val (t, ts) = findTab(length-tab, tabs)
                                in
                                    (t+length, ts)
                                end
                                else (tab-length, tabs)
                            val (spaces, nextTab) = findTab(size f, tabs)
                        in
                            f :: nSpaces spaces :: rebuild(l, nextTab)
                        end

                    |   rebuild([], _) = []
                in
                    compilerOut(PrettyString(String.concat(rebuild(fields, tabStops))));
                    buff := "";
                    printStream(Substring.string(Substring.slice(b, 1, NONE))) 
                end
            end
        in
            printStream
        end
    end

    (* Types that can be shared. *)
    structure Sharing =
    struct
        type pretty     = pretty
        and  context    = context
    end
end;
