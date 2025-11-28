(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)


(*
    Title:      Lexical symbols for ML compiler.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

structure Symbols : SymbolsSig =

struct
    datatype sys =
        AbortParse
    |   Ident
    |   AbstypeSy
    |   AndSy 
    |   AndalsoSy 
    |   AsSy 
    |   CaseSy
    |   DatatypeSy 
    |   DoSy 
    |   ElseSy 
    |   EndSy 
    |   ExceptionSy 
    |   FnSy 
    |   FunSy
    |   HandleSy 
    |   IfSy 
    |   InSy 
    |   InfixSy 
    |   InfixrSy 
    |   LetSy 
    |   LocalSy
    |   NonfixSy 
    |   OfSy 
    |   OpSy 
    |   OpenSy 
    |   OrelseSy 
    |   RaiseSy 
    |   RecSy 
    |   ThenSy
    |   TypeSy 
    |   ValSy 
    |   WithSy 
    |   WhileSy 
    |   LeftParen 
    |   RightParen
    |   LeftBrack
    |   RightBrack
    |   Comma 
    |   Colon 
    |   Semicolon 
    |   ThickArrow
    |   VerticalBar
    |   EqualsSign
    |   Underline 
    |   TypeIdent
    |   StringConst
    |   IntegerConst
    |   Asterisk 
    |   Arrow 
    |   RealConst 
    |   LeftCurly
    |   RightCurly
    |   ThreeDots
    |   ColonGt
    |   HashSign 
    |   StructureSy 
    |   SignatureSy
    |   StructSy 
    |   SigSy 
    |   SharingSy 
    |   FunctorSy 
    |   WithtypeSy
    |   EqtypeSy
    |   IncludeSy
    |   WhereSy
    |   WordConst
    |   CharConst
    |   Othersy

    fun repr AbortParse     = "\^G"
    |   repr Ident          = "<identifier>"
    |   repr AbstypeSy      = "abstype"
    |   repr AndSy          = "and"
    |   repr AndalsoSy      = "andalso"
    |   repr AsSy           = "as"
    |   repr CaseSy         = "case"
    |   repr DatatypeSy     = "datatype"
    |   repr DoSy           = "do"
    |   repr ElseSy         = "else"
    |   repr EndSy          = "end"
    |   repr ExceptionSy    = "exception"
    |   repr FnSy           = "fn"
    |   repr FunSy          = "fun"
    |   repr HandleSy       = "handle"
    |   repr IfSy           = "if"
    |   repr InSy           = "in"
    |   repr InfixSy        = "infix"
    |   repr InfixrSy       = "infixr"
    |   repr LetSy          = "let"
    |   repr LocalSy        = "local"
    |   repr NonfixSy       = "nonfix"
    |   repr OfSy           = "of"
    |   repr OpSy           = "op"
    |   repr OpenSy         = "open"
    |   repr OrelseSy       = "orelse"
    |   repr RaiseSy        = "raise"
    |   repr RecSy          = "rec"
    |   repr ThenSy         = "then"
    |   repr TypeSy         = "type"
    |   repr ValSy          = "val"
    |   repr WithSy         = "with"
    |   repr WhileSy        = "while"
    |   repr LeftParen      = "("
    |   repr RightParen     = ")"
    |   repr LeftBrack      = "["
    |   repr RightBrack     = "]"
    |   repr Comma          = ","
    |   repr Colon          = ":"
    |   repr Semicolon      = ";"
    |   repr ThickArrow     = "=>"
    |   repr VerticalBar    = "|"
    |   repr EqualsSign     = "="
    |   repr Underline      = "_"
    |   repr TypeIdent      = "<type identifier>"
    |   repr StringConst    = "<string constant>"
    |   repr IntegerConst   = "<integer constant>"
    |   repr Asterisk       = "*"
    |   repr Arrow          = "->"
    |   repr RealConst      = "<real constant>"
    |   repr LeftCurly      = "{"
    |   repr RightCurly     = "}"
    |   repr ThreeDots      = "..."
    |   repr ColonGt        = ":>"
    |   repr HashSign       = "#"
    |   repr StructureSy    = "structure"
    |   repr SignatureSy    = "signature"
    |   repr StructSy       = "struct"
    |   repr SigSy          = "sig"
    |   repr SharingSy      = "sharing"
    |   repr FunctorSy      = "functor"
    |   repr WithtypeSy     = "withtype"
    |   repr EqtypeSy       = "eqtype"
    |   repr IncludeSy      = "include"
    |   repr WhereSy        = "where"
    |   repr WordConst      = "<word constant>"
    |   repr CharConst      = "<char constant>"
    |   repr Othersy        = "unknown symbol"

    local
        open HashTable;

        val reservedWords =
        [
            AbstypeSy,
            AndSy,
            AndalsoSy,
            AsSy,
            CaseSy,
            DatatypeSy, 
            DoSy,
            ElseSy, 
            EndSy,
            ExceptionSy,
            FnSy,
            FunSy,
            HandleSy,
            IfSy,
            InSy,
            InfixSy,
            InfixrSy,
            LetSy,
            LocalSy,
            NonfixSy,
            OfSy,
            OpSy,
            OpenSy,
            OrelseSy,
            RaiseSy,
            RecSy,
            ThenSy,
            TypeSy,
            ValSy,
            WithSy,
            WhileSy,
            StructureSy,
            SignatureSy,
            StructSy,
            SigSy,
            SharingSy,
            FunctorSy,
            WithtypeSy,
            EqtypeSy,
            IncludeSy,
            WhereSy,
            (* Reserved or special symbols *)
            Arrow,
            EqualsSign,
            ThickArrow,
            Asterisk,
            ColonGt,
            Underline,
            Colon,
            VerticalBar,
            HashSign
        ]

        local
            (* Make table sparse to minimise collisions. *)
            val table = hashMake (4 * List.length reservedWords);

            (* Enter each reserved word in the hash table *)
            val () = List.app (fn n => hashSet (table, repr n, n)) reservedWords
        in
            val table = hashFreeze table
        end
    in
        (* Returns the symbol if it is in the table else ident *)
        fun lookup str = getOpt(hashSub (table, str), Ident);
    end;

    structure Sharing =
    struct
        type sys = sys
    end
end (* Symbols *);
