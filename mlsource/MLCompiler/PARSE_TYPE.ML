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
    Title:  Parse Types.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

functor PARSE_TYPE (

    structure SYMBOLS : SymbolsSig
    structure SYMSET : SymsetSig
    structure LEX : LEXSIG
    structure SKIPS : SKIPS

    structure UTILITIES :
    sig
        val noDuplicates: (string * 'a * 'a -> unit) -> 
                           { apply: (string * 'a -> unit) -> unit,
                             enter:  string * 'a -> unit,
                             lookup: string -> 'a option };
    end
    
    structure PARSETREE: PARSETREESIG

    sharing LEX.Sharing
    =   SKIPS.Sharing
    =   SYMBOLS.Sharing
    =   SKIPS.Sharing
    =   PARSETREE.Sharing
 
    sharing type SYMBOLS.sys = SYMSET.sys
    sharing type SYMSET.symset = SKIPS.symset
                 
) :  PARSETYPESIG =

struct
    open LEX
    open SYMSET
    open SKIPS
    open SYMBOLS  
    open UTILITIES
    open PARSETREE
   
    infix 8 ++;
    infix 8 inside;
    
    val tyseqSyntax = SYMSET.comma ++ SYMSET.rightParen
    val lrSyntax    = SYMSET.comma ++ SYMSET.rightCurly
   
    fun parseType (fsys, lex, env) =
    let
        fun tupleType fsys =
        let
            fun basicType fsys =
            let (* First part may be a type sequence. *)
                val sym = sy lex and startLocn = location lex
                val (tySeq, seqLocn) =
                    case sym of
                        LeftParen => (* sequence of types *)
                        let
                            fun processList () =
                            let
                                val thisType = 
                                    if sy lex inside startTypeSys
                                    then #1 (parseType (fsys ++ tyseqSyntax, lex, env))
                                    else
                                    (
                                        badsyms (TypeIdent, lex);
                                        ParseTypeBad (* not there *)
                                    );
                                fun testfor (sym, startsys, lex) =
                                  (* repeat if the separator or a starting sym is found *)
                                  if sy lex = sym
                                    then (insymbol lex; true)
                                  else if sy lex inside startsys
                                    then (badsyms (sym, lex); true)
                                  else false;

                            in (* Check for any more *)
                                if testfor (SYMBOLS.Comma, startTypeSys, lex)
                                then thisType :: processList() (* get some more *)
                                else [thisType] (* that's it *)
                            end (* processList *);

                            val ()      = insymbol lex;  (* Remove opening bracket *)
                            val sequence = processList(); (* read list of items *)
                            val endLocn = location lex (* Should be the loc. of the close paren. *)
                        in
                            getsym (SYMBOLS.RightParen, lex);
                            (sequence, locSpan(startLocn, endLocn))
                        end

                    |   LeftCurly =>
                        let
                            val () = insymbol lex; (* Remove opening bracket *)
                            val posEnd = location lex
                        in
                            case sy lex of
                                RightCurly =>
                                let
                                    val () = insymbol lex
                                    val locs = locSpan(startLocn, posEnd)
                                in
                                    ([unitTree locs], locs)
                                end
        
                            |   _ =>
                                let
                                    (* The same label name should not be used more than once. *)
                                    fun reportDup (name, newLoc, _) =
                                        errorMessage (lex, newLoc, "Label (" ^ name ^ ") appears more than once.")
                                    val dupCheck = noDuplicates reportDup
                                    (* All the labels should be the same sort. *)
                                    fun getNameAndType () =
                                        let
                                            val nameAndLoc as (_, nameLoc) =
                                                getLabel (fsys ++ SYMSET.colon, lex);
                                            val () = #enter dupCheck nameAndLoc;
                                            val () = getsym (SYMBOLS.Colon, lex);
                                            val (types, typeLoc) = parseType (fsys ++ lrSyntax, lex, env)
                                            val fullLoc = locSpan(nameLoc, typeLoc)
                                        in
                                            ((nameAndLoc, types, fullLoc), fullLoc, ())
                                        end
                                    val (l, _, _) = 
                                        getList (SYMBOLS.Comma, empty, lex) getNameAndType ()
                                    val locs = locSpan(startLocn, location lex) (* Include '}' *)
                                in
                                    getsym (SYMBOLS.RightCurly, lex);
                                    ([makeParseTypeLabelled(l, true, locs) (* frozen *)], locs)
                                end
                        end
                    
                    |   TypeIdent =>
                        let (* type variable *)
                            val ty = #lookupTvar env (id lex);
                        in
                            getsym (TypeIdent, lex);
                            ([makeParseTypeId(ty, startLocn)], startLocn)
                        end
      
                    |   Ident =>
                        (* Constructor such as `int' *)
                        let
                            val idLocn as (_, locn) = getid (SYMSET.ident, fsys, lex)
                        in
                            ([makeParseTypeConstruction (idLocn, ([], locn), locn)], locn)
                        end

                    |   _ =>
                        (
                            badsyms (SYMBOLS.Ident, lex);
                            ([], startLocn)
                        )
            in
                (* Type sequence read. Can now have some type constructors. *)
                case (sy lex, tySeq) of
                    (Ident, _) =>
                    let (* Returns the type made from the constructors. *)
                        fun constructors(args, argLoc) =
                        let
                            val idAndLoc as (_, idLoc) = (id lex, location lex)
                            val loc = locSpan(argLoc, idLoc)
                            val constructed = makeParseTypeConstruction(idAndLoc, (args, argLoc), loc);
                        in
                            insymbol lex;
                            if sy lex = SYMBOLS.Ident
                            then constructors([constructed], loc)
                            else (constructed, loc)
                        end;
                    in
                        constructors(tySeq, seqLocn)
                    end

                (* no constructor - get the first part of the sequence
                   and check that that's all. *)
                |   (_, [])   =>    (ParseTypeBad, seqLocn)
                |   (_, [t])  => (t, seqLocn)
                |   (_, t::_) => (badsyms (SYMBOLS.Ident, lex); (t, seqLocn))
            end (* basicType *);

            (* ty * .. * ty  *)
            fun getProduct () =
            let
                val fsys' = fsys ++ SYMSET.asterisk;
                val (firstPart, firstLocn) = basicType fsys'
            in
                case sy lex of
                    Asterisk =>
                    let
                        val () = insymbol lex
                        val (rest, restLocn) = getProduct ()
                    in
                        (firstPart :: rest, locSpan(firstLocn, restLocn))
                    end
                |   _ => ([firstPart], firstLocn)
            end
        in
            case getProduct () of
                ([notProduct], locn) => (notProduct, locn)
            |   (product, locn) => (makeParseTypeProduct(product, locn), locn)
        end  (* tupleType *)(* ty -> ty *)
   
        val (firstType, firstLoc) = tupleType (fsys ++ SYMSET.arrow);
    in
        case sy lex of
            Arrow =>
            let
                val () = insymbol lex
                val (resType, resLocn) = parseType (fsys, lex, env)
                val locs = locSpan(firstLoc, resLocn)
            in
                (makeParseTypeFunction (firstType, resType, locs), locs)
            end
        |   _ =>
            (
                skipon (fsys, empty, "End of type", lex);
                (firstType, firstLoc)
            )
    end

    structure Sharing =
    struct
        type symset         = symset
        type lexan          = lexan
        type typeParsetree  = typeParsetree
        type parseTypeVar   = parseTypeVar
    end

end;
