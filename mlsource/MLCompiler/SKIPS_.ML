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
    Title:      Auxiliary Parsing Functions.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

functor SKIPS_ (
structure SYMBOLS : SymbolsSig
structure LEX : LEXSIG
structure SYMSET : SymsetSig

sharing SYMBOLS.Sharing = LEX.Sharing
sharing type SYMSET.sys = SYMBOLS.sys

) : SKIPS

=

struct
  infix 5 inside;
  infix 6 ++;

  open SYMBOLS LEX SYMSET
        
  local
    (* Result is in ``id'' *)    
    val idSys = ident ++ typeIdent ++ integerConst ++ realConst;
  in
    (* Try to recreate the original lexical item. *)
    fun notfound (expected, lex) = 
    let
      val found = 
        if sy lex = SYMBOLS.StringConst 
          then "\"" ^ id lex ^ "\""
        else if sy lex inside idSys 
              then id lex
        else repr (sy lex)
    in
      errorMessage (lex, location lex,
         expected ^ " expected but " ^ found ^ " was found")
    end;
  end;

  fun badsyms (sym, lex) = 
    (notfound (repr sym, lex); insymbol lex);

  fun getsym (sym, lex) = 
     if (sy lex) = sym then insymbol lex else notfound (repr sym, lex);

  infix 5 notin;
  
  fun a notin b = not (a inside b);

  (* used at the end of a syntactic unit to check that the current symbol
      is a suitable terminator *)
  fun skipon (a, b, errmss, lex) = 
    if (sy lex) notin a
    then
      (
        notfound (errmss, lex);
        while sy lex notin (a ++ b) do insymbol lex
      )
    else ();

  (* returns an identifier *)
    fun getid (syms, fsys, lex) = 
    if (sy lex) inside syms
    then
    let
        val iden = id lex
        val loc = location lex
    in
        insymbol lex;
        (iden, loc)
    end
    else
      (
        notfound ("Identifier", lex);
        while sy lex notin fsys do insymbol lex;
        ("", nullLocation)
      );

  (* Read a label and check that it is valid if numeric. *)
    fun getLabel (fsys, lex) = 
    if (sy lex) = SYMBOLS.IntegerConst
    then
    let
        val iden = id lex;
        val loc = location lex
        val firstCh = String.str(String.sub(iden, 0));
    in
        insymbol lex;
        if firstCh = "~" orelse firstCh = "0"
        then errorMessage (lex, location lex, "Labels must be 1,2,3,....")
        else ();
        (iden, loc)
    end
    else getid (declarableVarSys, fsys, lex);

  (* Tests for a symbol and removes it if it is there.
     startsys is the set of symbols which can start each element. *)
  fun testfor (sym, startsys, lex) =
      (* repeat if the separator or a starting sym is found *)
      if sy lex = sym
        then (insymbol lex; true)
      else if sy lex inside startsys
        then (badsyms (sym, lex); true)
      else false;


    fun getList (separator, startsys, lex) foreach n =
    let
        val startLoc = location lex
        fun forList list n =
        let
            val (item, itemLoc, next) = foreach n
        in
            (* Add each item to the list.  The final span is from
               the start to the final location.  Make the list and then
               reverse it.  That's avoids quadratic performance on long lists. *)
            if testfor (separator, startsys, lex)
            then forList(item :: list) next
            else (List.rev(item :: list), locSpan(startLoc, itemLoc), n)
        end
    in
        forList [] n
    end;
    
    structure Sharing =
    struct
        type sys        = sys
        and lexan       = lexan
        and symset      = symset
    end

end (* SKIPS *);
