(*
    Copyright (c) 2013. 2025 David C.J. Matthews

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

signature SymbolsSig =
sig
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
  
  val repr: sys -> string
  val lookup: string -> sys

    structure Sharing:
    sig
        type sys = sys
    end
end;
