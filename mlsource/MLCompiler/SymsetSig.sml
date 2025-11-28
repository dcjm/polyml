(*
    Copyright (c) 2013 David C.J. Matthews

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

signature SymsetSig =
sig
  type symset;
  type sys
   
  val inside: sys * symset -> bool;
  
  val ++ :symset * symset -> symset;  
  val abortParse:   symset;
  val ident:        symset;
  val abstypeSy:    symset;
  val andSy:        symset;
  val andalsoSy:    symset;
  val asSy:         symset;
  val caseSy:       symset;
  val datatypeSy:   symset;
  val doSy:         symset;
  val elseSy:       symset;
  val endSy:        symset;
  val exceptionSy:  symset;
  val fnSy:         symset;
  val funSy:        symset;
  val handleSy:     symset;
  val ifSy:         symset;
  val inSy:         symset;
  val infixSy:      symset;
  val infixrSy:     symset;
  val letSy:        symset;
  val localSy:      symset;
  val nonfixSy:     symset;
  val ofSy:         symset;
  val opSy:         symset;
  val openSy:       symset;
  val orelseSy:     symset;
  val raiseSy:      symset;
  val recSy:        symset;
  val thenSy:       symset;
  val typeSy:       symset;
  val valSy:        symset;
  val withSy:       symset;
  val whileSy:      symset;
  val leftParen:    symset;
  val rightParen:   symset;
  val leftBrack:    symset;
  val rightBrack:   symset;
  val comma:        symset;
  val colon:        symset;
  val semicolon:    symset;
  val thickArrow:   symset;
  val verticalBar:  symset;
  val equalsSign:   symset;
  val underline:    symset;
  val typeIdent:    symset;
  val stringConst:  symset;
  val integerConst: symset;
  val asterisk:     symset;
  val arrow:        symset;
  val realConst:    symset;
  val wordConst:    symset;
  val charConst:    symset;
  val leftCurly:    symset;
  val rightCurly:   symset;
  val threeDots:    symset;
  val colonGt:      symset;
  val hashSign:     symset;
  val structureSy:  symset;
  val signatureSy:  symset;
  val structSy:     symset;
  val sigSy:        symset;
  val sharingSy:    symset;
  val functorSy:    symset;
  val withtypeSy:   symset;
  val eqtypeSy:     symset;
  val includeSy:    symset;
  val whereSy:      symset;
  val othersy:      symset;
  val empty:        symset;

  val variableSys:        symset;
  val constructorSys:     symset;
  val startAtomicSys:     symset;
  val startPatternSys:    symset;
  val startMatchSys:      symset;
  val startExpressionSys: symset;
  val startDecSys:        symset;
  val declarableVarSys:   symset;
  val startTypeSys:       symset;
  val startSigSys:        symset;
  val startTopSys:        symset;

end;

