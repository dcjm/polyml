(*
    Copyright (c) 2013, 2015, 2025 David C.J. Matthews

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

signature MatchCompilerSig =
sig
    type parsetree
    type level
    type codetree
    type matchtree
    type codeBinding
    type lexan

    type location =
        { file: string, startLine: FixedInt.int, startPosition: FixedInt.int,
          endLine: FixedInt.int, endPosition: FixedInt.int }
    and matchContext = { mkAddr: int->int, level: level, lex: lexan }

    val codeMatchPatterns:
        matchtree list * codetree * bool * location * (int -> codetree) * matchContext -> codetree * bool
    and codeBindingPattern:
        parsetree * codetree * location * matchContext -> codeBinding list * bool

    structure Sharing:
    sig
        type parsetree = parsetree
        type level = level
        type codetree = codetree
        type matchtree = matchtree
        type codeBinding = codeBinding
        type lexan = lexan
    end
end;
