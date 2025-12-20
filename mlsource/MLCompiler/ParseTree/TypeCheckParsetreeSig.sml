(*
    Copyright (c) 2013, 2025 David C.J. Matthews

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

signature TypeCheckParsetreeSig =
sig
    type parsetree
    type types
    type parseTypeVar
    type typeId
    type lexan
    type env

    type location =
        { file: string, startLine: FixedInt.int, startPosition: FixedInt.int,
          endLine: FixedInt.int, endPosition: FixedInt.int }

    type typeIdDescription = { location: location, name: string, description: string }

    val pass2:
        parsetree * (bool * bool * (parseTypeVar list * types option) * typeIdDescription -> typeId) *
        env * lexan * (int -> bool) -> types

    structure Sharing:
    sig
        type parsetree = parsetree
        type types = types
        type parseTypeVar = parseTypeVar
        type typeId = typeId
        type lexan = lexan
        type env = env
    end
end;
