(*
    Copyright (c) 2000-7
        Cambridge University Technical Services Limited

    Further Development
        Copyright 2009, 2016 David C.J. Matthews.
        Copyright 2025, Martin Desharnais

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

signature PARSEDECSIG =
(*****************************************************************************)
(*                  PARSEDEC export signature                                *)
(*****************************************************************************)
sig
    type symset;
    type lexan;
    type fixStatus;
    type fixes =
        { enterFix:  string * fixStatus -> unit,
          lookupFix: string -> fixStatus option };
    type params =
        { recordPunning: bool };
    type program;

    val parseDec: symset * lexan * fixes * params -> program;
end
