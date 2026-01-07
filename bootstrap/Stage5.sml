(*
    Title:      Fifth stage bootstrap from interpreted code.
    Copyright   David Matthews 2020

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

val () = Bootstrap.print "******Bootstrap stage 5 of 7******\n"

val () = Bootstrap.use "basis/build.sml";

val args = CommandLine.arguments()
fun getPath [] = "." (* Default path *)
|   getPath ("-I" :: path :: _) = path
|   getPath (_::tl) = getPath tl
val path = OS.Path.concat(getPath args,  "bootstrap/Stage6.sml");
(* We've now set up the new name space.  Compile the compiler. *)
PolyML.use path;

