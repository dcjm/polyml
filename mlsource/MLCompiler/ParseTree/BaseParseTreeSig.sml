(*
    Copyright (c) 2013-2016, 2025 David C.J. Matthews

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

(*
    Derived from the original parse-tree

    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further development:
    Copyright (c) 2000-13, 2025 David C.J. Matthews

    Title:      Parse Tree Structure and Operations.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985

*)

signature BaseParseTreeSig =
sig
    type types
    and  parseTypeVar
    and  typeConstrSet
    and  values
    and  infixity
    and  structVals
    and  instanceType

    type typeParsetree

    type location =
        { file: string, startLine: FixedInt.int, startPosition: FixedInt.int,
          endLine: FixedInt.int, endPosition: FixedInt.int }

    type breakPoint = bool ref

    datatype parsetree = 
        Ident               of
      (* An identifier is just a name. In the second pass it is associated
         with a particular declaration and the type is assigned into the
         type field. The type of this identifier is needed to deal with
         overloaded operators. If we have an occurence of ``='', say, the
         type of the value will be 'a * 'a -> bool but the type of a particular
         occurence, i.e. the type of the identifier must be int * int -> bool,
         say, after all the unification has been done. *)
        {
            name: string,
            expType: (instanceType * types list) ref,
            value: values ref,
            location: location,
            possible: (unit -> string list) ref (* Used with the IDE. *)
        }

    |   Literal             of
           (* Literal constants may be overloaded on more than one type. The
              types are specified by installing appropriate conversion functions:
              convInt, convReal, convChar, convString and convWord. *)
            { converter: values, expType: instanceType ref, literal: string, location: location }

    |   Applic              of
            (* Function application *)
            { f: parsetree, arg: parsetree, location: location, isInfix: bool, expType: instanceType ref }

    |   Cond                of
            (* Conditional *)
        {
            test: parsetree,
            thenpt: parsetree,
            elsept: parsetree,
            location: location,
            thenBreak: breakPoint option ref,
            elseBreak: breakPoint option ref
        } 

    |   TupleTree           of { fields: parsetree list, location: location, expType: types ref }

    |   ValDeclaration      of
        {
            dec:    valbind list,
            explicit: {lookup: string -> parseTypeVar option,
                       apply: (string * parseTypeVar -> unit) -> unit },
            implicit: {lookup: string -> parseTypeVar option,
                       apply: (string * parseTypeVar -> unit) -> unit },
            location: location
        }

    |   FunDeclaration      of
        {
            dec:    fvalbind list,
            explicit: {lookup: string -> parseTypeVar option,
                       apply: (string * parseTypeVar -> unit) -> unit },
            implicit: {lookup: string -> parseTypeVar option,
                       apply: (string * parseTypeVar -> unit) -> unit },
            location: location
        } 

    |   OpenDec             of
            (* Open a structure.  The variables, structures and types are just needed if
               debugging information is being generated. *)
        {
            decs: structureIdentForm list,
            variables: values list ref,
            structures: structVals list ref,
            typeconstrs: typeConstrSet list ref,
            location: location
        }

    |   Constraint          of
           (* Constraint (explicit type given) *)
           (* A constraint has a value and a type. The actual type, will, however
              be the unification of these two and not necessarily the given type. *)
            { value: parsetree, given: typeParsetree, location: location }

    |   Layered             of
          (* Layered pattern. Equivalent to an ordinary pattern except that the
             variable is given the name of the object which is to be matched. *)
            { var: parsetree, pattern: parsetree, location: location }

    |   Fn of { matches: matchtree list, location: location }

    |   Localdec            of (* Local dec in dec and let dec in exp. *)
        {
            decs: (parsetree * breakPoint option ref) list,
            body: (parsetree * breakPoint option ref) list,
            isLocal: bool,
            varsInBody: values list ref, (* Variables in the in..dec part
                                            of a local declaration. *)
            location: location
        } 

    |   TypeDeclaration     of typebind list * location

    |   AbsDatatypeDeclaration  of (* Datatype and Abstract Type declarations *)
        {
            isAbsType: bool,
            typelist:  datatypebind list,
            withtypes: typebind list,
            declist:   (parsetree * breakPoint option ref) list,
            location:  location,
            equalityStatus: bool list ref
        }

    |   DatatypeReplication of
        {
            newType:  string,
            oldType:  string,
            oldLoc:   location,
            newLoc:   location,
            location: location
        }

    |   ExpSeq              of (parsetree * breakPoint option ref) list * location

    |   Directive           of
            (* Directives are infix, infixr and nonfix. They are processed by the
               parser itself and only appear in the parse tree for completeness. *)
            { tlist: string list, fix: infixity, location: location } 

    |   ExDeclaration       of exbind list * location

    |   Raise               of parsetree * location

    |   HandleTree          of
            (* Execute an expression and catch any exceptions. *)
            { exp: parsetree, hrules: matchtree list, location: location, listLocation: location }

    |   While               of
            (* Ordinary while-loop *)
            { test: parsetree, body: parsetree, location: location, breakPoint: breakPoint option ref } 

    |   Case                of
            (* Case-statement *)
            { test: parsetree, match: matchtree list, location: location, listLocation: location }

    |   Andalso             of { first: parsetree, second: parsetree, location: location } 

    |   Orelse              of { first: parsetree, second: parsetree, location: location }

    |   Labelled            of
        (* Labelled record & the entry in the list. "frozen" is false if it's
           a pattern with "...". *)
            { recList: labelRecEntry list, frozen: bool, expType: types ref, location: location }

    |   Selector            of
            { name: string, labType: types, typeof: types, location: location }

    |   List of { elements: parsetree list, location: location }
    |   EmptyTree
    |   WildCard            of location
    |   Unit                of location
    |   Parenthesised       of parsetree * location
   
    and valbind = (* Value bindings.*)
        ValBind of (* Consists of a declaration part (pattern) and an expression. *)
        {
            dec: parsetree,
            exp: parsetree,
            line: location,
            isRecursive: bool,
            variables: values list ref (* list of variables declared and their poly vars *)
        } 
    
   and fvalbind = (* Function binding *)
   (* `Fun' bindings *)
      (* A function binding is a list of clauses, each of which uses a
         valBinding to hold the list of patterns and the corresponding function
         body. The second pass extracts the function variable and the number of
         patterns in each clause. It checks that they are the same in each
         clause. *)
       FValBind of
         {
           clauses:     fvalclause list, 
           numOfPatts:  int ref,
           functVar:    values ref,
           argType:     types ref,
           resultType:  types ref,
           location:    location
         }

    and fvalclause = (* Clause within a function binding. *)
        FValClause of
        {
            dec: funpattern,
            exp: parsetree,
            line: location,
            breakPoint: breakPoint option ref
        }
        
    and typebind = (* Non-generative type binding *)
        TypeBind of
         {
           name: string,
           typeVars: parseTypeVar list,
           decType: typeParsetree option,
           isEqtype: bool, (* True if this was an eqtype in a signature. *)
           tcon:     typeConstrSet ref,
           nameLoc:  location,
           fullLoc:  location
         } 

    and datatypebind = (* Generative type binding *)
        DatatypeBind of
         {
           name:          string,
           typeVars:      parseTypeVar list,
           constrs:       valueConstr list,
           tcon:          typeConstrSet ref,
           nameLoc:       location,
           fullLoc:  location
         }

   and exbind = (* An exception declaration. It has a name and
                   optionally a previous exception and a type. *)
        ExBind of
         {
           name:         string,
           previous:     parsetree,
           ofType:       typeParsetree option,
           value:        values ref,
           nameLoc:      location,
           fullLoc:      location
         } 

    and matchtree =
    (* A match is a pattern and an expression. If the pattern matches then
       the expression is evaluated in the environment of the pattern. *)
        MatchTree of
        {
            vars: parsetree,
            exp: parsetree,
            location: location,
            argType: types ref,
            resType: types ref,
            breakPoint: breakPoint option ref
        } 

    (* Name of a structure. Used only in an ``open'' declaration. *)
    withtype structureIdentForm = 
    {
        name:   string,
        value:  structVals option ref,
        location: location
    } 

    (* An entry in a label record in an expression or a pattern. *)
    and labelRecEntry =
    {
        name: string,
        nameLoc: location,
        valOrPat: parsetree,
        fullLocation: location,
        expType: types ref
    }
    
    and funpattern = (* The declaration part of a fun binding. *)
        { ident: { name: string, expType: types ref, location: location },
          isInfix: bool, args: parsetree list, constraint: typeParsetree option }

    and valueConstr =
        {constrName: string, constrArg: typeParsetree option, idLocn: location, constrVal: values ref}

    structure Sharing:
    sig
        type types = types
        and  parseTypeVar = parseTypeVar
        and  typeConstrSet = typeConstrSet
        and  values = values
        and  infixity = infixity
        and  structVals = structVals
        and  typeParsetree = typeParsetree
        and  parsetree = parsetree
        and  valbind = valbind
        and  fvalbind = fvalbind
        and  fvalclause = fvalclause
        and  typebind = typebind
        and  datatypebind = datatypebind
        and  exbind = exbind
        and  matchtree = matchtree
        and  instanceType = instanceType
    end
end;
