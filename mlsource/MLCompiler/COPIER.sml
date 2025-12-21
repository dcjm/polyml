(*
    Copyright (c) 2009-2015, 2025 David C.J. Matthews

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
    Derived from the STRUCTURES module:
    Copyright (c) 2000-9
        Cambridge University Technical Services Limited
    Title:      Module Structure and Operations.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985

*)

functor COPIER(
    structure STRUCTVALS : STRUCTVALSIG;
    structure TYPETREE : TYPETREESIG

    structure UNIVERSALTABLE:
    sig
        type universal = Universal.universal
        type univTable
        type 'a tag = 'a Universal.tag

        val univEnter:  univTable * 'a tag * string * 'a -> unit;
        val univLookup: univTable * 'a tag * string -> 'a option;
        val univFold:   univTable * (string * universal * 'a -> 'a) * 'a -> 'a;
    end;
    
    structure UTILITIES:
    sig
        val splitString: string -> { first:string,second:string }
    end

sharing STRUCTVALS.Sharing = TYPETREE.Sharing

sharing type
    UNIVERSALTABLE.univTable
=   STRUCTVALS.univTable
)
:COPIERSIG =
struct
    open STRUCTVALS TYPETREE UNIVERSALTABLE UTILITIES
    open Universal; (* for tag record selectors *)

    type tsvEnv = { enterType:   string * typeConstrSet -> unit,
                  enterStruct: string * structVals  -> unit,
                  enterVal   : string * values      -> unit };

    (* Helper function from STRUCT_VALS.  Should be replaced by patterns. *)
    fun tcName       (TypeConstrs {name,...})       = name
    fun tcIdentifier (TypeConstrs {identifier,...}) = identifier
    fun tcLocations  (TypeConstrs {locations, ...}) = locations

    fun tsConstr(TypeConstrSet(ts, _)) = ts
    and tsConstructors(TypeConstrSet(_, tvs)) = tvs

    (* Type constructor cache.  This maps typeIDs in the copied signature to
       type constructors.  More importantly, it identifies a type constructor
       that carries that type ID so that when we copy the values the string
       name is appropriate. *)

    (* Generate new entries for all the elements of the signature.
       As well as copying the signature it also keeps track of addresses used in
       the signature for values.  This is needed because when we're constructing a signature
       we need to know the maximum address used.
       This is used to two cases only: when we have a named signature with possible sharing or
       "where types" or when including a signature.  Really these cases should renumber the
       value entries. *)
    fun localCopySig(sourceTab, resEnv, mapTypeId, singleLevel, strName, newMap, cacheTail): unit =
    let

        fun buildTypeCache(sourceTab, strName, mapTypeId, buildDatatypes, initialCache, cacheTail) =
        let
            (* Process sub-directories first.  That way they will be further down the list. *)
            fun foldSubStructs(dName, dVal, rest) =
            if tagIs structVar dVal
            then
            let
                val Struct { signat = Signatures { tab, typeIdMap, ...}, ...} = tagProject structVar dVal
            in
                buildTypeCache(tab, strName ^ dName ^ "." (* Add structure names. *),
                    composeMaps(typeIdMap, mapTypeId), buildDatatypes, initialCache, rest)
            end
            else rest

            (* Then the types within this structure. *)
            fun foldTypes(_, dVal, rest) =
            if tagIs typeConstrVar dVal
            then
            let
                val TypeConstrSet(tcon, _) = tagProject typeConstrVar dVal
                fun makeName s = strName ^ s
                fun copyId(TypeId{idKind=Bound{ offset, ...}, ...}) = SOME(mapTypeId offset)
                |   copyId _ = NONE
                fun makeTypeConstructor (name, uid, locations) =
                    TypeConstrs
                    {
                        name       = name,
                        identifier = uid,
                        locations = locations
                    }
            in
                (* On the first pass we build datatypes, on the second type abbreviations
                   using the copied datatypes. *)
                case tcIdentifier tcon of
                    TypeId{idKind=TypeFn{arity, resType=equiv, ...}, access, description, ...} =>
                    if buildDatatypes then rest (* Not on this pass. *)
                    else (* Build a new entry whether the typeID has changed or not. *)
                    let
                        val copiedEquiv =
                            copyType(equiv, fn _ => NONE,
                                fn tcon =>
                                    copyTypeConstrWithCache(tcon, copyId, makeName, initialCache))
                        val copiedId =
                            makeGeneralTypeFunction(arity, copiedEquiv, description, access)
                    in
                        makeTypeConstructor(makeName(tcName tcon), copiedId, tcLocations tcon) :: rest
                    end

                |   id =>
                    if not buildDatatypes then rest (* Not on this pass. *)
                    else
                    (
                        case copyId id of
                            NONE => rest (* Skip (or add to cache?) *)
                        |   SOME newId => makeTypeConstructor (makeName(tcName tcon), newId, tcLocations tcon) :: rest
                    )
            end
            else rest
        in
             univFold(sourceTab, foldTypes,
                univFold(sourceTab, foldSubStructs, cacheTail))
        end

        (* Process datatypes.  While processing these we make new entries for every
           datatype even if they are already in the cache.  That way we end up with
           the last entry in the list being the most local and that's the one we want
           to use for type abbreviations and values. *)
        val datatypeCache =
            buildTypeCache(sourceTab, strName, mapTypeId, true, (* Datatypes *) [], cacheTail)
        (* Now add any type abbreviations.  These can refer to datatypes we added in the
           previous pass but don't reuse type abbreviations we add elsewhere. *)
        val typeCache =
            buildTypeCache(sourceTab, strName, mapTypeId, false, (* Type abbreviations. *)datatypeCache, datatypeCache)

        fun copyTypeCons (tcon : typeConstrs) : typeConstrs =
        let
            fun copyId(TypeId{idKind=Bound{ offset, ...}, ...}) = SOME(mapTypeId offset)
            |   copyId _ = NONE
        in
            copyTypeConstrWithCache (tcon, copyId, fn s => strName ^ s, typeCache)
        end

        fun copyTyp (t : types) : types = copyType (t, fn _ => NONE, copyTypeCons)
 
    in
        univFold
            (sourceTab,
            fn (dName: string, dVal: universal, _) =>
            if tagIs structVar dVal
            then 
            let
                val Struct { signat, name=structName, access, locations=structLocs, ...} = tagProject structVar dVal
                val Signatures { name=sigName, tab, typeIdMap, firstBoundIndex, locations=sigLocs, ...} = signat

                val newSig =
                    if singleLevel
                    then (* Just compose the maps. *)
                        makeSignature(sigName, tab, firstBoundIndex, sigLocs, composeMaps(typeIdMap, mapTypeId), [])
                    else (* Recursive copy. *)
                    let
                        (* Make a new sub-structure. *)
                        val newTab = makeSignatureTable ();
                        (* Copy everything into the new signature. *)
                        val () =
                            localCopySig 
                                (tab,
                                {
                                    enterType   = fn (s,v) => univEnter (newTab, typeConstrVar, s, v),
                                    enterStruct = fn (s,v) => univEnter (newTab, structVar,     s, v),
                                    enterVal    = fn (s,v) => univEnter (newTab, valueVar,      s, v)
                                },
                                composeMaps(typeIdMap, mapTypeId), false, strName ^ dName ^ ".", newMap, typeCache)
                    in
                        (* If we're copying it all set the resulting map to the new map. *)
                        makeSignature(sigName, newTab, firstBoundIndex, sigLocs, newMap, [])
                    end
                val newStruct =
                    Struct { name = structName, signat = newSig, access = access, locations = structLocs}
            in
                #enterStruct resEnv (dName, newStruct)
            end (* structures *)
                 
            else if tagIs typeConstrVar dVal
            then
            let
                val TypeConstrSet(oldConstr, tcConstructors) = tagProject typeConstrVar dVal
                val newConstr = copyTypeCons oldConstr;
                (* Copy the value constructors for a datatype. *)
       
                fun copyValueConstr(
                        v as Value{name, typeOf=ValueType(typeOf, templates), class, access, locations, references, ...}) =
                let
                    (* Copy its type and make a new constructor if the type has changed. *)
                    val newType = copyTyp typeOf
                in
                    if not (identical (newType, typeOf))
                    then Value{name=name, typeOf=ValueType(newType, templates), class=class,
                               access=access, locations = locations, references = references }
                    else v
                end;

                val copiedConstrs = map copyValueConstr tcConstructors
            in
                #enterType resEnv (dName, TypeConstrSet(newConstr, copiedConstrs))
            end

            (* Finally the values and exceptions. *)
            else if tagIs valueVar dVal
            then
            let
                val v as Value {typeOf=ValueType(typeOf, templates), class, name, access, locations, references, ...} =
                    tagProject valueVar dVal;
                val newType = copyTyp typeOf
                (* Can save creating a new object if the address and type
                   are the same as they were. *)
                val res =
                    if not (identical (newType, typeOf))
                    then Value {typeOf=ValueType(newType, templates), class=class, name=name,
                                    access=access,locations=locations, references = references}
                    else v
            in
                #enterVal resEnv (name, res)
            end 
            else (),
            ()
            )
    end (* fullCopySig *)

    (* Exported versions of these. *)

    (* Open a structure or include a signature. *)
    fun openSignature(Signatures{ tab, typeIdMap, ...}, resEnv, strName) =
        localCopySig(tab, resEnv, typeIdMap, true (* One level. *), strName, typeIdMap, [])

    and fullCopyDatatype(oldConstr:typeConstrSet, mapTypeId, strName) =
    let
        val sigSpace = makeSignatureTable()
        val Env { enterType, ...} = makeEnv sigSpace
        val () = enterType(tcName(tsConstr oldConstr), oldConstr)
        val resType = ref NONE
        val resEnv =
            {
                enterType = fn (_, tc) => resType := SOME tc,
                enterStruct = fn (s, _) => raise Misc.InternalError ("enterStruct "^s),
                enterVal = fn (s, _) => raise Misc.InternalError ("enterVal "^s)
            }
        val () = localCopySig(sigSpace, resEnv, mapTypeId, true, strName, fn _ => raise Subscript, [])
    in
        valOf(! resType)
    end

    fun replaceMap(Signatures{tab=sourceTab, name = sourceName, locations, ...},
                   mapTypeId: int -> typeId, min, boundIds, newMap): signatures =
    let
        (* Make a new signature. *)
        val tab = makeSignatureTable ();

        val tsvEnv =
        {
            enterType   = fn (s,v) => univEnter (tab, typeConstrVar, s, v),
            enterStruct = fn (s,v) => univEnter (tab, structVar, s, v),
            enterVal = fn (s, v) => univEnter (tab, valueVar, s, v)
        }
        (* Copy everything into the new signature. *)
        val () = localCopySig(sourceTab, tsvEnv, mapTypeId, false, "", newMap, [])
    in
        makeSignature(sourceName, tab, min, locations, newMap, boundIds)
    end (* replaceMap *)

    (* Find the maximum run-time offset used for a value or structure in a signature.
       This excludes type IDs. *)
    fun getNextRuntimeOffset(Signatures{tab, ...}): int =
    let
        fun getOffset(_, dVal, m) =
        if tagIs valueVar dVal
        then case tagProject valueVar dVal of
            Value { access = Formal addr, ...} => Int.max(addr+1, m)
        |   _ => m
        else if tagIs structVar dVal
        then case tagProject structVar dVal of
            Struct{access=Formal addr, ...} => Int.max(addr+1, m)
        |   _ => m
        else if tagIs typeConstrVar dVal
        then
        let
            fun getConstrOffset(Value { access = Formal addr, ...}, m) = Int.max(addr+1, m)
            |   getConstrOffset(_, m) = m
        in
            List.foldl getConstrOffset m (tsConstructors (tagProject typeConstrVar dVal))
        end
        else m
    in
        univFold(tab, getOffset, 0)
    end

    structure Sharing =
    struct
        type signatures     = signatures
        type typeConstrSet  = typeConstrSet
        type structVals     = structVals
        type values         = values
        type typeId         = typeId
        type valAccess      = valAccess
        type types          = types
        type univTable      = univTable
    end

end;
