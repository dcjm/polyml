(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Further development:
    Copyright (c) 2000-15 David C.J. Matthews

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
    Title:      Parse Tree Structure and Operations.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

functor PARSE_TREE (

structure BASEPARSETREE : BaseParseTreeSig
structure PRINTTREE: PrintParsetreeSig
structure EXPORTTREE: ExportParsetreeSig
structure TYPECHECKTREE: TypeCheckParsetreeSig
structure CODEGENPARSETREE: CodegenParsetreeSig

structure LEX : LEXSIG
structure STRUCTVALS : STRUCTVALSIG;
structure TYPETREE : TYPETREESIG

sharing LEX.Sharing = TYPETREE.Sharing = STRUCTVALS.Sharing
       = BASEPARSETREE.Sharing = PRINTTREE.Sharing = EXPORTTREE.Sharing = CODEGENPARSETREE.Sharing
       = TYPECHECKTREE.Sharing

) : PARSETREESIG =
   
struct 

    open LEX
    open STRUCTVALS
    open TYPETREE
    open BASEPARSETREE
    open PRINTTREE
    open EXPORTTREE
    open CODEGENPARSETREE
    open TYPECHECKTREE
   
    val badType              = BadType
  
    fun isIdent               (Ident _)               = true | isIdent _               = false;
  
    val unit      = Unit;
    val wildCard  = WildCard;
    val emptyTree = EmptyTree;

    (* A general type variable for an expression.  This is used to record the type. *)
    fun makeGeneralTypeVar() = mkTypeVar(generalisable, false, false, false)
  
    fun mkIdent (name, loc) : parsetree = 
      Ident
        {
          name   = name,
          expType = ref EmptyType,
          value  = ref undefinedValue,
          location = loc,
          possible = ref(fn () => [])
        };
    
    local    
       (* Make overloaded functions for the conversions. *)
       (* For the moment we make the type string->t and raise an exception
          if the constant cannot be converted. *)
       val ty      = mkOverloadSet[]
       val funType = mkFunctionType (stringType, ty);
       fun mkOverloaded name : values = makeOverloaded (name, funType, TypeDep)
    in
        val convString = mkOverloaded "convString"
        and convInt = mkOverloaded "convInt"
        and convWord = mkOverloaded "convWord"
        and convChar = mkOverloaded "convChar"
        and convReal = mkOverloaded "convReal"
    end;

    fun mkString(s: string, loc): parsetree =
        Literal{converter=convString, literal=s, expType=ref EmptyType, location=loc};
    
    fun mkInt  (i : string, loc) : parsetree =
        Literal{converter=convInt, literal=i, expType=ref EmptyType, location=loc};
    
    fun mkReal (r : string, loc) : parsetree =
        Literal{converter=convReal, literal=r, expType=ref EmptyType, location=loc};
    
    fun mkChar (c : string, loc) : parsetree = 
        Literal{converter=convChar, literal=c, expType=ref EmptyType, location=loc};

    fun mkWord (w : string, loc) : parsetree =
        Literal{converter=convWord, literal=w, expType=ref EmptyType, location=loc};
    
    fun mkApplic (f, arg, loc, isInfix) : parsetree  =
      Applic
        {
          f   = f,
          arg = arg,
          location = loc,
          isInfix = isInfix,
          expType = ref EmptyType
        };
    
    fun mkCond (test, thenpt, elsept, location) : parsetree  = 
        Cond  
        {
            test   = test,
            thenpt = thenpt,
            elsept = elsept,
            location = location,
            thenBreak = ref NONE,
            elseBreak = ref NONE
        }
       
    fun mkTupleTree(fields, location) = TupleTree { fields=fields, location=location, expType = ref EmptyType }
    
    fun mkValDeclaration (dec, explicit, implicit, location) : parsetree = 
        ValDeclaration 
        {
            dec   = dec,
            explicit = explicit,
            implicit = implicit,
            location = location
        };
    
    fun mkFunDeclaration (dec, explicit, implicit, location) : parsetree =
      FunDeclaration
        {
            dec=dec,
            explicit = explicit,
            implicit = implicit,
            location = location
        };
    
    fun mkOpenTree(ptl : structureIdentForm list, location): parsetree =
        OpenDec{decs=ptl, variables=ref [], structures = ref [], typeconstrs = ref [], location = location};
    
    fun mkStructureIdent (name, location) : structureIdentForm =
        { 
          name  = name,
          value = ref NONE,
          location = location
        }; 
 
    fun mkValBinding (dec, exp, isRecursive, line) : valbind = 
        ValBind
        {
            dec  = dec,
            exp  = exp,
            isRecursive = isRecursive,
            line = line,
            variables = ref nil
        };

    fun mkClausal(clauses, location) : fvalbind =
       FValBind
         { 
           clauses    = clauses,
           numOfPatts = ref 0,
           functVar   = ref undefinedValue,
           argType    = ref badType,
           resultType = ref badType,
           location   = location
         }; 

    (* A clause for a clausal function is initially parsed as a pattern because that is
       the easiest way to handle it but that's actually more general than the syntax allows.
       Process it at this point to check for some validity. *)
    fun mkFunPattern (fPat, lex): funpattern * string * int =
    let
        fun makeId(name, loc) =
            {name = name, expType = ref EmptyType, location = loc }

        fun unpick (Applic{ f, arg, isInfix, ... }) =
                (* "Application" of function to a parameter. *)
            let
                val () =
                (* This could be an infixed application and since it has been parsed using the
                   normal infix handler the arguments could be prefixed constructor applications
                   or infixed constructor applications with a higher precedence.  These are not
                   allowed because the arguments are supposed to just be "atpats".  Any
                   applications should have been parenthesised. *)
                    case (isInfix, arg) of
                        (true, TupleTree{fields=[Applic _, _], location, ...}) =>
                            errorMessage(lex, location,
                                "Constructor applications in fun bindings must be parenthesised.")
                    |   (true, TupleTree{fields=[_, Applic _], location, ...}) =>
                            errorMessage(lex, location,
                                "Constructor applications in fun bindings must be parenthesised.")
                    |   _ => ();
                val { ident, isInfix, args, ... } = unpick f
            in
                { ident=ident, isInfix=isInfix, args = args @ [arg], constraint = NONE }
            end

        |   unpick (Ident{ name, location, ...}) =
            {
                ident={ name = name, location = location, expType = ref EmptyType},
                isInfix=false, args = [], constraint = NONE
            }

        |   unpick (Parenthesised(Applic{ f = Ident { name, location, ...}, isInfix=true, arg, ... }, _)) =
            {
                ident={ name = name, location = location, expType = ref EmptyType},
                isInfix=true, args = [arg], constraint = NONE
            }

        |   unpick (Parenthesised(_, location)) =
                (* Only the bottom (i.e. first) application may be parenthesised and then
                   only if the application is infixed. *)
                (
                    errorMessage(lex, location,
                        "Parentheses are only allowed for infixed applications in fun bindings.");
                    { ident=makeId("", location), isInfix=false, args = [], constraint = NONE }
                )

        |   unpick _ =
                (
                    errorMessage(lex, location lex,
                        "Syntax error: fun binding is not an identifier applied to one or more patterns.");
                    { ident=makeId("", location lex), isInfix=false, args = [], constraint = NONE }
                )

        val unpicked as { ident = { name, ...}, args, ...} =
            (* The "pattern" may have a single constraint giving the result
               type of the function.  Otherwise it must be a set of one or more,
               possibly infixed, applications. *)
            case fPat of
                Constraint { value = value as Applic _, given, ... } =>
                let
                    val { ident, isInfix, args, ... } = unpick value
                in
                    { ident = ident, isInfix = isInfix, args = args, constraint = SOME given }
                end

            |   Constraint { value = value as Parenthesised(Applic _, _), given, ... } =>
                let
                    val { ident, isInfix, args, ... } = unpick value
                in
                    { ident = ident, isInfix = isInfix, args = args, constraint = SOME given }
                end

            |   fPat as Parenthesised(Applic _, _) =>
                    unpick fPat

            |   fPat as Applic _ =>
                    unpick fPat

            |   _ =>
                (
                    errorMessage(lex, location lex,
                        "Syntax error: fun binding is not an identifier applied to one or more patterns.");
                    { ident=makeId("", location lex), isInfix=false, args = [], constraint = NONE }
                )
    in
        (unpicked, name, List.length args)
    end;

    fun mkClause (dec, exp, line) : fvalclause =
        FValClause
        {
          dec  = dec,
          exp  = exp,
          line = line,
          breakPoint = ref NONE
        }

    fun mkList(elem, loc) = List{ elements = elem, location = loc, expType = ref EmptyType }
    
    fun mkConstraint (value, given, location) : parsetree = 
      Constraint 
        { 
          value = value,
          given = given,
          location = location
        };
      
    fun mkLayered (var, pattern, location) : parsetree = 
      Layered
        {
          var     = var,
          pattern = pattern,
          location = location
        };
    
    fun mkFn(matches, location) =
        Fn { matches = matches, location = location, expType = ref EmptyType }
    
    fun mkMatchTree (vars, exp, location) : matchtree = 
      MatchTree 
        {
          vars = vars,
          exp  = exp,
          location = location,
          argType = ref badType,
          resType = ref badType,
          breakPoint = ref NONE
        }
  
    fun mkLocalDeclaration (decs, body, location, isLocal) : parsetree =
      Localdec 
        {
          decs = map (fn p => (p, ref NONE)) decs,
          body = map (fn p => (p, ref NONE))body,
          isLocal  = isLocal,
          varsInBody = ref [],
          location = location
        };
      
    val mkTypeDeclaration : typebind list * location -> parsetree = TypeDeclaration;

    fun mkDatatypeDeclaration (typelist, withtypes, location) : parsetree =
        AbsDatatypeDeclaration
        {
            isAbsType = false,
            typelist  = typelist,
            withtypes = withtypes,
            declist   = [],
            location  = location,
            equalityStatus = ref []
        };
    
    fun mkAbstypeDeclaration (typelist, withtypes, declist, location) : parsetree =
        AbsDatatypeDeclaration
        {
            isAbsType = true,
            typelist  = typelist,
            withtypes = withtypes,
            declist   = map (fn p => (p, ref NONE)) declist,
            location  = location,
            equalityStatus = ref []
        };

    val mkDatatypeReplication = DatatypeReplication
    
    fun mkTypeBinding (name, typeVars, decType, isEqtype, nameLoc, fullLoc) : typebind =
      TypeBind 
        {
          name     = name,
          typeVars = typeVars,
          decType  = decType,
          isEqtype = isEqtype,
          tcon     = ref(TypeConstrSet(undefConstr, [])),
          nameLoc = nameLoc,
          fullLoc = fullLoc
        };
    
    fun mkDatatypeBinding (name, typeVars, constrs, typeNameLoc, fullLoc) : datatypebind =
      DatatypeBind
        {
          name         = name,
          typeVars     = typeVars,
          constrs      = constrs,
          tcon         = ref(TypeConstrSet(undefConstr, [])),
          nameLoc      = typeNameLoc,
          fullLoc = fullLoc
        }
    
    fun mkValueConstr (name, arg, locn) =
        {constrName=name, constrArg=arg, idLocn=locn, constrVal=ref undefinedValue}
   
    fun mkExBinding (name, previous, typeof, nameLoc, fullLoc) : exbind =
      ExBind 
        {
          name        = name,
          previous    = previous,
          ofType      = typeof,
          value       = ref undefinedValue,
          nameLoc     = nameLoc,
          fullLoc     = fullLoc
        };

    fun mkLabelledTree (recList, frozen, location) : parsetree = 
     Labelled
       {
         recList = recList,
         frozen  = frozen,
         expType  = ref EmptyType,
         location = location
       };
       
    fun mkLabelRecEntry (name, nameLoc, valOrPat, fullLocation) =
    {
        name = name,
        nameLoc = nameLoc,
        valOrPat = valOrPat,
        fullLocation = fullLocation,
        expType = ref EmptyType
    }

    fun mkSelector(name, location) : parsetree =
    let
        (* Make a type for this.  It's equivalent to
          fn { name = exp, ...} => exp. *)
      val resType   = makeGeneralTypeVar();
      val entryType = mkLabelEntry (name, resType);
      val labType   = mkLabelled ([entryType], false) (* Not frozen*);
    in
      Selector
        {
          name      = name,
          labType   = labType,
          typeof    = mkFunctionType (labType, resType),
          location  = location
        }
    end;
    
    val mkRaise : parsetree * location -> parsetree = Raise;
    
    fun mkHandleTree (exp, hrules, location, listLocation) : parsetree = 
       HandleTree
         { 
           exp    = exp,
           hrules = hrules,
           location = location,
           listLocation = listLocation
         };
       
    fun mkWhile (test, body, location) : parsetree =
        While
        { 
            test = test,
            body = body,
            location = location,
            breakPoint = ref NONE
        }
      
    fun mkCase (test, match, location, listLocation) : parsetree =
      Case
        {
            test  = test,
            match = match,
            location = location,
            listLocation = listLocation,
            expType = ref EmptyType
        };
      
    fun mkAndalso (first, second, location) : parsetree =
      Andalso
        {
          first  = first,
          second = second,
          location = location
        };
      
    fun mkOrelse (first, second, location) : parsetree =
      Orelse
        {
          first  = first,
          second = second,
          location = location
        };
      
    fun mkDirective (tlist, fix, location) : parsetree = 
      Directive
        {
          tlist = tlist,
          fix   = fix,
          location = location
        };
       
    fun mkExpseq (pl: parsetree list, l: location) = ExpSeq (map (fn p => (p, ref NONE)) pl, l)
    
    val mkExDeclaration  : exbind list * location -> parsetree = ExDeclaration;  
    
    val mkParenthesised = Parenthesised

    (* Types that can be shared. *)
    structure Sharing =
    struct
        type lexan      = lexan
        and  pretty     = pretty
        and  environEntry = environEntry
        and  codetree   = codetree
        and  codeBinding = codeBinding
        and  types      = types
        and  values     = values
        and  typeId     = typeId
        and  structVals = structVals
        and  typeConstrs= typeConstrs
        and  typeVarForm=typeVarForm
        and  env        = env
        and  infixity   = infixity
        and  structureIdentForm = structureIdentForm
        and  typeParsetree = typeParsetree
        and  parsetree  = parsetree
        and  valbind    = valbind
        and  fvalbind   = fvalbind
        and  fvalclause = fvalclause
        and  typebind   = typebind
        and  datatypebind=datatypebind
        and  exbind     = exbind
        and  labelRecEntry=labelRecEntry
        and  ptProperties = ptProperties
        and  matchtree   = matchtree
        and  typeVarMap = typeVarMap
        and  level = level
        and  debuggerStatus   = debuggerStatus
    end

end (* PARSETREE *);
