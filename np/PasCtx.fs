namespace rec Pas

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open dnlib.DotNet
open dnlib.DotNet.Emit
open Pas.Intrinsics

type ScopeRec = {
    Namespace: string
    State: PasState
    Unit: TypeDef // all procedures and global variables are defined in special class
    Module: ModuleDef // main module of application / library
} with
    static member Create ns state defs m = {
            Namespace = ns
            State = state
            Unit = defs
            Module = m
        }

module Utils =
    let stdIdent = Ident >> List.singleton >> DIdent

    let tryAddMetaType (symbols: Dictionary<_,_>) (name: CompilerName) typ =
        match name with
        | AnonName -> Some typ // ie for char sets
        | _ ->
            match symbols.TryAdd(name, TypeSym typ) with
            | true -> Some typ
            | false -> None
            
    let addMetaType (symbols: Dictionary<_,_>) (name: CompilerName) typ =
        tryAddMetaType symbols name typ |> Option.defaultWith (doInternalError "2020082200")

    let typeCheck ctx at bt =
        // at is used for function params
        // bt can be error too
        at = ctx.sysTypes.constParam
        || at = ctx.sysTypes.varParam
        || at = ctx.sysTypes.unknown || bt = ctx.sysTypes.unknown
        || at.kind = bt.kind
        || match at, bt with
           | FloatType, IntType -> true
           | IntType, IntType -> true
           | PointerType, PointerType -> true // Todo better pointer types check
           | ProcVarType, ProcVarType -> true // Todo better function pointers types check
           | ProcVarType, PointerType -> true // Todo better function pointers types check
           | _ -> false

    let inline toMap kvps =
        kvps
        |> Seq.map (|KeyValue|)
        |> Map.ofSeq

type SymOwner =
    | GlobalSpace
    | StandaloneMethod of ReferencedDef
    | WithSpace

type Ctx = {
        units: Ctx list
        messages: CompilerMessages
        variables: List<VariableKind> list
        labels: List<DIdent * LabelRec> list
        block: GlobalBlock
        symbols: (SymOwner * Dictionary<CompilerName, Symbol>) list
        forward: Dictionary<CompilerName, ReferencedDef * Dictionary<CompilerName,Symbol>>
        localVariables: int ref
        lang: Ctx.LangCtx
        res: List<MetaInstruction>
        details: Ctx.ModuleDetails
        loop: Stack<BranchLabel ref * BranchLabel ref>
        enumSet: Dictionary<PasType, PasType>
        sysTypes: Ctx.SystemTypes
        sysProc: Ctx.SystemProc
    } with

    member inline self.NewMsg (pos: ^T) mk =
        let p = (^T : (member BoxPos : obj) pos)
        match self.messages.PosMap.TryGetValue p with
        | true, p -> self.messages.AddMsg p mk
        | _ -> raise (InternalError "2020082001") // for example possible if system symbol not found (BoxPos not exist)

    static member Create = Ctx.createCtx

    member self.NewSymbols = snd self.symbols.Head
    member self.SymOwner = fst self.symbols.Head
    member self.SymLabelOwner =
        List.tryFind (fun (o, _) -> match o with | GlobalSpace _ | StandaloneMethod _ -> true | _ -> false) self.symbols
        |> Option.defaultWith (doInternalError "2020082202") |> fst

    member self.Next block =
        { self with
            block = block
            localVariables = ref 0
            variables = List<_>()::self.variables
            res = List<MetaInstruction>()
            loop = Stack<_>()}

    member self.Inner symbolsEntry =
        { self.Next NormalBlock with symbols = symbolsEntry::self.symbols }

    member self.PickSym sym =
        self.symbols |> List.tryPick (fun (o, st) -> match st.TryGetValue sym with | true, v -> Some (o, v) | _ -> None)

    member self.FindSym sym =
        match self.PickSym sym with
        | Some(StandaloneMethod(md,_), NestedRoutineSym (sym, t)) -> // add nested routine to nested methods if needed
            match self.symbols with
            | (StandaloneMethod(originMd,nestedParams),_)::_ ->
                if originMd.raw <> md.raw then
                    let symId = (|NestedRoutineId|) (sym, sym) // second param has no meaning
                    let np = !nestedParams
                    match List.tryFind (fun (NestedRoutineId id) -> symId.Equals(id)) np with
                    | None ->
                        let symbols = snd self.symbols.Head
                        let paramName = sprintf "@nested$%d" symbols.Count
                        let md = originMd.raw.ResolveMethodDef()
                        let paramIdx = md.ParamDefs.Count
                        let pd = ParamDefUser(UTF8String paramName, paramIdx + 1 |> uint16)
                        md.ParamDefs.Add pd
                        t.Sig |> PtrSig |> md.MethodSig.Params.Add
                        let newSym = VariableSym(ParamVariable(RefVar, md.Parameters.[paramIdx]), t)
                        symbols.Add(CompilerName.FromString paramName, newSym)
                        nestedParams := (sym, newSym)::!nestedParams
                        Some newSym
                    | Some(_, newSym) -> Some newSym
                else
                    Some sym
            | _ -> Some sym
        | Some(_, sym) -> Some sym
        | _ -> None

    member self.InUnits f = List.tryPick f (self::self.units)
    
    member self.FindType sym =
        match sym with
        | ErrorName -> Some self.sysTypes.unknown
        | _ -> self.InUnits (fun c -> match c.PickSym sym with | Some(_,TypeSym ts) -> Some ts | _ -> None)

    member self.TryFindTypeId = CompilerName.FromTypeId >> self.FindType
        
    member self.FindTypeId t =
        let reportError = fun() -> ``Error: Cannot find type identifier '%O'`` t |> self.NewMsg t; self.sysTypes.unknown
        self.TryFindTypeId t |> Option.defaultWith reportError

    member self.EnsureVariable ?kind =
        let ikey = !self.localVariables
        let key = string ikey
        incr self.localVariables
        // TODO manager of variables for reuse
//        let result = match self.variables.TryGetValue key with
//                     | true, value ->
//                         value
//                     | _ ->
        let varType = defaultArg kind self.sysTypes.int32
        let varDef = varType.Sig |> Local
        let varKind = varDef |> LocalVariable
        self.res.Add(DeclareLocal(varDef))
        self.variables.Head.Add(varKind)
        (snd self.symbols.Head).Add(CompilerName.FromString key, VariableSym(varKind, varType))
        (key, varDef)

    member self.FindLabel name =
        match self.PickSym (CompilerName.FromDIdent name) with
        | Some(owner, LabelSym l) -> Some(owner, l)
        | _ -> None

    // Types module
    member self.AddTypeSetForEnum = TypesDef.addTypeSetForEnum self
    member self.AddTypeSet = TypesDef.addTypeSet self
    member self.AddType = TypesDef.addType self
    member self.AddTypePointer = TypesDef.addTypePointer self
    member self.GetInternalType = TypesDef.getInternalType self
    member self.AddTypeArray = TypesDef.addTypeArray self
    member self.AddProcType = TypesDef.addProcType self

    // SymSearch module
    member self.FindSymbol did =
        let res = self.InUnits (SymSearch.findSymbol did)
        if res.IsNone then ``Error: Cannot find symbol '%O'`` did |> self.NewMsg did
        res

    member self.FindMethodReferenceOpt =
        Utils.stdIdent >> self.FindSymbol >> chainToSLList >>
        function | [CallableLoad(Referenced({raw=mr}, _))], _ -> Some mr | _ -> None
    member self.FindMethodReferenceUnsafe = self.FindMethodReferenceOpt >> Option.defaultValue null
    member self.FindMethodReference = self.FindMethodReferenceOpt >> Option.defaultWith (doInternalError "2020082000")
    member self.FindConstSym =
        self.FindSymbol >> chainToSLList >>
        function | [ValueLoad(v)], Some(t) -> v, t | _ -> failwith "IE"
    member self.FindFunction = SymSearch.findFunction self

    // EvalConstExpr module
    member self.EvalConstExpr = EvalConstExpr.evalConstExpr self

    // EvalExpr module
    member self.ExprToIl = EvalExpr.exprToIl self
    member self.ChainLoadToIl = EvalExpr.chainLoadToIl self
    member self.ChainWriterFactory = EvalExpr.chainWriterFactory self
    member self.ChainReaderFactory = EvalExpr.chainReaderFactory self
    member self.DoCall = EvalExpr.doCall self

module Ctx =

    type LangCtx() =
        let mutable ec = caseInsensitive
        member self.caseSensitive
            with get() = ec = caseSensitive
            and set(cs) =  ec <- if cs then caseSensitive else caseInsensitive

        interface IEqualityComparer<CompilerName> with
            member _.GetHashCode(x) = ec.GetHashCode(x)
            member _.Equals(x,y) = ec.Equals(x, y)

    type ModuleDetails = {
            TypesCount: int ref
            MainModule: ModuleDef
            Namespace: string
            UnitModule: TypeDef
            ValueType: TypeRef
            UnsafeValueTypeAttr: CustomAttribute
            anonSizeTypes: Dictionary<int, PasRawType>
        } with
        static member Create (sr: ScopeRec) =
            let m = sr.Module
            {
                TypesCount = ref 0
                MainModule = m
                Namespace = sr.Namespace
                UnitModule = sr.Unit
                ValueType = m.CorLibTypes.GetTypeRef("System", "ValueType") |> m.Import
                UnsafeValueTypeAttr =
                    let td = m.CorLibTypes.GetTypeRef("System.Runtime.CompilerServices", "UnsafeValueTypeAttribute").ResolveTypeDef()
                    td.FindDefaultConstructor() |> m.Import |> CustomAttribute
                anonSizeTypes = Dictionary<_, _>()
            }

        member self.UniqueTypeName() =
            incr self.TypesCount
            "T" + string !self.TypesCount

        member self.NewSizedType (size: int) =
                let attributes = TypeAttributes.SequentialLayout ||| TypeAttributes.AnsiClass ||| TypeAttributes.Sealed ||| TypeAttributes.Public
                let at = TypeDefUser(UTF8String self.Namespace, self.UniqueTypeName() |> UTF8String, self.ValueType)
                at.Attributes <- attributes
                at.CustomAttributes.Add(self.UnsafeValueTypeAttr)
                at.ClassSize <- uint32 size
                at.PackingSize <- 1us
                self.MainModule.Types.Add(at)
                PasRawType at

        member self.SelectAnonSizeType size =
            match self.anonSizeTypes.TryGetValue size with
            | true, t -> t
            | _ ->
                let at = self.NewSizedType size
                self.anonSizeTypes.Add(size, at)
                at

        member self.AddBytesConst (bytes: byte[]) =
            let ast = self.SelectAnonSizeType bytes.Length
            let fd = FieldDefUser(null, FieldSig ast.Sig, FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.HasFieldRVA)
            fd.InitialValue <- bytes
            self.UnitModule.Fields.Add fd
            fd :> FieldDef

    type SystemTypes = {
        int32: PasType
        int64: PasType
        single: PasType
        string: PasType
        setStorage: PasType
        char: PasType
        file: PasType
        value: PasType
        pointer: PasType
        unit: PasType
        unknown: PasType
        constParam: PasType
        varParam: PasType
        boolean: PasType
        net_obj: PasType
        net_void: PasType
    }

    type SystemProc = {
        GetMem: IMethod
        FreeMem: IMethod
        WriteLine: IMethod
        Exit: IMethod
        ConvertU1ToChar: IMethod
        PtrToStringAnsi: IMethod
        Round: IMethod
    }

    let private createSystemTypes details (symbols: Dictionary<CompilerName, Symbol>) =
        let mb = details.MainModule
        let valueType = PasRawType(mb.ImportAsTypeSig typeof<ValueType>)
        let ot = PasRawType(mb.ImportAsTypeSig typeof<obj>)
        let addAnyType (name: CompilerName) (raw: TypeSig) kind =
            let t = PasType.Create(name, raw, kind)
            Utils.addMetaType symbols name t
        let addOrdType name raw ordKind ordType = TkOrd(ordKind, ordType) |> addAnyType (CompilerName.FromString name) raw
        let addFloatType name raw = TkFloat(FtSingle) |> addAnyType (CompilerName.FromString name) raw
        let addArrayType name (p: PasType) ad = addAnyType name p.Sig ad

        addOrdType "Byte" mb.CorLibTypes.Byte OkInteger (OtUByte(int Byte.MinValue, int Byte.MaxValue)) |> ignore
        addOrdType "ShortInt" mb.CorLibTypes.SByte OkInteger (OtSByte(int SByte.MinValue, int SByte.MaxValue)) |> ignore
        addOrdType "Word" mb.CorLibTypes.UInt16 OkInteger (OtUWord(int UInt16.MinValue, int UInt16.MaxValue)) |> ignore
        addOrdType "SmallInt" mb.CorLibTypes.Int16 OkInteger (OtSWord(int Int16.MinValue, int Int16.MaxValue)) |> ignore
        addOrdType "LongWord" mb.CorLibTypes.UInt32 OkInteger (OtULong(int UInt32.MinValue, int UInt32.MaxValue)) |> ignore
        addOrdType "UInt64" mb.CorLibTypes.UInt64 OkInteger (OtUQWord(UInt64.MinValue, UInt64.MaxValue)) |> ignore
        let charType = addOrdType "Char" mb.CorLibTypes.Byte OkChar (OtUByte(int Byte.MinValue, int Byte.MaxValue))
        let strType = {name=AnonName;raw=(details.NewSizedType 256);kind=TkUnknown 256}
        let strDim = {
                        low = 1
                        high = 255
                        size = 256
                        elemSize = 1
                        elemType = charType
                        selfType = ref strType
                     }
        // allow to use string as array

        // file = name + handle
        let fileType = details.NewSizedType (256 + ptrSize)
        let tidf = CompilerName <| TIdFile()
        let fileType = Utils.addMetaType symbols tidf {name=tidf;kind=TkUnknown(256 + ptrSize);raw=fileType}
        let voidSig = mb.CorLibTypes.Void
        let voidType = PasRawType voidSig
        let newVoidPtr() = PtrSig voidSig
        {
            int32 = addOrdType "Integer" mb.CorLibTypes.Int32 OkInteger (OtSLong(int Int32.MinValue, int Int32.MaxValue))
            int64 = addOrdType "Int64" mb.CorLibTypes.Int64 OkInteger (OtSQWord(Int64.MinValue, Int64.MaxValue))
            single = addFloatType "Real" mb.CorLibTypes.Single
            string = addArrayType (CompilerName <| TIdString()) strType (TkArray(AkSString 255uy, [strDim], charType))
            setStorage = {name=AnonName;raw=details.NewSizedType 256;kind=TkUnknown 0}
            char = charType
            file = fileType
            value = {name=AnonName;raw=valueType;kind=TkUnknown 0}
            pointer = addAnyType (CompilerName.FromString "Pointer") (newVoidPtr()) (TkPointer({name=AnonName;raw=voidType;kind=TkUnknown 0}))
            unit = {name=AnonName;raw=voidType;kind=TkUnknown 0}
            unknown = {name=ErrorName;raw=voidType;kind=TkUnknown 0}
            constParam = {name=AnonName;raw=newVoidPtr() |> PasRawType;kind=TkUnknown 0}
            varParam = {name=AnonName;raw=newVoidPtr() |> PasRawType;kind=TkUnknown 0}
            boolean = addOrdType "Boolean" mb.CorLibTypes.Byte OkBool (OtUByte(0, 1))
            net_obj = {name=AnonName;raw=ot;kind=TkUnknown 0}
            net_void = {name=AnonName;raw=voidType;kind=TkUnknown 0}
        }

    let private createSystemProc details =
        let mb = details.MainModule
//        let mathTrunc = typeof<System.MathF>.GetMethod("Truncate", [| typeof<single> |])  |> mb.ImportReference
        {
            GetMem = typeof<System.Runtime.InteropServices.Marshal>.GetMethod("AllocCoTaskMem") |> mb.Import
            FreeMem = typeof<System.Runtime.InteropServices.Marshal>.GetMethod("FreeCoTaskMem") |> mb.Import
            WriteLine = typeof<System.Console>.GetMethod("WriteLine", [| typeof<string> ; typeof<obj array> |]) |> mb.Import
            Exit = typeof<System.Environment>.GetMethod("Exit", [| typeof<int> |]) |> mb.Import
            ConvertU1ToChar = typeof<System.Convert>.GetMethod("ToChar", [| typeof<byte> |]) |> mb.Import
            PtrToStringAnsi = typeof<System.Runtime.InteropServices.Marshal>.GetMethod("PtrToStringAnsi", [| typeof<nativeint> |]) |> mb.Import
            Round = typeof<System.MathF>.GetMethod("Round", [| typeof<single> |])  |> mb.Import
        }

    let private addSystemRoutines ctx =
        let symbols = snd ctx.symbols.Head
        let tsingle = ctx.sysTypes.single
        symbols.Add(CompilerName.FromString "true", ConstBool 0xFFuy |> ConstSym)
        symbols.Add(CompilerName.FromString "false", ConstBool 0uy |> ConstSym)
        //newSymbols.Add("GetMem", Referenced allocMem |> MethodSym)
        //newSymbols.Add("FreeMem", Referenced freeMem |> MethodSym)
        let intrinsic name i = symbols.Add(CompilerName.FromString name, Intrinsic i |> MethodSym)
        intrinsic "Inc" IncProc
        intrinsic "Dec" DecProc 
        intrinsic "Read" ReadProc 
        intrinsic "Write" WriteProc 
        intrinsic "ReadLn" ReadLnProc 
        intrinsic "WriteLn" WriteLnProc 
        intrinsic "WriteLine" WriteLineProc 
        intrinsic "New" NewProc 
        intrinsic "Dispose" DisposeProc 
        intrinsic "Break" BreakProc 
        intrinsic "Continue" ContinueProc 
        intrinsic "Exit" ExitProc 
        intrinsic "Halt" HaltProc 
        intrinsic "HaltAtLine" HaltAtLineProc
        intrinsic "SizeOf" SizeOfFunc
        intrinsic "Ord" OrdFunc 
        intrinsic "Chr" ChrFunc 
        intrinsic "Pred" PredProc 
        intrinsic "Succ" SuccProc 
        intrinsic "Round" RoundFunc 
        intrinsic "Trunc" TruncFunc
        // function Abs(x: T): T;
        // function Sqr(x: T): T;
        // function Sin(x: Real): Real;
        // function Cos(x: Real): Real;
        // function Arctan(x: Real): Real;
        let singleScalar raw =
           Referenced({
               paramList = [|{typ=tsingle;ref=RefNone}|]
               result = Some {typ=tsingle; var=None} // imported methods dont need local var -> internal purposes only
               raw = raw
           }, ref[]) |> MethodSym
        let mathLog = typeof<System.MathF>.GetMethod("Log", [| typeof<single> |])  |> ctx.details.MainModule.Import
        let mathExp = typeof<System.MathF>.GetMethod("Exp", [| typeof<single> |])  |> ctx.details.MainModule.Import
        symbols.Add(CompilerName.FromString "Exp", singleScalar mathExp)
        symbols.Add(CompilerName.FromString "Ln", singleScalar mathLog)
        ctx

    let createCtx owner sr units =
        let lang = LangCtx()
        let details = ModuleDetails.Create sr
        let symbols = Dictionary<CompilerName,Symbol>(lang)
        {
            units = units
            messages = sr.State.messages
            variables = [List<_>()]
            labels = [List<_>()]
            block = NormalBlock
            symbols = [owner,symbols]
            forward = Dictionary<_, _>(lang)
            localVariables = ref 0
            lang = LangCtx()
            res = List<MetaInstruction>()
            details = details
            loop = Stack<_>()
            enumSet = Dictionary<_, _>()
            sysTypes = Ctx.createSystemTypes details symbols
            sysProc = Ctx.createSystemProc details
        } |> addSystemRoutines

module TypesDef =

    let addTypeSetForEnum ctx pasType name =
        let setType = Utils.addMetaType (snd ctx.symbols.Head) name {name=name;kind=TkSet(pasType);raw=ctx.sysTypes.setStorage.raw}
        ctx.enumSet.TryAdd(pasType, setType) |> ignore
        setType

    let addTypeSet (ctx: Ctx) typeName name =
        let t =
            match ctx.FindType typeName with
            | Some(EnumType as t) ->
                // TODO check ord size (for int max / min values)
                t
            | Some(ChrType as t) -> t
            | Some(ErrorType as t) -> t // error handled
            | Some _ | None ->
                ``Illegal type for set construction`` typeName |> ctx.NewMsg typeName
                ctx.sysTypes.unknown

        addTypeSetForEnum ctx t name

    let addType ctx name typ =
        Utils.tryAddMetaType ctx.NewSymbols name typ
        |> Option.defaultWith (doInternalError "2020082201")

    let addTypePointer (ctx: Ctx) count typeName name =
        let t =
            match ctx.FindType typeName with
            | Some t -> t
            | _ -> failwith "IE unknown type"
        let mutable pt = PtrSig t.Sig
        for i = 2 to count do pt <- PtrSig pt
        if pt = null then raise <| InternalError "2020103101" 
        PasType.Create(name, pt, TkPointer(t)) |> addType ctx name

    let getInternalType (ctx: Ctx) t =
        match ctx.TryFindTypeId t with
        | Some t -> t
        | None -> // inline type?
            match t with
            | TIdPointer(count, typeId) -> addTypePointer ctx count (CompilerName.FromTypeId typeId) (CompilerName t)
            | TIdArray(ArrayDef(_, dimensions, tname)) -> addTypeArray ctx dimensions tname (CompilerName t)
            | TIdSet(_, typeId) -> addTypeSet ctx (CompilerName.FromTypeId typeId) (CompilerName t)
            | _ ->
                ``Error: Cannot find type identifier '%O'`` t |> ctx.NewMsg t
                ctx.sysTypes.unknown

    let addTypeArray ctx dimensions tname name =
        let newSubType (dims, size) (typ: PasType, typSize) name =
            let size = size * typSize
            let at = ctx.details.NewSizedType size
            FieldDefUser(UTF8String "item0", FieldSig typ.Sig, FieldAttributes.Public)
            |> at.Def.Fields.Add
            let name = CompilerName.FromString name
            {name=name;raw=at;kind=TkArray(AkArray,dims,typ)}.ResolveArraySelfType()

        let rec doArrayDef dimensions tname dims name =

            let newDims, typAndSize, newTyp =
                match tname with
                | TIdArray(ArrayDef(_, dimensions, tname)) -> doArrayDef dimensions tname dims (name + "$a$")
                | t ->
                    let typ = getInternalType ctx t
                    dims, (typ, typ.SizeOf), typ

            let newArrayDim ad ((dims, totalSize), elemType, i, newTyp) =
                match ad with
                | DimensionExpr(ConstExpr(l),ConstExpr(h)) ->
                    let pint32 = ctx.sysTypes.int32
                    let l = match ctx.EvalConstExpr (Some pint32) l with | CERInt(i,_) -> i | _ -> failwith "IE"
                    let h = match ctx.EvalConstExpr (Some pint32) h with | CERInt(i,_) -> i | _ -> failwith "IE"
                    if l > h then failwith "IE"
                    let size = (h - l) + 1
                    let newDim = {
                        low = l
                        high = h
                        size = size
                        elemSize = totalSize*(snd typAndSize)
                        elemType = newTyp
                        selfType = ref Unchecked.defaultof<_>
                    }
                    let totalSize = totalSize * size
                    let dimsAndSize = newDim::dims,totalSize
                    let newTyp = newSubType dimsAndSize typAndSize (name + "$" + string(i))
                    dimsAndSize, elemType, i+1, newTyp
                | _ -> failwith "IE"

            let (res, _, _, typ) = List.foldBack newArrayDim dimensions (newDims, null, 0, newTyp)
            res, typAndSize, typ
        // final array
        let strName = string ((string name).Length)
        let _,_,typ = doArrayDef dimensions tname ([], 1) strName
        //typ.raw.Name <- strName
        addType ctx name typ
        //(doArrayDef dimensions tname (ref []) name).Name <- name

    let createMethodInfo (ctx: Ctx) (name: CompilerName) (mRes: TypeIdentifier option) (mPara: ParamList) =
        let mr =
            match mRes with
            | Some r -> // create result variable
              let res = ctx.FindTypeId r
              { typ = res; var = res.Sig |> Local |> LocalVariable |> Some }
            | _ -> { typ = ctx.sysTypes.net_void; var = None }
                         
        let methodAttributes = MethodAttributes.Public ||| MethodAttributes.Static
        let methodImplAttributes = MethodImplAttributes.IL ||| MethodImplAttributes.Managed
        let msig = MethodSig.CreateStatic mr.typ.Sig
        let md = MethodDefUser(UTF8String(name.ToString()), msig, methodImplAttributes, methodAttributes)
        let mp =
          let mutable idx = 1
          defaultArg mPara []
          |> List.collect
              (fun (k, (ps, t)) ->
               let t = match t with Some t -> Some(ctx.FindTypeId t) | _ -> None
               [for (p: string) in ps do
                   let typSig, refKind, typ =
                       match k, t with
                       | Some Const, Some t -> t.Sig, RefConst, t
                       | Some Var, Some t -> PtrSig t.Sig :> TypeSig, RefVar, t
                       | Some Const, None -> ctx.sysTypes.constParam.Sig, RefUntypedConst, ctx.sysTypes.constParam
                       | Some Var, None -> ctx.sysTypes.varParam.Sig, RefUntypedVar, ctx.sysTypes.varParam
                       | None, Some t -> t.Sig, RefNone, t
                       | _ -> raise(InternalError "2020082101")
                   msig.Params.Add typSig
                   md.ParamDefs.Add(ParamDefUser(UTF8String p, uint16 idx))
                   idx <- idx + 1
                   yield {typ=typ;ref=refKind}]
              )
          |> Array.ofList
        md.Parameters.UpdateParameterTypes()
        {
            paramList = mp
            result = if mr.var.IsSome then Some mr else None
            raw = md
        }
    
    let addProcType ctx ((_, mRes, mPara): ProcHeader) name =
        let methodInfo = TypesDef.createMethodInfo ctx name mRes mPara
        PasType.Create(name, FnPtrSig methodInfo.raw.MethodSig, TkProcVar methodInfo) |> addType ctx name
        
module SymSearch =

    let findSymbol (DIdent ident) (ctx: Ctx) =
        let mainSym = ident.Head |> function | Ident n -> ctx.FindSym(CompilerName.FromString n)

        let rec findSym ref acc = function
        | (Designator.Array _) as a::t -> acc, a::t // TODO ?
        | (Designator.Deref _) as d::t -> acc, d::t // TODO ?
        | Ident(h)::t ->
            // TODO ? here is solved auto records dereference (x.y instead of x^.y) and dereference for array elements
            let ref = match ref.kind with | TkPointer r -> r | _ -> ref
            let symbols = match ref.kind with | TkRecord(d,_) -> d | _ -> failwith "IE"
            let sym = symbols.[h]
            findSym (snd sym) (sym::acc) t
        | t -> acc, t

        let rec resolveTail acc vt  = function
            | [] -> (List.rev acc, vt)
            | h::t as ht->
                match h with
                | Deref _ ->
                    // TODO check dereferencable
                    match vt.kind with
                    | TkPointer tref -> resolveTail (DerefLoad::acc) tref t
                    | _ ->
                        ``Cannot do deref of %s`` (vt.kind.ToCompilerStr()) |> ctx.NewMsg h
                        [], ctx.sysTypes.unknown
                | Ident _ ->
                    let sl, restOfTail = findSym vt [] ht
                    let typ = snd sl.Head
                    resolveTail (StructLoad(List.rev sl)::acc) typ restOfTail
                | Designator.Array exprs ->
                    match vt.kind with
                    | TkArray (_, dims, elemTyp) -> 
                        // TODO rework this slow comparison
                        if exprs.Length > dims.Length then failwith "IE"
                        // TODO assign sub array parts?
                        let elems, tail = List.splitAt exprs.Length dims
                        let elemLoad = elems
                                       |> List.zip exprs
                                       |> fun dims -> ElemLoad (dims, elemTyp)
                        // TODO validation of dims ?
                        let subElemTyp = match List.tryHead tail with | Some h -> !h.selfType | _ -> elemTyp
                        let newAcc = elemLoad::acc
                        resolveTail newAcc subElemTyp t
                    | _ ->
                        ``Error: Expected %s type but '%O' found`` "array" (vt.kind.ToCompilerStr()) |> ctx.NewMsg h
                        [], ctx.sysTypes.unknown

        let varLoadChain vt dlist vd =
            let (tail, finalType) = resolveTail [] vt dlist
            Some(VariableLoad(vd)::tail, Some finalType)

        let mainSym = defaultArg mainSym UnknownSym

        match mainSym with
        | VariableSym (v, t as vt) -> vt |> varLoadChain t ident.Tail
        | WithSym (v, t as vt) -> vt |> varLoadChain t ident
        | EnumValueSym(i, t) when ident.Tail = [] -> Some([ValueLoad(ValueInt(i))], Some t)
        | MethodSym m ->  Some([CallableLoad m], m.ReturnType)
        | ConstSym v ->
            match v with
            | ConstInt i -> Some([ValueLoad(ValueInt i)], Some ctx.sysTypes.int32)
            | ConstFloat f -> Some([ValueLoad(ValueFloat f)], Some ctx.sysTypes.single)
            | ConstValue(fd, pt) -> (GlobalVariable fd, pt) |> varLoadChain pt ident.Tail
            | ConstBool b ->
                let i = int b
                Some([ValueLoad(ValueInt i)], Some ctx.sysTypes.boolean)
            | _ -> raise <| InternalError "2020112101"
        | TypeSym t -> Some([TypeCastLoad t], Some t)
        | _ -> None

    type FoundFunction =
        | RealFunction of (MethodSym * IlInstruction list)
        | UnknownFunction
        | TypeCast of PasType

    let findFunction (ctx: Ctx) ident =
        let callChain = ctx.FindSymbol ident
        // TODO more advanced calls like foo().x().z^ := 10
        match callChain with
        | Some([CallableLoad cl], _) ->
           match cl with
           | Referenced ({raw=mr}, _) -> RealFunction(cl, [+Call(mr)])
           | Intrinsic _ -> RealFunction(cl, [])
        | Some([TypeCastLoad t], _) -> TypeCast t
        | Some(([VariableLoad v], Some{kind=TkProcVar mi}) as cc) ->
            let cl = Referenced(mi,ref[])
            RealFunction(cl, [yield! fst <| EvalExpr.loadSymList ctx true false cc; +Calli(mi.raw)])
        | _ ->
            // TODO replace first error "Cannot find symbol" with
            // ctx.NewError ident (sprintf "Unknown function '%O'" ident)
            UnknownFunction

module EvalExpr =

    open Intrinsics
    open EvalConstExpr

    type ValueKind = (IlInstruction list * PasType)

    let useHelperOp ctx helperName valueType byRef =
        let _,v = (ctx: Ctx).EnsureVariable(valueType)
        [
            +Ldloca v
            +Call(ctx.FindMethodReference helperName)
            +(if byRef then Ldloca else Ldloc) v
        ]

    let handleOperator (ctx: Ctx) et byRef (op, (ils, at, bt)) =
        let convCmpOp = function | Cgt -> Clt | Clt -> Cgt | o -> o
        match at, bt, op with
        | ErrorType, _, _ | _, ErrorType, _ -> []
        | _, _, InInst -> [yield! ils; +Call(ctx.FindMethodReference "INSET")]
        | (StrType as t), StrType, AddInst -> ils @ useHelperOp ctx "ConcatStr" t byRef
        | StrType, StrType, BoolOp -> [+Ldc_I4 0; yield! ils; +Call(ctx.FindMethodReference "CompareStr"); +convCmpOp op]
        | (SetType at), ((SetType bt) as t), AddInst when at = bt -> ils @ useHelperOp ctx "SetUnion" t byRef
        | (SetType at), ((SetType bt) as t), MinusInst when at = bt -> ils @ useHelperOp ctx "SetDifference" t byRef
        | NumericType, NumericType, op -> [yield! ils; +op]
        | _ -> failwith "IE"
        ,match et with | Some t -> t | _ -> at

    let rec exprToIlGen refRes (ctx: Ctx) exprEl expectedType =
        let rec exprToMetaExpr (el: ExprEl) et refRes =
            let constConvertTo op (aExpr: ExprEl, aVal, aTyp) (bExpr: ExprEl, bVal, bTyp) =
                let typesConverter (expr: ExprEl) aVal aTyp bTyp =
                    match aTyp, bTyp with
                    | ChrType, CharacterType ctx.sysTypes.string (t) -> exprToMetaExpr expr (Some t) false
                    | _ -> aVal, aTyp
                op,
                match op with
                | InInst -> (aVal, aTyp), exprToMetaExpr bExpr (Some bTyp) true
                | BoolOp -> (aVal, aTyp), (bVal, bTyp)
                | _ ->
                    typesConverter aExpr aVal aTyp bTyp,
                    typesConverter bExpr bVal bTyp aTyp

            let typeConvertTo op ((aVal, aTyp), (bVal, bTyp)) =
                let convertChrToStr c t =
                    let _,v = ctx.EnsureVariable t
                    [
                        +Ldloca v
                        yield! c
                        +Stind Ind_U1
                        +Ldloc v
                    ]
                let strt = ctx.sysTypes.string
                op,
                match op, aTyp, bTyp with
                | _, ErrorType, _ | _, _, ErrorType -> [], aTyp, bTyp
                | BoolOp, _, _ -> [yield! aVal; yield! bVal],aTyp,bTyp
                | _, ChrType, (StrType as t) -> [yield! convertChrToStr aVal t; yield! bVal],t,t
                | _, (StrType as t), ChrType -> [yield! aVal; yield! convertChrToStr bVal t],t,t
                | _, ChrType, ChrType -> [yield! aVal; yield! convertChrToStr bVal strt],strt,strt
                | _ when sameTypeKind(aTyp, bTyp) -> [yield! aVal; yield! bVal],aTyp,bTyp
                | InInst, _, _ -> [yield! aVal; yield! bVal],aTyp,bTyp
                | _ -> [yield! aVal; yield! bVal; aTyp.RefToConv],aTyp,bTyp

            let add2OpIlTyped aExpr bExpr i et =
                let a, at = exprToMetaExpr aExpr et false
                let b, bt =  exprToMetaExpr bExpr (if i = InInst then None else Some at) false
                let opIls, typ =
                    constConvertTo i (aExpr, a, at) (bExpr, b, bt)
                    ||> typeConvertTo
                    |> handleOperator ctx et refRes
                ([
                    yield! opIls
                    // need to cast to proper type (for simple types)
                    match et with
                    | Some NumericType -> yield et.Value.RefToConv
                    | _ -> ()
                ], typ)
            let add2OpIl a b i = add2OpIlTyped a b i et
            let add2OpIlBool a b i = add2OpIlTyped a b i (Some ctx.sysTypes.boolean)
            let inline add1OpIl a i =
                let a, at = exprToMetaExpr a et false
                [ yield! a; yield +i; yield at.RefToConv], at
            match el with
            | Value v -> valueToValueKind ctx v et refRes
            | Add(a, b) -> add2OpIl a b AddInst
            | Multiply(a, b) -> add2OpIl a b MultiplyInst
            | Minus(a, b) -> add2OpIl a b MinusInst
            | Divide(a, b) -> add2OpIl a b DivideInst
            | And(a, b) -> add2OpIl a b AndInst
            | Or(a, b) -> add2OpIl a b OrInst
            | Xor(a, b) -> add2OpIl a b XorInst
            | Mod(a, b) -> add2OpIl a b Rem
            | Div(a, b) -> add2OpIl a b DivideInst
            | Shl(a, b) -> add2OpIl a b ShlInst
            | Shr(a, b) -> add2OpIl a b ShrInst
            | Not(a) -> add1OpIl a NotInst
            | UnaryMinus(a) -> add1OpIl a NegInst
            | Equal(a, b) -> add2OpIl a b Ceq
            | NotEqual(a, b) ->
                match add2OpIlBool a b Ceq with
                | o, ot -> ([ yield! o; +Ldc_I4 0; +Ceq ], ctx.sysTypes.boolean)
            | StrictlyLessThan(a, b) -> add2OpIlBool a b Clt
            | StrictlyGreaterThan(a, b) -> add2OpIlBool a b Cgt
            | LessThanOrEqual(a, b) ->
                match add2OpIlBool a b Cgt with
                | o, ot -> [ yield! o; +Ldc_I4 0; +Ceq ], ctx.sysTypes.boolean
            | GreaterThanOrEqual(a, b) ->
                match add2OpIlBool a b Clt with
                | o, ot -> [ yield! o; +Ldc_I4 0; +Ceq ], ctx.sysTypes.boolean
            | In(a, b) -> add2OpIlBool a b InInst
            | Addr(a) ->
                ([
                    match a with
                    | Value((VIdent _) as v) -> yield! (valueToValueKind ctx v et true |> fst)
                    | _ -> failwith "IE"
                ], ctx.sysTypes.pointer)
            | UnitOp -> ([], ctx.sysTypes.unit) // call `WriteLn();`
            | _ -> failwith "IE"
            |> ValueKind

        let a, at = exprToMetaExpr exprEl expectedType refRes
        [
         yield! a
         if expectedType.IsSome then
            let typ = expectedType.Value
            match typ.kind with | TkOrd _ | TkFloat _ -> yield typ.RefToConv | _ -> ()
        ], at

    let exprToIl = exprToIlGen false

    let callParamToIl ctx cp (idxmr: Option<int * MethodInfo>) =
        let param, refKind =
            match idxmr with
            | Some(idx,{paramList=pl}) when idx < pl.Length -> // for foo() where () has meaning because idx is valid but pl.Length = 0
                let p = pl.[idx]
                Some p.typ, p.ref
            | _ -> None, RefNone
        match cp with
        | ParamExpr expr -> exprToIl ctx (if refKind.IsRef then Addr expr else expr) param
        | ParamIdent id ->
            let useRef = refKind.IsRef || (param.IsSome && (param.Value = ctx.sysTypes.constParam || param.Value = ctx.sysTypes.varParam))
            if useRef then findSymbolAndGetPtr ctx id
            else findSymbolAndLoad ctx id
        |> fun(il, t) ->
            if param.IsSome then
                if not(Utils.typeCheck ctx param.Value t) then
                    ``Error: Incompatible types ('%O' and '%O') for %d parameter`` param.Value.name t.name (fst idxmr.Value)
                    |> ctx.NewMsg cp
            (il, t)

    let doCall (ctx: Ctx) (CallExpr(ident, cp)) popResult =
        
//        let veryfyCallParams mi cp =
//            let length = (cp: CallParam list).Length
//            if mi.paramList.Length > length then
//                ``Error: More parameters expected`` |> ctx.NewMsg ident
//                ([], Some ctx.sysTypes.unknown)
//            elif mi.paramList.Length < length then // TODO handle empty tuple param with unitop  
//                ``Error: Unexpected parameter`` |> ctx.NewMsg ident
//                ([], Some ctx.sysTypes.unknown)            
        
        match ctx.FindFunction ident with
        | SymSearch.TypeCast({kind=TkProcVar pv} as t) ->
            match cp with
            | [ParamIdent(cpi)]::tailCalls when tailCalls.IsEmpty ->
                // TODO handle self parameter in the future
                // cast to parameterless func/proc and call at once, but only for head (cpi)
                if pv.paramList.Length = 0 then
                    ([
                        yield! fst <| EvalExpr.findSymbolAndLoad ctx cpi
                        +Calli pv.raw
                        if pv.result.IsSome then +Pop
                    ], pv.ResultType)
                else // cant call proc with params expected
                    // TODO default params
                    ``Error: More parameters expected`` |> ctx.NewMsg ident
                    ([], Some ctx.sysTypes.unknown)
            | [ParamIdent(cpi)]::tailCalls ->
                // advanced calls like TFoo(x)(1,2); or TFoo(x)(); or TFoo(x)()();
                let h = tailCalls.Head
                if pv.paramList.Length > h.Length then
                    ``Error: More parameters expected`` |> ctx.NewMsg ident
                    ([], Some ctx.sysTypes.unknown)
                elif pv.paramList.Length < h.Length then // TODO handle empty tuple param   
                    ``Error: Unexpected parameter`` |> ctx.NewMsg ident
                    ([], Some ctx.sysTypes.unknown)
                else
                    [
                        yield! h
                        |> List.mapi (fun i p -> fst <| callParamToIl ctx p (Some(i, pv)))
                        |> List.concat
                        yield! fst <| EvalExpr.findSymbolAndLoad ctx cpi
                        +Calli pv.raw
                        if pv.result.IsSome then +Pop
                    ], pv.ResultType
                // TODO more levels of call ?
            | _ ->
                ``Error: %s expected`` "Only one parameter" |> ctx.NewMsg ident
                ([], Some ctx.sysTypes.unknown)
        | SymSearch.TypeCast t ->
            match cp with
            | [cp]::[] ->
                let callInstr, typ = callParamToIl ctx cp None
                // TODO some real conversion ? Conv_I4 + explicit operators?
                ([
                    yield! callInstr
                    t.RefToConv
                ], Some t)
            | _ ->
                if cp.Tail.Length > 0 then
                    ``Error: Improper expression`` |> ctx.NewMsg ident
                else    
                    ``Error: %s expected`` "For typecast only one parameter is" |> ctx.NewMsg ident
                ([], Some ctx.sysTypes.unknown)
        | SymSearch.RealFunction rf ->
            let cp = if cp.IsEmpty then [] else cp.Head
            match rf with
            | Referenced(mr,np) as rm, f ->
                ([
                    yield! cp
                    |> List.mapi (fun i p -> fst <| callParamToIl ctx p (Some(i, mr)))
                    |> List.concat
                    // TODO recursive call nested! For now before nested routine is added call can be generated
                    yield! !np
                    |> List.map (fun (VariableSymLoad vlt) -> loadSymList ctx false true vlt |> fst)
                    |> List.concat
                    yield! f
                    // TODO error/IE ? for popResult && mr.result.IsNone
                    if popResult && mr.result.IsSome then
                        yield +Pop
                 ], rm.ReturnType)
            | Intrinsic i, _ -> handleIntrinsic i {ctx=ctx;ident=ident;cp=cp;popResult=popResult}
            | _ -> raise (InternalError "2020111600")
        | SymSearch.UnknownFunction ->
            ``Error: %s expected`` "callable ident" |> ctx.NewMsg ident
            ([], Some ctx.sysTypes.unknown)

    let valueToValueKind ctx v (expectedType: PasType option) byRef =
        match v, byRef with
        | VIdent _, true | VSet _, true -> ()
        | _, false -> ()
        | _ -> failwith "IE"

        match v, expectedType with
        | VInteger i, Some FloatType -> [+Ldc_R4(single i)], ctx.sysTypes.single
        | VInteger i, _ -> [+Ldc_I4 i], ctx.sysTypes.int32
        | VFloat f, _ -> [+Ldc_R4 (single f)], ctx.sysTypes.single
        | VIdent i, _ -> if byRef then findSymbolAndGetPtr ctx i else findSymbolAndLoad ctx i
        | VString s, _ ->
            let doChr() =
                let cv = int(s.Chars 0)
                [+Ldc_I4 cv], ctx.sysTypes.char
            // TODO protect string as char interpretation when needed
            match expectedType with
            | None when s.Length = 1 -> doChr()
            | Some ChrType ->
                if s.Length <> 1 then failwith "IE"
                else doChr()
            | _ -> // handle string
                ([
                    strToSStr s
                    |> ctx.details.AddBytesConst
                    |> Ldsfld |> (~+)
                ], ctx.sysTypes.string)
        | VCallResult((CallExpr(ident, _)) as ce), _ ->
            match doCall ctx ce false with
            | ils, Some t -> ils, t
            | _ ->
                ``Error: %s expected`` "function" |> ctx.NewMsg ident
                [], ctx.sysTypes.unknown
        | VNil, _ -> [+Ldnull], ctx.sysTypes.pointer
        | VSet al, _ ->
            let bytes, ft = match setValueToCER ctx expectedType al with
                            | CEROrdSet(b,t) -> b, t
                            | _ -> failwith "IE"
            let fd = ctx.details.AddBytesConst bytes
            [+(if byRef then Ldsflda fd else Ldsfld fd)], ft

    let chainReaderFactory (ctx: Ctx) asValue addr ltp =
        let valOrPtr v p (t: TypeSig) = (if asValue || (t.IsPointer && addr = false) then v else p) |> List.singleton
        match ltp with
        | LTPVar(v, vt) -> match v with
                           | LocalVariable v -> valOrPtr +(Ldloc v) +(Ldloca v) vt.Sig
                           | GlobalVariable v -> valOrPtr +(Ldsfld v) +(Ldsflda v) vt.Sig
                           | ParamVariable(r, v) -> // handle by ref params
                             match r with
                             | RefNone | RefConst -> valOrPtr +(Ldarg v) +(Ldarga v) vt.Sig
                             | RefVar ->
                                 [
                                    +Ldarg v
                                    if asValue then
                                        +Ldobj vt.DefOrRef
                                 ]
                             | RefUntypedVar | RefUntypedConst ->
                                 if asValue then failwith "IE"
                                 [+Ldarg v]
        | LTPStruct (fld, _) -> valOrPtr +(Ldfld fld) +(Ldflda fld) fld.FieldSig.Type
        | LTPDeref (dt, force) ->
            match dt, addr, force, asValue with
            | _, true, _, _ -> []
            | NumericType, _, _, _ | ChrType, _, _, _ -> dt.IndKind |> Ldind |> (~+) |> List.singleton
            | _, _, true, _ | _, _, _, true ->
                if dt.Sig.IsValueType = false then failwith "IE"
                [+Ldobj dt.DefOrRef]
            | _ -> []
        | LTPNone -> []

    let chainWriterFactory (ctx: Ctx) = function
        | LTPVar(v, t) -> match v with
                          | LocalVariable v -> [+Stloc v]
                          | GlobalVariable v -> [+Stsfld v]
                          | ParamVariable(r, v) ->
                               match r with // handle by ref params
                               | RefNone | RefConst -> [+Starg v]
                               | RefVar -> chainWriterFactory ctx (LTPDeref(t, true))
                               | RefUntypedConst | RefUntypedVar -> failwith "IE"
        | LTPStruct(fld, _) -> [+Stfld fld]
        | LTPDeref(dt, _) ->
            if dt.Sig.IsValueType then
                [+Stobj dt.DefOrRef]
            else
                dt.IndKind |> Stind |> (~+) |> List.singleton
        | LTPNone -> []

    let derefLastTypePoint = function
        | LTPVar(_, t) -> derefType t
        | LTPDeref(t, _) -> derefType t
        | LTPStruct(_, t) -> derefType t
        | _ -> failwith "cannot deref"

    let loadSymList (ctx: Ctx) value addr (sl, t) =
        let lastPoint = ref LTPNone
        sl
        |> List.collect (chainLoadToIl ctx lastPoint (chainReaderFactory ctx false false))
        |> fun l -> l @ (chainReaderFactory ctx value addr !lastPoint)
        ,match t with | Some t -> t | _ -> failwith "IE"

    let findSymbolInternal value addr (ctx: Ctx) ident =
        let sid = ctx.FindSymbol ident |> chainToSLList
        match sid with
        | _, Some _ -> sid |> loadSymList ctx value addr
        | [CallableLoad sl], None ->
            // handle x := foo; where foo is procedured
            [+Ldftn sl.MethodInfo.raw], sl.MethodInfo.PasType
        | _, None ->
            ``Error: %s expected`` "value" |> ctx.NewMsg ident
            [], ctx.sysTypes.unknown

    let findSymbolAndLoad = findSymbolInternal true false

    let findSymbolAndGetPtr = findSymbolInternal false true

    let chainLoadToIl ctx lastType factory symload =
        let res = factory !lastType
        match symload with
        | VariableLoad vs ->
            lastType := LTPVar vs
            res
        | DerefLoad ->
            let dt = derefLastTypePoint !lastType
            match dt.kind with
            // TODO what with arrays ?
            | TkRecord _ | TkArray _ -> lastType := LTPDeref(dt, false) // special case like someRec^.Field or str^[4]
            | _ -> lastType := LTPDeref(dt, true)
            res
        | ElemLoad (exprs, rt) ->
            lastType := LTPDeref(rt, false)
            [
                yield! res
                for e, d in exprs do
                    // do not minus if not needed
                    yield! fst <| exprToIl ctx (Multiply(Minus(e,d.low |> VInteger |> Value), d.elemSize |> VInteger |> Value)) (Some ctx.sysTypes.int32)
                    yield +AddInst
            ]
        | StructLoad fds ->
            let instr, last, count = List.fold (fun (acc, _, c) f -> +Ldflda (fst f)::acc, f, c+1) ([],Unchecked.defaultof<_>,0) fds
            lastType := LTPStruct last
            res @ (List.take (count-1) (instr |> List.rev))
        | ValueLoad evs ->
            lastType := LTPNone
            res @
            [
                match evs with
                | ValueInt i -> +Ldc_I4 i
                | ValueFloat f -> +Ldc_R4 f
            ]
        | CallableLoad (Referenced(mr,_)) ->
            if mr.result.IsNone then failwith "IE"
            [+Call mr.raw]
        // probably below IE can be reduced
        | _ -> raise (InternalError "2020062201")


module EvalConstExpr =

    let inline eval1 ctx typ e1 = evalConstExpr ctx typ e1

    let evalExprOp2 (opS: Option<string->string->string>) (opI: Option<int->int->int>) (r1, r2) =
        match r1, opS, opI with
        | CERString s1, Some so, _ -> match r2 with CERString s2 -> CERString(so s1 s2) | _ -> assert(false); CERUnknown
        | CERInt(i1, t1), _, Some io -> match r2 with CERInt(i2, t2) -> CERInt(io i1 i2, t1) | _ -> assert(false); CERUnknown
        | _ -> CERUnknown

    let evalExprOp1 (opS: Option<string->string>) (opI: Option<int->int>) r1 =
        match r1, opS, opI with
        | CERString s1, Some so, _ -> CERString(so s1)
        | CERInt(i1, t1), _, Some io -> CERInt(io i1, t1)
        | _ -> CERUnknown

    let setValueToCER ctx typ al =
        let inline eval1 e1 = eval1 ctx typ e1

        let exprToByteValue = function
            | CERString s when s.Length = 1 -> byte(s.Chars 0), ctx.sysTypes.char
            | CERInt(i,t) when i >= 0 && i <= 255 -> byte(i), t
            | _ -> failwith "IE"

        let exprToSetItem = function
            | SValue e -> let r, t = eval1 e |> exprToByteValue in [r], t
            | SRange(e1, e2) ->
                // TODO types check
                let r1, t1 = eval1 e1 |> exprToByteValue
                let r2, t2 = eval1 e2 |> exprToByteValue
                if r2 < r1 then failwith "IE"
                [r1..r2], t1

        let finalSet: byte[] = Array.zeroCreate 256
        let items, enumTyp =
            List.map exprToSetItem al
            |> List.unzip
            |> (fun (i, t) -> i, t.Head)
        List.concat items
        |> List.fold (fun s i -> if Set.contains i s then failwith "IE" else Set.add i s) Set.empty // TODO contains better error
        |> Set.iter (fun i -> finalSet.[int i] <- 0xFFuy)
        match typ, enumTyp with
        | Some t, _ -> CEROrdSet(finalSet, t)
        | None, EnumType | None, ChrType ->
            let typeSet =
                match ctx.enumSet.TryGetValue enumTyp with
                | true, t -> t
                | _ -> ctx.AddTypeSetForEnum enumTyp AnonName
            CEROrdSet(finalSet, typeSet)
        | _ -> failwith "IE"

    let evalConstExpr (ctx: Ctx) typ expr =

        let inline eval2 e1 e2 = (evalConstExpr ctx typ e1, evalConstExpr ctx typ e2)
        let inline eval1 e1 = eval1 ctx typ e1

        match expr with
        | Value v ->
            match v with
            | VFloat f -> CERFloat <| single f
            | VInteger i -> CERInt(i, ctx.sysTypes.int32)
            | VString s -> CERString s
            | VIdent id ->
                match ctx.FindConstSym id with
                | ValueInt i, t ->
                    // TODO Type check with typ
                    match t with
                    | EnumType -> CERInt(i, t)
                    | _ -> CERInt(i, ctx.sysTypes.int32)
                | _ -> failwith "IE"
            | VCallResult _ -> CERUnknown
            | VNil -> CERUnknown
            | VSet al -> setValueToCER ctx typ al
        | Expr e -> evalConstExpr ctx typ e
        | Add(e1, e2)      -> eval2 e1 e2 |> evalExprOp2 (Some (+)) (Some (+)  )
        | Multiply(e1, e2) -> eval2 e1 e2 |> evalExprOp2 None       (Some (*)  )
        | Minus(e1, e2)    -> eval2 e1 e2 |> evalExprOp2 None       (Some (-)  )
        | Divide(e1, e2)   -> eval2 e1 e2 |> evalExprOp2 None       (Some (/)  )
        | And(e1, e2)      -> eval2 e1 e2 |> evalExprOp2 None       (Some (&&&))
        | Or(e1, e2)       -> eval2 e1 e2 |> evalExprOp2 None       (Some (|||))
        | Xor(e1, e2)      -> eval2 e1 e2 |> evalExprOp2 None       (Some (^^^))
        | Mod(e1, e2)      -> eval2 e1 e2 |> evalExprOp2 None       (Some (%)  )
        | Div(e1, e2)      -> eval2 e1 e2 |> evalExprOp2 None       (Some (/)  )
        | Shl(e1, e2)      -> eval2 e1 e2 |> evalExprOp2 None       (Some (<<<))
        | Shr(e1, e2)      -> eval2 e1 e2 |> evalExprOp2 None       (Some (>>>))
        | Not(e1)          -> eval1 e1 |> evalExprOp1 None (Some (~~~))
        | UnaryPlus(e1)    -> eval1 e1 |> evalExprOp1 None (Some Microsoft.FSharp.Core.Operators.(~+) )
        | UnaryMinus(e1)   -> eval1 e1 |> evalExprOp1 None (Some (~-) )
        | _ -> CERUnknown
        (*
        | As of ExprEl * ExprEl
        | Addr of ExprEl
        | Equal of ExprEl * ExprEl
        | NotEqual of ExprEl * ExprEl
        | StrictlyLessThan of ExprEl * ExprEl
        | StrictlyGreaterThan of ExprEl * ExprEl
        | LessThanOrEqual of ExprEl * ExprEl
        | GreaterThanOrEqual of ExprEl * ExprEl
        | Is of ExprEl * ExprEl
        | In of ExprEl * ExprEl
        *)

module Intrinsics =

    open EvalExpr
    
    let VariablePasType (ctx: Ctx) id =
        match ctx.FindSymbol id |> chainToSLList with
        | _, Some t -> Some([], t)
        | _ -> None

    let TypePasType (ctx: Ctx) = function
        | DIdent[Ident(id)] ->
            match ctx.FindSym(CompilerName.FromString id) with
            | Some (TypeSym t) -> Some([], t)
            | _ -> None
        | _ -> None
        
    // TODO check only first ident part (see FromDIdent). More advanced cases are handled in findSymbolAndGetPtr
    let VariableIdent (ctx: Ctx) byRef id =
        match ctx.FindSym(CompilerName.FromDIdent id) with
        | Some s ->
            match s with
            | VariableSym _ ->
                (findSymbolInternal (not byRef) byRef ctx id) |> Some
            | _ -> None
        | _ -> None

    let getParamIdent cp =
        match cp with
        | ParamIdent id -> Some(id)
        | _ -> None

    type ParamRec = { Instructions: IlInstruction list; Type: PasType }

    type ParamLocationPolicy =
        | InAny
        | InExpr // param acceptable only in expressions
        | InBlock // param acceptable only in block
        
    type ParamRefPolicy =
        | ByDefault
        | ByRef
        
    type ParamModifierPolicy =
        | Expression
        | VarIdent     // variable ident
        | VarIdentType // variable ident, but return variable type
        | PasType // type ident

    type ParamTypPolicy = // typ is very generic term for this DU
        | AnyTyp
        | PtrTyp  // allow pointer types like PInteger (type ident for pointers, no variable of some type)
        | FloatTyp
        | IntTyp
        | OrdTyp
    with
        member this.ToPattern() =
            match this with
            | IntTyp -> (|IntType|_|) 
            | FloatTyp -> (|FloatType|_|)
            | OrdTyp -> (|OrdType|_|)
            | PtrTyp -> (|PointerType|_|)
            | AnyTyp -> (fun _ -> Some())
        
        member this.IsFloat = match this with | FloatTyp -> true | _ -> false
        
    type ParamBuildRec = {
        Location: ParamLocationPolicy option
        Ref: ParamRefPolicy option
        Modifier: ParamModifierPolicy option
        Typ: ParamTypPolicy option
    }
    with
        static member Empty = { Location = None; Ref = None; Modifier = None; Typ = None }

    type ParamBuildState =
        | PBProcess
        | PBError
        | PBIgnore
        | PBOk of ParamRec
    with
        member this.bind v =
            match this, v with
            | PBProcess, Ok x -> PBProcess, x
            | PBError, Ok x -> PBError, x
            | PBOk _, Ok _ -> raise (InternalError "2020080600")
            | PBIgnore, Ok _ -> raise (InternalError "2020081100")
            | PBProcess, Error x -> PBError, x
            | PBError, Error x -> PBError, x
            | PBOk _, Error _ -> raise (InternalError "2020080601")
            | PBIgnore, Error _ -> raise (InternalError "2020081101")
            
        member this.bindMap v map = let state, r = this.bind v in state, map r
    
    type ParamBuilderResult = {
        Candidates: ParamBuildRec list
        Current: ParamBuildRec
        State: ParamBuildState
    }
    with
        static member Empty =
            {
                Candidates = []
                Current = ParamBuildRec.Empty
                State = PBProcess
            }
        member this.Location = defaultArg this.Current.Location InAny
        member this.Ref = defaultArg this.Current.Ref ByDefault
        member this.Modifier = defaultArg this.Current.Modifier Expression
        member this.Typ = defaultArg this.Current.Typ AnyTyp    
    
    module Result =
        
        let noneLocation pbr = pbr.Location.IsNone
        let noneRef pbr = pbr.Ref.IsNone
        let noneModifier pbr = pbr.Modifier.IsNone
        let noneTyp pbr = pbr.Typ.IsNone
        
        let bindParam r n f pr =
            match pr.State with
            | PBOk _ | PBIgnore -> pr
            | PBError | PBProcess ->
                if r pr.Current then
                    let newState, current = pr.State.bindMap (f pr.Current) (n pr.Current)
                    { pr with
                        State = newState
                        Current = current }
                else raise (InternalError "2020080203")
        
        let bindParamLocation = bindParam noneLocation (fun c l -> { c with Location = Some l })
        let bindParamRef = bindParam noneRef (fun c r -> { c with Ref = Some r })
        let bindParamModifier = bindParam noneModifier (fun c m -> { c with Modifier = Some m })
        let bindParamTyp = bindParam noneTyp (fun c t -> { c with Typ = Some t })
        let bindParamIF f pr =
            match pr.State with
            | PBProcess -> if f then pr else { pr with State = PBIgnore }
            | PBOk _ | PBError | PBIgnore -> pr
    
    [<AllowNullLiteral>]
    type ParamBuilder(cp, ci) =
        let ctx = ci.ctx
        
        let isExpr _ = if ci.InExpr then Ok InExpr else Error InExpr
        let isBlock _ = if ci.InBlock then Ok InBlock else Error InBlock
            
        let tryAsSimpleType (typ: ParamTypPolicy) =
            let inst, t = callParamToIl ctx cp None
            match typ.ToPattern() t with 
            | Some _ -> Some{Instructions=inst; Type=t}
            | _ -> None
            
        let tryAsCompilerType f (typ: ParamTypPolicy) =
            match getParamIdent cp with
            | Some t ->
                match f t with
                | Some (il, t) ->
                    match t |> typ.ToPattern() with
                    | Some _ -> Some{Instructions=il; Type=t}
                    | _ -> None
                | _ -> None
            | _ -> None
       
        let tryAsPasType = tryAsCompilerType (TypePasType ctx)
        let tryAsVarIdentType = tryAsCompilerType (VariablePasType ctx)

        let tryAsVarIdent ref =
            let byRef = match ref with | ByRef -> true | _ -> false
            tryAsCompilerType (VariableIdent ctx byRef)
             
        let bindParamNextAttempt pr =
            let doNext addCurrent =
                { pr with
                    Candidates = if addCurrent then pr.Current::pr.Candidates else pr.Candidates
                    Current = ParamBuildRec.Empty
                    State = PBProcess }
            match pr.State with
            | PBOk _ -> pr
            | PBProcess ->
                let result =
                    match pr.Modifier with
                    | Expression -> tryAsSimpleType
                    | VarIdent -> tryAsVarIdent pr.Ref
                    | VarIdentType -> tryAsVarIdentType
                    | PasType -> tryAsPasType
                    <| pr.Typ
                match result with
                | Some r -> { pr with State = PBOk r } // TODO ? basic check pr.current ?
                | None -> doNext true
            | PBError -> doNext true
            | PBIgnore -> doNext false
                        
        member _.Yield (_: unit) = ParamBuilderResult.Empty
        
        [<CustomOperation("inExpr",MaintainsVariableSpace=true)>]
        member _.InExpr v = Result.bindParamLocation isExpr v

        [<CustomOperation("inBlock",MaintainsVariableSpace=true)>]
        member _.InBlock v = Result.bindParamLocation isBlock v

        [<CustomOperation("byRef",MaintainsVariableSpace=true)>]
        member _.ByRef v = Result.bindParamRef (fun _ -> Ok ByRef) v
                
        [<CustomOperation("int",MaintainsVariableSpace=true)>]
        member _.TryAsInt v = Result.bindParamTyp (fun _ -> Ok IntTyp) v

        [<CustomOperation("float",MaintainsVariableSpace=true)>]
        member _.TryAsFloat v = Result.bindParamTyp (fun _ -> Ok FloatTyp) v

        [<CustomOperation("ord",MaintainsVariableSpace=true)>]
        member _.TryAsOrd v = Result.bindParamTyp (fun _ -> Ok OrdTyp) v

        [<CustomOperation("ptr",MaintainsVariableSpace=true)>]
        member _.TryAsPtr v = Result.bindParamTyp (fun _ -> Ok PtrTyp) v

        [<CustomOperation("any",MaintainsVariableSpace=true)>]
        member _.TryAsAny v = Result.bindParamTyp (fun _ -> Ok AnyTyp) v

        [<CustomOperation("pasType",MaintainsVariableSpace=true)>]
        member _.TryAsPasType v = Result.bindParamModifier (fun _ -> Ok PasType) v

        [<CustomOperation("varType",MaintainsVariableSpace=true)>]
        member _.TryAsVarType v = Result.bindParamModifier (fun _ -> Ok VarIdentType) v

        [<CustomOperation("var",MaintainsVariableSpace=true)>]
        member _.TryAsVar v = Result.bindParamModifier (fun _ -> Ok VarIdent) v

        [<CustomOperation("IF",MaintainsVariableSpace=true)>]
        member _.DoIf (v, f) = Result.bindParamIF f v

        [<CustomOperation("OR",MaintainsVariableSpace=true)>]
        member _.DoOr v = bindParamNextAttempt v
                
        member this.Run v =
            let r = bindParamNextAttempt v // finalize process
            match r.State with
            | PBOk res ->
                Some {| Instructions = res.Instructions
                        Type = res.Type
                        Location = r.Location
                        Ref = r.Ref
                        Modifier = r.Modifier
                        Typ = r.Typ |}
            | PBError -> raise (InternalError "2020080205")
            | PBProcess ->
                let finalList = List<string>()
                for i in r.Candidates do
                    match i.Modifier with
                    | Some Expression | None -> "expression"
                    | Some VarIdent | Some VarIdentType -> "variable"
                    | Some PasType -> "identifier"
                    + " of " +
                    match i.Typ with
                    | Some AnyTyp | None -> "any"
                    | Some PtrTyp -> "pointer"
                    | Some FloatTyp -> "float"
                    | Some IntTyp -> "integer"
                    | Some OrdTyp -> "ordinal"
                    + " type" +
                    match i.Location with
                    | Some InExpr -> " in expression"
                    | Some InBlock -> " in code block"
                    | _ -> ""
                    |> finalList.Add
                ``Error: %s expected`` (String.concat " or " finalList) |> ctx.NewMsg cp
                None
            | _ -> raise (InternalError "2020080801")

    type ParamsBuilder(ci: CallInfo, rt: PasType option) =
        let ctx = ci.ctx
        let cp = Queue(ci.cp)
        let finalIls = List<IlInstruction list>()
        let paramsList =
            List<{| Instructions: IlInstruction list
                    Type: PasType
                    Location: ParamLocationPolicy
                    Ref: ParamRefPolicy
                    Modifier: ParamModifierPolicy
                    Typ: ParamTypPolicy |} option>()
        
        let nextParam() =
            match cp.TryDequeue() with
            | true, cp -> ParamBuilder(cp, ci) |> Some |> Some
            | _ ->
                ``Error: More parameters expected`` |> ctx.NewMsg ci.ident
                None
        
        let nextOptionalParam() =
            match cp.TryDequeue() with
            | true, cp -> ParamBuilder(cp, ci) |> Some |> Some
            | false, _ -> None |> Some
            
        let noParams f () =
            match cp.TryPeek() with
            | true, cp ->
                ``Error: Unexpected parameter`` |> ctx.NewMsg cp
                None
            | false, _ ->
                f() |> finalIls.Add
                Some()
                
        let paramSpecialize f1 f2 = function
            | Some p ->
                match f1(p) with
                | Some p ->
                    paramsList.Add(Some p)
                    f2(p) |> finalIls.Add
                    Some()
                | _ -> None
            | None -> None
            
        let optionalParamSpecialize f1 f2 f3 = function
            | Some p ->
                match f1(p) with
                | Some p ->
                    paramsList.Add(Some p)
                    f2(p) |> finalIls.Add
                    Some()
                | _ -> None
            | None ->
                paramsList.Add None
                f3() |> finalIls.Add
                Some()
        
        let doIl il () = finalIls.Add il; Some()

        member val ReturnType = rt with get, set
        
        member _.Return a = a
        member _.Return (_: unit) = Some()
        
        [<CustomOperation("inExpr",MaintainsVariableSpaceUsingBind=true)>]
        member _.InExpr (_) =
            if ci.popResult then // TODO warning ? about ignored expression
                ``Error: Improper expression`` |> ctx.NewMsg ci.ident
                None
            else
                Some()

        [<CustomOperation("inBlock",MaintainsVariableSpaceUsingBind=true)>]
        member _.InBlock (_) =
            if not ci.popResult then
                ``Error: Improper expression`` |> ctx.NewMsg ci.ident
                None
            else
                Some()

        member _.Bind(p: ParamBuilder option option, f) = // for 'into' proper type inference 
            Option.map f p

        [<CustomOperation("parameter",MaintainsVariableSpaceUsingBind=true,AllowIntoPattern=true)>]
        member _.NextParam v = Option.bind nextParam v 

        [<CustomOperation("optional",MaintainsVariableSpaceUsingBind=true,AllowIntoPattern=true)>]
        member _.NextOptionalParam v = Option.bind nextOptionalParam v

        [<CustomOperation("noParams",MaintainsVariableSpaceUsingBind=true)>]
        member _.NoParams (v, [<ProjectionParameter>] f) = Option.bind (noParams f) v
        
        [<CustomOperation("doParameter", MaintainsVariableSpaceUsingBind=true)>]
        member _.ParamSpecialize (p: ParamBuilder option option, [<ProjectionParameter>] f1, [<ProjectionParameter>] f2) =
            Option.bind (paramSpecialize f1 f2) p

        [<CustomOperation("doOptional", MaintainsVariableSpaceUsingBind=true)>]
        member _.OptionalParamSpecialize (p: ParamBuilder option option, [<ProjectionParameter>] f1, [<ProjectionParameter>] f2,
                                  [<ProjectionParameter>] f3) =
            Option.bind (optionalParamSpecialize f1 f2 f3) p

        [<CustomOperation("doIl", MaintainsVariableSpaceUsingBind=true)>]
        member _.DoIl(v, il) = Option.bind (doIl il) v
                
        member self.Run(r) =
            let rt = self.ReturnType
            match r, cp.TryDequeue() with
            | _, (true, cp) ->
                ``Error: Unexpected parameter`` |> ctx.NewMsg cp
                [], rt
            | Some _, (false, _) -> finalIls |> Seq.concat |> List.ofSeq, rt // final generated    
            | _ -> [], rt // other error reported in some operators / other CE
                
        member _.Params = paramsList
                
    let private doGenWrite ci =
        let ctx, cp = ci.ctx, ci.cp
        let file, cp =
            match cp with
            | ParamIdent id::tail ->
                // TODO : do not generate twice the same error like in tarrtest.pas for F1 ( Writeln(p^[2]^); // bad deref)
                let sl, typ = findSymbolAndGetPtr ctx id
                if typ.raw = ctx.sysTypes.file.raw then Some sl, tail
                else None, cp
            | _ -> None, cp
        let file() = if file.IsNone then fst <| findSymbolAndGetPtr ctx (Utils.stdIdent "STDOUTPUTFILE")
                     else file.Value

        let doParam = fun (cp: CallParam) ->
            let someInt = Some ctx.sysTypes.int32
            let e,w,p = match cp with
                        | ParamExpr(TupleExpr[v;w;p]) -> ParamExpr(v),fst(exprToIl ctx w someInt),fst(exprToIl ctx p someInt)
                        | ParamExpr(TupleExpr[v;w]) -> ParamExpr(v),fst(exprToIl ctx w someInt),[+Ldc_I4 0]
                        | _ -> cp,[+Ldc_I4 0],[+Ldc_I4 0]
            let valParam, typ = callParamToIl ctx e None

            if typ = ctx.sysTypes.unit then
                []
            else
                match typ.kind with
                | TkOrd(OkInteger,_) -> "WRITEINTF"
                | TkOrd(OkBool,_) -> "WRITEBOOLEANF"
                | TkOrd(OkChar,_) -> "WRITECHARF"
                | TkFloat _ -> "WRITEREALF"
                | TkPointer _ -> "WRITEPOINTERF"
                | TkArray(AkSString _,_,_) -> "WRITESTRINGF"
                | _ -> ``Error: Unknown type kind of expression for Write/WriteLn: %O`` cp |> ctx.NewMsg cp
                       ""
                |> (fun f -> if String.IsNullOrEmpty f then None else ctx.FindMethodReferenceOpt f) |>
                function
                | Some subWrite ->
                    [
                        yield! file()
                        +Ldnull
                        yield! valParam
                        yield! w
                        yield! p
                        +Call subWrite
                    ]
                | None -> []
        file, cp |> List.collect doParam

    let private doGenRead ci =
        let ctx, cp = ci.ctx, ci.cp
        let file, cp =
            match cp with
            | ParamIdent id::tail ->
                let sl, typ = findSymbolAndGetPtr ctx id
                if typ.raw = ctx.sysTypes.file.raw then Some sl, tail
                else None, cp
            | _ -> None, cp
        let file() = if file.IsNone then fst <| findSymbolAndGetPtr ctx (Utils.stdIdent "STDINPUTFILE")
                     else file.Value

        let doParam = function
            | ParamIdent id ->
                let valParam, typ = findSymbolAndGetPtr ctx id
                let subWrite = match typ.kind with
                               | TkOrd(OkInteger,OtUByte _) -> "READBYTE"
                               | TkOrd(OkInteger,OtSByte _) -> "READSHORTINT"
                               | TkOrd(OkInteger,OtUWord _) -> "READWORD"
                               | TkOrd(OkInteger,OtSWord _) -> "READSMALLINT"
                               | TkOrd(OkInteger,OtULong _) -> "READINT"
                               | TkOrd(OkInteger,OtSLong _) -> "READINT"
                               | TkOrd(OkBool,_) -> "READBOOLEAN"
                               | TkOrd(OkChar,_) -> "READCH"
                               | TkFloat _ -> "READREAL"
                               | TkArray(AkSString _,_,_) -> "READSTRING"
                               |> ctx.FindMethodReference
                [
                    yield! file()
                    +Ldnull
                    yield! valParam
                    +Call subWrite
                ]
            | _ -> failwith "IE"
        file, cp |> List.collect doParam

    type CallInfo = { ctx: Ctx; ident: DIdent; cp: CallParam list; popResult: bool }
    with
        member self.InExpr = not self.popResult
        member self.InBlock = self.popResult

    let private doWrite ci =
        let _, writeParams = doGenWrite ci
        (writeParams, None)

    let private doWriteLn ci =
        let file, writeParams = doGenWrite ci
        ([
            yield! writeParams
            yield! file()
            +Ldnull
            +Call(ci.ctx.FindMethodReference "WRITENEWLINE")
         ], None)

    let private doRead ci =
        let _, readParams = doGenRead ci
        (readParams, None)

    let private doReadLn ci =
        let file, readParams = doGenRead ci
        ([
            yield! readParams
            yield! file()
            +Ldnull
            +Call(ci.ctx.FindMethodReference "READNEWLINE")
         ], None)

    let private doReadLine ci =
        let ctx, cp = ci.ctx, ci.cp
        let high = ref 0
        let cparams,str = cp |> List.mapi (fun i p -> incr high; callParamToIl ctx p None, sprintf "{%d}" i) |> List.unzip
        let str = String.Concat(str)
        ([
            +Ldstr str
            +Ldc_I4 !high
            +Newarr ctx.sysTypes.net_obj.DefOrRef
            yield! cparams
                   |> List.mapi (
                       fun idx (i, t) ->
                           let putArrayElem i elems =
                               +Dup::+Ldc_I4 idx::i @
                               [yield! elems ; +Stelem Elem_Ref]
                           match t with
                           | StrType ->
                               [+Call ctx.sysProc.PtrToStringAnsi]
                               // TODO critical handle ptr to strings! bug found in .NET 32 bit
                               |> putArrayElem (match ilToAtom i with | [Ldsfld f] -> [+Ldsflda f] | _ -> i)
                           | ChrType ->
                               [
                                   +Conv Conv_U1
                                   +Call ctx.sysProc.ConvertU1ToChar
                                   +Box ctx.sysTypes.char.DefOrRef
                               ]
                               |> putArrayElem i
                           | _ -> [+Box t.DefOrRef] |> putArrayElem i
                       )
                   |> List.concat
            +Call ctx.sysProc.WriteLine
         ], None)
        
    type private LoopLabels =
        | ContinueLabel
        | BreakLabel
    with
        override self.ToString() =
            match self with
            | ContinueLabel -> "Continue"
            | BreakLabel -> "Break"

        member self.GetLabel (continueLabel, breakLabel) =
            match self with
            | ContinueLabel -> continueLabel
            | BreakLabel -> breakLabel

    let private doLoopBranch (label: LoopLabels) ci =
        ParamsBuilder(ci, None) {
            noParams (
                match ci.ctx.loop.TryPeek() with
                | true, labels -> [IlBranch(IlBr, label.GetLabel labels)]
                | _ ->
                    ``Error: %O intrinsic cannot be used outside loop`` label
                    |> ci.ctx.NewMsg ci.ident
                    []
            )}

    // TODO handle Exit(result);
    let private doExit ci = ParamsBuilder(ci, None) { noParams [+.Ret] }

    type DeltaKind =
        | NegativeDelta
        | PositiveDelta

    let deltaToIl dInst delta =
        [
            match dInst with
            | Some inst ->
                yield! inst
                match delta with
                | PositiveDelta -> ()
                | NegativeDelta -> +NegInst
            | None ->
                match delta with
                | PositiveDelta -> +Ldc_I4 +1 // TODO ? Convert to float id needed ?
                | NegativeDelta -> +Ldc_I4 -1 // TODO ? Convert to float id needed ?
        ]

    let private deltaModify delta ci =
        let variableIsFloat = ref false
        let variableType: PasType ref = ref ci.ctx.sysTypes.unknown
        let pb = ParamsBuilder(ci, if ci.InBlock then None else Some ci.ctx.sysTypes.unknown)
        let handleDelta valueIls = [
                let t = !variableType
                let indKind = t.IndKind
                +Ldind indKind
                yield! deltaToIl valueIls delta
                +AddInst
                +Stind indKind
                if ci.popResult = false then
                    +Ldind indKind
                    pb.ReturnType <- Some t
            ]            
        pb {
            parameter into variable
            doParameter ( variable { byRef; float; var; OR; byRef; ord; var } ) [
                variableIsFloat := variable.Typ.IsFloat
                variableType := variable.Type
                yield! variable.Instructions
                if ci.popResult = false then +Dup
                +Dup
            ]
            optional into deltaValue
            doOptional
                (deltaValue { IF !variableIsFloat; float; OR; ord })
                (handleDelta (Some deltaValue.Instructions))
                (handleDelta None)
        }

    let private deltaAdd delta ci =
        let variableIsFloat = ref false
        let pb = ParamsBuilder(ci, if ci.InBlock then None else Some ci.ctx.sysTypes.unknown)
        pb {
            inExpr
            parameter into value
            doParameter ( value { float; OR; ord }) (
                    variableIsFloat := value.Typ.IsFloat
                    pb.ReturnType <- Some value.Type
                    value.Instructions
                )
            optional into deltaValue
            doOptional
                (deltaValue { IF !variableIsFloat; float; OR; ord })
                (deltaToIl (Some deltaValue.Instructions) delta)
                (deltaToIl None delta)
            doIl [+AddInst]
        }

    let private doNew ci =
        let pb = ParamsBuilder(ci, Some ci.ctx.sysTypes.unknown)
        pb {
            parameter into ident // TODO warning for untyped "Pointer"
            doParameter ( ident { inBlock; byRef; ptr; var; OR; inExpr; ptr; pasType } ) [
                match ident.Modifier, ident.Type with
                | VarIdent, {kind=TkPointer pt} -> // TODO more complicated New like New(foo.x^.x.z)
                    pb.ReturnType <- None // TODO allow return pointer result for New(varId) (forbridden in FPC)
                    yield! ident.Instructions
                    +Ldc_I4 pt.SizeOf
                    +Call ci.ctx.sysProc.GetMem
                    +Dup
                    +Initobj pt.DefOrRef
                    +Stind Ind_U
                | PasType, ({kind=TkPointer pt} as t) ->
                    pb.ReturnType <- Some t
                    +Ldc_I4 pt.SizeOf
                    +Call ci.ctx.sysProc.GetMem
                    +Dup
                    +Initobj pt.DefOrRef
                | _ -> raise (InternalError "2020080200")
            ]}
    
    let private doDispose ci =
        ParamsBuilder(ci, None) {
            inBlock
            parameter into ident
            doParameter ( ident { ptr; var } ) [yield! ident.Instructions; +Call ci.ctx.sysProc.FreeMem]
        }
    
    // TODO errorcode global variable
    let private doHalt ci =
        let exitProcess = +Call(ci.ctx.FindMethodReference "EXITPROCESS")
        ParamsBuilder(ci, None) {
            inBlock
            optional into param
            doOptional (param { int }) [yield! param.Instructions; exitProcess] [+Ldc_I4 0; exitProcess]
        }

    let private doHaltAtLineProc ci =
        ParamsBuilder(ci, None) {
            inBlock
            noParams [
                    let exitCode = int (ci.ctx.messages.PosMap.[ci.ident.BoxPos]).Line
                    +Ldc_I4 exitCode
                    +Call(ci.ctx.FindMethodReference "EXITPROCESS") // +Call ctx.sysProc.Exit
                ]
        }

    // TODO: warnings for const expressions > 255 or < 0
    let private doChr ci =
        ParamsBuilder(ci, Some ci.ctx.sysTypes.char) {
            inExpr
            parameter into param
            doParameter (param { int }) [ yield! param.Instructions; +Conv Conv_U1 ]
        }

    let private doOrd ci =
        let pb = ParamsBuilder(ci, Some ci.ctx.sysTypes.int32)
        pb {
            inExpr
            parameter into param
            doParameter (param { ord })
                [
                    yield! param.Instructions
                    match param.Type with
                    | Ord64Type ->
                        pb.ReturnType <- Some ci.ctx.sysTypes.int64
                        +Conv Conv_I8
                    | _ -> +Conv Conv_I4
                ]
        }

    let private callFloatFunToInt64 f ci =
        ParamsBuilder(ci, Some ci.ctx.sysTypes.int64) {
            inExpr
            parameter into param
            doParameter (param { float; OR; int })
                [
                    yield! param.Instructions
                    if (f: IMethod voption).IsSome then +Call f.Value
                    +Conv Conv_I8
                ]
        }

    let private doTrunc = callFloatFunToInt64 ValueNone
    let private doRound ci = callFloatFunToInt64 (ValueSome ci.ctx.sysProc.Round) ci
                
    let private doSizeOf (ci: CallInfo) =
        ParamsBuilder(ci, Some ci.ctx.sysTypes.int32) {
            inExpr
            parameter into ident
            doParameter (ident { pasType; OR; varType }) [+Ldc_I4 ident.Type.SizeOf]
        }

    let handleIntrinsic intrinsicSym =
        match intrinsicSym with
        | IncProc -> deltaModify PositiveDelta
        | DecProc -> deltaModify NegativeDelta
        | SuccProc -> deltaAdd PositiveDelta
        | PredProc -> deltaAdd NegativeDelta
        | ContinueProc -> doLoopBranch ContinueLabel
        | BreakProc -> doLoopBranch BreakLabel
        | ExitProc -> doExit
        | WriteProc -> doWrite
        | WriteLnProc -> doWriteLn
        | ReadProc -> doRead
        | ReadLnProc -> doReadLn
        | WriteLineProc -> doReadLine
        | NewProc -> doNew
        | DisposeProc -> doDispose
        | HaltProc -> doHalt
        | HaltAtLineProc -> doHaltAtLineProc
        | ChrFunc -> doChr
        | OrdFunc -> doOrd
        | TruncFunc -> doTrunc
        | RoundFunc -> doRound
        | SizeOfFunc -> doSizeOf
        | _ -> raise (InternalError "2020063001")