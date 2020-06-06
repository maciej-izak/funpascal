namespace rec Pas

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open Mono.Cecil
open Mono.Cecil.Cil

module Utils =
    let stdIdent = Ident >> List.singleton >> DIdent

    let addMetaType (symbols: Dictionary<_,_>) (name: TypeName) typ =
        match name with
        | AnonName -> () // ie for char sets
        | _ -> symbols.Add(name, TypeSym typ)
        typ

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
        errors: List<string>
        variables: List<VariableKind> list
        labels: Dictionary<string, BranchLabel ref> list
        symbols: (SymOwner * Dictionary<TypeName, Symbol>) list
        forward: Dictionary<string, ReferencedDef * Dictionary<TypeName,Symbol> * VariableKind option>
        localVariables: int ref
        lang: Ctx.LangCtx
        res: List<MetaInstruction>
        details: Ctx.ModuleDetails
        loop: Stack<BranchLabel ref * BranchLabel ref>
        enumSet: Dictionary<PasType, PasType>
        posMap: Dictionary<obj, FParsec.Position>
        sysTypes: Ctx.SystemTypes
        sysProc: Ctx.SystemProc
    } with

    member inline self.NewError(pos: ^T) s =
        let fmtPos (pos: FParsec.Position) = sprintf "[Error] %s(%d,%d)" (pos.StreamName) (pos.Line) (pos.Column)
        let p = (^T : (member BoxPos : obj) pos)
        self.errors.Add(fmtPos (self.posMap.[p]) + " " + s)

    static member Create = Ctx.createCtx

    member self.NewSymbols = snd self.symbols.Head
    member self.SymOwner = fst self.symbols.Head

    member self.Inner symbolsEntry =
        { self with
            symbols = symbolsEntry::self.symbols
            localVariables = ref 0
            variables = List<_>()::self.variables
            labels = Dictionary<_,_>()::self.labels
            res = List<MetaInstruction>()
            loop = Stack<_>()}

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
                        let pd = ParameterDefinition(paramName, ParameterAttributes.None, ByReferenceType(t.raw))
                        let newSym = VariableSym(ParamVariable(RefVar, pd), t)
                        symbols.Add(StringName paramName, newSym)
                        originMd.raw.Parameters.Add pd
                        nestedParams := (sym, newSym)::!nestedParams
                        Some newSym
                    | Some(_, newSym) -> Some newSym
                else
                    Some sym
            | _ -> Some sym
        | Some(_, sym) -> Some sym
        | _ -> None

    member self.FindType sym = match self.PickSym sym with | Some(_,TypeSym ts) -> Some ts | _ -> None

    member self.FindTypeId = TypeName.FromTypeId >> self.FindType

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
        let varDef = varType.raw |> VariableDefinition
        let varKind = varDef |> LocalVariable
        self.res.Add(DeclareLocal(varDef))
        self.variables.Head.Add(varKind)
        (snd self.symbols.Head).Add(StringName key, VariableSym(varKind, varType))
        (key, varDef)

    member self.FindLabel name =
        self.labels |> List.tryPick (fun l -> match l.TryGetValue name with | true, bl-> Some bl | _ -> None)

    member self.FindLabelUnsafe name =
        match self.FindLabel name with
        | Some bl -> bl
        | _ -> failwithf "IE cannot find label %s" name

    // Types module
    member self.AddTypeSetForEnum = TypesDef.addTypeSetForEnum self
    member self.AddTypeSet = TypesDef.addTypeSet self
    member self.AddType = TypesDef.addType self
    member self.AddTypePointer = TypesDef.addTypePointer self
    member self.GetInternalType = TypesDef.getInternalType self
    member self.AddTypeArray = TypesDef.addTypeArray self

    // SymSearch module
    member self.FindSymbol = SymSearch.findSymbol self
    member self.FindMethodReferenceOpt =
        Utils.stdIdent >> self.FindSymbol >> chainToSLList >>
        function | [CallableLoad(Referenced({raw=mr}, _))], _ -> Some mr | _ -> None
    member self.FindMethodReferenceUnsafe = self.FindMethodReferenceOpt >> function | Some r -> r | _ -> null
    member self.FindMethodReference = self.FindMethodReferenceOpt >> function | Some r -> r | _ -> failwith "IE"
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

        interface IEqualityComparer<TypeName> with
            member _.GetHashCode(x) = ec.GetHashCode(x)
            member _.Equals(x,y) = ec.Equals(x, y)

    type ModuleDetails = {
            typesCount: int ref
            moduleBuilder: ModuleDefinition
            ns: string
            tb: TypeDefinition
            vt: TypeReference
            uvt: MethodReference
            anonSizeTypes: Dictionary<int, TypeDefinition>
        } with
        static member Create moduleBuilder ns tb =
            {
                typesCount = ref 0
                moduleBuilder = moduleBuilder
                ns = ns
                tb = tb
                vt = moduleBuilder.ImportReference(typeof<ValueType>)
                uvt = moduleBuilder.ImportReference(typeof<UnsafeValueTypeAttribute>.GetConstructor(Type.EmptyTypes))
                anonSizeTypes = Dictionary<_, _>()
            }

        member self.UniqueTypeName() =
            incr self.typesCount
            "T" + string !self.typesCount

        member self.NewSizedType size =
                let attributes = TypeAttributes.SequentialLayout ||| TypeAttributes.AnsiClass ||| TypeAttributes.Sealed ||| TypeAttributes.Public
                let at = TypeDefinition(self.ns, self.UniqueTypeName(), attributes)

                at.CustomAttributes.Add(CustomAttribute self.uvt)
                at.ClassSize <- size
                at.PackingSize <- 1s;
                at.BaseType <- self.vt
                self.moduleBuilder.Types.Add(at)
                at

        member self.SelectAnonSizeType size =
            match self.anonSizeTypes.TryGetValue size with
            | true, t -> t
            | _ ->
                let at = self.NewSizedType size
                self.anonSizeTypes.Add(size, at)
                at

        member self.AddBytesConst (bytes: byte[]) =
            let ast = self.SelectAnonSizeType bytes.Length
            let fd = FieldDefinition(null, FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.HasFieldRVA, ast)
            fd.InitialValue <- bytes
            self.tb.Fields.Add fd
            fd

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
        GetMem: MethodReference
        FreeMem: MethodReference
        WriteLine: MethodReference
        Exit: MethodReference
        ConvertU1ToChar: MethodReference
        PtrToStringAnsi: MethodReference
        Round: MethodReference
    }

    let private createSystemTypes details (symbols: Dictionary<TypeName, Symbol>) =
        let mb = details.moduleBuilder
        let vt = mb.ImportReference(typeof<ValueType>)
        let ot = mb.ImportReference(typeof<obj>)
        let addAnyType name raw kind =
            let t = {name=name;raw=raw;kind=kind}
            Utils.addMetaType symbols name t
        let addOrdType name raw ordKind ordType = TkOrd(ordKind, ordType) |> addAnyType (StringName name) raw
        let addFloatType name raw = TkFloat(FtSingle) |> addAnyType (StringName name) raw
        let addArrayType name (p: PasType) ad = addAnyType name p.raw ad

        addOrdType "Byte" mb.TypeSystem.Byte OkInteger (OtUByte(int Byte.MinValue, int Byte.MaxValue)) |> ignore
        addOrdType "ShortInt" mb.TypeSystem.SByte OkInteger (OtSByte(int SByte.MinValue, int SByte.MaxValue)) |> ignore
        addOrdType "Word" mb.TypeSystem.UInt16 OkInteger (OtUWord(int UInt16.MinValue, int UInt16.MaxValue)) |> ignore
        addOrdType "SmallInt" mb.TypeSystem.Int16 OkInteger (OtSWord(int Int16.MinValue, int Int16.MaxValue)) |> ignore
        addOrdType "LongWord" mb.TypeSystem.UInt32 OkInteger (OtULong(int UInt32.MinValue, int UInt32.MaxValue)) |> ignore
        addOrdType "UInt64" mb.TypeSystem.UInt64 OkInteger (OtUQWord(UInt64.MinValue, UInt64.MaxValue)) |> ignore
        let charType = addOrdType "Char" mb.TypeSystem.Byte OkChar (OtUByte(int Byte.MinValue, int Byte.MaxValue))
        let strType = {name=AnonName;raw=(details.NewSizedType 256) :> TypeReference;kind=TkUnknown 256}
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
        let fileType = (details.NewSizedType (256 + ptrSize)) :> TypeReference
        let fileType = Utils.addMetaType symbols (TypedName TIdFile) {name=TypedName TIdFile;kind=TkUnknown(256 + ptrSize);raw=fileType}
        {
            int32 = addOrdType "Integer" mb.TypeSystem.Int32 OkInteger (OtSLong(int Int32.MinValue, int Int32.MaxValue))
            int64 = addOrdType "Int64" mb.TypeSystem.Int64 OkInteger (OtSQWord(Int64.MinValue, Int64.MaxValue))
            single = addFloatType "Real" mb.TypeSystem.Single
            string = addArrayType (TypedName TIdString) strType (TkArray(AkSString 255uy, [strDim], charType))
            setStorage = {name=AnonName;raw=details.NewSizedType 256;kind=TkUnknown 0}
            char = charType
            file = fileType
            value = {name=AnonName;raw=vt;kind=TkUnknown 0}
            pointer = addAnyType (StringName "Pointer") (PointerType mb.TypeSystem.Void) (TkPointer({name=AnonName;raw=mb.TypeSystem.Void;kind=TkUnknown 0}))
            unit = {name=AnonName;raw=mb.TypeSystem.Void;kind=TkUnknown 0}
            unknown = {name=ErrorName;raw=mb.TypeSystem.Void;kind=TkUnknown 0}
            constParam = {name=AnonName;raw=PointerType(mb.TypeSystem.Void);kind=TkUnknown 0}
            varParam = {name=AnonName;raw=PointerType(mb.TypeSystem.Void);kind=TkUnknown 0}
            boolean = addOrdType "Boolean" mb.TypeSystem.Byte OkBool (OtUByte(0, 1))
            net_obj = {name=AnonName;raw=ot;kind=TkUnknown 0}
            net_void = {name=AnonName;raw=mb.TypeSystem.Void;kind=TkUnknown 0}
        }

    let private createSystemProc details =
        let mb = details.moduleBuilder
//        let mathTrunc = typeof<System.MathF>.GetMethod("Truncate", [| typeof<single> |])  |> mb.ImportReference
        {
            GetMem = typeof<System.Runtime.InteropServices.Marshal>.GetMethod("AllocCoTaskMem")  |> mb.ImportReference
            FreeMem = typeof<System.Runtime.InteropServices.Marshal>.GetMethod("FreeCoTaskMem")  |> mb.ImportReference
            WriteLine = typeof<System.Console>.GetMethod("WriteLine", [| typeof<string> ; typeof<obj array> |]) |> mb.ImportReference
            Exit = typeof<System.Environment>.GetMethod("Exit", [| typeof<int> |]) |> mb.ImportReference
            ConvertU1ToChar = typeof<System.Convert>.GetMethod("ToChar", [| typeof<byte> |]) |> mb.ImportReference
            PtrToStringAnsi = typeof<System.Runtime.InteropServices.Marshal>.GetMethod("PtrToStringAnsi", [| typeof<nativeint> |]) |> mb.ImportReference
            Round = typeof<System.MathF>.GetMethod("Round", [| typeof<single> |])  |> mb.ImportReference
        }

    let private addSystemRoutines ctx =
        let symbols = snd ctx.symbols.Head
        let tsingle = ctx.sysTypes.single
        symbols.Add(StringName "true", ConstBool 0xFFuy |> ConstSym)
        symbols.Add(StringName "false", ConstBool 0uy |> ConstSym)
        //newSymbols.Add("GetMem", Referenced allocMem |> MethodSym)
        //newSymbols.Add("FreeMem", Referenced freeMem |> MethodSym)
        let intrinsic name i = symbols.Add(StringName name, Intrinsic i |> MethodSym)
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
               paramList = [|{typ=tsingle;ref=false}|]
               result = Some tsingle
               raw = raw
           }, ref[]) |> MethodSym
        let mathLog = typeof<System.MathF>.GetMethod("Log", [| typeof<single> |])  |> ctx.details.moduleBuilder.ImportReference
        let mathExp = typeof<System.MathF>.GetMethod("Exp", [| typeof<single> |])  |> ctx.details.moduleBuilder.ImportReference
        symbols.Add(StringName "Exp", singleScalar mathExp)
        symbols.Add(StringName "Ln", singleScalar mathLog)
        ctx

    let createCtx moduleBuilder ns tb owner pm =
        let lang = LangCtx()
        let details = ModuleDetails.Create moduleBuilder ns tb
        let symbols = Dictionary<TypeName,Symbol>(lang)
        {
            errors = List<_>()
            variables = [List<VariableKind>()]
            labels = [Dictionary<_,_>()]
            symbols = [owner,symbols]
            forward = Dictionary<_, _>()
            localVariables = ref 0
            lang = LangCtx()
            res = List<MetaInstruction>()
            details = details
            loop = Stack<_>()
            enumSet = Dictionary<_, _>()
            posMap = pm
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
            | Some _ -> failwith "IE illegal type for set construction"
            | _ -> failwith "IE unknown type"
        addTypeSetForEnum ctx t name

    let addType ctx = Utils.addMetaType (snd ctx.symbols.Head)

    let addTypePointer (ctx: Ctx) count typeName name =
        let t =
            match ctx.FindType typeName with
            | Some t -> t
            | _ -> failwith "IE unknown type"
        let mutable pt = PointerType(t.raw)
        for i = 2 to count do pt <- PointerType(pt)
        addType ctx name {name=name;kind=TkPointer(t);raw=pt:>TypeReference}

    let getInternalType (ctx: Ctx) t =
        let name = TypeName.FromTypeId t
        match ctx.FindType name with
        | Some t -> t
        | None -> // inline type?
            match t with
            | TIdPointer(count, typeId) -> addTypePointer ctx count (TypeName.FromTypeId typeId) (TypedName t)
            | TIdArray(ArrayDef(_, dimensions, tname)) -> addTypeArray ctx dimensions tname (TypedName t)
            | TIdSet(_, typeId) -> addTypeSet ctx (TypeName.FromTypeId typeId) (TypedName t)
            | _ -> ctx.NewError t (sprintf "Cannot find type identifier \"%O\"" t); ctx.sysTypes.unknown

    let addTypeArray ctx dimensions tname name =
        let newSubType (dims, size) (typ: PasType, typSize) name =
            let size = size * typSize
            let at = ctx.details.NewSizedType size
            FieldDefinition("item0", FieldAttributes.Public, typ.raw)
            |> at.Fields.Add
            let name = StringName name
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

module SymSearch =

    let findSymbol (ctx: Ctx) (DIdent ident as dident) =
        let mainSym = ident.Head |> function | Ident n -> ctx.FindSym(StringName n)

        let rec findSym ref acc = function
        | (Designator.Array a)::t -> acc, Designator.Array(a)::t // TODO ?
        | (Designator.Deref)::t -> acc, Designator.Deref::t // TODO ?
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
                | Deref ->
                    // TODO check dereferencable
                    let tref = match vt.kind with | TkPointer t -> t | _ -> failwithf "IE cannot do deref of %A" vt.kind
                    resolveTail (DerefLoad::acc) tref t
                | Ident _ ->
                    let sl, restOfTail = findSym vt [] ht
                    let typ = snd sl.Head
                    resolveTail (StructLoad(List.rev sl)::acc) typ restOfTail
                | Designator.Array exprs ->
                    let tref = match vt.kind with | TkArray a -> a | _ -> failwithf "IE array expected %A" vt.kind
                    let _, dims, elemTyp = tref
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

        let varLoadChain vt dlist vd =
            let (tail, finalType) = resolveTail [] vt dlist
            Ok(VariableLoad(vd)::tail, Some finalType)

        let mainSym = defaultArg mainSym UnknownSym

        match mainSym with
        | VariableSym (v, t as vt) -> vt |> varLoadChain t ident.Tail
        | WithSym (v, t as vt) -> vt |> varLoadChain t ident
        | EnumValueSym(i, t) when ident.Tail = [] -> Ok([ValueLoad(ValueInt(i))], Some t)
        | MethodSym m ->  Ok([CallableLoad m], m.ReturnType)
        | ConstSym v ->
            match v with
            | ConstInt i -> Ok([ValueLoad(ValueInt i)], Some ctx.sysTypes.int32)
            | ConstFloat f -> Ok([ValueLoad(ValueFloat f)], Some ctx.sysTypes.single)
            | ConstValue(fd, pt) -> (GlobalVariable fd, pt) |> varLoadChain pt ident.Tail
            | ConstBool b ->
                let i = int b
                Ok([ValueLoad(ValueInt i)], Some ctx.sysTypes.boolean)
            | _ -> failwith "IE"
        | TypeSym t -> Ok([TypeCastLoad t], Some t)
        | _ ->
            ctx.NewError dident (sprintf "Cannot find symbol '%O'" dident)
            Error()

    type FoundFunction =
        | RealFunction of (MethodSym * IlInstruction option)
        | TypeCast of PasType

    let findFunction (ctx: Ctx) ident =
            let callChain = findSymbol ctx ident
            // TODO more advanced calls like foo().x().z^ := 10
            match callChain with
            | Ok([CallableLoad cl], _) ->
               match cl with
               | Referenced ({raw=mr}, _) -> RealFunction(cl, +Call(mr) |> Some)
               | Intrinsic _ -> RealFunction(cl, None)
            | Ok([TypeCastLoad t], _) -> TypeCast t
            | _ -> failwith "Not supported"

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
                | _ -> [yield! aVal; yield! bVal; typeRefToConv aTyp.raw],aTyp,bTyp

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
                    | Some NumericType -> yield typeRefToConv et.Value.raw
                    | _ -> ()
                ], typ)
            let add2OpIl a b i = add2OpIlTyped a b i et
            let add2OpIlBool a b i = add2OpIlTyped a b i (Some ctx.sysTypes.boolean)
            let inline add1OpIl a i =
                let a, at = exprToMetaExpr a et false
                [ yield! a; yield +i; yield typeRefToConv at.raw], at
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
            match typ.kind with | TkOrd _ | TkFloat _ -> yield typeRefToConv typ.raw | _ -> ()
        ], at

    let exprToIl = exprToIlGen false

    let callParamToIl ctx cp (idxmr: Option<int * MethodInfo>) =
        let param, byRef =
            match idxmr with
            | Some(idx,{paramList=pl}) ->
                let p = pl.[idx]
                Some p.typ, p.ref
            | _ -> None, false
        match cp with
        | ParamExpr expr -> exprToIl ctx (if byRef then Addr expr else expr) param
        | ParamIdent id ->
            let useRef = byRef || (param.IsSome && (param.Value = ctx.sysTypes.constParam || param.Value = ctx.sysTypes.varParam))
            if useRef then findSymbolAndGetPtr ctx id
            else findSymbolAndLoad ctx id
        |> fun(il, t) ->
            if param.IsSome then
                if not(Utils.typeCheck ctx param.Value t) then
                    ctx.NewError cp (sprintf "Incompatible types ('%O' and '%O') for %d parameter" param.Value.name t.name (fst idxmr.Value))
            (il, t)

    let doCall (ctx: Ctx) (CallExpr(ident, cp)) popResult =
        match ctx.FindFunction ident with
        | SymSearch.TypeCast t ->
            let cp = match cp with | [cp] -> cp | _ -> failwith "IE only one param allowed"
            let callInstr, typ = callParamToIl ctx cp None
            // TODO some real conversion ? Conv_I4 + explicit operators?
            ([
                yield! callInstr
                typeRefToConv t.raw
            ], Some t)
        | SymSearch.RealFunction rf ->
            match rf with
            | Referenced(mr,np), Some f ->
                ([
                    yield! cp
                    |> List.mapi (fun i p -> fst <| callParamToIl ctx p (Some(i, mr)))
                    |> List.concat
                    // TODO recursive call nested! For now before nested routine is added call can be generated
                    yield! !np
                    |> List.map (fun (VariableSymLoad vlt) -> loadSymList ctx false true vlt |> fst)
                    |> List.concat
                    yield f
                    // TODO error/IE ? for popResult && mr.result.IsNone
                    if popResult && mr.result.IsSome then
                        yield +Pop
                 ], mr.result)
            | Intrinsic i, _ -> handleIntrinsic i {ctx=ctx;ident=ident;cp=cp;popResult=popResult}
            | _ -> failwith "IE"
        | _ -> failwith "IE"

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
        | VCallResult(ce), _ ->
            match doCall ctx ce false with
            | ils, Some t -> ils, t
            | _ -> failwith "IE"
        | VNil, _ -> [+Ldnull], ctx.sysTypes.pointer
        | VSet al, _ ->
            let bytes, ft = match setValueToCER ctx expectedType al with
                            | CEROrdSet(b,t) -> b, t
                            | _ -> failwith "IE"
            let fd = ctx.details.AddBytesConst bytes
            [+(if byRef then Ldsflda fd else Ldsfld fd)], ft

    let chainReaderFactory (ctx: Ctx) asValue addr ltp =
        let valOrPtr v p (t: TypeReference) = (if asValue || (t :? PointerType && addr = false) then v else p) |> List.singleton
        match ltp with
        | LTPVar(v, vt) -> match v with
                           | LocalVariable v -> valOrPtr +(Ldloc v) +(Ldloca v) vt.raw
                           | GlobalVariable v -> valOrPtr +(Ldsfld v) +(Ldsflda v) vt.raw
                           | ParamVariable(r, v) -> // handle by ref params
                             match r with
                             | RefNone | RefConst -> valOrPtr +(Ldarg v) +(Ldarga v) vt.raw
                             | RefVar ->
                                 [
                                    +Ldarg v
                                    if asValue then
                                        +Ldobj vt.raw
                                 ]
                             | RefUntypedVar | RefUntypedConst ->
                                 if asValue then failwith "IE"
                                 [+Ldarg v]
        | LTPStruct (fld, _) -> valOrPtr +(Ldfld fld) +(Ldflda fld) fld.FieldType
        | LTPDeref (dt, force) ->
            match dt, addr, force, asValue with
            | _, true, _, _ -> []
            | NumericType, _, _, _ | ChrType, _, _, _ -> dt.IndKind |> Ldind |> (~+) |> List.singleton
            | _, _, true, _ | _, _, _, true ->
                if dt.raw.MetadataType <> MetadataType.ValueType then failwith "IE"
                [+Ldobj dt.raw]
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
            if dt.raw.MetadataType = MetadataType.ValueType then
                [+Stobj dt.raw]
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
        ctx.FindSymbol ident
        |> chainToSLList
        |> loadSymList ctx value addr

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

    let (|VariablePasType|_|) (ctx: Ctx) id =
        match ctx.FindSymbol id |> chainToSLList with
        | _, Some(t) -> Some(VariablePasType t)
        | _ -> None

    let (|TypePasType|_|) (ctx: Ctx) = function
        | DIdent[Ident(id)] ->
            match ctx.FindSym(StringName id) with
            | Some (TypeSym t) -> Some(TypePasType t)
            | _ -> None
        | _ -> None

    let private doWrite ctx cp =
        let file, cp =
            match cp with
            | ParamIdent id::tail ->
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
                | _ -> ctx.NewError cp (sprintf "Unknown type kind of expression for Write/WriteLn: %O" cp); ""
                |> ctx.FindMethodReferenceOpt |>
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

    let private doRead ctx cp =
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

    let (|FloatOrOrdParam|_|) ci =
        match ci.cp with
        | cp::t ->
            let inst, typ = callParamToIl ci.ctx cp None
            match typ with
            // TODO ? Convert to float id needed ?
            | FloatType | IntType -> Some(inst, (typ, {ci with cp=t}))
            | _ ->
                ci.ctx.NewError cp (sprintf "Expected ordinal or float type but '%O' found" typ.name)
                None
        | [] ->
            ci.ctx.NewError ci.ident ("Expected ordinal or float type parameter but nothing found")
            None

    let (|FloatOrOrdParamIdent|_|) ci =
        match ci.cp with
        | ((ParamIdent id) as cp)::t ->
            // TODO : IDENT NOT FOUND?
            let inst, typ = findSymbolAndGetPtr ci.ctx id
            match typ with
            // TODO ? Convert to float id needed ?
            | FloatType | IntType -> Some(inst, (typ, {ci with cp=t}))
            | _ ->
                ci.ctx.NewError cp (sprintf "Expected ordinal or float type but '%O' found" typ.name)
                None
        | cp::_ ->
            ci.ctx.NewError cp ("Expected ident parameter but expression found")
            None
        | [] ->
            ci.ctx.NewError ci.ident ("Expected ident parameter but nothing found")
            None

    let (|EofOptFloatOrOrdParamValue|_|) (expectedType, ci) =
        match ci.cp with
        | cp::t ->
            let inst, typ = callParamToIl ci.ctx cp None
            match expectedType, typ, t with
            // TODO ? Convert to float id needed ?
            | FloatType, FloatOrIntType, [] -> Some(expectedType, Some inst)
            | IntType, IntType, [] -> Some(expectedType, Some inst)
            | FloatType, _, [] ->
                ci.ctx.NewError cp (sprintf "Expected integer or float type but '%O' found" typ.name)
                None
            | IntType, _, [] ->
                ci.ctx.NewError cp (sprintf "Expected integer type but '%O' found" typ.name)
                None
            | _, _, cp::_ ->
                ci.ctx.NewError cp "Unexpected parameter"
                None
        | [] -> Some(expectedType, None)

    let (|EofParam|_|) (_,ci) =
        match ci.cp with
        | [] -> Some()
        | cp::_ -> ci.ctx.NewError cp "Unexpected parameter"; None

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
        match ci with
        | FloatOrOrdParamIdent(inst, EofOptFloatOrOrdParamValue(typ, dInst)) ->
            let indKind = typ.IndKind
            ([
                yield! inst
                if ci.popResult = false then +Dup
                +Dup
                +Ldind indKind
                yield! deltaToIl dInst delta
                +AddInst
                +Stind indKind
                if ci.popResult = false then +Ldind indKind
            ], None)
        | _ -> ([], None)

    let private deltaAdd delta ci =
        match ci with
        | FloatOrOrdParam(inst, EofOptFloatOrOrdParamValue(typ, dInst)) ->
            ([
                 yield! inst
                 yield! deltaToIl dInst delta
                 +AddInst
            ], Some typ)
        | _ -> ([], None)

    let private callFloatFunToInt64 f ci =
        let ctx = ci.ctx
        match ci with
        | FloatOrOrdParam(ils,EofParam) ->
            ([
                yield! ils
                if (f: MethodReference voption).IsSome then +Call f.Value
                match ci.popResult with
                | true  -> +Pop
                | false -> +Conv Conv_I8
             ], Some ctx.sysTypes.int64)
        | _ -> ([], Some ctx.sysTypes.int64)

    let handleIntrinsic intrinsicSym ci =
        let ctx = ci.ctx
        let popResult = ci.popResult
        match (intrinsicSym, ci.cp) with
        | IncProc, _ -> deltaModify PositiveDelta ci
        | DecProc, _ -> deltaModify NegativeDelta ci
        | SuccProc, _ -> deltaAdd PositiveDelta ci
        | PredProc, _ -> deltaAdd NegativeDelta ci
        | ContinueProc, [] -> match ctx.loop.TryPeek() with
                              | true, (continueLabel, _) ->
                                ([IlBranch(IlBr, continueLabel)], None)
                              | _ -> failwith "IE"
        | BreakProc, [] -> match ctx.loop.TryPeek() with
                           | true, (_, breakLabel) ->
                            ([IlBranch(IlBr, breakLabel)], None)
                           | _ -> failwith "IE"
        | ExitProc, [] -> // TODO handle Exit(result);
            ([
                +.Ret
             ], None)
        | WriteProc, cp ->
            let _, writeParams = doWrite ctx cp
            (writeParams, None)
        | WriteLnProc, cp ->
            let file, writeParams = doWrite ctx cp
            ([
                yield! writeParams
                yield! file()
                +Ldnull
                +Call(ctx.FindMethodReference "WRITENEWLINE")
             ], None)
        | ReadProc, cp ->
            let _, readParams = doRead ctx cp
            (readParams, None)
        | ReadLnProc, cp ->
            let file, readParams = doRead ctx cp
            ([
                yield! readParams
                yield! file()
                +Ldnull
                +Call(ctx.FindMethodReference "READNEWLINE")
             ], None)
        | WriteLineProc, cp ->
            let high = ref 0
            let cparams,str = cp |> List.mapi (fun i p -> incr high; callParamToIl ctx p None, sprintf "{%d}" i) |> List.unzip
            let str = String.Concat(str)
            ([
                +Ldstr str
                +Ldc_I4 !high
                +Newarr ctx.sysTypes.net_obj.raw
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
                                                   +Box ctx.details.moduleBuilder.TypeSystem.Char
                                               ]
                                               |> putArrayElem i
                                           | _ -> [+Box t.raw] |> putArrayElem i
                                    )
                       |> List.concat
                +Call ctx.sysProc.WriteLine
             ], None)
        | NewProc, [ParamIdent id] ->
            let ils, t = findSymbolAndGetPtr ctx id
            let t = match t.kind with
                    | TkPointer pt -> pt
                    | _ -> failwith "IE"
            ([
                yield! ils
                +Ldc_I4 t.SizeOf
                +Call ctx.sysProc.GetMem
                +Dup
                +Initobj t.raw
                +Stind Ind_U
            ], None)
        | DisposeProc, [ParamIdent id] ->
            let ils, _ = findSymbolAndLoad ctx id
            ([
                yield! ils
                +Call ctx.sysProc.FreeMem
            ], None)
        | HaltProc, cp ->
            let exitCode = match cp with
                           | [] -> [+Ldc_I4 0]
                           | [cp] -> fst <| callParamToIl ctx cp None // TODO typecheck ?
                           | _ -> failwith "IE only one param allowed"
            ([
                 yield! exitCode
                 +Call(ctx.FindMethodReference "EXITPROCESS")
                 // +Call ctx.sysProc.Exit
            ], None)
        | ChrFunc, cp ->
            let cp = match cp with | [cp] -> cp | _ -> failwith "IE only one param allowed"
            let callInstr, typ = callParamToIl ctx cp None
            ([
                yield! callInstr
                if popResult then yield +Pop // TODO or not generate call ?
             ], Some ctx.sysTypes.char)
        | OrdFunc, cp ->
            let cp = match cp with | [cp] -> cp | _ -> failwith "IE only one param allowed"
            let callInstr, typ = callParamToIl ctx cp None
            ([
                yield! callInstr
                if popResult then yield +Pop // TODO or not generate call ?
             ], Some ctx.sysTypes.int32)
        | TruncFunc, _ -> callFloatFunToInt64 ValueNone ci
        | RoundFunc, _ -> callFloatFunToInt64 (ValueSome ctx.sysProc.Round) ci
        | SizeOfFunc, [ParamIdent id] ->
            let t = match id with
                    | VariablePasType ctx t -> t
                    | TypePasType ctx t -> t
                    | _ -> failwithf "IE cannot get size of %A" id
            ([
                +Ldc_I4 t.SizeOf
                if popResult then +Pop // TODO or not generate call ?
             ], Some ctx.sysTypes.int32)
        | _ -> failwith "IE"