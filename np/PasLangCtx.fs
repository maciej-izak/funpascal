[<AutoOpen>]
module NP.PasLangCtx

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open Mono.Cecil
open Mono.Cecil.Cil

type LangCtx() =
    let mutable ec = caseInsensitive
    member self.caseSensitive
        with get() = ec = caseSensitive
        and set(cs) =  ec <- if cs then caseSensitive else caseInsensitive

    interface IEqualityComparer<TypeName> with
        member _.GetHashCode(x) = ec.GetHashCode(x)
        member _.Equals(x,y) = ec.Equals(x, y)

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
        Trunc: MethodReference
        Round: MethodReference
    }

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

let stdIdent = Ident >> List.singleton >> DIdent
let stdType  = stdIdent >> TIdIdent

let addMetaType (symbols: Dictionary<_,_>) (name: TypeName) typ =
    match name with
    | AnonName -> () // ie for char sets
    | _ -> symbols.Add(name, TypeSym typ)
    typ

type SymOwner =
    | GlobalSpace
    | StandaloneMethod of ReferencedDef
    | WithSpace

let fmtPos (pos: FParsec.Position) = sprintf "[Error] %s(%d,%d)" (pos.StreamName) (pos.Line) (pos.Column)

type Ctx = {
        errors: List<string>
        variables: List<VariableKind> list
        labels: Dictionary<string, BranchLabel ref> list
        symbols: (SymOwner * Dictionary<TypeName, Symbol>) list
        forward: Dictionary<string, ReferencedDef * Dictionary<TypeName,Symbol> * VariableKind option>
        localVariables: int ref
        lang: LangCtx
        res: List<MetaInstruction>
        details: ModuleDetails
        loop: Stack<BranchLabel ref * BranchLabel ref>
        enumSet: Dictionary<PasType, PasType>
        posMap: Dictionary<obj, FParsec.Position>
        sysTypes: SystemTypes
        sysProc: SystemProc
    } with

    member inline self.NewError(pos: ^T) s =
        let p = (^T : (member BoxPos : obj) pos)
        self.errors.Add(fmtPos (self.posMap.[p]) + " " + s)

    static member CreateStdTypes(details, (symbols: Dictionary<TypeName, Symbol>)) =
        let mb = details.moduleBuilder
        let vt = mb.ImportReference(typeof<ValueType>)
        let ot = mb.ImportReference(typeof<obj>)
        let addAnyType name raw kind =
            let t = {name=name;raw=raw;kind=kind}
            addMetaType symbols name t
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
        let fileType = addMetaType symbols (TypedName TIdFile) {name=TypedName TIdFile;kind=TkUnknown(256 + ptrSize);raw=fileType}
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

    static member Create owner symbols (lang: LangCtx) pm details =
        let moduleBuilder = details.moduleBuilder
        let exitMethod =
            typeof<System.Environment>.GetMethod("Exit", [| typeof<int> |]) |> moduleBuilder.ImportReference
        let writeLineMethod =
            typeof<System.Console>.GetMethod("WriteLine", [| typeof<string> ; typeof<obj array> |]) |> moduleBuilder.ImportReference
        let ptrToStringAnsi =
            typeof<System.Runtime.InteropServices.Marshal>.GetMethod("PtrToStringAnsi", [| typeof<nativeint> |]) |> moduleBuilder.ImportReference
        let convertU1ToChar =
            typeof<System.Convert>.GetMethod("ToChar", [| typeof<byte> |]) |> moduleBuilder.ImportReference
        let allocMem =
            typeof<System.Runtime.InteropServices.Marshal>.GetMethod("AllocCoTaskMem")  |> moduleBuilder.ImportReference
        let freeMem =
            typeof<System.Runtime.InteropServices.Marshal>.GetMethod("FreeCoTaskMem")  |> moduleBuilder.ImportReference
        let mathLog =
            typeof<System.MathF>.GetMethod("Log", [| typeof<single> |])  |> moduleBuilder.ImportReference
        let mathTrunc =
            typeof<System.MathF>.GetMethod("Truncate", [| typeof<single> |])  |> moduleBuilder.ImportReference
        let mathExp =
            typeof<System.MathF>.GetMethod("Exp", [| typeof<single> |])  |> moduleBuilder.ImportReference
        let mathRound =
            typeof<System.MathF>.GetMethod("Round", [| typeof<single> |])  |> moduleBuilder.ImportReference
        let res = {
            errors = List<_>()
            variables = [List<VariableKind>()]
            labels = [Dictionary<_,_>()]
            symbols = [owner,symbols]
            forward = Dictionary<_, _>()
            localVariables = ref 0
            lang = lang
            res = List<MetaInstruction>()
            details = details
            loop = Stack<_>()
            enumSet = Dictionary<_, _>()
            posMap = pm
            sysTypes = Ctx.CreateStdTypes(details,symbols)
            sysProc = {
                            GetMem = allocMem
                            FreeMem = freeMem
                            WriteLine = writeLineMethod
                            Exit = exitMethod
                            ConvertU1ToChar = convertU1ToChar
                            PtrToStringAnsi = ptrToStringAnsi
                            Trunc = mathTrunc
                            Round = mathRound
                      }
        }

        let tsingle = res.sysTypes.single
        symbols.Add(StringName "true", ConstBool 0xFFuy |> ConstSym)
        symbols.Add(StringName "false", ConstBool 0uy |> ConstSym)
        //newSymbols.Add("GetMem", Referenced allocMem |> MethodSym)
        //newSymbols.Add("FreeMem", Referenced freeMem |> MethodSym)
        let singleScalar raw =
           Referenced({
               paramList = [|{typ=tsingle;ref=false}|]
               result = Some tsingle
               raw = raw
           }, ref[]) |> MethodSym
        symbols.Add(StringName "Inc", Intrinsic IncProc |> MethodSym)
        symbols.Add(StringName "Dec", Intrinsic DecProc |> MethodSym)
        symbols.Add(StringName "Read", Intrinsic ReadProc |> MethodSym)
        symbols.Add(StringName "Write", Intrinsic WriteProc |> MethodSym)
        symbols.Add(StringName "ReadLn", Intrinsic ReadLnProc |> MethodSym)
        symbols.Add(StringName "WriteLn", Intrinsic WriteLnProc |> MethodSym)
        symbols.Add(StringName "WriteLine", Intrinsic WriteLineProc |> MethodSym)
        symbols.Add(StringName "New", Intrinsic NewProc |> MethodSym)
        symbols.Add(StringName "Dispose", Intrinsic DisposeProc |> MethodSym)
        symbols.Add(StringName "Break", Intrinsic BreakProc |> MethodSym)
        symbols.Add(StringName "Continue", Intrinsic ContinueProc |> MethodSym)
        symbols.Add(StringName "Exit", Intrinsic ExitProc |> MethodSym)
        symbols.Add(StringName "Halt", Intrinsic HaltProc |> MethodSym)
        symbols.Add(StringName "SizeOf", Intrinsic SizeOfFunc |> MethodSym)
        symbols.Add(StringName "Ord", Intrinsic OrdFunc |> MethodSym)
        symbols.Add(StringName "Chr", Intrinsic ChrFunc |> MethodSym)
        symbols.Add(StringName "Pred", Intrinsic PredProc |> MethodSym)
        symbols.Add(StringName "Succ", Intrinsic SuccProc |> MethodSym)
        symbols.Add(StringName "Round", Intrinsic RoundFunc |> MethodSym)
        // function Abs(x: T): T;
        // function Sqr(x: T): T;
        // function Sin(x: Real): Real;
        // function Cos(x: Real): Real;
        // function Arctan(x: Real): Real;
        symbols.Add(StringName "Exp", singleScalar mathExp)
        symbols.Add(StringName "Ln", singleScalar mathLog)
        symbols.Add(StringName "Trunc", Intrinsic TruncFunc  |> MethodSym)
        res

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

    member self.findLabel name =
        self.labels |> List.tryPick (fun l -> match l.TryGetValue name with | true, bl-> Some bl | _ -> None)

    member self.findLabelUnsafe name =
        match self.findLabel name with
        | Some bl -> bl
        | _ -> failwithf "IE cannot find label %s" name

    static member resolveSysLabels head labels =
        match head with
        | Some((IlResolvedEx(_,_,rex)) as h) ->
            rex := labels |> List.fold (fun s l -> let ll = LazyLabel(h, nullRef()) in (l := ll; ll::s)) !rex
        | Some h -> for l in labels do l := LazyLabel(h, nullRef())
        | _ -> ()

exception CompilerFatalError of Ctx