namespace rec Pas

open System
open System.Collections.Generic

open dnlib.DotNet
open dnlib.DotNet.Emit

type BuildScope =
    | MainScope of ScopeRec * Ctx list
    | LocalScope of Ctx

[<AutoOpen>]
module LangStmt =

    let resolveLabels inst labels =
        match List.tryHead inst with
        | Some(IlDelayed ild) -> ild.ResolveLabels labels
        | Some h -> for l in labels do l := LazyLabel(h, nullRef())
        | _ -> ()

    let doAssignStm (ident, expr) (ctx: Ctx) =
        // add param for findSymbol to set purpose (like this `assign`)
        match ctx.FindSymbol ident with
        | Some(symbols,identType) -> // TODO type chceck
            let loadDest, ltp =
                let load, ltp = ctx.CollectChain symbols
                match symbols with // needed to proper store values to ref parameters in methods
                | VariableLoad(ParamVariable(RefVar, i),_)::[] -> +Ldarg i::load
                | _ -> load
                , !ltp
            let expr, exprType = ctx.ExprToIl expr (Some ltp.ToTypeRef)
            if not(Utils.typeCheck ctx ltp.PasType exprType) then
                ``Error: Incompatible types ('%O' and '%O') for '%O'`` ltp.PasType.name exprType.name ident
                |> ctx.NewMsg ident
            [
                yield! loadDest
                yield! expr
                yield! ctx.ChainWriterFactory ltp
            ]
        | None -> []
        |> fun ils -> (ils, [])

    let doWithStm (idents, stmt) (ctx: Ctx) =
        let ils = List<_>()
        let foldWithSymbols symbols i =
            let loadVarW =
                match ctx.FindSymbol i with
                | Some (symbols, _) ->
                    let cl, ltp = ctx.CollectChain symbols
                    let vt = match !ltp with // TODO allow structs only ? (ValueType as records/classes only)
                             | LTPVar(_,t) -> t
                             | LTPStruct(_,t) -> t
                             | LTPDeref(dt,_) when dt.Sig.IsValueType -> dt
                             | _ -> failwith "IE"
                    // TODO check type of vt for with ?
                    match vt.kind with | TkRecord _ -> () | _ -> failwithf "IE bad type for with %A" vt.kind
                    let pvt = PasType.NewPtr(vt)
                    using(ctx.EnsureVariable pvt) (fun v ->
                    Some([
                            yield! cl
                            yield! ctx.ChainReaderFactory EvalExpr.ReadAddr !ltp
                            +Stloc v.Local
                        ], (v.Local, vt.raw, pvt)))
                | None -> None

            match loadVarW with
            | Some loadVarW ->
                let newSymbols = Dictionary<_,_>()
                let (v, td, ptd) = snd loadVarW
                for f in td.Def.Fields do
                    newSymbols.Add(CompilerName.FromString (string f.Name), WithSym(LocalVariable v, ptd))
                ils.Add(fst loadVarW)
                (WithSpace, newSymbols)::symbols
            | None -> symbols

        let withSymbols = List.fold foldWithSymbols ctx.symbols idents
        let newCtx = { ctx with symbols = withSymbols }
        // TODO check if some part is unused in ils
        let (branch, labels) = stmtListToIlList newCtx stmt
        ([
            for i in ils do yield! i
            yield! branch
        ], labels)

    let doIfStm (expr, tb, fb) (ctx: Ctx) =
        // if logic
        let firstEnfOfStm = ref ForwardLabel
        let lastEndOfStm = ref ForwardLabel
        let condition = fst <| ctx.ExprToIl expr (Some ctx.sysTypes.boolean)
        let (trueBranch, trueLabels) = stmtListToIlList ctx tb
        let (falseBranch, falseLabels) = stmtListToIlList ctx fb
        let hasFalseBranch = falseBranch.Length > 0
        // TODO rethink LazyLabel here
        let checkCondition = [IlBranch(IlBrfalse,if hasFalseBranch then ref (LazyLabel(falseBranch.[0], nullRef())) else firstEnfOfStm)]
        ([
            yield! condition
            yield! checkCondition
            yield! trueBranch
            yield IlBranch(IlBr,if hasFalseBranch then firstEnfOfStm else lastEndOfStm)
            if hasFalseBranch then
                yield! falseBranch
                yield IlBranch(IlBr,lastEndOfStm)
        ], List.concat [[firstEnfOfStm;lastEndOfStm];trueLabels;falseLabels])

    let doCaseStm (expr, mainLabels, stmt) (ctx: Ctx) =
        let lastEndOfStm = ref ForwardLabel
        let (defBranch, defLabels) = stmtListToIlList ctx stmt
        // TODO reduce creation of new var if we want to just read variable
        let ilExpr, exprType = ctx.ExprToIl expr None
        using(ctx.EnsureVariable(exprType)) (fun v ->
        let omitCase: BranchLabel ref option ref = ref None
        let ensureOmitCase i =
            match !omitCase with
            | Some c ->
                c := LazyLabel (i, nullRef())
                omitCase := None
            | _ -> ()
        let casec =
            [for (tocheck, stmt) in mainLabels do
                let (caseBranch, caseLabels) = stmtListToIlList ctx stmt
                if List.isEmpty caseBranch = false then
                  yield
                    (
                     [
                        for l in tocheck do
                            let beginOfCase = +Ldloc v.Local
                            // for ranges we need to skip
                            ensureOmitCase beginOfCase
                            yield beginOfCase
                            match l with
                            | CaseExpr(ConstExpr(ce)) ->
                                 yield! (fst <| ctx.ExprToIl ce None) // TODO type handle
                                 yield IlBranch(IlBeq,ref(LazyLabel(caseBranch.[0], nullRef())))
                            | CaseRange(ConstExpr(ce1), ConstExpr(ce2)) ->
                                 // TODO check proper range
                                 // lower range
                                 let nextCase = ref ForwardLabel
                                 omitCase := Some nextCase
                                 yield! (fst <| ctx.ExprToIl ce1 None) // TODO type handle
                                 yield IlBranch(IlBlt, nextCase)
                                 // higher range
                                 yield +Ldloc v.Local
                                 yield! (fst <| ctx.ExprToIl ce2 None) //TODO type handle
                                 yield IlBranch(IlBgt, nextCase)
                                 yield IlBranch(IlBr, ref(LazyLabel (caseBranch.[0], nullRef())))
                            | _ -> failwith "IE";
                        ], [yield! caseBranch ; IlBranch(IlBr, lastEndOfStm)], caseLabels)
             let defaultCase = [yield! defBranch; IlBranch(IlBr, lastEndOfStm)]
             yield (defaultCase, [], defLabels)
             // for ranges we need to skip
             match (!omitCase, List.tryHead defaultCase) with
             | None, _ -> ()
             | Some _, Some i -> ensureOmitCase i
             | Some oc, None -> yield ([],[],[oc])
            ]
        let (cases, casesbodies, labels) = List.unzip3 casec |||> (fun a b c -> (List.concat a, List.concat b, List.concat c))
        (
            [
             yield! ilExpr
             +Stloc v.Local
             yield! cases
             yield! casesbodies
            ]
         , [yield! labels ; yield lastEndOfStm]))

    let doWhileStm (expr, stmt) (ctx: Ctx) =
        let condition = fst <| ctx.ExprToIl expr (Some ctx.sysTypes.boolean)
        let conditionLabel = ref (LazyLabel (condition.Head, nullRef()))
        // push loop context
        let breakLabel = ref ForwardLabel
        let continueLabel = ref (LazyLabel (condition.Head, nullRef()))
        ctx.loop.Push(continueLabel, breakLabel)
        let (whileBranch, whileLabels) = stmtListToIlList ctx stmt
        resolveLabels condition whileLabels
        ctx.loop.Pop() |> ignore
        ([
            yield IlBranch(IlBr,conditionLabel)
            yield! whileBranch
            yield! condition
            yield IlBranch(IlBrtrue, ref (LazyLabel
                                   ((match List.tryHead whileBranch with
                                    | Some h -> h
                                    | _ -> condition.Head), nullRef())))
        ],[breakLabel])

    let doRepeatStm (stmt, expr) (ctx: Ctx) =
        let condition = fst <| ctx.ExprToIl expr (Some ctx.sysTypes.boolean)
        // push loop context
        let breakLabel = ref ForwardLabel
        let continueLabel = ref (LazyLabel(condition.Head,nullRef()))
        ctx.loop.Push(continueLabel, breakLabel)
        let (repeatBranch, whileLabels) = stmtListToIlList ctx stmt
        resolveLabels condition whileLabels
        ctx.loop.Pop() |> ignore
        ([
            yield! repeatBranch
            yield! condition
            yield IlBranch(IlBrfalse,ref (LazyLabel
                                   ((match List.tryHead repeatBranch with
                                    | Some h -> h
                                    | _ -> condition.[0]),nullRef())))
        ],[breakLabel])

    let doForStm (ident, initExpr, delta, finiExpr, stmt) (ctx: Ctx) =
        let var, varType = ctx.FindSymbol ident |> function
                             | Some([VariableLoad(vs, vt)], _) -> vs, vt
                             | _ -> failwith "IE"
        // TODO allow only specified kind of variables for loops
        using(ctx.EnsureVariable varType)(fun v ->
        // TODO optimization for simple values (dont store in var)
        let (loopInitializeVariables, _) =
            [AssignStm(ident, initExpr);AssignStm(Utils.stdIdent v.Name, finiExpr)] |> stmtListToIlList ctx
        let breakLabel = ref ForwardLabel
        // TODO method param?
        let varLoad = match var with | GlobalVariable vk -> +Ldsfld vk | LocalVariable vk -> +Ldloc vk
        let loopInitialize =
            [
                varLoad
                +Ldloc v.Local
                let doBranch(bgt, blt) = IlBranch((if delta = 1 then bgt else blt), breakLabel)
                match varType.kind with
                | TkOrd(_,ot) -> match ot with
                                 | OrdTypeS -> IlBgt, IlBlt
                                 | OrdTypeU -> IlBgt_Un, IlBlt_Un
                                 |> doBranch
                +Ldloc v.Local
                +Ldc_I4 delta
                +AddInst
                +Stloc v.Local
            ]
        let (incLoopVar, _) = [AssignStm(ident, Add(Value(VIdent(ident)), Value(VInteger delta)))] |> stmtListToIlList ctx
        // push loop context between
        let continueLabel = ref (LazyLabel (incLoopVar.Head, nullRef()))
        ctx.loop.Push(continueLabel, breakLabel)
        let (forBranch, forLabels) = stmtListToIlList ctx stmt
        let stmtLabel = (defaultArg (List.tryHead forBranch) incLoopVar.Head, nullRef()) |> LazyLabel |> ref
        let condition =
            [
              // TODO move simple global loop variables into local void variable
              varLoad
              +Ldloc v.Local
              IlBranch(IlBne_Un, stmtLabel)
            ]
        resolveLabels incLoopVar forLabels
        ctx.loop.Pop() |> ignore
        ([
            yield! loopInitializeVariables
            yield! loopInitialize
            yield! forBranch
            yield! incLoopVar
            yield! condition
        ],[breakLabel]))

    let doCallStm ce (ctx: Ctx) = (fst (ctx.DoCall ce true), [])
    let doIdentStm i = doCallStm (CallExpr(i,[]))
    let doLabelStm l (ctx: Ctx) =
        match ctx.FindLabel l with
        | Some(owner, label) ->
            match LanguagePrimitives.PhysicalEquality ctx.SymLabelOwner owner with
            | true -> Some label
            | false ->
                ``Error: Cannot use label '%O' from different code block`` l |> ctx.NewMsg l
                None
        | _ ->
            ``Error: Cannot find label '%O'`` l |> ctx.NewMsg l
            None
            
    let doLabelSpecStm lid (ctx: Ctx) =
        match doLabelStm lid ctx with
        | Some l ->
            let doSpec() =
                l.set <- true
                l.block <- ctx.block
                [],[l.branch]
            match l.set, l.block with
            | true, _ ->
                ``Label '%O' already declared`` lid |> ctx.NewMsg lid
                [],[]
            | _, NormalBlock -> doSpec()
            | _ when l.block = ctx.block -> doSpec()
            | _ -> // handle initialization / finalization
                ``Error: Cannot use label '%O' from different code block`` lid |> ctx.NewMsg lid
                [],[]
        | _ -> [],[]
        
    let doGotoStm lid (ctx: Ctx) =
        match doLabelStm lid ctx with
        | Some l ->
            let doGoto() =
                l.used <- true
                l.block <- ctx.block
                [IlBranch(IlBr,l.branch)],[] // will do LazyLabel
            match l.block with
            | NormalBlock -> doGoto()
            | _ when l.block = ctx.block  -> doGoto()
            | _ -> // handle initialization / finalization
                ``Error: Cannot use label '%O' from different code block`` lid |> ctx.NewMsg lid
                [],[]
        | _ -> [],[]
        
    let doEmptyStm = fun _ -> ([],[])
    let doStm = function
        | CallStm stm -> doCallStm stm
        | IdentStm stm -> doIdentStm stm
        | AssignStm stm -> doAssignStm stm
        | IfStm stm -> doIfStm stm
        | LabelStm stm -> doLabelSpecStm stm
        | GotoStm stm -> doGotoStm stm
        | CaseStm stm -> doCaseStm stm
        | WhileStm stm -> doWhileStm stm
        | RepeatStm stm -> doRepeatStm stm
        | ForStm stm -> doForStm stm
        | WithStm stm -> doWithStm stm
        | EmptyStm -> doEmptyStm

    // TODO fix peepholes about jump to next opcode
    let stmtListToIlList ctx stmtList: (IlInstruction list * BranchLabel ref list) =
        let resolveLabels =
            fun ((i, nl) as r, l) ->
                resolveLabels i l
                // return new labels or new and unresolved old (for example for empty instruction)
                if i.IsEmpty then (i, nl @ l) else r
        let doStmt labels stmt = resolveLabels(doStm stmt ctx, labels)
        stmtList |> List.mapFold doStmt [] |> fun (i, l) -> List.concat i, l

[<AutoOpen>]
module LangDecl =

    let declTypeAlias (isStrong, origin) name (ctx: Ctx) =
        let originType = ctx.FindTypeId origin
        {originType with name = name} |> ctx.AddType name

    let declTypePtr (count, typeId) name (ctx: Ctx) =
        // TODO like for declTypeSet?
        let typ = CompilerName.FromTypeId typeId
        ctx.AddTypePointer count typ name

    let declTypeSet (packed, typeId) name (ctx: Ctx) =
        // other approach : let typ = ctx.FindTypeId typeId
        // below as second parameter can be used typ.name, but it is bad for internal types like arrays
        ctx.AddTypeSet (CompilerName.FromTypeId typeId) name

    let declTypeEnum enumValues name (ctx: Ctx) =
        let max = (enumValues: string list).Length // TODO get explicit max value
        let enumType = ctx.AddType name {name=name;kind=TkOrd(OkEnumeration, OtULong(0, max));raw=ctx.sysTypes.int32.raw}
        // TODO set items duplicated identifier error ?
        enumValues |> List.iteri (fun i v -> ctx.NewSymbols.Add (CompilerName.FromString v, EnumValueSym(i, enumType)))
        enumType

    let declTypeRecord (packed, fields) (name: CompilerName) ctx =
        let mutable size = 0;
        let td = TypeDefUser(UTF8String ctx.details.Namespace, UTF8String(ctx.details.UniqueTypeName()))
        td.Attributes <- TypeAttributes.Sealed ||| TypeAttributes.BeforeFieldInit ||| TypeAttributes.SequentialLayout
        td.ClassSize <- 0u
        td.BaseType <- ctx.sysTypes.value.DefOrRef
        td.PackingSize <- 1us // if packed then 1s else 0s
        let fieldsMap = Dictionary<string,_>()
        for (names, typeName) in fields do
            for (name: string) in names do
                let typ = ctx.GetInternalType typeName
                let fd = FieldDefUser(UTF8String name, FieldSig typ.Sig)
                fd.Attributes <- FieldAttributes.Public
                td.Fields.Add fd
                fieldsMap.Add(name, (fd :> FieldDef,typ))
                size <- size + typ.SizeOf
        ctx.details.MainModule.Types.Add(td)
        PasType.Create(name,td,TkRecord(fieldsMap, size)) |> ctx.AddType name

    let declTypeArray (packed, dimensions, tname) name (ctx: Ctx) = ctx.AddTypeArray dimensions tname name
    
    let declProcType header name (ctx: Ctx) = ctx.AddProcType header name

    let declType = function
        | TypeAlias decl -> declTypeAlias decl
        | TypePtr decl -> declTypePtr decl
        | TypeSet decl -> declTypeSet decl
        | TypeEnum decl -> declTypeEnum decl
        | TypeRecord decl -> declTypeRecord decl
        | TypeArray(ArrayDef decl) -> declTypeArray decl
        | ProcType header -> declProcType header
        | _ -> raise(InternalError "2020090300")

    let tryDeclType td (name: DIdent) (ctx: Ctx) =
        let cname = CompilerName.FromDIdent name
        match ctx.NewSymbols.ContainsKey cname with
        | true -> ``Duplicated identifier of '%O'`` name |> ctx.NewMsg name
        | false -> declType td cname ctx |> ignore
    
    let declTypes types ctx = List.iter (fun (n, t) -> tryDeclType t n ctx) types

    let declVar (ctx: Ctx) (vn, t: PasType) =
        let tryAddSymbol vk =
            match ctx.NewSymbols.TryAdd(CompilerName.FromDIdent vn, VariableSym(vk, t)) with
            | true -> ctx.variables.Head.Add (VariableDecl.New vk)
            | false -> ``Duplicated identifier of '%O'`` vn |> ctx.NewMsg vn
        match ctx.SymOwner with
        | StandaloneMethod _ -> Local t.Sig |> LocalVariable |> tryAddSymbol
        | GlobalSpace -> 
            let fd = FieldDefUser(UTF8String(vn.ToString()), FieldSig t.Sig)
            fd.Attributes <- FieldAttributes.Public ||| FieldAttributes.Static
            ctx.details.UnitModule.Fields.Add fd
            (fd:>FieldDef) |> GlobalVariable |> tryAddSymbol
        | _ -> raise(InternalError "2020082103")

    let declVariables variables (ctx: Ctx) =
        let doTypedList (l, t) = let dt = ctx.GetInternalType t in l |> List.map (fun v -> v, dt)
        variables
        |> List.collect doTypedList
        |> List.iter (declVar ctx)

    let declConst (ctx: Ctx) (name, ctype, valueExpr) =
        let ctype = match ctype with | Some t -> Some(ctx.GetInternalType t) | _ -> None
        let rec addConstAtom pt ce =
            match pt, ce with
            | None, ConstExpr expr ->
                match ctx.EvalConstExpr None expr with
                | CERInt(i, _) -> ConstInt i
                | CERFloat f -> ConstFloat f
                | _ -> failwith "IE"
            | Some t, ConstExpr expr ->
                match t with
                | SetType pt ->
                    match ctx.EvalConstExpr (Some pt) expr with
                    | CEROrdSet(b, _) -> ConstTempValue(b, t)
                    | _ -> failwith "IE"
                | OrdType ->
                    match ctx.EvalConstExpr pt expr with
                    | CERInt(i,_) -> ConstInt i
                    | _ -> failwith "IE"
                | StrType ->
                    match ctx.EvalConstExpr pt expr with
                    | CERString s -> ConstString s
                    | _ -> failwith "IE"
                | _ -> failwith "IE"
            | Some t, ConstConstr exprs ->
                match t.kind with
                | TkArray (_,_,pt) ->
                    let symToBytes = function
                        | ConstInt i ->
                            match pt.kind, pt.SizeOf with
                            | TkOrd _, 1 -> [|byte i|]
                            | TkOrd _, 2 -> BitConverter.GetBytes(uint16 i)
                            | TkOrd _, 4 -> BitConverter.GetBytes(i)
                            | TkOrd _, 8 -> BitConverter.GetBytes(uint64 i)
                            | _ -> failwith "IE"
                        | ConstFloat f -> BitConverter.GetBytes(f)
                        | ConstString s ->
                            match pt with
                            | ChrType -> [|byte(s.Chars 0)|]
                            | StrType -> strToSStr s
                            | _ -> failwith "IE"
                        | ConstBool b -> [|b|]
                        | ConstTempValue(b,_) -> b
                    (exprs |> List.map (addConstAtom (Some pt) >> symToBytes) |> Array.concat, t)
                    |> ConstTempValue
                //| Some t, ConstStructConstr exprs -> () // handle records
                | _ -> failwith "IE"
            | _ -> failwith "IE"
        match addConstAtom ctype valueExpr with
        | ConstTempValue(b, t) -> ConstValue(ctx.details.AddBytesConst b, t) // add final as static value
        | c -> c
        |> fun sym ->
            if not <| ctx.NewSymbols.TryAdd(CompilerName.FromDIdent name, ConstSym sym) then
                ``Duplicated identifier of '%O'`` name |> ctx.NewMsg name

    let declConstants constants ctx = List.iter (declConst ctx) constants

    let declLabels labels (ctx: Ctx) =
        let tryAddLabel l =
            let lr = { branch = ref UserLabel; used = false; set = false; block = ctx.block }
            if not <| ctx.NewSymbols.TryAdd(CompilerName.FromDIdent l, LabelSym lr) then
                ``Duplicated identifier of '%O'`` l |> ctx.NewMsg l
            else
                ctx.labels.Head.Add(l, lr)
        List.iter tryAddLabel labels

    let doDeclProcAndFunc (name, mRes, mPara, decl) (ctx: Ctx) =
        let methodName = CompilerName.FromDIdent name
        let methodSym, newMethodSymbols =
            match ctx.forward.TryGetValue methodName with
            | true, md ->
                // TODO check signature - must be identical
                ctx.forward.Remove methodName |> ignore
                md
            | _ ->
                let methodInfo = TypesDef.createMethodInfo ctx methodName mRes mPara
                // TODO optimization ? -> ResolveMethodDefThrow is executed twice
                let methodParams = methodInfo.raw.ResolveMethodDefThrow().Parameters
                let newMethodSymbols = Dictionary<_,_>(ctx.lang)
                match methodInfo.result with
                | Some { typ = typ; var = Some var } -> // var always expected (except import scenario, here is no case)
                    newMethodSymbols.Add(CompilerName.FromString "Result", (var, typ) |> VariableSym)
                | None -> ()
                | _ -> raise(InternalError "2020111601")
                methodInfo.paramList |> Array.iteri
                    (fun idx item ->
                        let param = methodParams.[idx]
                        let name = CompilerName.FromString param.Name
                        let vs = VariableSym(ParamVariable(item.ref, param), item.typ)
                        newMethodSymbols.Add(name, vs)
                    )
                let ms = methodInfo, ref None
                if not <| ctx.NewSymbols.TryAdd(methodName, ms |> Referenced |> MethodSym) then
                    ``Duplicated identifier of '%O'`` name |> ctx.NewMsg name
                ms, newMethodSymbols
        let methodInfo = fst methodSym
        let methodBuilder = methodInfo.raw.ResolveMethodDefThrow()
        match decl with
        | BodyDeclr (decls, stmts) ->
            // TODO better handle forward ?
            let scope = LocalScope(ctx.Inner (StandaloneMethod methodSym, newMethodSymbols))
            match Ctx.BuildDeclIl(decls,scope,("result",methodInfo.result)) |> Ctx.BuildStmtIl stmts with
            | Ok (ctx: Ctx) -> ctx.CompileBlock methodBuilder ctx.details.UnitModule |> ignore
            | Error _ -> ()
        | ExternalDeclr (lib, procName) ->
            let libRef = ModuleRefUser(ctx.details.MainModule, UTF8String lib)
            let externalAttributes = MethodAttributes.HideBySig ||| MethodAttributes.PinvokeImpl
            methodBuilder.Attributes <- methodBuilder.Attributes ||| externalAttributes
            methodBuilder.IsPreserveSig <- true // as is

            // https://stackoverflow.com/questions/7255936/how-to-create-exported-functions-in-mono-cecil
            methodBuilder.ImplMap <-
                         let flags = PInvokeAttributes.CharSetAnsi
                                 ||| PInvokeAttributes.SupportsLastError ||| PInvokeAttributes.CallConvWinapi
                         ImplMapUser(libRef, UTF8String procName, flags)
            ctx.details.UnitModule.Methods.Add(methodBuilder)
        | ForwardDeclr ->
            ctx.forward.Add(methodName, (methodSym, newMethodSymbols))

    let declProcAndFunc ({head = (name, mRes, mPara); decl = d} as proc) (ctx: Ctx) =
        match name with
        | Some n -> doDeclProcAndFunc (n, mRes, mPara, d) ctx
        | _ ->
            if (mRes: TypeIdentifier option).IsNone then "procedure" else "function"
            |> sprintf "name for %s" |> ``Error: %s expected`` |> ctx.NewMsg proc

    let doDecl ctx decl =
        match decl with
        | Types types -> declTypes types
        | Variables variables -> declVariables variables
        | Constants consts -> declConstants consts
        | Labels labels -> declLabels labels
        | ProcAndFunc proc -> declProcAndFunc proc
        <| ctx

[<AutoOpen>]
module LangParser =
    
    open System.Text
    open System.IO
    open FParsec
    open Microsoft.FSharp.Core
    
    let applyParser (parser: Parser<'Result,'UserState>) (stream: CharStream<'UserState>) =
        let reply = parser stream
        if reply.Status = FParsec.Primitives.Ok then
            Success(reply.Result, stream.UserState, stream.Position)
        else
            let error = ParserError(stream.Position, stream.UserState, reply.Error)
            FParsec.CharParsers.Failure(error.ToString(stream), error, stream.UserState)
    
    let doPasStream proj parser fileName stream =
        let addParserError (us: PasState) parserError =
            us.messages.Errors.Add parserError
            Error us
        let us = PasState.Create (InitialPass proj) (new PasStream(stream)) proj fileName
        use stream1 = new CharStream<PasState>(us.stream, Encoding.Unicode)
        stream1.UserState <- us
        stream1.Name <- fileName
        match applyParser initialPassParser stream1 with
        | Success _ when not us.HasError -> // Do second pass, parsing success may means failure in AST
            let us = { us with pass = MainPass(proj) }
            use stream2 = new CharStream<PasState>(us.stream, Encoding.Unicode)
            stream2.UserState <- us
            stream2.Name <- fileName
            match applyParser parser stream2 with
            | Success(ast, _, _) when not us.HasError -> Ok(ast, us)
            | FParsec.CharParsers.Failure(s, _, _) -> addParserError us s
            | _ -> Error us
        | FParsec.CharParsers.Failure(s, _, _) -> addParserError us s
        | _ -> Error us
        
    let loadAndDoFile proj parser file =
        let bytes = File.ReadAllText file |> Encoding.Unicode.GetBytes
        using (new MemoryStream(bytes)) (doPasStream proj parser (Path.GetFileName file))

[<AutoOpen>]
module LangBuilder =

    open System.IO
    
    let stmtListToIl sl (ctx: Ctx) (res: Local) =
        let finalizeVariables =
            ctx.variables.Head
            |> Seq.collect
               (function
                | {varKind=LocalVariable v} ->
                     match v.Type with
                     //| t when t = ctx.sysTypes.string -> []//stmtListToIlList ctx (IfStm())
                     | _ -> []
                | {varKind=GlobalVariable v} ->
                     match v.FieldType with
                     //| t when t = ctx.sysTypes.string ->
//                         [
//                             Ldsfld v |> ilResolve
//                             Call ctx.details.FreeMem |> ilResolve
//                         ]
                     | _ -> []
                )
            |> List.ofSeq
        let (instructions, labels) = stmtListToIlList ctx sl
        let returnBlock = [
            if res <> null then +Ldloc res
            +Ret
        ]
        let finallyBlock = match finalizeVariables with
                           | [] ->
                               resolveLabels returnBlock labels
                               None
                           | _ ->
                               let finallyBlock = [yield! finalizeVariables; +Endfinally]
                               resolveLabels finallyBlock labels
                               Some finallyBlock
        (instructions, finallyBlock, returnBlock)
        |> HandleFunction
        |> ctx.res.Add
        for (id, r) in ctx.labels.Head do
            match r.set, r.used, !r.branch with
            | false, true, UserLabel -> ``Label '%O' declared and referenced but never set`` id |> ctx.NewMsg id
            | false, false, UserLabel -> ``Warning: Label '%O' declared but never set`` id |> ctx.NewMsg id
            | true, false, _ -> ``Warning: Label '%O' declared and set but never referenced`` id |> ctx.NewMsg id
            | _ -> ()

    let moduleTypeAttr =
        TypeAttributes.Public
        ||| TypeAttributes.Abstract
        ||| TypeAttributes.Sealed
        ||| TypeAttributes.AutoLayout
        ||| TypeAttributes.AnsiClass
        ||| TypeAttributes.BeforeFieldInit
    
    let sysMethodBuilder (name: string) (mb: ModuleDef) =
        let methodAttributes = MethodAttributes.Public ||| MethodAttributes.Static
        let methodImplAttributes = MethodImplAttributes.IL ||| MethodImplAttributes.Managed
        MethodDefUser(UTF8String name, MethodSig.CreateStatic(mb.CorLibTypes.Void), methodImplAttributes, methodAttributes)
    
    type UsesModule =
        | SystemModule of DIdent
        | UserModule of DIdent
    
    let systemUnitName = DIdent.FromString "System" |> SystemModule
    
    type Ctx with
        member self.CompileBlock (methodBuilder: MethodDef) (typeBuilder : TypeDef) =
            let body = CilBody()
            methodBuilder.Body <- body
            let ilGenerator = body |> emit
            typeBuilder.Methods.Add(methodBuilder)
            Seq.iter ilGenerator self.res
            self.variables.Head
            |> Seq.iter (function // TODO dont declare unused variables
                | {varKind=LocalVariable t; usages=u} when u >= 0 -> t |> body.Variables.Add |> ignore
                | _ -> ()) 
            body.InitLocals <- true
            // https://github.com/jbevain/cecil/issues/365
            body.OptimizeMacros()
            methodBuilder

        static member BuildDeclIl(decl, buildScope, ?methodResult) =
            let ctx = match buildScope with
                      | MainScope(sr, units) -> Ctx.Create GlobalSpace sr units
                      | LocalScope ctx -> ctx
            let result = match methodResult with
                         | Some (name, Some{var=Some v}) ->
                            ctx.variables.Head.Add (VariableDecl.New v)
                            match v with
                            | LocalVariable v -> v
                            | _ -> null
                         | Some (_, None) -> null // no result (void)
                         | Some (_, Some{var=None}) -> raise(InternalError "2020111602") // impossible scenario, means try of implementing imported function (!)
                         | _ -> null // main program
            decl |> List.iter (doDecl ctx)
            ctx, result

        static member BuildStmtIl stmt (ctx: Ctx, result) =
            // do implementation section only if interface section has no error
            match ctx.messages.HasError with
            | true -> Error ctx
            | _ -> // after implementation analise, check for errors again
                stmtListToIl stmt ctx result
                if ctx.messages.HasError then Error ctx else Ok ctx

        static member BuildUnit state moduleBuilder unit =
            let unitName, isSystem = match unit with
                                     | SystemModule n -> n, true
                                     | UserModule n -> n, false
            match state.proj.FindUnit (sprintf "%O.pas" unitName) with
            | UnitFound f ->
                match loadAndDoFile state.proj parseUnitModule f with
                | Ok((_, us) as res) ->
                    match Ctx.BuildUnitModule res moduleBuilder isSystem with
                    | Some(ctx, _, _ as res) ->
                        state.proj.AddModule f (PascalModule.unit us.messages res)
                        Some ctx
                    | None ->
                        state.proj.AddModule f (PascalModule.invalid us.messages)
                        None
                | Error us ->
                    state.proj.AddModule f (PascalModule.invalid us.messages)
                    None
            | UnitCompiled o -> Some(unbox o)
            | UnitNotFound ->
                state.messages.AddFatal (sprintf "Cannot find unit '%O'" unitName)
                None
            
        static member BuildUses sysModules modules state moduleBuilder =
                [yield! sysModules; yield! (modules |> List.map UserModule)]
                |> List.map (Ctx.BuildUnit state moduleBuilder)
                |> List.choose id
                |> List.rev // priority meaning
               
        static member BuildMainModule (pasModule: MainModuleRec, state) =
            let moduleName = pasModule.ProgramName
            
            let asmResolver = AssemblyResolver();
            let modCtx = ModuleContext(asmResolver);
            // All resolved assemblies will also get this same modCtx
            asmResolver.DefaultModuleContext <- modCtx
            let moduleBuilder = new ModuleDefUser(UTF8String moduleName)
            moduleBuilder.Context <- modCtx
            moduleBuilder.Kind <- ModuleKind.Console
            // for 32 bit assembly
            // moduleBuilder.Attributes <- ModuleAttributes.Required32Bit ||| moduleBuilder.Attributes
            
            (AssemblyDefUser(UTF8String moduleName, Version(0,0,0,0))).Modules.Add(moduleBuilder);
            
            let typeBuilder =
                TypeDefUser(UTF8String moduleName, UTF8String moduleName, moduleBuilder.CorLibTypes.Object.ToTypeDefOrRef())
            typeBuilder.Attributes <- moduleTypeAttr
            moduleBuilder.Types.Add(typeBuilder)
            
            let units = Ctx.BuildUses [systemUnitName] pasModule.uses state (moduleBuilder :> ModuleDef)
            
            let sr = ScopeRec.Create moduleName state typeBuilder moduleBuilder
            let (ctx, _) as ctxvd = Ctx.BuildDeclIl(pasModule.block.decl, MainScope(sr, units))
            // initialization, sysini + initializations of modules
            ctx.res.Add(
                           InstructionList([
                               +Call(ctx.FindMethodReference "InitSystem")
                               yield!
                                   state.proj.Modules.Ordered
                                   |> Seq.filter(fun m -> m.Init <> null)
                                   |> Seq.map(fun m -> +Call m.Init)
                           ])
                       )
            match Ctx.BuildStmtIl pasModule.block.stmt ctxvd with
            | Error _ -> None
            | Ok ctx ->
                // TODO finally should be done in different way
//                res.Insert(res.Count - 1,
//                           state.proj.Modules.Ordered
//                           |> Seq.filter(fun m -> m.Init <> null)
//                           |> List.ofSeq
//                           |> List.rev
//                           |> List.map(fun m -> +Call m.Init)
//                           |> InstructionList
//                       )
                let mainBlock = ctx.CompileBlock (sysMethodBuilder "Main" moduleBuilder) typeBuilder
                moduleBuilder.EntryPoint <- mainBlock
                if state.HasError then None
                else Some moduleBuilder
                // TODO version of target framework
                (*
                let v = moduleBuilder.ImportReference(typeof<TargetFrameworkAttribute>.GetConstructor([|typeof<string>|]));
                let c = CustomAttribute(v);
                let sr = moduleBuilder.ImportReference(typeof<string>)
                let ca = CustomAttributeArgument(sr, box ".NETCoreApp,Version=v3.0")
                c.ConstructorArguments.Add(ca)
                assemblyBuilder.CustomAttributes.Add(c)
                *)
        
        static member BuildUnitModule (pasModule: UnitModuleRec, state) (moduleBuilder: ModuleDef) isSystem =
            let moduleName = pasModule.UnitName
            let expectedModuleName = Path.GetFileNameWithoutExtension(state.fileName)
            let correctModuleName = String.Equals(expectedModuleName, moduleName, StringComparison.OrdinalIgnoreCase)
            match correctModuleName with
            | true ->
                let typeBuilder =
                    TypeDefUser(UTF8String moduleName, UTF8String moduleName, moduleBuilder.CorLibTypes.Object.TypeDefOrRef)
                typeBuilder.Attributes <- moduleTypeAttr
                moduleBuilder.Types.Add(typeBuilder)

                // todo circural ref
                let units = Ctx.BuildUses [if not isSystem then systemUnitName] pasModule.intf.uses state moduleBuilder
                
                let sr = ScopeRec.Create moduleName state typeBuilder moduleBuilder
                // TODO uses handle for module
                let (ctx, _) = Ctx.BuildDeclIl(pasModule.intf.decl, MainScope(sr, units))
                let (ctx, vd) = Ctx.BuildDeclIl(pasModule.impl.decl, LocalScope ctx)
                let init = match (ctx.Next InitializationBlock, vd) |> Ctx.BuildStmtIl pasModule.init with
                           | Ok ctx -> Some <| ctx.CompileBlock (sysMethodBuilder ("$module$init") moduleBuilder) typeBuilder
                           | Error _ -> None
                let fini = match (ctx.Next FinalizationBlock, vd) |> Ctx.BuildStmtIl pasModule.fini with
                           | Ok ctx -> Some <| ctx.CompileBlock (sysMethodBuilder ("$module$fini") moduleBuilder) typeBuilder
                           | Error _ -> None
                match init, fini with
                | Some init, Some fini -> Some(ctx, init, fini)
                | _ -> None
                    
            | false ->
                ``Improper unit name '%O' (expected name: '%s')`` moduleName (expectedModuleName.ToUpper()) |> state.NewMsg pasModule.name.BoxPos
                None