namespace rec Pas

open System
open System.Collections.Generic

open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks

type BuildScope =
    | MainScope of ScopeRec * Ctx list
    | LocalScope of Ctx

[<AutoOpen>]
module LangStmt =

    let resolveLabels inst labels =
        match List.tryHead inst with
        | Some((IlResolvedEx(_,_,rex)) as h) ->
            rex := labels |> List.fold (fun s l -> let ll = LazyLabel(h, nullRef()) in (l := ll; ll::s)) !rex
        | Some h -> for l in labels do l := LazyLabel(h, nullRef())
        | _ -> ()

    let doAssignStm (ident, expr) (ctx: Ctx) =
        // add param for findSymbol to set purpose (like this `assign`)
        match ctx.FindSymbol ident with
        | Some(symbols,_) -> // TODO type chceck
            let ltp = ref LTPNone
            let loadDest =
                let load = List.collect (ctx.ChainLoadToIl ltp (ctx.ChainReaderFactory false false)) symbols
                match symbols with // needed to proper store values to ref parameters in methods
                | VariableLoad(ParamVariable(RefVar, i),_)::[] -> +Ldarg i::load
                | _ -> load
            let ltp = !ltp
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
                    let ltp = ref LTPNone
                    let cl = List.collect (ctx.ChainLoadToIl ltp (ctx.ChainReaderFactory false false)) symbols
                    let vt = match !ltp with // TODO allow structs only ? (ValueType as records/classes only)
                             | LTPVar(_,t) -> t
                             | LTPStruct(_,t) -> t
                             | LTPDeref(dt,_) when dt.raw.MetadataType = MetadataType.ValueType -> dt
                             | _ -> failwith "IE"
                    // TODO check type of vt for with ?
                    match vt.kind with | TkRecord _ -> () | _ -> failwithf "IE bad type for with %A" vt.kind
                    let pvt = PasType.NewPtr(vt)
                    let (_, vv) = ctx.EnsureVariable pvt
                    Some([
                            yield! cl
                            yield! ctx.ChainReaderFactory false true !ltp
                            +Stloc vv
                        ], (vv, vt.raw :?> TypeDefinition, pvt))
                | None -> None

            match loadVarW with
            | Some loadVarW ->
                let newSymbols = Dictionary<_,_>()
                let (v, td, ptd) = snd loadVarW
                for f in td.Fields do
                    newSymbols.Add(CompilerName.FromString f.Name, WithSym(LocalVariable v, ptd))
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
        let _, var = ctx.EnsureVariable(exprType)
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
                            let beginOfCase = +Ldloc var
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
                                 yield +Ldloc var
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
             +Stloc var
             yield! cases
             yield! casesbodies
            ]
         , [yield! labels ; yield lastEndOfStm])

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
        let (varFinalName, varFinal) = ctx.EnsureVariable varType
        // TODO optimization for simple values (dont store in var)
        let (loopInitializeVariables, _) =
            [AssignStm(ident, initExpr);AssignStm(Utils.stdIdent varFinalName, finiExpr)] |> stmtListToIlList ctx
        let breakLabel = ref ForwardLabel
        // TODO method param?
        let varLoad = match var with | GlobalVariable vk -> +Ldsfld vk | LocalVariable vk -> +Ldloc vk
        let loopInitialize =
            [
                varLoad
                +Ldloc varFinal
                let doBranch(bgt, blt) = IlBranch((if delta = 1 then bgt else blt), breakLabel)
                match varType.kind with
                | TkOrd(_,ot) -> match ot with
                                 | OrdTypeS -> IlBgt, IlBlt
                                 | OrdTypeU -> IlBgt_Un, IlBlt_Un
                                 |> doBranch
                +Ldloc varFinal
                +Ldc_I4 delta
                +AddInst
                +Stloc varFinal
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
              +Ldloc varFinal
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
        ],[breakLabel])

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
            
    let doLabelSpecStm l (ctx: Ctx) =
        match doLabelStm l ctx with
        | Some l -> [],[l]
        | _ -> [],[]
        
    let doGotoStm l (ctx: Ctx) =
        match doLabelStm l ctx with
        | Some l -> [IlBranch(IlBr,l)],[] // will do LazyLabel
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
        let typ = ctx.FindTypeId typeId
        ctx.AddTypePointer count typ.name name

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

    let declTypeRecord (packed, fields) name ctx =
        let mutable size = 0;
        let td = TypeDefinition(ctx.details.Namespace, ctx.details.UniqueTypeName(), TypeAttributes.Sealed ||| TypeAttributes.BeforeFieldInit ||| TypeAttributes.SequentialLayout)
        td.ClassSize <- 0
        td.BaseType <- ctx.sysTypes.value.raw
        td.PackingSize <- 1s // if packed then 1s else 0s
        let fieldsMap = Dictionary<_,_>()
        for (names, typeName) in fields do
            for name in names do
                let typ = ctx.GetInternalType typeName
                let fd = FieldDefinition(name, FieldAttributes.Public, typ.raw)
                td.Fields.Add fd
                fieldsMap.Add(name, (fd,typ))
                size <- size + typ.SizeOf
        ctx.details.MainModule.Types.Add(td)
        ctx.AddType name {name=name;kind=TkRecord(fieldsMap, size);raw=td}

    let declTypeArray (packed, dimensions, tname) name (ctx: Ctx) = ctx.AddTypeArray dimensions tname name

    let declType = function
        | TypeAlias decl -> declTypeAlias decl
        | TypePtr decl -> declTypePtr decl
        | TypeSet decl -> declTypeSet decl
        | TypeEnum decl -> declTypeEnum decl
        | TypeRecord decl -> declTypeRecord decl
        | TypeArray(ArrayDef decl) -> declTypeArray decl
        | _ -> failwith "IE"

    let tryDeclType td (name: DIdent) (ctx: Ctx) =
        let cname = CompilerName.FromDIdent name
        match ctx.NewSymbols.ContainsKey cname with
        | true -> ``Duplicated identifier of '%O'`` name |> ctx.NewMsg name
        | false -> declType td cname ctx |> ignore
    
    let declTypes types ctx = List.iter (fun (n, t) -> tryDeclType t n ctx) types

    let declVar (ctx: Ctx) (vn, t) =
        let tryAddSymbol vk =
            match ctx.NewSymbols.TryAdd(CompilerName.FromDIdent vn, VariableSym(vk, t)) with
            | true -> ctx.variables.Head.Add vk
            | false -> ``Duplicated identifier of '%O'`` vn |> ctx.NewMsg vn
        match ctx.SymOwner with
        | StandaloneMethod _ -> VariableDefinition (t: PasType).raw |> LocalVariable |> tryAddSymbol
        | GlobalSpace -> 
            let fd = FieldDefinition(vn.ToString(), FieldAttributes.Public ||| FieldAttributes.Static, t.raw)
            ctx.details.UnitModule.Fields.Add fd
            fd |> GlobalVariable |> tryAddSymbol
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
            if not <| ctx.NewSymbols.TryAdd(CompilerName.FromDIdent l, LabelSym(ref UserLabel)) then
                ``Duplicated identifier of '%O'`` l |> ctx.NewMsg l
        List.iter tryAddLabel labels

    let doDeclProcAndFunc (name, mRes, mPara, decl) (ctx: Ctx) =
        let methodName = CompilerName.FromDIdent name
        let methodSym, newMethodSymbols, rVar =
            match ctx.forward.TryGetValue methodName with
            | true, md ->
                // TODO check signature - must be identical
                ctx.forward.Remove methodName |> ignore
                md
            | _ ->
                let newMethodSymbols = Dictionary<_,_>(ctx.lang)
                let mRes, rVar = match mRes with
                                 | Some r ->
                                       let res = ctx.FindTypeId r
                                       let resultVar = VariableDefinition res.raw |> LocalVariable
                                       newMethodSymbols.Add(CompilerName.FromString "Result", (resultVar, res) |> VariableSym)
                                       res, Some resultVar
                                 | _ -> ctx.sysTypes.net_void, None

                let ps = defaultArg mPara []
                         |> List.collect
                            (fun (k, (ps, t)) ->
                             let t = match t with Some t -> Some(ctx.FindTypeId t) | _ -> None
                             [for p in ps do
                                 let (typ, byref, t, isref) =
                                     match k, t with
                                     | Some Const, Some t -> t.raw, RefConst, t, false
                                     | Some Var, Some t -> ByReferenceType(t.raw) :> TypeReference, RefVar, t, true
                                     | Some Const, None -> ctx.sysTypes.constParam.raw, RefUntypedConst, ctx.sysTypes.constParam, true
                                     | Some Var, None -> ctx.sysTypes.varParam.raw, RefUntypedVar, ctx.sysTypes.varParam, true
                                     | None, Some t -> t.raw, RefNone, t, false
                                     | _ -> raise(InternalError "2020082101")
                                 let pd = ParameterDefinition(p, ParameterAttributes.None, typ)
                                 newMethodSymbols.Add(CompilerName.FromString p, VariableSym(ParamVariable(byref, pd), t))
                                 yield (pd, {typ=t;ref=isref})]
                            )
                let ps, mp = ps |> List.unzip
                let methodAttributes = MethodAttributes.Public ||| MethodAttributes.Static
                let md = MethodDefinition(name.ToString(), methodAttributes, mRes.raw)
                List.iter md.Parameters.Add ps
                let methodInfo = {
                    paramList = Array.ofList mp
                    result = if rVar.IsSome then Some mRes else None
                    raw = md
                }
                let ms = methodInfo, ref []
                if not <| ctx.NewSymbols.TryAdd(methodName, ms |> Referenced |> MethodSym) then
                    ``Duplicated identifier of '%O'`` name |> ctx.NewMsg name
                ms, newMethodSymbols, rVar
        let methodBuilder = (fst methodSym).raw :?> MethodDefinition
        match decl with
        | BodyDeclr (decls, stmts) ->
            // TODO better handle forward ?
            let scope = LocalScope(ctx.Inner (StandaloneMethod methodSym, newMethodSymbols))
            match Ctx.BuildDeclIl(decls,scope,("result",rVar)) |> Ctx.BuildStmtIl stmts with
            | Ok(res, _) -> Ctx.CompileBlock methodBuilder ctx.details.UnitModule res |> ignore
            | Error _ -> ()
        | ExternalDeclr (lib, procName) ->
            let libRef = ModuleReference(lib)
            ctx.details.MainModule.ModuleReferences.Add(libRef)
            let externalAttributes = MethodAttributes.HideBySig ||| MethodAttributes.PInvokeImpl
            methodBuilder.Attributes <- methodBuilder.Attributes ||| externalAttributes
            methodBuilder.IsPreserveSig <- true // as is

            // https://stackoverflow.com/questions/7255936/how-to-create-exported-functions-in-mono-cecil
            methodBuilder.PInvokeInfo <-
                         let flags = PInvokeAttributes.CharSetAnsi
                                 ||| PInvokeAttributes.SupportsLastError ||| PInvokeAttributes.CallConvWinapi
                         PInvokeInfo(flags, procName, libRef)
            ctx.details.UnitModule.Methods.Add(methodBuilder)
        | ForwardDeclr ->
            ctx.forward.Add(methodName, (methodSym, newMethodSymbols, rVar))

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
    
    let stmtListToIl sl (ctx: Ctx) (res: VariableDefinition) =
        let finalizeVariables =
            ctx.variables.Head
            |> Seq.collect
               (function
                | LocalVariable v ->
                     ctx.res.Add(DeclareLocal(v))
                     match v.VariableType with
                     //| t when t = ctx.sysTypes.string -> []//stmtListToIlList ctx (IfStm())
                     | _ -> []
                | GlobalVariable v ->
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
        ctx.res

    let moduleTypeAttr =
        TypeAttributes.Public
        ||| TypeAttributes.Abstract
        ||| TypeAttributes.Sealed
        ||| TypeAttributes.AutoLayout
        ||| TypeAttributes.AnsiClass
        ||| TypeAttributes.BeforeFieldInit
    
    let sysMethodBuilder name mb =
        let methodAttributes = MethodAttributes.Public ||| MethodAttributes.Static
        MethodDefinition(name, methodAttributes, (mb: ModuleDefinition).TypeSystem.Void)
    
    type UsesModule =
        | SystemModule of DIdent
        | UserModule of DIdent
    
    let systemUnitName = DIdent.FromString "System" |> SystemModule
    
    type Ctx with
        static member CompileBlock (methodBuilder: MethodDefinition) (typeBuilder : TypeDefinition) (instr: List<MetaInstruction>) =
            let ilGenerator = methodBuilder.Body.GetILProcessor() |> emit
            typeBuilder.Methods.Add(methodBuilder)
            Seq.iter ilGenerator instr
            methodBuilder.Body.InitLocals <- true
            // https://github.com/jbevain/cecil/issues/365
            methodBuilder.Body.OptimizeMacros()
            methodBuilder

        static member BuildDeclIl(decl, buildScope, ?resVar) =
            let ctx = match buildScope with
                      | MainScope(sr, units) -> Ctx.Create GlobalSpace sr units
                      | LocalScope ctx -> ctx
            let result = match resVar with
                         | Some (name, Some(v)) ->
                            ctx.variables.Head.Add v
                            match v with
                            | LocalVariable v -> v
                            | _ -> null
                         | Some (_, None) -> null // no result (void)
                         | _ -> null // main program
            decl |> List.iter (doDecl ctx)
            ctx, result

        static member BuildStmtIl stmt (ctx: Ctx, result) =
            // do implementation section only if interface section has no error
            match ctx.messages.HasError with
            | true -> Error ctx
            | _ -> // after implementation analise, check for errors again
                let res = stmtListToIl stmt ctx result
                if ctx.messages.HasError then Error ctx else Ok (res, ctx)

        static member BuildUnit state moduleBuilder unit =
            let unitName, isSystem = match unit with
                                     | SystemModule n -> n, true
                                     | UserModule n -> n, false
            match state.proj.FindUnit (sprintf "%O.pas" unitName) with
            | UnitFound f ->
                match loadAndDoFile state.proj parseUnitModule f with
                | Ok((_, us) as res) ->
                    match Ctx.BuildUnitModule res moduleBuilder isSystem with
                    | Some ctx ->
                        state.proj.AddModule f { Messages = us.messages; Obj = Some(box ctx) }
                        Some ctx
                    | None ->
                        state.proj.AddModule f { Messages = us.messages; Obj = None }
                        None
                | Error us ->
                    state.proj.AddModule f { Messages = us.messages; Obj = None }
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
            let assemblyBuilder =
                let assemblyName = AssemblyNameDefinition(moduleName, Version(0,0,0,0))
                AssemblyDefinition.CreateAssembly(assemblyName, moduleName, ModuleKind.Console)
            let moduleBuilder = assemblyBuilder.MainModule
            // for 32 bit assembly
            // moduleBuilder.Attributes <- ModuleAttributes.Required32Bit ||| moduleBuilder.Attributes

            let typeBuilder =
                TypeDefinition(moduleName, moduleName, moduleTypeAttr, moduleBuilder.TypeSystem.Object)
            moduleBuilder.Types.Add(typeBuilder)
            
            let units = Ctx.BuildUses [systemUnitName] pasModule.uses state moduleBuilder
            
            let sr = ScopeRec.Create moduleName state typeBuilder moduleBuilder
            let (ctx, _) as ctxvd = Ctx.BuildDeclIl(pasModule.block.decl, MainScope(sr, units))
            ctx.res.Add(InstructionList([+Call(ctx.FindMethodReference "InitSystem")]))
            match Ctx.BuildStmtIl pasModule.block.stmt ctxvd with
            | Error _ -> None
            | Ok (res, _) ->
                let mainBlock = Ctx.CompileBlock (sysMethodBuilder "Main" moduleBuilder) typeBuilder res
                assemblyBuilder.EntryPoint <- mainBlock
                if state.HasError then None
                else Some assemblyBuilder
                // TODO version of target framework
                (*
                let v = moduleBuilder.ImportReference(typeof<TargetFrameworkAttribute>.GetConstructor([|typeof<string>|]));
                let c = CustomAttribute(v);
                let sr = moduleBuilder.ImportReference(typeof<string>)
                let ca = CustomAttributeArgument(sr, box ".NETCoreApp,Version=v3.0")
                c.ConstructorArguments.Add(ca)
                assemblyBuilder.CustomAttributes.Add(c)
                *)
        
        static member BuildUnitModule (pasModule: UnitModuleRec, state) (moduleBuilder: ModuleDefinition) isSystem =
            let moduleName = pasModule.UnitName
            let expectedModuleName = Path.GetFileNameWithoutExtension(state.fileName)
            let correctModuleName = String.Equals(expectedModuleName, moduleName, StringComparison.OrdinalIgnoreCase)
            match correctModuleName with
            | true ->
                let typeBuilder =
                    TypeDefinition(moduleName, moduleName, moduleTypeAttr, moduleBuilder.TypeSystem.Object)
                moduleBuilder.Types.Add(typeBuilder)

                // todo circural ref
                let units = Ctx.BuildUses [if not isSystem then systemUnitName] pasModule.intf.uses state moduleBuilder
                
                let sr = ScopeRec.Create moduleName state typeBuilder moduleBuilder
                // TODO uses handle for module
                let ctxvdIntf = Ctx.BuildDeclIl(pasModule.intf.decl, MainScope(sr, units))
                let ctxvdImpl = Ctx.BuildDeclIl(pasModule.impl.decl, LocalScope (fst ctxvdIntf))
                match ctxvdImpl |> Ctx.BuildStmtIl pasModule.init with
                | Ok (res, ctx) -> // TODO fini ?
                    Ctx.CompileBlock (sysMethodBuilder ("module$init") moduleBuilder) typeBuilder res |> ignore
                    Some ctx
                | Error _ -> None
            | false ->
                ``Improper unit name '%O' (expected name: '%s')`` moduleName (expectedModuleName.ToUpper()) |> state.NewMsg pasModule.name.BoxPos
                None