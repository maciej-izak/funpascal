namespace rec Pas

open System
open System.Collections.Generic

open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks

type BuildScope =
    | MainScope of (string * TypeDefinition * PasState * ModuleDefinition)
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
        | Ok(symbols,_) -> // TODO type chceck
            let ltp = ref LTPNone
            let loadDest =
                let load = List.collect (ctx.ChainLoadToIl ltp (ctx.ChainReaderFactory false false)) symbols
                match symbols with // needed to proper store values to ref parameters in methods
                | VariableLoad(ParamVariable(RefVar, i),_)::[] -> +Ldarg i::load
                | _ -> load
            let ltp = !ltp
            let expr, exprType = ctx.ExprToIl expr (Some ltp.ToTypeRef)
            if not(Utils.typeCheck ctx ltp.PasType exprType) then
                ctx.NewError ident (sprintf "Incompatible types ('%O' and '%O') for \"%O\"" ltp.PasType.name exprType.name ident)
            [
                yield! loadDest
                yield! expr
                yield! ctx.ChainWriterFactory ltp
            ]
        | Error() -> []
        |> fun ils -> (ils, [])

    let doWithStm (idents, stmt) (ctx: Ctx) =
        let ils = List<_>()
        let foldWithSymbols symbols i =
            let loadVarW =
                match ctx.FindSymbol i with
                | Ok (symbols, _) ->
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
                | Error() -> None

            match loadVarW with
            | Some loadVarW ->
                let newSymbols = Dictionary<_,_>()
                let (v, td, ptd) = snd loadVarW
                for f in td.Fields do
                    newSymbols.Add(StringName f.Name, WithSym(LocalVariable v, ptd))
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
                             | Ok([VariableLoad(vs, vt)], _) -> vs, vt
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
    let doLabelStm l (ctx: Ctx) = ([],[ctx.FindLabelUnsafe l])
    let doGotoStm s (ctx: Ctx) = ([IlBranch(IlBr,ctx.FindLabelUnsafe s)],[]) // will do LazyLabel
    let doEmptyStm = fun _ -> ([],[])
    let doStm = function
        | CallStm stm -> doCallStm stm
        | IdentStm stm -> doIdentStm stm
        | AssignStm stm -> doAssignStm stm
        | IfStm stm -> doIfStm stm
        | LabelStm stm -> doLabelStm stm
        | GotoStm stm -> doGotoStm stm
        | CaseStm stm -> doCaseStm stm
        | WhileStm stm -> doWhileStm stm
        | RepeatStm stm -> doRepeatStm stm
        | ForStm stm -> doForStm stm
        | WithStm stm -> doWithStm stm
        | EmptyStm -> doEmptyStm

    // TODO fix peepholes about jump to next opcode
    let stmtListToIlList ctx stmtList: (IlInstruction list * BranchLabel ref list) =
        let resolveLabels = function | (i, _) as r, l -> resolveLabels i l ; r // return new labels
        let doStmt labels stmt = resolveLabels(doStm stmt ctx, labels)
        stmtList |> List.mapFold doStmt [] |> fun (i, l) -> List.concat i, l

[<AutoOpen>]
module LangDecl =

    let declTypeAlias (isStrong, origin) name (ctx: Ctx) =
        let name = StringName name
        let originType = match ctx.FindTypeId origin with | Some t -> t | _ -> failwith "IE not found"
        {originType with name = name} |> ctx.AddType name

    let declTypePtr (count, typeId) name (ctx: Ctx) =
        let typ = match ctx.FindTypeId typeId with | Some t -> t | _ -> failwith "IE not found"
        ctx.AddTypePointer count typ.name (StringName name)

    let declTypeSet (packed, typeId) name (ctx: Ctx) =
        let typ = match ctx.FindTypeId typeId with | Some t -> t | _ -> failwith "IE not found"
        ctx.AddTypeSet typ.name (StringName name)

    let declTypeEnum enumValues name (ctx: Ctx) =
        let name = StringName name
        let max = (enumValues: string list).Length // TODO get explicit max value
        let enumType = ctx.AddType name {name=name;kind=TkOrd(OkEnumeration, OtULong(0, max));raw=ctx.sysTypes.int32.raw}
        enumValues |> List.iteri (fun i v -> ctx.NewSymbols.Add (StringName v, EnumValueSym(i, enumType)))
        enumType

    let declTypeRecord (packed, fields) name ctx =
        let mutable size = 0;
        let td = TypeDefinition(ctx.details.ns, ctx.details.UniqueTypeName(), TypeAttributes.Sealed ||| TypeAttributes.BeforeFieldInit ||| TypeAttributes.SequentialLayout)
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
        ctx.details.moduleBuilder.Types.Add(td)
        let name = StringName name
        ctx.AddType name {name=name;kind=TkRecord(fieldsMap, size);raw=td}

    let declTypeArray (packed, dimensions, tname) name (ctx: Ctx) = ctx.AddTypeArray dimensions tname (StringName name)

    let declType = function
        | TypeAlias decl -> declTypeAlias decl
        | TypePtr decl -> declTypePtr decl
        | TypeSet decl -> declTypeSet decl
        | TypeEnum decl -> declTypeEnum decl
        | TypeRecord decl -> declTypeRecord decl
        | TypeArray(ArrayDef decl) -> declTypeArray decl
        | _ -> failwith "IE"

    let declTypes types ctx = List.iter (fun (n, t) -> declType t n ctx |> ignore) types

    let declVar (ctx: Ctx) nt =
        let addVar vk = vk |> ctx.variables.Head.Add
        match ctx.SymOwner with
        | StandaloneMethod _ -> fun (vn, t) ->
                            VariableDefinition (t: PasType).raw
                            |> LocalVariable
                            |> fun vk ->
                                ctx.NewSymbols.Add(StringName vn, VariableSym(vk, t))
                                addVar vk
        | GlobalSpace -> fun (vn, t) ->
                            let fd = FieldDefinition(vn, FieldAttributes.Public ||| FieldAttributes.Static, t.raw)
                            fd |> GlobalVariable
                            |> fun vk ->
                                ctx.NewSymbols.Add(StringName vn, VariableSym(vk,t))
                                ctx.details.tb.Fields.Add fd
                                addVar vk
        | _ -> failwith "IE"
        |> fun declVar -> declVar nt

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
        |> fun sym -> ctx.NewSymbols.Add(StringName name, ConstSym sym)

    let declConstants constants ctx = List.iter (declConst ctx) constants

    let declLabels labels ctx = List.iter (fun l -> ctx.labels.Head.Add(l, ref (UserLabel l))) labels

    let declProcAndFunc ((name, mRes, mPara), d) ctx =
        let name = match name with
                   | Some n -> n
                   | _ -> failwith "name expected"
        let methodName = StringName name
        let methodSym, newMethodSymbols, rVar =
            match ctx.forward.TryGetValue name with
            | true, md ->
                // TODO check signature - must be identical
                ctx.forward.Remove name |> ignore
                md
            | _ ->
                let newMethodSymbols = Dictionary<_,_>(ctx.lang)
                let mRes, rVar = match mRes with
                                 | Some r ->
                                       let res = match ctx.FindTypeId r with | Some t -> t | _ -> failwith "IE"
                                       let resultVar = VariableDefinition res.raw |> LocalVariable
                                       newMethodSymbols.Add(StringName "result", (resultVar, res) |> VariableSym)
                                       res, Some resultVar
                                 | _ -> ctx.sysTypes.net_void, None

                let ps = defaultArg mPara []
                         |> List.collect
                            (fun (k, (ps, t)) ->
                             let t = match t with Some t -> ctx.FindTypeId t | _ -> None
                             [for p in ps do
                                 let (typ, byref, t, isref) =
                                     match k, t with
                                     | Some Const, Some t -> t.raw, RefConst, t, false
                                     | Some Var, Some t -> ByReferenceType(t.raw) :> TypeReference, RefVar, t, true
                                     | Some Const, None -> ctx.sysTypes.constParam.raw, RefUntypedConst, ctx.sysTypes.constParam, true
                                     | Some Var, None -> ctx.sysTypes.varParam.raw, RefUntypedVar, ctx.sysTypes.varParam, true
                                     | None, Some t -> t.raw, RefNone, t, false
                                     | _ -> failwith "IE"
                                 let pd = ParameterDefinition(p, ParameterAttributes.None, typ)
                                 newMethodSymbols.Add(StringName p, VariableSym(ParamVariable(byref, pd), t))
                                 yield (pd, {typ=t;ref=isref})]
                            )
                let ps, mp = ps |> List.unzip
                let methodAttributes = MethodAttributes.Public ||| MethodAttributes.Static
                let md = MethodDefinition(name, methodAttributes, mRes.raw)
                List.iter md.Parameters.Add ps
                let methodInfo = {
                    paramList = Array.ofList mp
                    result = if rVar.IsSome then Some mRes else None
                    raw = md
                }
                let ms = methodInfo, ref []
                ctx.NewSymbols.Add(methodName, ms |> Referenced |> MethodSym)
                ms, newMethodSymbols, rVar
        let methodBuilder = (fst methodSym).raw :?> MethodDefinition
        match d with
        | BodyDeclr (decls, stmts) ->
            let scope = LocalScope(ctx.Inner (StandaloneMethod methodSym, newMethodSymbols))
            match Ctx.BuildIl(Block.Create(decls, stmts),scope,("result",rVar)) with
            | Ok res ->
                let mainBlock: MethodDefinition = Ctx.CompileBlock methodBuilder ctx.details.tb res
                mainBlock.Body.InitLocals <- true
                // https://github.com/jbevain/cecil/issues/365
                mainBlock.Body.OptimizeMacros()
            | Error _ -> ()
        | ExternalDeclr (lib, procName) ->
            let libRef = ModuleReference(lib)
            ctx.details.moduleBuilder.ModuleReferences.Add(libRef)
            let externalAttributes = MethodAttributes.HideBySig ||| MethodAttributes.PInvokeImpl
            methodBuilder.Attributes <- methodBuilder.Attributes ||| externalAttributes
            methodBuilder.IsPreserveSig <- true // as is

            // https://stackoverflow.com/questions/7255936/how-to-create-exported-functions-in-mono-cecil
            methodBuilder.PInvokeInfo <-
                         let flags = PInvokeAttributes.CharSetAnsi
                                 ||| PInvokeAttributes.SupportsLastError ||| PInvokeAttributes.CallConvWinapi
                         PInvokeInfo(flags, procName, libRef)
            ctx.details.tb.Methods.Add(methodBuilder)
        | ForwardDeclr ->
            ctx.forward.Add(name, (methodSym, newMethodSymbols, rVar))

    let doDecl ctx decl =
        match decl with
        | Types types -> declTypes types
        | Variables variables -> declVariables variables
        | Constants consts -> declConstants consts
        | Labels labels -> declLabels labels
        | ProcAndFunc proc -> declProcAndFunc proc
        <| ctx

[<AutoOpen>]
module LangBuilder =

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
        match ctx.symbols.Head with
        | GlobalSpace, _ -> ctx.res.Add(InstructionList([+Call(ctx.FindMethodReference "InitSystem")]))
        | _ -> ()
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

    type Ctx with
        static member CompileBlock (methodBuilder: MethodDefinition) (typeBuilder : TypeDefinition) (instr: List<MetaInstruction>) =
            let ilGenerator = methodBuilder.Body.GetILProcessor() |> emit
            typeBuilder.Methods.Add(methodBuilder)
            Seq.iter ilGenerator instr
            methodBuilder

        static member BuildIl(block: Block, buildScope, ?resVar) =
            let ctx = match buildScope with
                      | MainScope (ns, tb, s, mb) -> Ctx.Create mb ns tb GlobalSpace s.posMap
                      | LocalScope ctx -> ctx
            let result = match resVar with
                         | Some (name, Some(v)) ->
                            ctx.variables.Head.Add v
                            match v with
                            | LocalVariable v -> v
                            | _ -> null
                         | Some (_, None) -> null // no result (void)
                         | _ -> null // main program

            block.decl |> List.iter (doDecl ctx)
            // do implementation section only if interface section has no error
            let res = match ctx.errors.Count with
                      | 0 -> ValueSome(stmtListToIl block.stmt ctx result)
                      | _ -> ValueNone
            match ctx.errors.Count, res with
            | 0, ValueSome res -> Ok res
            | _ -> Error ctx

        static member BuildModule (ProgramAst(name, block)) state = //, methods: Method list) =
            let moduleName = match name with | Some n -> n | None -> "Program"
            let moduleNameWithoutExtension = System.IO.Path.GetFileNameWithoutExtension moduleName
            let assemblyBuilder =
                let assemblyName = AssemblyNameDefinition(moduleName, Version(0,0,0,0))
                AssemblyDefinition.CreateAssembly(assemblyName, moduleNameWithoutExtension, ModuleKind.Console)
            let moduleBuilder = assemblyBuilder.MainModule
            // for 32 bit assembly
            // moduleBuilder.Attributes <- ModuleAttributes.Required32Bit ||| moduleBuilder.Attributes

            let typeBuilder =
                let className = moduleName
                let typeAttributes =
                        TypeAttributes.Public
                        ||| TypeAttributes.Abstract
                        ||| TypeAttributes.Sealed
                        ||| TypeAttributes.AutoLayout
                        ||| TypeAttributes.AnsiClass
                        ||| TypeAttributes.BeforeFieldInit
                TypeDefinition(moduleName, className, typeAttributes, moduleBuilder.TypeSystem.Object)
            moduleBuilder.Types.Add(typeBuilder)
            let methodBuilder =
                let methodAttributes = MethodAttributes.Public ||| MethodAttributes.Static
                let methodName = "Main"
                MethodDefinition(methodName, methodAttributes, moduleBuilder.TypeSystem.Void)
            match Ctx.BuildIl(block, MainScope(moduleName, typeBuilder, state, moduleBuilder)) with
            | Error ctx -> Error ctx.errors
            | Ok res ->
                let mainBlock = Ctx.CompileBlock methodBuilder typeBuilder res
                mainBlock.Body.InitLocals <- true
                // https://github.com/jbevain/cecil/issues/365
                mainBlock.Body.OptimizeMacros()
                assemblyBuilder.EntryPoint <- mainBlock
                Ok assemblyBuilder
                // TODO version of target framework
                (*
                let v = moduleBuilder.ImportReference(typeof<TargetFrameworkAttribute>.GetConstructor([|typeof<string>|]));
                let c = CustomAttribute(v);
                let sr = moduleBuilder.ImportReference(typeof<string>)
                let ca = CustomAttributeArgument(sr, box ".NETCoreApp,Version=v3.0")
                c.ConstructorArguments.Add(ca)
                assemblyBuilder.CustomAttributes.Add(c)
                *)
