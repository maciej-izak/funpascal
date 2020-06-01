namespace Pas

open System
open System.Collections.Generic

open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks

[<AutoOpen>]
module rec Lang =

    let getVar4Assign (ctx: Ctx) (ident: DIdent) expr =
     // add param for findSymbol to set purpose (like this `assign`)
     match ctx.FindSymbol ident with
     | ChainLoad(symbols,_) ->
         let ltp = ref LTPNone
         let loadDest =
             let load = List.collect (ctx.ChainLoadToIl ltp (ctx.ChainReaderFactory false false)) symbols
             match symbols with // needed to proper store values to ref parameters in methods
             | VariableLoad(ParamVariable(RefVar, i),_)::[] -> +Ldarg i::load
             | _ -> load
         let ltp = !ltp
         let expr, exprType = expr (Some ltp.ToTypeRef)
         if not(Utils.typeCheck ctx ltp.PasType exprType) then
             ctx.NewError ident (sprintf "Incompatible types ('%O' and '%O') for \"%O\"" ltp.PasType.name exprType.name ident)
         [
             yield! loadDest
             yield! expr
             yield! ctx.ChainWriterFactory ltp
         ]
     | _ -> failwith "IE"

    let getVar4With (ctx: Ctx) idents =
        let ils = List<_>()
        List.fold
            (fun symbols i ->
                let loadVarW =
                    match ctx.FindSymbol i with
                    | ChainLoad (symbols, _) ->
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
                        ([
                            yield! cl
                            yield! ctx.ChainReaderFactory false true !ltp
                            +Stloc vv
                        ], (vv, vt.raw :?> TypeDefinition, pvt))
                    | _ -> failwith "IE"

                let newSymbols = Dictionary<_,_>()
                let (v, td, ptd) = snd loadVarW
                for f in td.Fields do
                    newSymbols.Add(StringName f.Name, WithSym(LocalVariable v, ptd))
                ils.Add(fst loadVarW)
                (WithSpace, newSymbols)::symbols
            ) ctx.symbols idents, ils

    let doIfStm (ctx: Ctx) (expr, tb, fb) =
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

    let doCaseStm (ctx: Ctx) (expr, mainLabels, stmt) =
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

    let doWhileStm (ctx: Ctx) (expr, stmt) =
        let condition = fst <| ctx.ExprToIl expr (Some ctx.sysTypes.boolean)
        let conditionLabel = ref (LazyLabel (condition.Head, nullRef()))
        // push loop context
        let breakLabel = ref ForwardLabel
        let continueLabel = ref (LazyLabel (condition.Head, nullRef()))
        ctx.loop.Push(continueLabel, breakLabel)
        let (whileBranch, whileLabels) = stmtListToIlList ctx stmt
        Ctx.resolveSysLabels (List.tryHead condition) whileLabels
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

    let doRepeatStm (ctx: Ctx) (stmt, expr) =
        let condition = fst <| ctx.ExprToIl expr (Some ctx.sysTypes.boolean)
        // push loop context
        let breakLabel = ref ForwardLabel
        let continueLabel = ref (LazyLabel(condition.Head,nullRef()))
        ctx.loop.Push(continueLabel, breakLabel)
        let (repeatBranch, whileLabels) = stmtListToIlList ctx stmt
        Ctx.resolveSysLabels (List.tryHead condition) whileLabels
        ctx.loop.Pop() |> ignore
        ([
            yield! repeatBranch
            yield! condition
            yield IlBranch(IlBrfalse,ref (LazyLabel
                                   ((match List.tryHead repeatBranch with
                                    | Some h -> h
                                    | _ -> condition.[0]),nullRef())))
        ],[breakLabel])

    let doForStm (ctx: Ctx) (ident, initExpr, delta, finiExpr, stmt) =
        let var, varType = ctx.FindSymbol ident |> function
                             | ChainLoad([VariableLoad(vs, vt)], _) -> vs, vt
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
        Ctx.resolveSysLabels (List.tryHead incLoopVar) forLabels
        ctx.loop.Pop() |> ignore
        ([
            yield! loopInitializeVariables
            yield! loopInitialize
            yield! forBranch
            yield! incLoopVar
            yield! condition
        ],[breakLabel])

    let rec stmtToIl (ctx: Ctx) sysLabels (s: Statement): (MetaInstruction * BranchLabel ref list) =
        let stmtToIlList = stmtListToIlList ctx
        let exprToIl = ctx.ExprToIl

        let (instructions, newSysLabels) =
                match s with
                | CallStm ce -> (fst (ctx.DoCall ce true), [])
                | IdentStm i -> (fst (ctx.DoCall (CallExpr(i,[])) true), [])
                | AssignStm(ident, expr) -> (getVar4Assign ctx ident (exprToIl expr), [])
                | IfStm stm -> doIfStm ctx stm
                | LabelStm l -> ([],[ctx.findLabelUnsafe l])
                | GotoStm s -> ([IlBranch(IlBr,ctx.findLabelUnsafe s)],[]) // will do LazyLabel
                | CaseStm stm -> doCaseStm ctx stm
                | WhileStm stm -> doWhileStm ctx stm
                | RepeatStm stm -> doRepeatStm ctx stm
                | ForStm stm -> doForStm ctx stm
                | WithStm (idents, stmt) ->
                    let (withSymbols, ils) = getVar4With ctx idents
                    let newCtx = { ctx with symbols = withSymbols }
                    // TODO check if some part is unused in ils
                    let (branch, labels) = stmtListToIlList newCtx stmt
                    ([
                        for i in ils do yield! i
                        yield! branch
                    ],labels)
                | EmptyStm -> ([],[])
                | _ -> ([],[])
        // TODO fix peepholes about jump to next opcode
        Ctx.resolveSysLabels (List.tryHead instructions) sysLabels
        (instructions |> InstructionList, newSysLabels)

    let stmtListToIlList (ctx: Ctx) sl: (IlInstruction list * BranchLabel ref list) =
        let lastSysLabels = ref []
        let instructions = [
              for s in sl do
                let (instructions, sysLabels) = stmtToIl ctx !lastSysLabels s
                lastSysLabels := sysLabels
                yield! metaToIlList instructions
            ]
        (instructions, !lastSysLabels)

type BuildScope =
    | MainScope of (string * TypeDefinition * PasState)
    | LocalScope of Ctx

type IlBuilder(moduleBuilder: ModuleDefinition) = class
    let mb = moduleBuilder

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
                               Ctx.resolveSysLabels (Some returnBlock.Head) labels
                               None
                           | _ ->
                               let finallyBlock = [yield! finalizeVariables; +Endfinally]
                               Ctx.resolveSysLabels (Some finallyBlock.Head) labels
                               Some finallyBlock
        (instructions, finallyBlock, returnBlock)
        |> HandleFunction
        |> ctx.res.Add
        ctx.res

    static member CompileBlock (methodBuilder: MethodDefinition) (typeBuilder : TypeDefinition) (instr: List<MetaInstruction>) =
        let ilGenerator = methodBuilder.Body.GetILProcessor() |> emit
        typeBuilder.Methods.Add(methodBuilder)
        Seq.iter ilGenerator instr
        methodBuilder

    member self.BuildIl(block: Block, buildScope, ?resVar) =
        let ctx = match buildScope with
                  | MainScope (ns, tb, s) -> Ctx.Create moduleBuilder ns tb GlobalSpace s.posMap
                  | LocalScope ctx -> ctx
        let newSymbols = snd ctx.symbols.Head
        let result = match resVar with
                     | Some (name, Some(v)) ->
                        ctx.variables.Head.Add v
                        match v with
                        | LocalVariable v -> v
                        | _ -> null
                     | Some (_, None) -> null // no result (void)
                     | _ -> null // main p`rogram

        let pint32 = ctx.sysTypes.int32

        let addConst typ ce =
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
            match addConstAtom typ ce with
            | ConstTempValue(b, t) -> ConstValue(ctx.details.AddBytesConst b, t) // add final as static value
            | c -> c
            |> ConstSym

        block.decl
        |> List.iter 
               (function
               | Types types ->
                   (
                       for (name, decl) in types do
                           match decl with
                           | TypeAlias (_, origin) ->
                               let name = StringName name
                               let originType = match ctx.FindTypeId origin with | Some t -> t | _ -> failwith "IE not found"
                               {originType with name = name} |> ctx.AddType name
                           | TypePtr (count, typeId) ->
                               let typ = match ctx.FindTypeId typeId with | Some t -> t | _ -> failwith "IE not found"
                               ctx.AddTypePointer count typ.name (StringName name)
                           | TypeSet (_, typeId) ->
                               let typ = match ctx.FindTypeId typeId with | Some t -> t | _ -> failwith "IE not found"
                               ctx.AddTypeSet typ.name (StringName name)
                           | TypeEnum enumValues ->
                               let name = StringName name
                               let max = enumValues.Length // TODO get explicit max value
                               let enumType = ctx.AddType name {name=name;kind=TkOrd(OkEnumeration, OtULong(0, max));raw=pint32.raw}
                               enumValues |> List.iteri (fun i v -> newSymbols.Add (StringName v, EnumValueSym(i, enumType)))
                               enumType
                           | Record (packed, fields) ->
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
                                mb.Types.Add(td)
                                let name = StringName name
                                ctx.AddType name {name=name;kind=TkRecord(fieldsMap, size);raw=td}
                           | Array(ArrayDef(_, dimensions, tname)) -> ctx.AddTypeArray dimensions tname (StringName name)
                           | _ -> failwith "IE"
                           |> ignore
                   )
               | Variables v ->
                   let addVar =
                        let addVar vk = vk |> ctx.variables.Head.Add
                        match buildScope with
                        | LocalScope _ -> fun (vn, t: PasType) ->
                                            VariableDefinition t.raw
                                            |> LocalVariable
                                            |> fun vk ->
                                                newSymbols.Add(StringName vn, VariableSym(vk, t))
                                                addVar vk
                        | MainScope _ -> fun (v, t) ->
                                            let fd = FieldDefinition(v, FieldAttributes.Public ||| FieldAttributes.Static, t.raw)
                                            fd |> GlobalVariable
                                            |> fun vk ->
                                                newSymbols.Add(StringName v, VariableSym(vk,t))
                                                ctx.details.tb.Fields.Add fd
                                                addVar vk
                   v
                   |> List.collect
                          (fun (l, t) ->
                       let dt = ctx.GetInternalType t
                       l |> List.map (fun v -> v, dt))
                   |> List.iter addVar
               | Constants consts ->
                    for (name, ctype, value) in consts do
                        let typ = match ctype with | Some t -> Some(ctx.GetInternalType t) | _ -> None
                        newSymbols.Add(StringName name, addConst typ value)
               | Labels labels -> (for l in labels do ctx.labels.Head.Add(l, ref (UserLabel l)))
               | ProcAndFunc((name, mRes, mPara), d) ->
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
                           newSymbols.Add(methodName, ms |> Referenced |> MethodSym)
                           ms, newMethodSymbols, rVar
                   let methodBuilder = (fst methodSym).raw :?> MethodDefinition
                   match d with
                   | BodyDeclr (decls, stmts) ->
                       let scope = LocalScope(ctx.Inner (StandaloneMethod methodSym, newMethodSymbols))
                       let mainBlock = self.BuildIl(Block.Create(decls, stmts),scope,("result",rVar))
                                       |> IlBuilder.CompileBlock methodBuilder ctx.details.tb
                       mainBlock.Body.InitLocals <- true
                       // https://github.com/jbevain/cecil/issues/365
                       mainBlock.Body.OptimizeMacros()
                   | ExternalDeclr (lib, procName) ->
                       let libRef = ModuleReference(lib)
                       mb.ModuleReferences.Add(libRef)
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
                   | _ -> failwith "no body def"
               | _ -> ())
        let res = stmtListToIl block.stmt ctx result
        match buildScope with
        | MainScope _ ->
            if ctx.errors.Count > 0 then
                raise (CompilerFatalError ctx)
            else res
        | _ -> res
end