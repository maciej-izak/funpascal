module NP.PasIl

open System
open System.Collections.Generic
open System.Linq.Expressions
open System.Linq
open System.Reflection.Metadata

open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks
open Microsoft.FSharp.Linq
open NP.PasSymbols

let addTypeSetForEnum (ctx: Ctx) pasType name =
    let setType = addMetaType (snd ctx.symbols.Head) name {name=name;kind=TkSet(pasType);raw=ctx.sysTypes.setStorage.raw}
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

let (|CharacterType|_|) (ctx: Ctx) = function
    | StrType as t -> Some(CharacterType t)
    | ChrType -> Some(CharacterType ctx.sysTypes.string)
    | _ -> None

let inline toMap kvps =
    kvps
    |> Seq.map (|KeyValue|)
    |> Map.ofSeq

let rec brtoinstr l =
    match !l with
    | LazyLabel(instr,ref) ->
         match instr with
         | IlBranch(_, i) -> brtoinstr i |> fst
         | IlResolved(_, i) -> i
         | IlResolvedEx(_, i, _) -> i
         , ref
    | _ -> failwithf "IE"

and private atomInstr = function
    | AddInst      -> Instruction.Create(OpCodes.Add)
    | MultiplyInst -> Instruction.Create(OpCodes.Mul)
    | MinusInst    -> Instruction.Create(OpCodes.Sub)
    | DivideInst   -> Instruction.Create(OpCodes.Div)
    | AndInst      -> Instruction.Create(OpCodes.And)
    | OrInst       -> Instruction.Create(OpCodes.Or)
    | XorInst      -> Instruction.Create(OpCodes.Xor)
    | Rem          -> Instruction.Create(OpCodes.Rem)
    | ShlInst      -> Instruction.Create(OpCodes.Shl)
    | ShrInst      -> Instruction.Create(OpCodes.Shr)
    | NotInst      -> Instruction.Create(OpCodes.Not)
    | NegInst      -> Instruction.Create(OpCodes.Neg)
    | Call mi      -> Instruction.Create(OpCodes.Call, mi)
    | Ldc k        -> match k with
                      | LdcI4 v -> Instruction.Create(OpCodes.Ldc_I4, v)
                      | LdcI8 v -> Instruction.Create(OpCodes.Ldc_I8, v)
                      | LdcR4 v -> Instruction.Create(OpCodes.Ldc_R4, v)
                      | LdcR8 v -> Instruction.Create(OpCodes.Ldc_R8, v)
    | Ldsfld f     -> Instruction.Create(OpCodes.Ldsfld, f)
    | Ldsflda f    -> Instruction.Create(OpCodes.Ldsflda, f)
    | Ldarg i      -> Instruction.Create(OpCodes.Ldarg, i)
    | Ldarga i     -> Instruction.Create(OpCodes.Ldarga, i)
    | Ldloc i      -> Instruction.Create(OpCodes.Ldloc, i)
    | Ldloca i     -> Instruction.Create(OpCodes.Ldloca, i)
    | Ldfld f      -> Instruction.Create(OpCodes.Ldfld, f)
    | Ldflda f     -> Instruction.Create(OpCodes.Ldflda, f)
    | Ldind it     -> match it with
                      | Ind_I -> Instruction.Create(OpCodes.Ldind_I)
                      | Ind_I1 -> Instruction.Create(OpCodes.Ldind_I1)
                      | Ind_I2 -> Instruction.Create(OpCodes.Ldind_I2)
                      | Ind_I4 -> Instruction.Create(OpCodes.Ldind_I4)
                      | Ind_I8 -> Instruction.Create(OpCodes.Ldind_I8)
                      | Ind_U -> Instruction.Create(OpCodes.Ldind_I)
                      | Ind_U1 -> Instruction.Create(OpCodes.Ldind_U1)
                      | Ind_U2 -> Instruction.Create(OpCodes.Ldind_U2)
                      | Ind_U4 -> Instruction.Create(OpCodes.Ldind_U4)
                      | Ind_U8 -> Instruction.Create(OpCodes.Ldind_I8)
                      | Ind_R4 -> Instruction.Create(OpCodes.Ldind_R4)
                      | Ind_R8 -> Instruction.Create(OpCodes.Ldind_R8)
                      | Ind_Ref -> Instruction.Create(OpCodes.Ldind_Ref)
    | Ldobj t      -> Instruction.Create(OpCodes.Ldobj, t)
    | Ldnull       -> Instruction.Create(OpCodes.Ldnull)
    | Stsfld f     -> Instruction.Create(OpCodes.Stsfld, f)
    | Starg i      -> Instruction.Create(OpCodes.Starg, i)
    | Stloc i      -> Instruction.Create(OpCodes.Stloc, i)
    | Stfld f      -> Instruction.Create(OpCodes.Stfld, f)
    | Stind it     -> match it with
                      | Ind_I -> Instruction.Create(OpCodes.Stind_I)
                      | Ind_I1 -> Instruction.Create(OpCodes.Stind_I1)
                      | Ind_I2 -> Instruction.Create(OpCodes.Stind_I2)
                      | Ind_I4 -> Instruction.Create(OpCodes.Stind_I4)
                      | Ind_I8 -> Instruction.Create(OpCodes.Stind_I8)
                      | Ind_U -> Instruction.Create(OpCodes.Stind_I)
                      | Ind_U1 -> Instruction.Create(OpCodes.Stind_I1)
                      | Ind_U2 -> Instruction.Create(OpCodes.Stind_I2)
                      | Ind_U4 -> Instruction.Create(OpCodes.Stind_I4)
                      | Ind_U8 -> Instruction.Create(OpCodes.Stind_I8)
                      | Ind_R4 -> Instruction.Create(OpCodes.Stind_R4)
                      | Ind_R8 -> Instruction.Create(OpCodes.Stind_R8)
                      | Ind_Ref -> Instruction.Create(OpCodes.Stind_Ref)
    | Conv ck      -> match ck with
                      | Conv_I  -> Instruction.Create(OpCodes.Conv_I)
                      | Conv_I1 -> Instruction.Create(OpCodes.Conv_I1)
                      | Conv_I2 -> Instruction.Create(OpCodes.Conv_I2)
                      | Conv_I4 -> Instruction.Create(OpCodes.Conv_I4)
                      | Conv_I8 -> Instruction.Create(OpCodes.Conv_I8)
                      | Conv_U  -> Instruction.Create(OpCodes.Conv_U)
                      | Conv_U1 -> Instruction.Create(OpCodes.Conv_U1)
                      | Conv_U2 -> Instruction.Create(OpCodes.Conv_U2)
                      | Conv_U4 -> Instruction.Create(OpCodes.Conv_U4)
                      | Conv_U8 -> Instruction.Create(OpCodes.Conv_U8)
                      | Conv_R4 -> Instruction.Create(OpCodes.Conv_R4)
                      | Conv_R8 -> Instruction.Create(OpCodes.Conv_R8)
    | Cpblk        -> Instruction.Create(OpCodes.Cpblk)
    | Cpobj t      -> Instruction.Create(OpCodes.Cpobj, t)
    | Stobj t      -> Instruction.Create(OpCodes.Stobj, t)
    | Initobj t    -> Instruction.Create(OpCodes.Initobj, t)
    | Newarr e     -> Instruction.Create(OpCodes.Newarr, e)
    | Stelem ek    -> match ek with
                      | Elem e   -> Instruction.Create(OpCodes.Stelem_Any, e)
                      | Elem_I   -> Instruction.Create(OpCodes.Stelem_I)
                      | Elem_I1  -> Instruction.Create(OpCodes.Stelem_I1)
                      | Elem_I2  -> Instruction.Create(OpCodes.Stelem_I2)
                      | Elem_I4  -> Instruction.Create(OpCodes.Stelem_I4)
                      | Elem_I8  -> Instruction.Create(OpCodes.Stelem_I8)
                      | Elem_U   -> Instruction.Create(OpCodes.Stelem_I)
                      | Elem_U1  -> Instruction.Create(OpCodes.Stelem_I1)
                      | Elem_U2  -> Instruction.Create(OpCodes.Stelem_I2)
                      | Elem_U4  -> Instruction.Create(OpCodes.Stelem_I4)
                      | Elem_U8  -> Instruction.Create(OpCodes.Stelem_I8)
                      | Elem_R4  -> Instruction.Create(OpCodes.Stelem_R4)
                      | Elem_R8  -> Instruction.Create(OpCodes.Stelem_R8)
                      | Elem_Ref -> Instruction.Create(OpCodes.Stelem_Ref)
    | Unaligned i  -> Instruction.Create(OpCodes.Unaligned, i)
    | Box t        -> Instruction.Create(OpCodes.Box, t)
    | Dup          -> Instruction.Create(OpCodes.Dup)
    | Pop          -> Instruction.Create(OpCodes.Pop)
    | Ret          -> Instruction.Create(OpCodes.Ret)
    | Endfinally   -> Instruction.Create(OpCodes.Endfinally)
    | Leave i      -> Instruction.Create(OpCodes.Leave, i)
    | Unknown      -> Instruction.Create(OpCodes.Nop)
    | Ceq          -> Instruction.Create(OpCodes.Ceq)
    | Clt          -> Instruction.Create(OpCodes.Clt)
    | Cgt          -> Instruction.Create(OpCodes.Cgt)
    | Nop          -> Instruction.Create(OpCodes.Nop)
    | Ldstr s      -> Instruction.Create(OpCodes.Ldstr, s)
    | Brfalse i    -> Instruction.Create(OpCodes.Brfalse, i)
    | Br i         -> Instruction.Create(OpCodes.Br, i)
    | Beq i        -> Instruction.Create(OpCodes.Beq, i)
    | Blt i        -> Instruction.Create(OpCodes.Blt, i)
    | Bgt i        -> Instruction.Create(OpCodes.Bgt, i)
    | Ble i        -> Instruction.Create(OpCodes.Ble, i)
    | Bge i        -> Instruction.Create(OpCodes.Bge, i)
    | Bne_Un i     -> Instruction.Create(OpCodes.Bne_Un, i)
    | Bgt_Un i     -> Instruction.Create(OpCodes.Bgt_Un, i)
    | Blt_Un i     -> Instruction.Create(OpCodes.Blt_Un, i)

let private instr = function
    | IlBranch(bk, i) ->
        match bk with
        | IlBrfalse -> OpCodes.Brfalse
        | IlBrtrue  -> OpCodes.Brtrue
        | IlBr      -> OpCodes.Br
        | IlBeq     -> OpCodes.Beq
        | IlBgt     -> OpCodes.Bgt
        | IlBlt     -> OpCodes.Blt
        | IlBle     -> OpCodes.Ble
        | IlBge     -> OpCodes.Bge
        | IlBne_Un  -> OpCodes.Bne_Un
        | IlBgt_Un  -> OpCodes.Bgt_Un
        | IlBlt_Un  -> OpCodes.Blt_Un
        |> fun opc ->
            let i, ref = brtoinstr i
            ref := Instruction.Create(opc, i)
            !ref
    | IlResolved(_,i) -> i
    | IlResolvedEx(_,i,_) -> i

let (~+) (i: AtomInstruction) = IlResolved(i, i |> atomInstr)
let (~+.) (i: AtomInstruction) = IlResolvedEx(i, i |> atomInstr, ref [])

let metaToIlList = function
    | InstructionList l -> l
    // may be extended for inline variables in the future
    | _ -> failwith "IE not supported metaToIlList"

let private emit (ilg : Cil.ILProcessor) inst =
    match inst with
    | DeclareLocal t -> t |> ilg.Body.Variables.Add
    | InstructionList p -> p |> List.iter (instr >> ilg.Append)
    | HandleFunction (instructionsBlock, finallyBlock, endOfAll) ->
        let appendAndReplaceRetGen (brOpcode: OpCode) (goto: Instruction) (il: IlInstruction) =
            match il with
            | IlResolvedEx(_,i,_) when i.OpCode = OpCodes.Ret -> il.FixRefs <| Instruction.Create(brOpcode, goto)
            | _ -> il |> instr
            |> (fun i -> ilg.Append i; i)

        let finallyBlock, appendAndReplaceRet = match finallyBlock with
                                                | Some block -> block, appendAndReplaceRetGen OpCodes.Leave
                                                | None -> [], appendAndReplaceRetGen OpCodes.Br
        let beginOfEnd = endOfAll.Head |> instr
        let processList replaceFun = function
            | head::tail ->
                let result = head |> replaceFun
                tail |> List.iter (replaceFun >> ignore)
                result
            | [] -> beginOfEnd
        let replaceRet = appendAndReplaceRet beginOfEnd
        let start = processList replaceRet instructionsBlock
        if List.isEmpty finallyBlock = false then
            if ilg.Body.Instructions.Last().OpCode <> OpCodes.Leave then
                ilg.Append(Instruction.Create(OpCodes.Leave, beginOfEnd))
            let startFinally = processList replaceRet finallyBlock
            ExceptionHandler(ExceptionHandlerType.Finally,
                                           TryStart     = start,
                                           TryEnd       = startFinally,
                                           HandlerStart = startFinally,
                                           HandlerEnd   = beginOfEnd)
            |> ilg.Body.ExceptionHandlers.Add
        ilg.Append beginOfEnd
        List.iter (instr >> ilg.Append) endOfAll.Tail

let derefType (t: PasType) =
    match t.kind with
    | TkPointer t -> t
    | _ -> failwith "Cannot dereference non pointer type"

let findSymbol (ctx: Ctx) (DIdent ident) =
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
        ChainLoad(VariableLoad(vd)::tail, Some finalType)

    let mainSym = defaultArg mainSym UnknownSym

    match mainSym with
    | VariableSym (v, t as vt) -> vt |> varLoadChain t ident.Tail
    | WithSym (v, t as vt) -> vt |> varLoadChain t ident
    | EnumValueSym(i, t) when ident.Tail = [] -> ChainLoad([ValueLoad(ValueInt(i))], Some t)
    | MethodSym m ->  ChainLoad([CallableLoad m], m.ReturnType)
    | ConstSym v ->
        match v with
        | ConstInt i -> ChainLoad([ValueLoad(ValueInt i)], Some ctx.sysTypes.int32)
        | ConstFloat f -> ChainLoad([ValueLoad(ValueFloat f)], Some ctx.sysTypes.single)
        | ConstValue(fd, pt) -> (GlobalVariable fd, pt) |> varLoadChain pt ident.Tail
        | ConstBool b ->
            let i = int b
            ChainLoad([ValueLoad(ValueInt i)], Some ctx.sysTypes.boolean)
        | _ -> failwith "IE"
    | TypeSym t -> ChainLoad([TypeCastLoad t], Some t)
    | _ -> SymbolLoadError

let (|VariablePasType|_|) ctx id =
    match findSymbol ctx id |> chainToSLList with
    | _, Some(t) -> Some(VariablePasType t)
    | _ -> None

let (|TypePasType|_|) (ctx: Ctx) = function
    | DIdent[Ident(id)] ->
        match ctx.FindSym(StringName id) with
        | Some (TypeSym t) -> Some(TypePasType t)
        | _ -> None
    | _ -> None

let findMethodReferenceOpt ctx =
    stdIdent >> findSymbol ctx >> chainToSLList >>
    function | [CallableLoad(Referenced({raw=mr}, _))], _ -> Some mr | _ -> None

let findMethodReferenceUnsafe ctx = findMethodReferenceOpt ctx >> function | Some r -> r | _ -> null
let findMethodReference ctx = findMethodReferenceOpt ctx >> function | Some r -> r | _ -> failwith "IE"

let findConstSym ctx =
    findSymbol ctx >> chainToSLList >>
    function | [ValueLoad(v)], Some(t) -> v, t | _ -> failwith "IE"

type LastTypePoint =
    | LTPVar of VariableKind * PasType
    | LTPDeref of PasType * bool
    | LTPStruct of FieldDefinition * PasType
    | LTPNone
with
    member self.ToTypeRef =
        match self with
        | LTPVar(_,tr) -> tr
        | LTPDeref(tr, _) -> tr
        | LTPStruct(_, tr) -> tr
        | _ -> failwith "IE"

    member self.PasType =
        match self with
        | LTPVar(_, pt) -> pt
        | LTPDeref(pt, _) -> pt
        | LTPStruct(_, pt) -> pt
        | _ -> failwith "IE"

type FoundFunction =
    | RealFunction of (MethodSym * IlInstruction option)
    | TypeCast of PasType

let findFunction (ctx: Ctx) ident =
        let callChain = findSymbol ctx ident
        // TODO more advanced calls like foo().x().z^ := 10
        match callChain with
        | ChainLoad([CallableLoad cl], _) ->
           match cl with
           | Referenced ({raw=mr}, _) -> RealFunction(cl, +Call(mr) |> Some)
           | Intrinsic _ -> RealFunction(cl, None)
        | ChainLoad([TypeCastLoad t], _) -> TypeCast t
        | _ -> failwith "Not supported"

type ValueKind = ValueKind of (IlInstruction list * PasType)

let Ldc_I4 i = LdcI4 i |> Ldc
let Ldc_U1 (i: byte) = [
    +Ldc (LdcI4 (int i))
    +Conv Conv_U1
]
let Ldc_R4 r = LdcR4 r |> Ldc

let typeRefToConv (r: TypeReference) =
    match r.MetadataType with
    | MetadataType.Pointer -> +Conv Conv_U
    | MetadataType.SByte   -> +Conv Conv_I1
    | MetadataType.Int16   -> +Conv Conv_I2
    | MetadataType.Int32   -> +Conv Conv_I4
    | MetadataType.Int64   -> +Conv Conv_I8
    | MetadataType.Byte    -> +Conv Conv_U1
    | MetadataType.Boolean -> +Conv Conv_U1
    | MetadataType.Single  -> +Conv Conv_R4
    | MetadataType.UInt16  -> +Conv Conv_U2
    | MetadataType.UInt32  -> +Conv Conv_U4
    | MetadataType.UInt64  -> +Conv Conv_U8
    | _ -> failwith "IE"

let useHelperOp ctx helperName valueType byRef =
    let _,v = (ctx: Ctx).EnsureVariable(valueType)
    [
        +Ldloca v
        +Call(findMethodReference ctx helperName)
        +(if byRef then Ldloca else Ldloc) v
    ]

let handleOperator (ctx: Ctx) et byRef (op, (ils, at, bt)) =
    let convCmpOp = function | Cgt -> Clt | Clt -> Cgt | o -> o
    match at, bt, op with
    | ErrorType, _, _ | _, ErrorType, _ -> []
    | _, _, InInst -> [yield! ils; +Call(findMethodReference ctx "INSET")]
    | (StrType as t), StrType, AddInst -> ils @ useHelperOp ctx "ConcatStr" t byRef
    | StrType, StrType, BoolOp -> [+Ldc_I4 0; yield! ils; +Call(findMethodReference ctx "CompareStr"); +convCmpOp op]
    | (SetType at), ((SetType bt) as t), AddInst when at = bt -> ils @ useHelperOp ctx "SetUnion" t byRef
    | (SetType at), ((SetType bt) as t), MinusInst when at = bt -> ils @ useHelperOp ctx "SetDifference" t byRef
    | NumericType, NumericType, op -> [yield! ils; +op]
    | _ -> failwith "IE"
    ,match et with | Some t -> t | _ -> at

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

let rec exprToIlGen refRes (ctx: Ctx) exprEl expectedType =
    let rec exprToMetaExpr (el: ExprEl) et refRes =
        let constConvertTo op (aExpr: ExprEl, aVal, aTyp) (bExpr: ExprEl, bVal, bTyp) =
            let typesConverter (expr: ExprEl) aVal aTyp bTyp =
                match aTyp, bTyp with
                | ChrType, CharacterType ctx (t) ->
                    exprToMetaExpr expr (Some t) false
                    |> (function | ValueKind t -> t)
                | _ -> aVal, aTyp
            op,
            match op with
            | InInst ->
                (aVal, aTyp), (exprToMetaExpr bExpr (Some bTyp) true |> (function | ValueKind t -> t))
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
            match exprToMetaExpr aExpr et false with
            | ValueKind(a, at) ->
                match exprToMetaExpr bExpr (if i = InInst then None else Some at) false with
                | ValueKind(b, bt) ->
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
            match exprToMetaExpr a et false with
            | ValueKind(a, at) -> [ yield! a; yield +i; yield typeRefToConv at.raw], at
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

    match exprToMetaExpr exprEl expectedType refRes with
    | ValueKind (a, at) ->
        [
         yield! a
         if expectedType.IsSome then
            let typ = expectedType.Value
            match typ.kind with | TkOrd _ | TkFloat _ -> yield typeRefToConv typ.raw | _ -> ()
        ], at

and exprToIl = exprToIlGen false

and callParamToIl ctx cp (idxmr: Option<int * MethodInfo>) =
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
            if not(typeCheck ctx param.Value t) then
                ctx.NewError cp (sprintf "Incompatible types ('%O' and '%O') for %d parameter" param.Value.name t.name (fst idxmr.Value))
        (il, t)

and doCall (ctx: Ctx) (CallExpr(ident, cp)) popResult =
    match findFunction ctx ident with
    | TypeCast t ->
        let cp = match cp with | [cp] -> cp | _ -> failwith "IE only one param allowed"
        let callInstr, typ = callParamToIl ctx cp None
        // TODO some real conversion ? Conv_I4 + explicit operators?
        ([
            yield! callInstr
            typeRefToConv t.raw
        ], Some t)
    | RealFunction rf ->
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
                if popResult && mr.result.IsSome then
                    yield +Pop
             ], mr.result)
        | Intrinsic i, _ ->
            let doWrite() =
                let file, cp =
                    match cp with
                    | ParamIdent id::tail ->
                        let sl, typ = findSymbolAndGetPtr ctx id
                        if typ.raw = ctx.sysTypes.file.raw then Some sl, tail
                        else None, cp
                    | _ -> None, cp
                let file() = if file.IsNone then fst <| findSymbolAndGetPtr ctx (stdIdent "STDOUTPUTFILE")
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
                        |> findMethodReferenceOpt ctx |>
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

            let doRead() =
                let file, cp =
                    match cp with
                    | ParamIdent id::tail ->
                        let sl, typ = findSymbolAndGetPtr ctx id
                        if typ.raw = ctx.sysTypes.file.raw then Some sl, tail
                        else None, cp
                    | _ -> None, cp
                let file() = if file.IsNone then fst <| findSymbolAndGetPtr ctx (stdIdent "STDINPUTFILE")
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
                                       |> findMethodReference ctx
                        [
                            yield! file()
                            +Ldnull
                            yield! valParam
                            +Call subWrite
                        ]
                    | _ -> failwith "IE"
                file, cp |> List.collect doParam

            let deltaModify delta id =
                let inst, typ = findSymbolAndGetPtr ctx id
                let indKind = typ.IndKind
                ([ // TODO change indirect to normal load/store?
                 yield! inst
                 +Dup
                 +Ldind indKind
                 +Ldc_I4 delta
                 +AddInst
                 +Stind indKind
                ], None)
            let deltaAdd delta cp =
                let inst, typ = callParamToIl ctx cp None
                ([ // TODO check type -> should be ordinal
                 yield! inst
                 +Ldc_I4 delta
                 +AddInst
                ], Some typ)
            match i, cp with
            | IncProc, [ParamIdent id] -> deltaModify +1 id
            | DecProc, [ParamIdent id] -> deltaModify -1 id
            | SuccProc, [cp] -> deltaAdd +1 cp
            | PredProc, [cp] -> deltaAdd -1 cp
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
            | WriteProc, _ ->
                let _, writeParams = doWrite()
                (writeParams, None)
            | WriteLnProc, _ ->
                let file, writeParams = doWrite()
                ([
                    yield! writeParams
                    yield! file()
                    +Ldnull
                    +Call(findMethodReference ctx "WRITENEWLINE")
                 ], None)
            | ReadProc, _ ->
                let _, readParams = doRead()
                (readParams, None)
            | ReadLnProc, _ ->
                let file, readParams = doRead()
                ([
                    yield! readParams
                    yield! file()
                    +Ldnull
                    +Call(findMethodReference ctx "READNEWLINE")
                 ], None)
            | WriteLineProc, _ ->
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
            | HaltProc, _ ->
                let exitCode = match cp with
                               | [] -> [+Ldc_I4 0]
                               | [cp] -> fst <| callParamToIl ctx cp None // TODO typecheck ?
                               | _ -> failwith "IE only one param allowed"
                ([
                     yield! exitCode
                     +Call(findMethodReference ctx "EXITPROCESS")
                     // +Call ctx.sysProc.Exit
                ], None)
            | ChrFunc, _ ->
                let cp = match cp with | [cp] -> cp | _ -> failwith "IE only one param allowed"
                let callInstr, typ = callParamToIl ctx cp None
                ([
                    yield! callInstr
                    if popResult then yield +Pop // TODO or not generate call ?
                 ], Some ctx.sysTypes.char)
            | OrdFunc, _ ->
                let cp = match cp with | [cp] -> cp | _ -> failwith "IE only one param allowed"
                let callInstr, typ = callParamToIl ctx cp None
                ([
                    yield! callInstr
                    if popResult then yield +Pop // TODO or not generate call ?
                 ], Some ctx.sysTypes.int32)
            | TruncFunc, _ ->
                match cp with
                | [cp] ->
                    let callInstr, typ = callParamToIl ctx cp None
                    match typ with
                    | FloatType | IntType ->
                        ([
                            yield! callInstr
                            match popResult with
                            | true  -> +Pop
                            | false -> +Conv Conv_I8
                         ], Some ctx.sysTypes.int64)
                    | _ ->
                        ctx.NewError cp (sprintf "Expected float type but '%O' found" typ.name)
                        ([], Some ctx.sysTypes.int64)
                | _ ->
                    ctx.NewError ident (sprintf "Expected 1 parameter but found %d" (cp.Length))
                    ([], Some ctx.sysTypes.int64)
            | RoundFunc, _ ->
                match cp with
                | [cp] ->
                    let callInstr, typ = callParamToIl ctx cp None
                    match typ with
                    | FloatType | IntType ->
                        ([
                            yield! callInstr
                            +Call ctx.sysProc.Round
                            match popResult with
                            | true  -> +Pop
                            | false -> +Conv Conv_I8
                         ], Some ctx.sysTypes.int64)
                    | _ ->
                        ctx.NewError cp (sprintf "Expected float type but '%O' found" typ.name)
                        ([], Some ctx.sysTypes.int64)
                | _ ->
                    ctx.NewError ident (sprintf "Expected 1 parameter but found %d" (cp.Length))
                    ([], Some ctx.sysTypes.int64)
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
        | _ -> failwith "IE"
    | _ -> failwith "IE"

and inline eval1 ctx typ e1 = evalConstExpr ctx typ e1

and SetValueToCER ctx typ al =
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
            | _ -> addTypeSetForEnum ctx enumTyp AnonName
        CEROrdSet(finalSet, typeSet)
    | _ -> failwith "IE"

and valueToValueKind ctx v (expectedType: PasType option) byRef =
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
        let bytes, ft = match SetValueToCER ctx expectedType al with
                        | CEROrdSet(b,t) -> b, t
                        | _ -> failwith "IE"
        let fd = ctx.details.AddBytesConst bytes
        [+(if byRef then Ldsflda fd else Ldsfld fd)], ft

and chainReaderFactory (ctx: Ctx) asValue addr ltp =
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

and chainWriterFactory (ctx: Ctx) = function
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

and derefLastTypePoint = function
    | LTPVar(_, t) -> derefType t
    | LTPDeref(t, _) -> derefType t
    | LTPStruct(_, t) -> derefType t
    | _ -> failwith "cannot deref"

and loadSymList (ctx: Ctx) value addr (sl, t) =
    let lastPoint = ref LTPNone
    sl
    |> List.collect (chainLoadToIl ctx lastPoint (chainReaderFactory ctx false false))
    |> fun l -> l @ (chainReaderFactory ctx value addr !lastPoint)
    ,match t with | Some t -> t | _ -> failwith "IE"

and findSymbolInternal value addr (ctx: Ctx) ident =
    findSymbol ctx ident
    |> chainToSLList
    |> loadSymList ctx value addr

and findSymbolAndLoad = findSymbolInternal true false

and findSymbolAndGetPtr = findSymbolInternal false true

and chainLoadToIl ctx lastType factory symload =
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

and evalConstExpr (ctx: Ctx) typ expr =

    let inline eval2 e1 e2 = (evalConstExpr ctx typ e1, evalConstExpr ctx typ e2)
    let inline eval1 e1 = eval1 ctx typ e1

    match expr with
    | Value v ->
        match v with
        | VFloat f -> CERFloat <| single f
        | VInteger i -> CERInt(i, ctx.sysTypes.int32)
        | VString s -> CERString s
        | VIdent id ->
            match findConstSym ctx id with
            | ValueInt i, t ->
                // TODO Type check with typ
                match t with
                | EnumType -> CERInt(i, t)
                | _ -> CERInt(i, ctx.sysTypes.int32)
            | _ -> failwith "IE"
        | VCallResult _ -> CERUnknown
        | VNil -> CERUnknown
        | VSet al -> SetValueToCER ctx typ al
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

let compileBlock (methodBuilder: MethodDefinition) (typeBuilder : TypeDefinition) (instr: List<MetaInstruction>) =
    let ilGenerator = methodBuilder.Body.GetILProcessor() |> emit
    typeBuilder.Methods.Add(methodBuilder)
    Seq.iter ilGenerator instr
    methodBuilder

let simplifiedDIdent = List.map <| function | Ident s -> s | _ -> failwith "IE"
let inline packedToStr(p: bool) = if p then "1" else "0"

let evalConstExprToStr (ctx: Ctx) = function
    | ConstExpr expr ->
        match evalConstExpr ctx (Some ctx.sysTypes.string) expr with
        | CERString s -> s
        | CERInt(i,_) -> string i
        | CERUnknown -> ""
        | _ -> failwith "IE"
    | _ -> failwith "IE"

let dimenstionsToStr ctx = List.map <| function | DimensionType s -> s | DimensionExpr (e1, e2) -> evalConstExprToStr ctx e1 + ".." + evalConstExprToStr ctx e2

let (|IlNotEqual|_|) (items: IlInstruction list) =
    if items.Length < 3 then
        None
    else
        let last3 = items.[items.Length-4..]
                    (*
    | NotEqual(a, b) -> [|yield! add2OpIl a b Ceq; ilResolve (Ldc_I4(0)); ilResolve Ceq|]
    | StrictlyLessThan(a, b) -> add2OpIl a b Clt
    | StrictlyGreaterThan(a, b) -> add2OpIl a b Cgt
    | LessThanOrEqual(a, b) -> [|yield! add2OpIl a b Cgt ; ilResolve (Ldc_I4(0)); ilResolve Ceq|]
    | GreaterThanOrEqual(a, b) -> [|yield! add2OpIl a b Clt; ilResolve (Ldc_I4(0)); ilResolve Ceq|]
                    *)
        match ilToAtom last3 with
        | [Ceq;Ldc(LdcI4 _);Ceq] -> Some(IlNotEqual)
        | _ -> None

type BuildScope =
    | MainScope of (string * TypeDefinition * PasState)
    | LocalScope of Ctx

type IlBuilder(moduleBuilder: ModuleDefinition) = class
    let mb = moduleBuilder

    let rec stmtToIl (ctx: Ctx) sysLabels (s: Statement): (MetaInstruction * BranchLabel ref list) =
        let stmtToIlList = stmtListToIlList ctx
        let exprToIl = exprToIl ctx
        let getVar4ForLoop = findSymbol ctx >>
                             function
                             | ChainLoad([VariableLoad(vs, vt)], _) -> vs, vt
                             | _ -> failwith "IE"
        let getVar4Assign (ident: DIdent) expr =
             // add param for findSymbol to set purpose (like this `assign`)
             match findSymbol ctx ident with
             | ChainLoad(symbols,_) ->
                 let ltp = ref LTPNone
                 let loadDest =
                     let load = List.collect (chainLoadToIl ctx ltp (chainReaderFactory ctx false false)) symbols
                     match symbols with // needed to proper store values to ref parameters in methods
                     | VariableLoad(ParamVariable(RefVar, i),_)::[] -> +Ldarg i::load
                     | _ -> load
                 let ltp = !ltp
                 let expr, exprType = expr (Some ltp.ToTypeRef)
                 if not(typeCheck ctx ltp.PasType exprType) then
                     ctx.NewError ident (sprintf "Incompatible types ('%O' and '%O') for \"%O\"" ltp.PasType.name exprType.name ident)
                 [
                     yield! loadDest
                     yield! expr
                     yield! chainWriterFactory ctx ltp
                 ]
             | _ -> failwith "IE"
        let getVar4With idents =
            let ils = List<_>()
            List.fold
                (fun symbols i ->
                    let loadVarW =
                        match findSymbol ctx i with
                        | ChainLoad (symbols, _) ->
                            let ltp = ref LTPNone
                            let cl = List.collect (chainLoadToIl ctx ltp (chainReaderFactory ctx false false)) symbols
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
                                yield! chainReaderFactory ctx false true !ltp
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

        let (instructions, newSysLabels) =
                match s with
                | CallStm ce -> (fst (doCall ctx ce true), [])
                | IdentStm i -> (fst (doCall ctx (CallExpr(i,[])) true), [])
                | AssignStm(ident, expr) ->
                    (getVar4Assign ident (exprToIl expr), [])
                | IfStm(expr, tb, fb) ->
                    // if logic
                    let firstEnfOfStm = ref ForwardLabel
                    let lastEndOfStm = ref ForwardLabel
                    let condition = fst <| exprToIl expr (Some ctx.sysTypes.boolean)
                    let (trueBranch, trueLabels) = stmtToIlList tb
                    let (falseBranch, falseLabels) = stmtToIlList fb
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
                | LabelStm l -> ([],[ctx.findLabelUnsafe l])
                | GotoStm s -> ([IlBranch(IlBr,ctx.findLabelUnsafe s)],[]) // will do LazyLabel
                | CaseStm (expr, mainLabels, stmt) ->
                    // let case = exprToIl expr
                    let lastEndOfStm = ref ForwardLabel
                    let (defBranch, defLabels) = stmtToIlList stmt
                    // TODO reduce creation of new var if we want to just read variable
                    let ilExpr, exprType = exprToIl expr None
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
                            let (caseBranch, caseLabels) = stmtToIlList stmt
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
                                             yield! (fst <| exprToIl ce None) // TODO type handle
                                             yield IlBranch(IlBeq,ref(LazyLabel(caseBranch.[0], nullRef())))
                                        | CaseRange(ConstExpr(ce1), ConstExpr(ce2)) ->
                                             // TODO check proper range
                                             // lower range
                                             let nextCase = ref ForwardLabel
                                             omitCase := Some nextCase
                                             yield! (fst <| exprToIl ce1 None) // TODO type handle
                                             yield IlBranch(IlBlt, nextCase)
                                             // higher range
                                             yield +Ldloc var
                                             yield! (fst <| exprToIl ce2 None) //TODO type handle
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
                | WhileStm (expr, stmt) ->
                    let condition = fst <| exprToIl expr (Some ctx.sysTypes.boolean)
                    let conditionLabel = ref (LazyLabel (condition.Head, nullRef()))
                    // push loop context
                    let breakLabel = ref ForwardLabel
                    let continueLabel = ref (LazyLabel (condition.Head, nullRef()))
                    ctx.loop.Push(continueLabel, breakLabel)
                    let (whileBranch, whileLabels) = stmtToIlList stmt
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
                | RepeatStm (stmt, expr) ->
                    let condition = fst <| exprToIl expr (Some ctx.sysTypes.boolean)
                    // push loop context
                    let breakLabel = ref ForwardLabel
                    let continueLabel = ref (LazyLabel(condition.Head,nullRef()))
                    ctx.loop.Push(continueLabel, breakLabel)
                    let (repeatBranch, whileLabels) = stmtToIlList stmt
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
                | ForStm (ident, initExpr, delta, finiExpr, stmt) ->
                    let var, varType = getVar4ForLoop ident
                    // TODO allow only specified kind of variables for loops
                    let (varFinalName, varFinal) = ctx.EnsureVariable varType
                    // TODO optimization for simple values (dont store in var)
                    let (loopInitializeVariables, _) =
                        [AssignStm(ident, initExpr);AssignStm(stdIdent varFinalName, finiExpr)] |> stmtToIlList
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
                    let (incLoopVar, _) = [AssignStm(ident, Add(Value(VIdent(ident)), Value(VInteger delta)))] |> stmtToIlList
                    // push loop context between
                    let continueLabel = ref (LazyLabel (incLoopVar.Head, nullRef()))
                    ctx.loop.Push(continueLabel, breakLabel)
                    let (forBranch, forLabels) = stmtToIlList stmt
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
                | WithStm (idents, stmt) ->
                    let (withSymbols, ils) = getVar4With idents
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

    and stmtListToIlList (ctx: Ctx) sl: (IlInstruction list * BranchLabel ref list) =
        let lastSysLabels = ref []
        let instructions = [
              for s in sl do
                let (instructions, sysLabels) = stmtToIl ctx !lastSysLabels s
                lastSysLabels := sysLabels
                yield! metaToIlList instructions
            ]
        (instructions, !lastSysLabels)

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
        | GlobalSpace, _ -> ctx.res.Add(InstructionList([+Call(findMethodReference ctx "InitSystem")]))
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

    member self.BuildIl(block: Block, buildScope, ?resVar) =
        let ctx = match buildScope with
                  | MainScope (ns, tb, s) ->
                    let langCtx = LangCtx()
                    let newSymbols = Dictionary<TypeName,_>(langCtx)
                    ModuleDetails.Create moduleBuilder ns tb
                    |> Ctx.Create GlobalSpace newSymbols langCtx s.posMap
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
        let addType = addMetaType newSymbols
        let addTypePointer count typeName name =
            let t =
                match ctx.FindType typeName with
                | Some t -> t
                | _ -> failwith "IE unknown type"
            let mutable pt = PointerType(t.raw)
            for i = 2 to count do pt <- PointerType(pt)
            addType name {name=name;kind=TkPointer(t);raw=pt:>TypeReference}

        let rec getInternalType t =
            let name = TypeName.FromTypeId t
            match ctx.FindType name with
            | Some t -> t
            | None -> // inline type?
                match t with
                | TIdPointer(count, typeId) -> addTypePointer count (TypeName.FromTypeId typeId) (TypedName t)
                | TIdArray(ArrayDef(_, dimensions, tname)) -> addTypeArray dimensions tname (TypedName t)
                | TIdSet(_, typeId) -> addTypeSet ctx (TypeName.FromTypeId typeId) (TypedName t)
                | _ -> ctx.NewError t (sprintf "Cannot find type identifier \"%O\"" t); ctx.sysTypes.unknown // raise (CompilerFatalError ctx)

        and addTypeArray dimensions tname name =
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
                        let typ = getInternalType t
                        dims, (typ, typ.SizeOf), typ

                let newArrayDim ad ((dims, totalSize), elemType, i, newTyp) =
                    match ad with
                    | DimensionExpr(ConstExpr(l),ConstExpr(h)) ->
                        let l = match evalConstExpr ctx (Some pint32) l with | CERInt(i,_) -> i | _ -> failwith "IE"
                        let h = match evalConstExpr ctx (Some pint32) h with | CERInt(i,_) -> i | _ -> failwith "IE"
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
            addType name typ
            //(doArrayDef dimensions tname (ref []) name).Name <- name

        let addConst typ ce =
            let rec addConstAtom pt ce =
                match pt, ce with
                | None, ConstExpr expr ->
                    match evalConstExpr ctx None expr with
                    | CERInt(i, _) -> ConstInt i
                    | CERFloat f -> ConstFloat f
                    | _ -> failwith "IE"
                | Some t, ConstExpr expr ->
                    match t with
                    | SetType pt ->
                        match evalConstExpr ctx (Some pt) expr with
                        | CEROrdSet(b, _) -> ConstTempValue(b, t)
                        | _ -> failwith "IE"
                    | OrdType ->
                        match evalConstExpr ctx pt expr with
                        | CERInt(i,_) -> ConstInt i
                        | _ -> failwith "IE"
                    | StrType ->
                        match evalConstExpr ctx pt expr with
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
                               {originType with name = name} |> addType name
                           | TypePtr (count, typeId) ->
                               let typ = match ctx.FindTypeId typeId with | Some t -> t | _ -> failwith "IE not found"
                               addTypePointer count typ.name (StringName name)
                           | TypeSet (_, typeId) ->
                               let typ = match ctx.FindTypeId typeId with | Some t -> t | _ -> failwith "IE not found"
                               addTypeSet ctx typ.name (StringName name)
                           | TypeEnum enumValues ->
                               let name = StringName name
                               let max = enumValues.Length // TODO get explicit max value
                               let enumType = addType name {name=name;kind=TkOrd(OkEnumeration, OtULong(0, max));raw=pint32.raw}
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
                                    let typ = getInternalType typeName
                                    let fd = FieldDefinition(name, FieldAttributes.Public, typ.raw)
                                    td.Fields.Add fd
                                    fieldsMap.Add(name, (fd,typ))
                                    size <- size + typ.SizeOf
                                mb.Types.Add(td)
                                let name = StringName name
                                addType name {name=name;kind=TkRecord(fieldsMap, size);raw=td}
                           | Array(ArrayDef(_, dimensions, tname)) -> addTypeArray dimensions tname (StringName name)
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
                       let dt = getInternalType t
                       l |> List.map (fun v -> v, dt))
                   |> List.iter addVar
               | Constants consts ->
                    for (name, ctype, value) in consts do
                        let typ = match ctype with | Some t -> Some(getInternalType t) | _ -> None
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
                                       |> compileBlock methodBuilder ctx.details.tb
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