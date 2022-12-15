namespace rec Pas

[<AutoOpen>]
module Instructions =

    open dnlib.DotNet
    open dnlib.DotNet.Emit

    type IndirectKind =
        | Ind_I
        | Ind_I1
        | Ind_I2
        | Ind_I4
        | Ind_I8
        | Ind_U
        | Ind_U1
        | Ind_U2
        | Ind_U4
        | Ind_U8
        | Ind_R4
        | Ind_R8
        | Ind_Ref

    type ConvKind =
        | Conv_I
        | Conv_I1
        | Conv_I2
        | Conv_I4
        | Conv_I8
        | Conv_U
        | Conv_U1
        | Conv_U2
        | Conv_U4
        | Conv_U8
        | Conv_R4
        | Conv_R8

    type ElemKind =
        | Elem of ITypeDefOrRef
        | Elem_I
        | Elem_I1
        | Elem_I2
        | Elem_I4
        | Elem_I8
        | Elem_U
        | Elem_U1
        | Elem_U2
        | Elem_U4
        | Elem_U8
        | Elem_R4
        | Elem_R8
        | Elem_Ref

    type IlBranch =
        | IlBrfalse
        | IlBrtrue
        | IlBr
        | IlBeq // =
        | IlBlt // <
        | IlBgt // >
        | IlBle // <=
        | IlBge // >=
        | IlBne_Un
        | IlBgt_Un
        | IlBlt_Un

    type LdcKind =
        | LdcI4 of int32
        | LdcI8 of int64
        | LdcR4 of single
        | LdcR8 of double

    type AtomInstruction =
        | Unknown
        | Call of IMethod
        | Calli of IMethod
        | Ldc of LdcKind
        | Ldsfld of FieldDef
        | Ldsflda of FieldDef
        | Ldarg of Parameter
        | Ldarga of Parameter
        | Ldloc of Local
        | Ldloca of Local
        | Ldfld of FieldDef
        | Ldflda of FieldDef
        | Ldftn of IMethod
        | Ldind of IndirectKind
        | Ldobj of ITypeDefOrRef
        | Ldnull
        | Stsfld of FieldDef
        | Starg of Parameter
        | Stloc of Local
        | Stfld of FieldDef
        | Stind of IndirectKind
        | Conv of ConvKind
        | Cpblk
        | Cpobj of ITypeDefOrRef
        | Stobj of ITypeDefOrRef
        | Initobj of ITypeDefOrRef
        | Newarr of ITypeDefOrRef
        | Stelem of ElemKind
        | Unaligned of byte
        | AddInst
        | MultiplyInst
        | MinusInst
        | DivideInst
        | AndInst
        | OrInst
        | XorInst
        | ShlInst
        | ShrInst
        | Rem
        | NotInst
        | NegInst
        | Box of ITypeDefOrRef
        | Dup
        | Pop
        | Ret
        | Endfinally
        | Leave of Instruction
        | Ceq
        | Clt
        | Cgt
        | Nop
        | Ldstr of string
        | Brfalse of Instruction
        | Br of Instruction
        | Beq of Instruction
        | Blt of Instruction
        | Bgt of Instruction
        | Ble of Instruction
        | Bge of Instruction
        | Bne_Un of Instruction
        | Bgt_Un of Instruction
        | Blt_Un of Instruction
        | InInst

    // Use Ldc for 32 bit ptr instead of Ldnull. Ldnull is invalid for .NET7
    // (CLR detected an invalid program / Expected I, but got O).
    let Ldnil = Ldc(LdcI4 0)
    
    let nullRef() = ref Unchecked.defaultof<Instruction>

    type BranchLabel =
        | LazyLabel of IlInstruction * Instruction ref
        | ForwardLabel
        | UserLabel

    and [<AbstractClass>] DelayedIl() =
        let mutable branchLabels: BranchLabel list = []
        let mutable owner: IlInstruction option = None
        let mutable finalCode: Instruction list option = None
 
        member _.ResolveLabels (labels: BranchLabel ref list) =
            if labels.IsEmpty = false then
                branchLabels <-
                    (branchLabels, labels)
                    ||> List.fold (fun s l -> let ll = LazyLabel(owner.Value, nullRef()) in (l := ll; ll::s))

        member _.InitializeOwner o =
            match owner with
            | None -> owner <- Some o; o
            | _ -> raise(InternalError "2020112802")
            
        member _.BranchLabels = branchLabels
        
        member self.FixRefs (instrs: Instruction list) =
            self.BranchLabels
            |> List.iter
                (function // contents can be null, for example when label is marked before goto, this is solved in proper way
                 | LazyLabel(_,{contents=i}) when i <> null && (i.Operand :? Instruction) -> i.Operand <- instrs.Head
                 | _ -> ())
            instrs
        
        member self.GenerateCode instr =
            match finalCode with
            | Some fc -> fc
            | _ -> finalCode <- self.InternalGenerateCode() |> List.collect instr |> Some; finalCode.Value |> self.FixRefs
        abstract member InternalGenerateCode : unit -> IlInstruction list

    and DelayedCode(gen: unit -> IlInstruction list) =
        inherit DelayedIl()
        override self.InternalGenerateCode() = gen()

    and DelayedExit() =
        inherit DelayedIl()
        member val ExitInstr: IlInstruction option = None with get, set
        override self.InternalGenerateCode() =
            match self.ExitInstr with
            | Some exitInstr -> [exitInstr]
            | _ -> raise (InternalError "2020120400")
        
        
    and IlDelayed =
        | IlDelayedCode of DelayedCode
        | IlDelayedExit of DelayedExit
    with
        member self.ResolveLabels (labels: BranchLabel ref list) =
            match self with
            | IlDelayedCode dnc -> dnc.ResolveLabels labels
            | IlDelayedExit de -> de.ResolveLabels labels

    and IlInstruction =
        | IlBranch of IlBranch * BranchLabel ref
        | IlResolved of (AtomInstruction * Instruction)
        | IlDelayed of IlDelayed
    with
        static member CreateDelayedCode(f) =
            let dc = DelayedCode(f)
            dc |> IlDelayedCode |> fun il -> IlDelayed(il) |> dc.InitializeOwner

        static member CreateDelayedExit() =
            let dc = DelayedExit()
            dc |> IlDelayedExit |> fun il -> IlDelayed(il) |> dc.InitializeOwner

    let ilToAtom ilList = ilList |> List.map (function | IlResolved(a,_) -> a | _ -> raise (InternalError "2020112900"))

    type VariableDeclKind =
        | NormalDecl
        | TempDecl of string * bool ref

    type ITempVariableDecl =
        inherit System.IDisposable
        abstract member Name: string
        abstract member Local: Local
        
    type VariableDecl = {
        mutable usages: int
        varKind: VariableKind
        declKind: VariableDeclKind
    } with
        static member DisposableUsage name local inUse =
            { new ITempVariableDecl with
                member _.Dispose() = inUse := false
                member _.Name = name
                member _.Local = local }
        static member New(kind: VariableKind) = { usages = 0; varKind = kind; declKind = NormalDecl}
        static member NewTemp(name: string, kind: VariableKind) =
            let inUse = ref true
            { usages = 0; varKind = kind; declKind = TempDecl(name, inUse) },
            VariableDecl.DisposableUsage name kind.Local inUse
        member self.TempVar =
            match self.declKind, self.varKind with
            | TempDecl(n, inUse), LocalVariable l -> VariableDecl.DisposableUsage n l inUse
            | _ -> raise (InternalError "2020122100")
        member self.IsFreeTemp =
            match self.declKind with
            | TempDecl(_, inUse) -> not !inUse
            | _ -> false
        
    type MetaInstruction =
        | InstructionList of IlInstruction list
        | HandleFunction of IlInstruction list * IlInstruction list option * IlInstruction list
        
[<AutoOpen>]
module IlEmit =

    open dnlib.DotNet
    open dnlib.DotNet.Emit
    open System.Linq

    let rec brtoinstr (df: IlDelayed -> (IlInstruction -> Instruction list) -> Instruction list) (fn: IlInstruction -> Instruction list) l =
        match !l with
        | LazyLabel(isr,ref) ->
             match isr with
             | IlBranch(_, i) -> brtoinstr df fn i |> fst
             | IlResolved(_, i) -> i
             | IlDelayed ild -> (df ild fn).Head
             | _ -> raise (InternalError "2020120200")
             , ref
        | _ -> raise (InternalError "2020082300")

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
        | Calli mi     -> Instruction.Create(OpCodes.Calli, mi.MethodSig)
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
        | Ldftn f     -> Instruction.Create(OpCodes.Ldftn, f)
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
                          | Elem e   -> Instruction.Create(OpCodes.Stelem, e)
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

    let private instr (df: IlDelayed -> (IlInstruction -> Instruction list) -> Instruction list) i : Instruction list =
        match i with
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
                let i, iref = brtoinstr df (instr df) i
                iref := Instruction.Create(opc, i)
                [!iref]
        | IlResolved(_,i) -> [i]
        | IlDelayed ild -> df ild (instr df)

    let (~+) (i: AtomInstruction) = IlResolved(i, i |> atomInstr)

    let emit (body : CilBody) inst =
        let sdf _ = raise(InternalError "2020120300")
        let df (brOpcode: OpCode) (goto: Instruction) ild fi =
            (
                match ild with
                | IlDelayedExit deil ->
                    deil.ExitInstr <- IlResolved(Unknown, Instruction.Create(brOpcode, goto)) |> Some
                    deil :> DelayedIl
                | IlDelayedCode dnc -> dnc :> DelayedIl
            ).GenerateCode fi
        
        match inst with
        | InstructionList p -> p |> List.collect (instr sdf) |> List.iter body.Instructions.Add
        | HandleFunction (instructionsBlock, finallyBlock, endOfAll) ->
            // return first instruction
            let beginOfEnd = endOfAll.Head |> instr sdf
            let finallyBlock, delayFactory = match finallyBlock with
                                                    | Some block -> block, df OpCodes.Leave beginOfEnd.Head
                                                    | None -> [], df OpCodes.Br beginOfEnd.Head

            let appendAndReplaceRetGen (il: IlInstruction) =
                il
                |> instr delayFactory
                |> (fun l -> List.iter body.Instructions.Add l; if l.IsEmpty then ValueNone else ValueSome l.Head) // can be empty for errors

            let processList replaceFun = function
                | head::tail ->
                    let result = head |> replaceFun
                    tail |> List.iter (replaceFun >> ignore)
                    match result with
                    | ValueSome v -> v
                    | _ -> beginOfEnd.Head
                | [] -> beginOfEnd.Head
            let start = processList appendAndReplaceRetGen instructionsBlock
            if List.isEmpty finallyBlock = false then
                if body.Instructions.Last().OpCode <> OpCodes.Leave then
                    body.Instructions.Add(Instruction.Create(OpCodes.Leave, beginOfEnd.Head))
                let startFinally = processList appendAndReplaceRetGen finallyBlock
                ExceptionHandler(ExceptionHandlerType.Finally,
                                               TryStart     = start,
                                               TryEnd       = startFinally,
                                               HandlerStart = startFinally,
                                               HandlerEnd   = beginOfEnd.Head)
                |> body.ExceptionHandlers.Add
            List.iter body.Instructions.Add beginOfEnd
            endOfAll.Tail |> List.iter ((instr delayFactory) >> List.iter body.Instructions.Add) 

    let Ldc_I4 i = LdcI4 i |> Ldc
    let Ldc_U1 (i: byte) = [
        +Ldc (LdcI4 (int i))
        +Conv Conv_U1
    ]
    let Ldc_R4 r = LdcR4 r |> Ldc

    // TODO for further optimizations
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
            
[<AutoOpen>]
module Types =
    
    open System
    open System.Text
    open System.Collections.Generic
    open dnlib.DotNet
    open dnlib.DotNet.Emit

    type ParamRefKind =
        | RefConst
        | RefVar
        | RefUntypedVar
        | RefUntypedConst
        | RefNone
    with
        member self.IsRef =
            match self with
            | RefNone | RefConst -> false
            | _ -> true

    type CompilerName =
        | CompilerName of TypeIdentifier
        | AnonName
        | ErrorName
    with
        static member FromTypeId = CompilerName
        static member FromString = TypeIdentifier.FromString >> CompilerName
        static member FromDIdent = function
            | DIdent(Ident id::_) -> TypeIdentifier.FromString id |> CompilerName
            | _ -> raise <| InternalError "2020062301"

        override self.ToString() =
            match self with
            | CompilerName tib -> tib.ToString()
            | AnonName -> "<anon>"
            | ErrorName -> "<error>"
            
        member self.BoxPos =
            match self with
            | CompilerName tn -> tn.BoxPos
            | _ -> raise(InternalError "2020082100")

    type TOrdType =
        | OtSByte of int * int
        | OtUByte of int * int
        | OtSWord of int * int
        | OtUWord of int * int
        | OtSLong of int * int
        | OtULong of int * int
        | OtSQWord of int64 * int64
        | OtUQWord of uint64 * uint64

    let (|OrdTypeS|OrdTypeU|) = function
        | OtSByte _ | OtSWord _ | OtSLong _ | OtSQWord _ -> OrdTypeS
        | OtUByte _ | OtUWord _ | OtULong _ | OtUQWord _ -> OrdTypeU

    type TFloatType = | FtSingle // | FtDouble | FtExtended | FtComp | FtCurr

    let ptrSize = 4

    type TOrdKind =
        | OkInteger
        | OkBool
        | OkEnumeration
        | OkChar

    type TArrayKind =
        | AkArray
        | AkSString of byte

    type TypeKind =
        | TkUnknown of int
        | TkOrd of OrdKind: TOrdKind * OrdType: TOrdType
        | TkFloat of FloatType: TFloatType
        | TkRecord of Dictionary<string, FieldDef * PasType> * int
        | TkPointer of PasType
        | TkArray of (TArrayKind * ArrayDim list * PasType)
        | TkSet of PasType
        | TkProcVar of MethodInfo
    with
        member self.ToCompilerStr() =
            match self with
            | TkUnknown _ -> "<unknown>"
            | TkOrd _ -> "ordinal"
            | TkFloat _ -> "float"
            | TkRecord _ -> "record"
            | TkPointer _ -> "pointer"
            | TkArray _ -> "array"
            | TkSet _ -> "set"
            | TkProcVar _ -> "procedure variable"
            | _ -> raise <| InternalError "2020112100"

    and PasRawType(raw: ITypeDefOrRef) =
        let sg = lazy(raw.ToTypeSig())
        let resolveRaw() =
            match raw.ResolveTypeDef() with
            | null -> raise <| InternalError "2020110200"
            | res -> res
        let def = lazy(resolveRaw())
        member _.Raw = raw
        member _.Sig = sg.Force()
        member _.Def = def.Force()
        new (raw: TypeSig) = PasRawType(raw.ToTypeDefOrRef())

    and PasType = {
          name: CompilerName
          kind: TypeKind
          raw : PasRawType
        }
    with
        member self.DefOrRef = self.raw.Raw
        member self.Sig = self.raw.Sig
        member self.Def = self.raw.Def
        
        member self.ToCompilerStr() = self.kind.ToCompilerStr()
        
        static member Create(name, defOrRef: ITypeDefOrRef, kind) = {name=CompilerName.FromString name;raw=PasRawType defOrRef;kind=kind}
        static member Create(name, ts: TypeSig, kind) = {name=name;raw=PasRawType ts;kind=kind}
        static member Create(name, ts: ITypeDefOrRef, kind) = {name=name;raw=PasRawType ts;kind=kind}
        
        static member NewPtr(pt: PasType, ?name) =
            let name = defaultArg name ""
            let ptrType = pt.Sig |> PtrSig |> TypeSpecUser
            PasType.Create(name, ptrType, TkPointer(pt))

        member this.SizeOf =
            let ordTypeSize = function
                | OtSByte _ | OtUByte _ -> 1
                | OtSWord _ | OtUWord _ -> 2
                | OtSLong _ | OtULong _ -> 4
                | OtSQWord _ | OtUQWord _ -> 8

            match this.kind with
            | TkOrd (_, ordType) -> ordTypeSize ordType
            | TkFloat floatType ->
                match floatType with
                | FtSingle -> 4
            | TkRecord (_, s) -> s
            | TkPointer _ -> ptrSize
            | TkArray _ -> int <| this.Def.ClassSize
            | TkUnknown s -> s
            | TkSet _ -> 256

        member this.ResolveArraySelfType() =
            match this.kind with
            | TkArray(_,h::_,_) -> h.selfType := this
            | _ -> raise <| InternalError "2020102000"
            this

        member this.IndKind =
            match this.Sig.ElementType with
            | ElementType.Ptr     -> Ind_U
            | ElementType.I1      -> Ind_I1
            | ElementType.I2      -> Ind_I2
            | ElementType.I4      -> Ind_I4
            | ElementType.I8      -> Ind_I8
            | ElementType.Boolean -> Ind_U1
            | ElementType.R4      -> Ind_R4
            | ElementType.R8      -> Ind_R8
            | ElementType.U1      -> Ind_U1
            | ElementType.U2      -> Ind_U2
            | ElementType.U4      -> Ind_U4
            | ElementType.U8      -> Ind_U8
            | _ -> raise <| InternalError "2020102001"
            
        member this.RefToConv =
            match this.Sig.ElementType with
            | ElementType.Ptr -> +Conv Conv_U
            | ElementType.I1 -> +Conv Conv_I1
            | ElementType.I2 -> +Conv Conv_I2
            | ElementType.I4 -> +Conv Conv_I4
            | ElementType.I8 -> +Conv Conv_I8
            | ElementType.Boolean -> +Conv Conv_U1
            | ElementType.R4 -> +Conv Conv_R4
            | ElementType.R8 -> +Conv Conv_R8
            | ElementType.U1 -> +Conv Conv_U1
            | ElementType.U2 -> +Conv Conv_U2
            | ElementType.U4 -> +Conv Conv_U4
            | ElementType.U8 -> +Conv Conv_U8
            | _ -> raise <| InternalError "2020110201"

    and [<CustomEquality; NoComparison>]
        ArrayDim = { low: int; high: int; size: int; elemSize: int; elemType: PasType; selfType: PasType ref }
    with
        override self.Equals a =
            match a with
            | :? ArrayDim as a -> self.low = a.low && self.high = a.high && self.size = a.size && self.elemType = a.elemType
            | _ -> false
        override self.GetHashCode() = HashCode.Combine(hash self.low, hash self.high, hash self.size, hash self.elemType)
        
    and VariableKind =
         | LocalVariable of Local
         | GlobalVariable of FieldDef
         | ParamVariable of ParamRefKind * Parameter
    with
        member self.Type() =
            match self with
            | LocalVariable v -> v.Type
            | GlobalVariable v -> v.FieldType
            | ParamVariable (_, v) -> v.Type
            
        member self.Local =
            match self with
            | LocalVariable l -> l
            | _ -> raise(InternalError "2020122200")

    and MethodParam = {
        typ: PasType
        ref: ParamRefKind
    }

    and MethodResult = {
        typ: PasType
        var: VariableKind option // None for imported methods
    }

    and [<CustomEquality;NoComparison>] MethodInfo = {
        paramList: MethodParam array
        result: MethodResult option
        raw: IMethod
    } with
        member self.PasType = 
            PasType.Create(AnonName, FnPtrSig self.raw.MethodSig, TkProcVar self)
        member self.ResultType = 
            match self.result with
            | Some rt -> Some rt.typ
            | _ -> None
            
        override self.Equals(b) =
            match b with
            | :? MethodInfo as mi ->
                let typesEquals a b = a.kind = b.kind
                let paramsEquals a b = a.ref = b.ref && (typesEquals a.typ b.typ)
                
                mi.paramList.Length = self.paramList.Length
                && mi.result.IsSome = self.result.IsSome
                && (if mi.result.IsSome then typesEquals mi.result.Value.typ self.result.Value.typ else true)
                && Seq.forall2 paramsEquals mi.paramList self.paramList
            | _ -> false
            
    let (|StrType|ChrType|OtherType|) = function
        | {kind=TkArray(AkSString _,_,_)} -> StrType
        | {kind=TkOrd(OkChar,_)} -> ChrType
        | _ -> OtherType

    let (|ErrorType|_|) (pt: PasType) =
        match pt with
        | {name=ErrorName} -> Some ErrorType
        | _ -> None

    let (|SetType|_|) = function
        | {kind=TkSet(pt)} -> Some pt
        | _ -> None

    let (|EnumType|_|) = function
        | {kind=TkOrd(OkEnumeration,_)} -> Some EnumType
        | _ -> None

    let (|OrdType|_|) = function
        | {kind=TkOrd _} -> Some OrdType
        | _ -> None

    let (|Ord64Type|_|) = function
        | {kind=TkOrd(_,OtSQWord _)} | {kind=TkOrd(_,OtUQWord _)} -> Some Ord64Type
        | _ -> None

    let (|IntType|_|) = function
        | {kind=TkOrd(OkInteger,_)} -> Some IntType
        | _ -> None

    let (|NumericType|_|) = function
        | {kind=TkOrd _} | {kind=TkFloat _} | {kind=TkPointer _} -> Some NumericType
        | _ -> None

    let (|PointerType|_|) = function
        | {kind=TkPointer _} -> Some PointerType
        | _ -> None

    let (|ProcVarType|_|) = function
        | {kind=TkProcVar _} -> Some ProcVarType
        | _ -> None

    let (|FloatType|_|) = function
        | {kind=TkFloat _} -> Some FloatType
        | _ -> None

    let (|UnitType|_|) = function
        | {name=AnonName;kind=TkUnknown 0} -> Some UnitType
        | _ -> None

    let (|BoolOp|_|) = function | Clt | Cgt | Ceq | InInst -> Some BoolOp | _ -> None

    let isChrType = function | ChrType -> true | _ -> false
    let isStrType = function | StrType -> true | _ -> false
    let strToSStr s =
        if (s:string).Length >= 256 then failwith "IE"
        (s + (String.replicate (256-s.Length) "\000")) |> Encoding.ASCII.GetBytes

    let rec sameTypeKind = function
        | {kind=TkArray(a,_,_)}, {kind=TkArray(b,_,_)} when a = b -> true
        | {kind=TkOrd(aok,aot)}, {kind=TkOrd(bok,bot)} when aok = bok && aot = bot -> true
        | {kind=TkFloat a}, {kind=TkFloat b} when a = b -> true
        | {kind=TkSet a}, {kind=TkSet b} when sameTypeKind(a,b) -> true
        | _ -> false

    let derefType (t: PasType) =
        match t.kind with
        | TkPointer t -> t
        | _ -> failwith "Cannot dereference non pointer type"

    let (|CharacterType|_|) (defStrTyp: PasType) = function
        | StrType as t -> Some(CharacterType t)
        | ChrType -> Some(CharacterType defStrTyp)
        | _ -> None
    