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

    type MetaInstruction =
        | DeclareLocal of Local
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
        | DeclareLocal t -> t |> body.Variables.Add |> ignore
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
                |> (fun l -> List.iter body.Instructions.Add l; l.Head)

            
            let processList replaceFun = function
                | head::tail ->
                    let result = head |> replaceFun
                    tail |> List.iter (replaceFun >> ignore)
                    result
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