[<AutoOpen>]
module Pas.IlEmit

open Mono.Cecil
open Mono.Cecil.Cil
open System.Linq

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

let emit (ilg : Cil.ILProcessor) inst =
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