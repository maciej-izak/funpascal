[<AutoOpen>]
module Pas.Instructions

open Mono.Cecil
open Mono.Cecil.Cil

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
    | Elem of TypeReference
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
    | Call of MethodReference
    | Ldc of LdcKind
    | Ldsfld of FieldDefinition
    | Ldsflda of FieldDefinition
    | Ldarg of ParameterDefinition
    | Ldarga of ParameterDefinition
    | Ldloc of VariableDefinition
    | Ldloca of VariableDefinition
    | Ldfld of FieldDefinition
    | Ldflda of FieldDefinition
    | Ldind of IndirectKind
    | Ldobj of TypeReference
    | Ldnull
    | Stsfld of FieldDefinition
    | Starg of ParameterDefinition
    | Stloc of VariableDefinition
    | Stfld of FieldDefinition
    | Stind of IndirectKind
    | Conv of ConvKind
    | Cpblk
    | Cpobj of TypeReference
    | Stobj of TypeReference
    | Initobj of TypeReference
    | Newarr of TypeReference
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
    | Box of TypeReference
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
    | UserLabel of string

and IlInstruction =
    | IlBranch of IlBranch * BranchLabel ref
    | IlResolved of (AtomInstruction * Instruction)
    | IlResolvedEx of (AtomInstruction * Instruction * BranchLabel list ref)
with
    member this.FixRefs(newInstruction: Instruction) =
        match this with
        | IlResolvedEx (_, _, refs) ->
            !refs |> List.iter
                (function
                 | LazyLabel(_,{contents=i}) when (i.Operand :? Instruction) -> i.Operand <- newInstruction
                 | _ -> ()
                )
        | _ -> ()
        newInstruction

let ilToAtom ilList =
    ilList
    |> List.map (function | IlResolved(a,_) -> a | IlResolvedEx(a,_,_) -> a | _ -> failwith "IE")

type MetaInstruction =
    | DeclareLocal of VariableDefinition
    | InstructionList of IlInstruction list
    | HandleFunction of IlInstruction list * IlInstruction list option * IlInstruction list