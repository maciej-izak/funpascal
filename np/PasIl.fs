module NP.PasIl

open System
open System.Collections
open System.Collections.Generic
open System.Linq.Expressions
open System.Linq
open System.Reflection.Metadata
open System.Text
open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks
open Microsoft.FSharp.Linq

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

type VariableKind =
     | LocalVariable of VariableDefinition
     | GlobalVariable of FieldDefinition
with
    member self.Type() =
        match self with
        | LocalVariable v -> v.VariableType
        | GlobalVariable v -> v.FieldType

type Intrinsic =
    | IncProc
    | DecProc
    | SuccProc
    | PredProc
    | ExitProc
    | HaltProc
    | ContinueProc
    | BreakProc
    | WriteProc
    | WriteLnProc
    | ReadProc
    | ReadLnProc
    | WriteLineProc
    | NewProc
    | DisposeProc
    | OrdFunc
    | ChrFunc
    | SizeOfFunc

type ConstEvalResult =
    | CERString of string
    | CERInt of int
    | CERBool of byte
    | CERUnknown

type ParamRefKind =
    | RefConst
    | RefVar
    | RefUntypedVar
    | RefUntypedConst
    | RefNone

type TypeName =
    | StringName of string
    | TypedName of TypeIdentifier
    | AnonName
with
    static member FromTypeId = function
        | TIdIdent(DIdent([PIName(id)])) -> StringName id
        | TIdIdent _ -> failwith "IE"
        | ti -> TypedName ti

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
    | TkRecord of Dictionary<string,FieldDefinition*PasType> * int
    | TkPointer of PasType
    | TkArray of (TArrayKind * ArrayDim list * PasType)

and PasType = {
      name: TypeName
      kind: TypeKind
      raw : TypeReference
    }
with
    static member NewPtr(pt, ?name) =
        let name = defaultArg name ""
        {name=StringName name;raw=PointerType(pt.raw);kind=TkPointer(pt)}

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
        | TkArray _ -> (this.raw :?> TypeDefinition).ClassSize
        | TkUnknown s -> s

    member this.ResolveArraySelfType() =
        match this.kind with
        | TkArray(_,d,_) -> d.Head.selfType := this
        | _ -> failwith "IE"
        this

    member this.IndKind =
        match this.raw.MetadataType with
        | MetadataType.Pointer -> Ind_U
        | MetadataType.SByte   -> Ind_I1
        | MetadataType.Int16   -> Ind_I2
        | MetadataType.Int32   -> Ind_I4
        | MetadataType.Int64   -> Ind_I8
        | MetadataType.Byte    -> Ind_U1
        | MetadataType.Boolean -> Ind_U1
        | MetadataType.Single  -> Ind_R4
        | MetadataType.UInt16  -> Ind_U2
        | MetadataType.UInt32  -> Ind_U4
        | MetadataType.UInt64  -> Ind_U8
        | _ -> failwith "IE"

and ArrayDim = { low: int; high: int; size: int; elemSize: int; elemType: PasType; selfType: PasType ref }

type MethodParam = {
    typ: PasType
    ref: bool
}

type MethodInfo = {
    paramList: MethodParam array
    result: PasType option
    raw: MethodReference
}

type MethodSym =
    | Referenced of MethodInfo
    | Intrinsic of Intrinsic
with
    member self.ReturnType =
        match self with
        | Referenced {result = result} -> result
        | Intrinsic _ -> None

type VariableParam = ParameterDefinition * PasType * ParamRefKind

type Symbol =
    | VariableParamSym of VariableParam
    | VariableSym of (VariableKind * PasType)
    | MethodSym of MethodSym
    | TypeSym of PasType
    | EnumValueSym of int
    | WithSym of (VariableKind * PasType)
    | ConstSym of ConstEvalResult
    | UnknownSym


type VariableLoadKind =
    | LoadParam of VariableParam
    | LoadVar of (VariableKind * PasType)

type SymbolLoad =
    | DerefLoad
    | ElemLoad of (ExprEl * ArrayDim) list * PasType
    | StructLoad of (FieldDefinition * PasType) list
    | VariableLoad of VariableLoadKind
    | ValueLoad of int
    | CallableLoad of MethodSym
    | TypeCastLoad of PasType

type ChainLoad =
    | ChainLoad of (SymbolLoad list * PasType option)
    | SymbolLoadError

let chainToSLList = function
    | ChainLoad sl -> sl
    | SymbolLoadError -> failwith "IE"

let caseSensitive = HashIdentity.Structural<TypeName>
let caseInsensitive =
    HashIdentity.FromFunctions
        (function
         | StringName s -> s.GetHashCode(StringComparison.InvariantCultureIgnoreCase)
         | TypedName t -> t.GetHashCode()
         | AnonName -> failwith "IE")
        (fun a b -> String.Equals(string a, string b, StringComparison.InvariantCultureIgnoreCase))

type SymbolsDict<'T>() =
    inherit Dictionary<string, 'T>()

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
        single: PasType
        string: PasType
        char: PasType
        file: PasType
        value: PasType
        pointer: PasType
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
    }

type ModuleDetails = {
        moduleBuilder: ModuleDefinition
        ns: string
        tb: TypeDefinition
        anonSizeTypes: Dictionary<int, TypeDefinition>
        sysTypes: SystemTypes
        sysProc: SystemProc
    } with
    static member Create moduleBuilder ns tb sysTypes sysProc =
        {
            moduleBuilder = moduleBuilder
            ns = ns
            tb = tb
            anonSizeTypes = Dictionary<_, _>()
            sysTypes = sysTypes
            sysProc = sysProc
        }

    static member NewSizedType ns (mb: ModuleDefinition) vt size name =
            let attributes = TypeAttributes.SequentialLayout ||| TypeAttributes.AnsiClass ||| TypeAttributes.Sealed ||| TypeAttributes.Public
            let at = TypeDefinition(ns, name + string size, attributes)
            at.ClassSize <- size
            at.PackingSize <- 1s;
            at.BaseType <- vt
            mb.Types.Add(at)
            at

    member self.SelectAnonSizeType size =
        match self.anonSizeTypes.TryGetValue size with
        | true, t -> t
        | _ ->
            let at = ModuleDetails.NewSizedType self.ns self.moduleBuilder self.sysTypes.value.raw size "anon"
            self.anonSizeTypes.Add(size, at)
            at

    member self.AddBytesConst (bytes: byte[]) =
        let ast = self.SelectAnonSizeType bytes.Length
        let fd = FieldDefinition(null, FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.HasFieldRVA, ast)
        fd.InitialValue <- bytes
        self.tb.Fields.Add fd
        fd

let stdIdent = PINameCreate >> List.singleton >> DIdent
let stdType  = stdIdent >> TIdIdent

type Ctx = {
        variables: List<VariableKind> list
        labels: Dictionary<string, BranchLabel ref> list
        symbols: Dictionary<TypeName, Symbol> list
        forward: Dictionary<string, MethodDefinition * Dictionary<TypeName,Symbol> * VariableKind option>
        localVariables: int ref
        lang: LangCtx
        res: List<MetaInstruction>
        details: ModuleDetails
        loop: Stack<BranchLabel ref * BranchLabel ref>
    } with
    static member Create symbols (lang: LangCtx) details =
        {
            variables = [List<VariableKind>()]
            labels = [Dictionary<_,_>()]
            symbols = [symbols]
            forward = Dictionary<_, _>()
            localVariables = ref 0
            lang = lang
            res = List<MetaInstruction>()
            details = details
            loop = Stack<_>()
        }

    member self.Inner newSymbols =
        { self with
            symbols = newSymbols::self.symbols
            localVariables = ref 0
            variables = List<_>()::self.variables
            labels = Dictionary<_,_>()::self.labels
            res = List<MetaInstruction>()
            loop = Stack<_>()}

    member self.FindSym sym =
        self.symbols |> List.tryPick (fun st -> match st.TryGetValue sym with | true, v -> Some v | _ -> None)

    member self.FindType sym =
        self.symbols |> List.tryPick (fun st -> match st.TryGetValue sym with | true, v -> Some v | _ -> None)
        |> function
           | Some(TypeSym ts) -> Some ts
           | _ -> None

    member self.FindTypeId = TypeName.FromTypeId >> self.FindType

    member self.EnsureVariable(?kind) =
        let ikey = !self.localVariables
        let key = string ikey
        incr self.localVariables
        // TODO manager of variables for reuse
//        let result = match self.variables.TryGetValue key with
//                     | true, value ->
//                         value
//                     | _ ->
        let varType = defaultArg kind self.details.sysTypes.int32
        let varDef = varType.raw |> VariableDefinition
        let varKind = varDef |> LocalVariable
        self.res.Add(DeclareLocal(varDef))
        self.variables.Head.Add(varKind)
        self.symbols.Head.Add(StringName key, VariableSym(varKind, varType))
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

(*

        int a = 0;
        int* p = &a;
        {
        // p is pinned as well as object, so create another pointer to show incrementing it.
            int** p2 = &p;
            Console.WriteLine( **p2 );
        }


        using System;
using System.Runtime.InteropServices;
public class C {

[DllImport("kernel32.dll")]
static extern void GetSystemTime(SystemTime systemTime);

[StructLayout(LayoutKind.Sequential)]
class SystemTime {
    public ushort Year;
    public ushort Month;
    public ushort DayOfWeek;
    public ushort Day;
    public ushort Hour;
    public ushort Minute;
    public ushort Second;
    public ushort Milsecond;
}

public static void Main(string[] args) {
    SystemTime st = new SystemTime();
    GetSystemTime(st);
    Console.WriteLine(st.Year);
}

    [StructLayout(LayoutKind.Sequential)]
public struct MyStruct {
   [MarshalAs(UnmanagedType.ByValArray, SizeConst=128)] public short[] s1;
}

    public struct InPlaceArray
{
[MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
public int[] values;
}

    public unsafe void M() {
        InPlaceArray x = new InPlaceArray();
        x.values[3] = 9;
    }
}

*)

let derefType (t: PasType) =
    match t.kind with
    | TkPointer t -> t
    | _ -> failwith "Cannot dereference non pointer type"

let findSymbol (ctx: Ctx) (DIdent ident) =
    let mainSym = ident.Head |> function | PIName n -> ctx.FindSym(StringName n)
    
    let rec findSym ref acc = function
    | (Designator.Array a)::t -> acc, Designator.Array(a)::t // TODO ?
    | PIName(h)::t ->
        // TODO ? here is solved auto records dereference (x.y instead of x^.y) and dereference for array elements
        let ref = match ref.kind with | TkPointer r -> r | _ -> ref
        let symbols = match ref.kind with | TkRecord(d,_) -> d | _ -> failwith "IE"
        let sym = symbols.[h]
        findSym (snd sym) (sym::acc) t
    | t -> acc, t
    
    let rec resolveTail acc vt  = function
        | [] -> (List.rev acc, vt)
        | h::t ->
            match h with
            | Deref ->
                // TODO check dereferencable
                let tref = match vt.kind with | TkPointer t -> t | _ -> failwithf "IE cannot do deref of %A" vt.kind
                resolveTail (DerefLoad::acc) tref t
            | Ident _ -> 
                let sl, restOfTail = findSym vt [] (h::t)
                let vt = List.last sl
                resolveTail (StructLoad(sl)::acc) (snd vt) restOfTail
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
    | VariableSym (v, t as vt) -> LoadVar vt |> varLoadChain t ident.Tail
    | WithSym (v, t as vt) -> LoadVar vt |> varLoadChain t ident
    | EnumValueSym(i) when ident.Tail = [] -> ChainLoad([ValueLoad(i)], Some ctx.details.sysTypes.int32)
    | MethodSym m ->  ChainLoad([CallableLoad m], m.ReturnType)
    | VariableParamSym ((i,t,r) as p) -> LoadParam p |> varLoadChain t ident.Tail
    | ConstSym v ->
        match v with
        | CERInt i -> ChainLoad([ValueLoad(i)], Some ctx.details.sysTypes.int32)
        | CERBool b ->
            let i = int b
            ChainLoad([ValueLoad(i)], Some ctx.details.sysTypes.int32)
        | _ -> failwith "IE"
    | TypeSym t -> ChainLoad([TypeCastLoad t], Some t)
    | _ -> SymbolLoadError

let findMethodReference ctx =
    stdIdent >> findSymbol ctx >> chainToSLList >>
    function | [CallableLoad(Referenced({raw=mr}))], _ -> mr | _ -> failwith "IE"

type LastTypePoint =
    | LTPVar of VariableKind * PasType
    | LTPParam of ParameterDefinition * PasType * ParamRefKind
    | LTPDeref of PasType * bool
    | LTPStruct of FieldDefinition * PasType
    | LTPNone
with
    member self.ToTypeRef =
        match self with
        | LTPVar(_,tr) -> tr
        | LTPParam(_,tr,_) -> tr
        | LTPDeref(tr, _) -> tr
        | LTPStruct(_, tr) -> tr
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
           | Referenced {raw=mr} -> RealFunction(cl, +Call(mr) |> Some)
           | Intrinsic _ -> RealFunction(cl, None)
        | ChainLoad([TypeCastLoad t], _) -> TypeCast t
        | _ -> failwith "Not supported"

type ValueKind = ValueKind of IlInstruction list * PasType

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

let rec exprToIl (ctx: Ctx) exprEl expectedType =
    let rec exprToMetaExpr el et =
        let add2OpIlTyped a b i et =
            match exprToMetaExpr a et with
            | ValueKind(a, at) ->
                match exprToMetaExpr b (Some at) with
                | ValueKind(b, bt) ->
                    let typ = match et with | Some t -> t | _ -> at
                    ([
                        yield! a
                        yield! b
                        if at <> bt then yield typeRefToConv at.raw
                        yield +i
                        if et.IsSome then yield typeRefToConv et.Value.raw
                    ], typ) |> ValueKind
        let add2OpIl a b i = add2OpIlTyped a b i et
        let add2OpIlBool a b i = add2OpIlTyped a b i (Some ctx.details.sysTypes.boolean)
        let inline add1OpIl a i =
            match exprToMetaExpr a et with
            | ValueKind(a, at) -> ValueKind([ yield! a; yield +i; yield typeRefToConv at.raw], at)
        match el with
        | Value v -> valueToValueKind ctx v et
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
            | ValueKind(o, ot) -> ([ yield! o; +Ldc_I4 0; +Ceq ], ctx.details.sysTypes.boolean) |> ValueKind
        | StrictlyLessThan(a, b) -> add2OpIlBool a b Clt
        | StrictlyGreaterThan(a, b) -> add2OpIlBool a b Cgt
        | LessThanOrEqual(a, b) ->
            match add2OpIlBool a b Cgt with
            | ValueKind(o, ot) -> ([ yield! o; +Ldc_I4 0; +Ceq ], ctx.details.sysTypes.boolean) |> ValueKind
        | GreaterThanOrEqual(a, b) ->
            match add2OpIlBool a b Clt with
            | ValueKind(o, ot) -> ([ yield! o; +Ldc_I4 0; +Ceq ], ctx.details.sysTypes.boolean) |> ValueKind
        | Addr(a) ->
            ([
                match a with
                | Value(VIdent i) -> yield! (findSymbolAndGetPtr ctx i |> fst)
                | _ -> failwith "IE"
            ], ctx.details.sysTypes.pointer) |> ValueKind
        | _ -> failwith "IE"

    match exprToMetaExpr exprEl expectedType with
    | ValueKind (a, at) ->
        [
         yield! a
         if expectedType.IsSome then
            let typ = expectedType.Value
            match typ.kind with | TkOrd _ | TkFloat _ -> yield typeRefToConv typ.raw | _ -> ()
        ], at

and callParamToIl ctx cp (idxmr: Option<int * MethodInfo>) =
    let param, byRef =
        match idxmr with
        | Some(idx,{paramList=pl}) ->
            let p = pl.[idx]
            Some p.typ, p.ref
        | _ -> None, false
    match cp with
    | ParamExpr expr -> exprToIl ctx expr param
    | ParamIdent id ->
        match param with
        | Some param when byRef -> findSymbolAndGetPtr ctx id
        | Some param when param = ctx.details.sysTypes.constParam -> findSymbolAndGetPtr ctx id
        | Some param when param = ctx.details.sysTypes.varParam -> findSymbolAndGetPtr ctx id
        | _ -> findSymbolAndLoad ctx id

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
        | Referenced mr, Some f ->
            ([
                yield! cp
                |> List.mapi (fun i p -> fst <| callParamToIl ctx p (Some(i, mr)))
                |> List.concat
                yield f
                if popResult && mr.result.IsSome then
                    yield +Pop
             ], mr.result)
        | Intrinsic i, _ ->
            let doWrite() =
                // WRITEINTF - for int
                // WRITEBOOLEANF - for boolean
                // WRITEREALF - for real
                // WRITEPOINTERF - for pointer
                // WRITESTRINGF - for string
                let file, cp =
                    match cp with
                    | ParamIdent(id)::tail ->
                        let sl, typ = findSymbolAndGetPtr ctx id
                        if typ.raw = ctx.details.sysTypes.file.raw then Some sl, tail
                        else None, cp
                    | _ -> None, cp
                let file() = if file.IsNone then fst <| findSymbolAndGetPtr ctx (stdIdent "STDOUTPUTFILE")
                             else file.Value

                let doParam = fun (cp: CallParam) ->
                    let someInt = Some ctx.details.sysTypes.int32
                    let e,w,p = match cp with
                                | ParamExpr(TupleExpr[v;w;p]) -> ParamExpr(v),fst(exprToIl ctx w someInt),fst(exprToIl ctx p someInt)
                                | ParamExpr(TupleExpr[v;w]) -> ParamExpr(v),fst(exprToIl ctx w someInt),[+Ldc_I4 0]
                                | _ -> cp,[+Ldc_I4 0],[+Ldc_I4 0]
                    let valParam, typ = callParamToIl ctx e None

                    let subWrite = match typ.kind with
                                   | TkOrd(OkInteger,_) -> "WRITEINTF"
                                   | TkOrd(OkBool,_) -> "WRITEBOOLEANF"
                                   | TkOrd(OkChar,_) -> "WRITECHARF"
                                   | TkFloat _ -> "WRITEREALF"
                                   | TkPointer _ -> "WRITEPOINTERF"
                                   | TkArray(AkSString _,_,_) -> "WRITESTRINGF"
                                   |> findMethodReference ctx
                    [
                        yield! file()
                        +Ldnull
                        yield! valParam
                        yield! w
                        yield! p
                        +Call subWrite
                    ]
                file, cp |> List.collect doParam

            let doRead() =
                // READINT - Integer argument
                // READSMALLINT - Small integer argument
                // READSHORTINT - Short integer argument
                // READWORD - Word argument
                // READBYTE - Byte argument
                // READBOOLEAN - Boolean argument
                // READCH - Character argument
                // READREAL - Real argument
                // READSTRING - String argument

                let file, cp =
                    match cp with
                    | ParamIdent(id)::tail ->
                        let sl, typ = findSymbolAndGetPtr ctx id
                        if typ.raw = ctx.details.sysTypes.file.raw then Some sl, tail
                        else None, cp
                    | _ -> None, cp
                let file() = if file.IsNone then fst <| findSymbolAndGetPtr ctx (stdIdent "STDINPUTFILE")
                             else file.Value

                let doParam = function
                    | ParamIdent(id) ->
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
            | IncProc, [ParamIdent(id)] -> deltaModify +1 id
            | DecProc, [ParamIdent(id)] -> deltaModify -1 id
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
                    +Newarr ctx.details.sysTypes.net_obj.raw
                    yield! cparams
                           |> List.mapi (
                                           fun idx (i, t) ->
                                               let putArrayElem i elems =
                                                   +Dup::+Ldc_I4 idx::i @
                                                   [yield! elems ; +Stelem Elem_Ref]
                                               match t with
                                               | _ when t = ctx.details.sysTypes.string ->
                                                   [+Call ctx.details.sysProc.PtrToStringAnsi]
                                                   // TODO critical handle ptr to strings! bug found in .NET 32 bit
                                                   |> putArrayElem (match ilToAtom i with | [Ldsfld f] -> [+Ldsflda f] | _ -> i)
                                               | _ when t = ctx.details.sysTypes.char ->
                                                   [
                                                       +Conv Conv_U1
                                                       +Call ctx.details.sysProc.ConvertU1ToChar
                                                       +Box ctx.details.moduleBuilder.TypeSystem.Char
                                                   ]
                                                   |> putArrayElem i
                                               | _ -> [+Box t.raw] |> putArrayElem i
                                        )
                           |> List.concat
                    +Call ctx.details.sysProc.WriteLine
                 ], None)
            | NewProc, [ParamIdent(id)] ->
                let ils, t = findSymbolAndGetPtr ctx id
                ([
                    yield! ils
                    +Ldc_I4 t.SizeOf
                    +Call ctx.details.sysProc.GetMem
                    +Dup
                    +Initobj t.raw
                    +Stind Ind_U
                ], None)
            | DisposeProc, [ParamIdent(id)] ->
                let ils, _ = findSymbolAndLoad ctx id
                ([
                    yield! ils
                    +Call ctx.details.sysProc.FreeMem
                ], None)
            | HaltProc, _ ->
                let exitCode = match cp with
                               | [] -> [+Ldc_I4 0]
                               | [cp] -> fst <| callParamToIl ctx cp None // TODO typecheck ?
                               | _ -> failwith "IE only one param allowed"
                ([

                     yield! exitCode
                     +Call ctx.details.sysProc.Exit
                ], None)
            | ChrFunc, _ ->
                let cp = match cp with | [cp] -> cp | _ -> failwith "IE only one param allowed"
                let callInstr, typ = callParamToIl ctx cp None
                ([
                    yield! callInstr
                    if popResult then yield +Pop // TODO or not generate call ?
                 ], Some ctx.details.sysTypes.char)
            | OrdFunc, _ ->
                let cp = match cp with | [cp] -> cp | _ -> failwith "IE only one param allowed"
                let callInstr, typ = callParamToIl ctx cp None
                ([
                    yield! callInstr
                    if popResult then yield +Pop // TODO or not generate call ?
                 ], Some ctx.details.sysTypes.int32)
            | SizeOfFunc, [ParamIdent(DIdent([PIName(id)]))] ->
                let sym = match ctx.FindSym(StringName id) with
                          | Some sym -> sym
                          | _ -> failwith "IE cannot find sym"

                let t = match sym with
                        | VariableParamSym (_,t,_) -> t
                        | VariableSym(vs, t) -> t
                        | TypeSym t -> t
                        | _ -> failwithf "IE cannot get size of %A" sym
                ([
                    +Ldc_I4 t.SizeOf
                    if popResult then +Pop // TODO or not generate call ?
                 ], Some ctx.details.sysTypes.int32)
            | _ -> failwith "IE"
        | _ -> failwith "IE"
    | _ -> failwith "IE"

and valueToValueKind ctx v (expectedType: PasType option) =
    match v with
    | VInteger i -> ValueKind([+Ldc_I4 i], ctx.details.sysTypes.int32)
    | VFloat f -> ValueKind([+Ldc_R4 (single f)], ctx.details.sysTypes.single)
    | VIdent i -> findSymbolAndLoad ctx i |> ValueKind
    | VString s ->
        // TODO protect string as char interpretation when needed
        match expectedType with
        | _ when s.Length = 1 -> // handle char
            let cv = int(s.Chars 0)
            ([+Ldc_I4 cv], ctx.details.sysTypes.char)
            |> ValueKind
        | _ -> // handle string
            if s.Length >= 256 then failwith "IE"
            let strBytes = (s + (String.replicate (256-s.Length) "\000")) |> Encoding.ASCII.GetBytes
            ([
                strBytes
                |> ctx.details.AddBytesConst
                |> Ldsfld |> (~+)
            ], ctx.details.sysTypes.string)
            |> ValueKind

// let _,v = ctx.EnsureVariable(ctx.details.sysTypes.string)
// TODO Ref count strin -> below some simple dynamic allocation for further rework
//            Ldc_I4 strBytes.Length
//            Call ctx.details.GetMem
//            Dup
//            Stloc v
//            strBytes
//            |> ctx.details.AddBytesConst
//            |> Ldsflda
//            Ldc_I4 strBytes.Length
//            // Unaligned 1uy // ??? https://stackoverflow.com/questions/24122973/what-should-i-pin-when-working-on-arrays/24127524
//            Cpblk
//            Ldloc v
    | VCallResult(ce) ->
        match doCall ctx ce false with
        | ils, Some t -> ValueKind(ils, t)
        | _ -> failwith "IE"
    | VNil -> ValueKind([+Ldnull], ctx.details.sysTypes.pointer)
    | _ -> failwith "IE"

and chainReaderFactory (ctx: Ctx) asValue addr ltp =
    let valOrPtr v p (t: TypeReference) = (if asValue || (t :? PointerType && addr = false) then v else p) |> List.singleton
    match ltp with
    | LTPVar(v, vt) -> match v with
                       | LocalVariable v -> valOrPtr +(Ldloc v) +(Ldloca v) vt.raw
                       | GlobalVariable v -> valOrPtr +(Ldsfld v) +(Ldsflda v) vt.raw
    | LTPParam(i,t,r) -> match r with // handle by ref params
                         | RefNone | RefConst -> valOrPtr +(Ldarg i) +(Ldarga i) t.raw
                         | RefVar ->
                             [
                                +Ldarg i
                                if asValue then
                                    +Ldobj t.raw
                             ]
                         | RefUntypedVar | RefUntypedConst ->
                             if asValue then failwith "IE"
                             [+Ldarg i]
    | LTPStruct (fld, _) -> valOrPtr +(Ldfld fld) +(Ldflda fld) fld.FieldType
    | LTPDeref (dt, force) ->
        if dt.raw.MetadataType = MetadataType.ValueType then
            if addr = false && force then [+Ldobj dt.raw]
            else []
        elif addr && force = false then []
        else
            dt.IndKind |> Ldind |> (~+) |> List.singleton
    | LTPNone -> []

and chainWriterFactory (ctx: Ctx) = function
    | LTPVar(v, _) -> match v with
                      | LocalVariable v -> [+Stloc v]
                      | GlobalVariable v -> [+Stsfld v]
    | LTPParam(i, t, r) -> match r with // handle by ref params
                           | RefNone | RefConst -> [+Starg i]
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
    | LTPParam(_,t,_) -> derefType t
    | _ -> failwith "cannot deref"

and findSymbolInternal value addr (ctx: Ctx) ident =
    let lastPoint = ref LTPNone
    let sl, t = findSymbol ctx ident
                |> chainToSLList
    sl
    |> List.collect (chainLoadToIl ctx lastPoint (chainReaderFactory ctx false false))
    |> fun l -> l @ (chainReaderFactory ctx value addr !lastPoint)
    ,match t with | Some t -> t | _ -> failwith "IE"

and findSymbolAndLoad = findSymbolInternal true false

and findSymbolAndGetPtr = findSymbolInternal false true

and chainLoadToIl ctx lastType factory symload =
    let res = factory !lastType
    match symload with
    | VariableLoad vs ->
        lastType := match vs with
                    | LoadVar vt -> LTPVar vt
                    | LoadParam p -> LTPParam p
        res
    | DerefLoad ->
        let dt = derefLastTypePoint !lastType
        match dt.kind with
        // TODO what with arrays ?
        | TkRecord _ -> lastType := LTPDeref(dt, false) // special case like someRec^.Field
        | _ -> lastType := LTPDeref(dt, true)
        res
    | ElemLoad (exprs, rt) ->
        lastType := LTPDeref(rt, false)
        [
            yield! res
            for e, d in exprs do
                // do not minus if not needed
                yield! fst <| exprToIl ctx (Multiply(Minus(e,d.low |> VInteger |> Value), d.elemSize |> VInteger |> Value)) (Some rt)
                yield +AddInst
        ]
    | StructLoad fds ->
        let instr, last, count = List.fold (fun (acc, _, c) f -> +Ldflda (fst f)::acc, f, c+1) ([],Unchecked.defaultof<_>,0) fds
        lastType := LTPStruct last
        res @ (List.take (count-1) instr)
    | ValueLoad evs ->
        lastType := LTPNone
        res @ [+Ldc_I4 evs]
    | CallableLoad (Referenced mr) ->
        if mr.result.IsNone then failwith "IE"
        [+Call mr.raw]

//let splitStruct = function
//    | StructLoad fdl ->
//        let fld =  fdl
//        (List.map (Ldflda >> ilResolve) fdl.Tail)
//        |> List.rev, LPStruct(fld.Head)
//    | _ -> failwith "IE"

//let splitArray = function
//    | ElemLoad fdl ->
//        let fld =  fdl
//        (List.map (Ldflda >> ilResolve) fdl.Tail)
//        |> List.rev, LPStruct(fld.Head)
//    | _ -> failwith "IE"

// type internal Marker = interface end
// let t = typeof<Marker>.DeclaringType
// let a = AssemblyDefinition.ReadAssembly(t.Assembly.Location);
// let methodToRef (m: System.Reflection.MethodInfo): MethodReference = a.MainModule.ImportReference(m)

let compileBlock (methodBuilder: MethodDefinition) (typeBuilder : TypeDefinition) (instr: List<MetaInstruction>) =
    let ilGenerator = methodBuilder.Body.GetILProcessor() |> emit
    typeBuilder.Methods.Add(methodBuilder)
    Seq.iter ilGenerator instr
    // ilGenerator (Call (methodToCall.GetElementMethod()))
    // if methodToCall.ReturnType <> null then
    //     ilGenerator (DeclareLocal methodToCall.ReturnType)
    //     ilGenerator Stloc_0
    //     ilGenerator (Ldloc_S 0uy)
    //     let tr = methodToCall.ReturnType
    //     let rt = System.Type.GetType(tr.FullName + ", " + tr.Scope.ToString()) // tr.Module.Assembly.FullName)
    //     let writeln = typeof<System.Console>.GetMethod("WriteLine", [| rt |]) |> moduleBuilder.ImportReference
    //     ilGenerator (Call(writeln))
    methodBuilder

let simplifiedDIdent = List.map <| function | PIName s -> s
let inline packedToStr(p: bool) = if p then "1" else "0"

let evalExprOp2 (opS: Option<string->string->string>) (opI: Option<int->int->int>) (r1, r2) =
    match       r1,     opS,     opI with
    | CERString s1, Some so,       _ -> match r2 with CERString s2 -> CERString(so s1 s2) | _ -> assert(false); CERUnknown
    | CERInt    i1,       _, Some io -> match r2 with CERInt    i2 -> CERInt   (io i1 i2) | _ -> assert(false); CERUnknown
    | _ -> CERUnknown

let evalExprOp1 (opS: Option<string->string>) (opI: Option<int->int>) r1 =
    match r1, opS, opI with
    | CERString s1, Some so, _ -> CERString(so s1)
    | CERInt i1, _, Some io -> CERInt(io i1)
    | _ -> CERUnknown

let rec evalConstExpr expr =

    let inline eval2 e1 e2 = (evalConstExpr e1, evalConstExpr e2)
    let inline eval1 e1 = evalConstExpr e1

    match expr with
    | Value v ->
        match v with
        | VFloat _ -> CERUnknown
        | VInteger i -> CERInt i
        | VString s -> CERString s
        | VIdent _ -> CERUnknown
        | VCallResult _ -> CERUnknown
        | VNil -> CERUnknown
        | VSet _ -> CERUnknown
    | Expr e -> evalConstExpr e
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


let evalConstExprToStr = function
    | ConstExpr expr ->
        match evalConstExpr expr with
        | CERString s -> s
        | CERInt i -> string i
        | CERUnknown -> ""
    | _ -> assert(false); ""

let dimenstionsToStr = List.map <| function | DimensionType s -> s | DimensionExpr (e1, e2) -> evalConstExprToStr e1 + ".." + evalConstExprToStr e2

let rec typeIdToStr = function
    | TIdString -> "$s"
    | TIdFile -> "$f"
    | TIdPointer(i, t) -> "$" + (String.replicate i "^") + (typeIdToStr t)
    | TIdSet(p, t) -> "$S" + packedToStr(p) + (typeIdToStr t)
    | TIdIdent(DIdent di) -> simplifiedDIdent di |> String.concat "$" |> (+) "$i"
    | TIdArray(ArrayDef(p, d, t)) -> "$a" + packedToStr(p) + (dimenstionsToStr d |> String.concat ",") + "$" + typeIdToStr t

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
    | MainScope of (string * TypeDefinition)
    | LocalScope of Ctx

type IlBuilder(moduleBuilder: ModuleDefinition) = class
    let mb = moduleBuilder
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

    let rec stmtToIl (ctx: Ctx) sysLabels (s: Statement): (MetaInstruction * BranchLabel ref list) =
        let stmtToIlList = stmtListToIlList ctx
        let exprToIl = exprToIl ctx
        let getVar4ForLoop = findSymbol ctx >>
                             function
                             | ChainLoad([VariableLoad(LoadVar(vs, vt))], _) -> vs, vt
                             | _ -> failwith "IE"
        let getVar4Assign (ident: DIdent) expr =
             // add param for findSymbol to set purpose (like this `assign`)
             match findSymbol ctx ident with
             | ChainLoad(symbols,_) ->
                 let ltp = ref LTPNone
                 let loadDest =
                     let load = List.collect (chainLoadToIl ctx ltp (chainReaderFactory ctx false false)) symbols
                     match symbols with // needed to proper store values to ref parameters in methods
                     | VariableLoad(LoadParam (i,_,RefVar))::[] -> +Ldarg i::load
                     | _ -> load
                 [
                     yield! loadDest
                     yield! (fst <| expr (Some (!ltp).ToTypeRef))
                     yield! chainWriterFactory ctx !ltp
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
                                     | LTPParam(_,t,_) -> t
                                     | LTPStruct(_,t) -> t
                                     | LTPDeref(dt,_) when dt.raw.MetadataType = MetadataType.ValueType -> dt
                                     | _ -> failwith "IE"
                            // TODO check type of vt for with ?
                            match vt.kind with | TkRecord _ -> () | _ -> failwithf "IE bad type for with %A" vt.kind
                            let pvt = PasType.NewPtr(vt)
                            let (_, vv) = ctx.EnsureVariable(pvt)
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
                    newSymbols::symbols
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
                    let condition = fst <| exprToIl expr (Some ctx.details.sysTypes.boolean)
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
                    let (name, var) = ctx.EnsureVariable()
                    let lastEndOfStm = ref ForwardLabel
                    let (defBranch, defLabels) = stmtToIlList stmt
                    // TODO reduce creation of new var if we want to just read variable
                    let (setCaseVar, _) = AssignStm(stdIdent name, expr) |> List.singleton |> stmtToIlList
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
                         yield! setCaseVar
                         yield! cases
                         yield! casesbodies
                        ]
                     , [yield! labels ; yield lastEndOfStm])
                | WhileStm (expr, stmt) ->
                    let condition = fst <| exprToIl expr (Some ctx.details.sysTypes.boolean)
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
                    let condition = fst <| exprToIl expr (Some ctx.details.sysTypes.boolean)
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
                    let (varFinalName, varFinal) = ctx.EnsureVariable(varType)
                    // TODO optimization for simple values (dont store in var)
                    let (loopInitializeVariables, _) =
                        [AssignStm(ident, initExpr);AssignStm(stdIdent varFinalName, finiExpr)] |> stmtToIlList
                    let breakLabel = ref ForwardLabel
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
                     //| t when t = ctx.details.sysTypes.string -> []//stmtListToIlList ctx (IfStm())
                     | _ -> []
                | GlobalVariable v ->
                     match v.FieldType with
                     //| t when t = ctx.details.sysTypes.string ->
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

    let vt = mb.ImportReference(typeof<ValueType>)
    let ot = mb.ImportReference(typeof<obj>)

    let findType =
        typeIdToStr

    let addMetaType (symbols: Dictionary<_,_>) (name: TypeName) typ =
        symbols.Add(name, TypeSym typ)
        typ

    let createStdTypes ns (symbols: Dictionary<_,_>) =
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
        addOrdType "Int64" mb.TypeSystem.Int64 OkInteger (OtSQWord(Int64.MinValue, Int64.MaxValue)) |> ignore
        let charType = addOrdType "Char" mb.TypeSystem.Byte OkChar (OtUByte(int Byte.MinValue, int Byte.MaxValue))
        let strType = {name=AnonName;raw=(ModuleDetails.NewSizedType ns mb vt 256 "") :> TypeReference;kind=TkUnknown 256}
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
        let fileType = (ModuleDetails.NewSizedType ns mb vt (256 + ptrSize) "") :> TypeReference
        let fileType = addMetaType symbols (TypedName TIdFile) {name=TypedName TIdFile;kind=TkUnknown(256 + ptrSize);raw=fileType}
        {
            int32 = addOrdType "Integer" mb.TypeSystem.Int32 OkInteger (OtSWord(int Int32.MinValue, int Int32.MaxValue))
            single = addFloatType "Real" mb.TypeSystem.Single
            string = addArrayType (TypedName TIdString) strType (TkArray(AkSString 255uy, [strDim], charType))
            char = charType
            file = fileType
            value = {name=AnonName;raw=vt;kind=TkUnknown 0}
            pointer = addAnyType (StringName "Pointer") (PointerType mb.TypeSystem.Void) (TkPointer({name=AnonName;raw=mb.TypeSystem.Void;kind=TkUnknown 0}))
            constParam = {name=AnonName;raw=PointerType(mb.TypeSystem.Void);kind=TkUnknown 0}
            varParam = {name=AnonName;raw=PointerType(mb.TypeSystem.Void);kind=TkUnknown 0}
            boolean = addOrdType "Boolean" mb.TypeSystem.Byte OkBool (OtUByte(0, 1))
            net_obj = {name=AnonName;raw=ot;kind=TkUnknown 0}
            net_void = {name=AnonName;raw=mb.TypeSystem.Void;kind=TkUnknown 0}
        }

    member self.BuildIl(block: Block, buildScope, ?resVar) =
        let ctx = match buildScope with
                  | MainScope (ns, tb) ->
                    let langCtx = LangCtx()
                    let newSymbols = Dictionary<TypeName,_>(langCtx)
                    let systemTypes = createStdTypes ns newSymbols
                    let systemProc = {
                        GetMem = allocMem
                        FreeMem = freeMem
                        WriteLine = writeLineMethod
                        Exit = exitMethod
                        ConvertU1ToChar = convertU1ToChar
                        PtrToStringAnsi = ptrToStringAnsi
                    }
                    let tsingle = systemTypes.single
                    newSymbols.Add(StringName "true", CERBool 0xFFuy |> ConstSym)
                    newSymbols.Add(StringName "false", CERBool 0uy |> ConstSym)
                    //newSymbols.Add("GetMem", Referenced allocMem |> MethodSym)
                    //newSymbols.Add("FreeMem", Referenced freeMem |> MethodSym)
                    let singleScalar raw =
                       Referenced {
                           paramList = [|{typ=tsingle;ref=false}|]
                           result = Some tsingle
                           raw = raw
                       } |> MethodSym
                    newSymbols.Add(StringName "Inc", Intrinsic IncProc |> MethodSym)
                    newSymbols.Add(StringName "Dec", Intrinsic DecProc |> MethodSym)
                    newSymbols.Add(StringName "Read", Intrinsic ReadProc |> MethodSym)
                    newSymbols.Add(StringName "Write", Intrinsic WriteProc |> MethodSym)
                    newSymbols.Add(StringName "ReadLn", Intrinsic ReadLnProc |> MethodSym)
                    newSymbols.Add(StringName "WriteLn", Intrinsic WriteLnProc |> MethodSym)
                    newSymbols.Add(StringName "WriteLine", Intrinsic WriteLineProc |> MethodSym)
                    newSymbols.Add(StringName "New", Intrinsic NewProc |> MethodSym)
                    newSymbols.Add(StringName "Dispose", Intrinsic DisposeProc |> MethodSym)
                    newSymbols.Add(StringName "Break", Intrinsic BreakProc |> MethodSym)
                    newSymbols.Add(StringName "Continue", Intrinsic ContinueProc |> MethodSym)
                    newSymbols.Add(StringName "Exit", Intrinsic ExitProc |> MethodSym)
                    newSymbols.Add(StringName "Halt", Intrinsic HaltProc |> MethodSym)
                    newSymbols.Add(StringName "SizeOf", Intrinsic SizeOfFunc |> MethodSym)
                    newSymbols.Add(StringName "Ord", Intrinsic OrdFunc |> MethodSym)
                    newSymbols.Add(StringName "Chr", Intrinsic ChrFunc |> MethodSym)
                    newSymbols.Add(StringName "Pred", Intrinsic PredProc |> MethodSym)
                    newSymbols.Add(StringName "Succ", Intrinsic SuccProc |> MethodSym)
                    newSymbols.Add(StringName "Round", singleScalar mathRound)
                    // function Abs(x: T): T;
                    // function Sqr(x: T): T;
                    // function Sin(x: Real): Real;
                    // function Cos(x: Real): Real;
                    // function Arctan(x: Real): Real;
                    newSymbols.Add(StringName "Exp", singleScalar mathExp)
                    newSymbols.Add(StringName "Ln", singleScalar mathLog)
                    newSymbols.Add(StringName "Trunc", singleScalar mathTrunc)
                    ModuleDetails.Create moduleBuilder ns tb systemTypes systemProc
                    |> Ctx.Create newSymbols langCtx
                  | LocalScope ctx -> ctx
        let newSymbols = ctx.symbols.Head
        let result = match resVar with
                     | Some (name, Some(v)) ->
                        ctx.variables.Head.Add v
                        match v with
                        | LocalVariable v -> v
                        | _ -> null
                     | Some (_, None) -> null // no result (void)
                     | _ -> null // main p`rogram

//        let rec sizeOf (typ: PasType) (td: TypeDefinition) =
//            let sizeOfTD (td: TypeDefinition) = td.Fields |> Seq.sumBy (fun f -> sizeOf f.FieldType null)
//            match typ.raw.MetadataType with
//            | MetadataType.SByte | MetadataType.Byte | MetadataType.Boolean  -> 1
//            | MetadataType.Int16 | MetadataType.UInt16 -> 2
//            | MetadataType.Int32 | MetadataType.UInt32 -> 4
//            | MetadataType.Int64  -> 8
//            | MetadataType.Single -> 4
//            | MetadataType.Double -> 8
//            | MetadataType.Void   -> ptrSize // TODO 4 or 8 -> target dependent
//            | MetadataType.Pointer-> ptrSize // TODO 4 or 8 -> target dependent
//            | MetadataType.ValueType ->
//                match ctx.FindType typ with
//                | Some(ArrayRange _) -> (tr :?> TypeDefinition).ClassSize
//                | Some(TypeSymbols _) ->
//                    // check mono_marshal_type_size -> https://github.com/dotnet/runtime/blob/487c940876b1932920454c44d2463d996cc8407c/src/mono/mono/metadata/marshal.c
//                    // check mono_type_to_unmanaged -> https://github.com/dotnet/runtime/blob/aa6d1ac74e6291b3aaaa9da60249d8c327593698/src/mono/mono/metadata/metadata.c
//                    sizeOfTD(tr :?> TypeDefinition)
//                | Some(SimpleType(s,_)) -> s
//                | _ when td <> null -> sizeOfTD td
//                | _ -> failwith "IE"
//            | _ -> failwith "IE"

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
                | _ -> failwith "IE"

        and addTypeArray dimensions tname name =
            let newSubType (dims, size) (typ: PasType, typSize) name =
                let size = size * typSize
                let at = ModuleDetails.NewSizedType ctx.details.ns mb vt size ""
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
                        let l = match evalConstExpr(l) with | CERInt i -> i | _ -> failwith "IE"
                        let h = match evalConstExpr(h) with | CERInt i -> i | _ -> failwith "IE"
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
            typ.raw.Name <- strName
            addType name typ
            //(doArrayDef dimensions tname (ref []) name).Name <- name



        //printfn "%A" block.decl
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
                           | TypeEnum enumValues ->
                               let max = enumValues |> List.fold (fun i v -> newSymbols.Add (StringName v, EnumValueSym(i)); i+1) 0
                               let name = StringName name
                               let pint32 = ctx.details.sysTypes.int32
                               addType name {name=name;kind=TkOrd(OkEnumeration, OtULong(0, max));raw=pint32.raw}
                           | Record (packed, fields) -> 
                                let mutable size = 0;
                                let td = TypeDefinition(ctx.details.ns, name, TypeAttributes.Sealed ||| TypeAttributes.BeforeFieldInit ||| TypeAttributes.SequentialLayout)
                                td.ClassSize <- 0
                                td.BaseType <- vt
                                td.PackingSize <- 1s // if packed then 1s else 0s
                                let fieldsMap = Dictionary<_,_>()
                                for (names, typeName) in fields do
                                  for name in names do
                                    let typ = match ctx.FindTypeId typeName with
                                              | Some t -> t | _ -> failwith "IE cannot find"
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
                    match value, ctype with
                    | ConstExpr expr, None -> ctx.symbols.Head.Add(StringName name, evalConstExpr expr |> ConstSym)
                    | _ -> failwith "IE"
                   ()
               | Labels labels -> (for l in labels do ctx.labels.Head.Add(l, ref (UserLabel l)))
               | ProcAndFunc((name, mRes, mPara), d) ->
                   let name = match name with
                              | Some n -> n
                              | _ -> failwith "name expected"
                   let methodBuilder, newMethodSymbols, rVar =
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
                                            | _ -> ctx.details.sysTypes.net_void, None

                           let ps = defaultArg mPara []
                                    |> List.collect
                                       (fun (k, (ps, t)) ->
                                        let t = match t with Some t -> ctx.FindTypeId t | _ -> None
                                        [for p in ps do
                                            let (typ, byref, t, isref) =
                                                match k, t with
                                                | Some Const, Some t -> t.raw, RefConst, t, false
                                                | Some Var, Some t -> ByReferenceType(t.raw) :> TypeReference, RefVar, t, true
                                                | Some Const, None -> ctx.details.sysTypes.constParam.raw, RefUntypedConst, ctx.details.sysTypes.constParam, true
                                                | Some Var, None -> ctx.details.sysTypes.varParam.raw, RefUntypedVar, ctx.details.sysTypes.varParam, true
                                                | None, Some t -> t.raw, RefNone, t, false
                                                | _ -> failwith "IE"
                                            let pd = ParameterDefinition(p, ParameterAttributes.None, typ)
                                            newMethodSymbols.Add(StringName p, VariableParamSym(pd, t, byref))
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
                           newSymbols.Add(StringName name, Referenced methodInfo |> MethodSym)
                           md, newMethodSymbols, rVar
                   match d with
                   | BodyDeclr (decls, stmts) ->
                       let scope = LocalScope(ctx.Inner newMethodSymbols)
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
                       ctx.forward.Add(name, (methodBuilder, newMethodSymbols, rVar))
                   | _ -> failwith "no body def"
               | _ -> ())

        stmtListToIl block.stmt ctx result
end