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

type BranchLabel =
    | LazyLabel of IlInstruction
    | Label of Instruction
    | ForwardLabel
    | UserLabel of string
    | IgnoreLabel

and AtomIlInstruction =
    | Unknown
    | Call of MethodReference
    | Ldc_I4 of int
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
    | Stsfld of FieldDefinition
    | Starg of ParameterDefinition
    | Stloc of VariableDefinition
    | Stfld of FieldDefinition
    | Stind of IndirectKind
    | Conv_I
    | Cpblk
    | Cpobj of TypeReference
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
    | Resolved of Instruction

and IlInstruction =
    | IlAtom of AtomIlInstruction ref
    | IlBrfalse of BranchLabel ref
    | IlBrtrue of BranchLabel ref
    | IlBr of BranchLabel ref
    | IlBeq of BranchLabel ref // =
    | IlBlt of BranchLabel ref // <
    | IlBgt of BranchLabel ref // >
    | IlBle of BranchLabel ref // <=
    | IlBge of BranchLabel ref // >=
    | IlResolved of Instruction

type MetaInstruction =
    | DeclareLocal of VariableDefinition
    | InstructionSingleton of IlInstruction
    | InstructionList of IlInstruction list
    | HandleFinally of IlInstruction list * IlInstruction list * Instruction * Instruction

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
    | ExitProc
    | WriteLnProc

type MethodSym =
    | Referenced of MethodReference
    | Intrinsic of Intrinsic
with
    member self.ReturnType =
        match self with
        | Referenced mr -> mr.ReturnType
        | Intrinsic _ -> null

type Symbol =
    | VariableParamSym of (ParameterDefinition * TypeReference * bool)
    | VariableSym of VariableKind
    | MethodSym of MethodSym
    | EnumValueSym of int
    | WithSym of VariableKind * TypeReference
    | UnknownSym

type ArrayDim = { low: int; high: int; size: int; elemSize: int; elemType: TypeReference; selfType: TypeReference ref }

type VariableLoadKind =
    | LoadParam of (ParameterDefinition * TypeReference * bool)
    | LoadVar of VariableKind

type SymbolLoad =
    | DerefLoad
    | ElemLoad of (ExprEl * ArrayDim) list * TypeReference
    | StructLoad of FieldDefinition list
    | VariableLoad of VariableLoadKind
    | ValueLoad of int
    | CallableLoad of MethodSym

type ChainLoad =
    | ChainLoad of (SymbolLoad list * TypeReference)
    | SymbolLoadError

let chainToSLList = function
    | ChainLoad sl -> sl
    | SymbolLoadError -> failwith "IE"

let caseSensitive = HashIdentity.Structural<string>
let caseInsensitive =
    HashIdentity.FromFunctions
        (fun (s: string) -> s.GetHashCode(StringComparison.InvariantCultureIgnoreCase))
        (fun a b -> String.Equals(a, b, StringComparison.InvariantCultureIgnoreCase))

type SymbolsDict<'T>() =
    inherit Dictionary<string, 'T>()

type LangCtx() =
    let mutable ec = caseInsensitive
    member self.caseSensitive
        with get() = ec = caseSensitive
        and set(cs) =  ec <- if cs then caseSensitive else caseInsensitive

    interface IEqualityComparer<string> with
        member _.GetHashCode(x) = ec.GetHashCode(x)
        member _.Equals(x,y) = ec.Equals(x, y)

type TypeInfo =
    | SimpleType of int
    | TypeSymbols of Dictionary<string,FieldDefinition> * int
    | ArrayRange of ArrayDim list * TypeReference

type SystemTypes = {
        string: TypeReference
        char: TypeReference
        value: TypeReference
        pointer: TypeReference
        boolean: TypeReference
        net_obj: TypeReference
    }

type SystemProc = {
        GetMem: MethodReference
        FreeMem: MethodReference
        WriteLine: MethodReference
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
            let attributes = TypeAttributes.ExplicitLayout ||| TypeAttributes.AnsiClass ||| TypeAttributes.Sealed ||| TypeAttributes.Public
            let at = TypeDefinition(ns, name + string size, attributes)
            at.ClassSize <- size
            at.PackingSize <- 0s;
            at.BaseType <- vt
            mb.Types.Add(at)
            at

    member self.SelectAnonSizeType size =
        match self.anonSizeTypes.TryGetValue size with
        | true, t -> t
        | _ ->
            let at = ModuleDetails.NewSizedType self.ns self.moduleBuilder self.sysTypes.value size "anon"
            self.anonSizeTypes.Add(size, at)
            at

    member self.AddBytesConst (bytes: byte[]) =
        let ast = self.SelectAnonSizeType bytes.Length
        let fd = FieldDefinition(null, FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.HasFieldRVA, ast)
        fd.InitialValue <- bytes
        self.tb.Fields.Add fd
        fd

type Ctx = {
        variables: Dictionary<string,VariableKind> list
        labels: Dictionary<string, BranchLabel ref> list
        types: Dictionary<TypeIdentifier, TypeReference> list
        symbols: Dictionary<string, Symbol> list
        typeInfo: Dictionary<TypeReference,TypeInfo> list
        forward: Dictionary<string, MethodDefinition>
        localVariables: int ref
        lang: LangCtx
        res: List<MetaInstruction>
        details: ModuleDetails
    } with
    static member Create types symbols typeInfo (lang: LangCtx) details =
        {
            variables = [Dictionary<_,_>(lang)]
            labels = [Dictionary<_,_>(lang)]
            types = [types]
            symbols = [symbols]
            typeInfo = [typeInfo]
            forward = Dictionary<_, _>()
            localVariables = ref 0
            lang = lang
            res = List<MetaInstruction>()
            details = details
        }

    member self.Inner newSymbols =
        { self with
            symbols = newSymbols::self.symbols
            localVariables = ref 0
            variables = Dictionary<_,_>(self.lang)::self.variables
            labels = Dictionary<_,_>(self.lang)::self.labels
            res = List<MetaInstruction>()}

    member self.FindSym sym =
        self.symbols |> List.tryPick (fun st -> match st.TryGetValue sym with | true, v -> Some v | _ -> None)
        
    member self.EnsureVariable(?kind) =
        let ikey = !self.localVariables
        let key = string ikey
        incr self.localVariables
        // TODO manager of variables for reuse
//        let result = match self.variables.TryGetValue key with
//                     | true, value ->
//                         value
//                     | _ ->
        let varDef = defaultArg kind self.details.moduleBuilder.TypeSystem.Int32 |> VariableDefinition
        let varKind = varDef |> LocalVariable
        self.res.Add(DeclareLocal(varDef))
        self.variables.Head.Add(key, varKind)
        self.symbols.Head.Add(key, VariableSym(varKind))
        (key, varDef)

    member self.findLabel name =
        self.labels |> List.tryPick (fun l -> match l.TryGetValue name with | true, bl-> Some bl | _ -> None)

    member self.findTypeInfo typeRef =
        self.typeInfo |> List.tryPick (fun l -> match l.TryGetValue typeRef with | true, ti-> Some ti | _ -> None)

    member self.findLabelUnsafe name =
        match self.findLabel name with
        | Some bl -> bl
        | _ -> failwithf "IE cannot find label %s" name

    member self.findTypeInfoUnsafe typeRef =
        match self.findTypeInfo typeRef with
        | Some ti -> ti
        | _ -> failwithf "IE cannot find typ info %A" typeRef

    static member resolveSysLabels head labels =
        match head with
        | Some h ->
            match h with
            | IlResolved il -> for l in labels do l := Label(il)
            | IlBr _ -> for l in labels do l := LazyLabel(h)
            | _ -> failwithf "Internal error (%s:%s)" __SOURCE_FILE__ __LINE__
        | _ -> ()

let inline toMap kvps =
    kvps
    |> Seq.map (|KeyValue|)
    |> Map.ofSeq

let brtoinstr l opc =
    let rec bril = function
           | Label l -> l
           | LazyLabel l -> match l with
                            | IlResolved i -> i
                            | IlBrfalse i -> bril !i
                            | IlBrtrue i -> bril !i
                            | IlBr i -> bril !i
                            | IlBeq i -> bril !i
                            | _ -> failwithf "Internal error (%s:%s)" __SOURCE_FILE__ __LINE__
           | IgnoreLabel -> null
           | _ -> failwithf "Internal error (%s:%s)" __SOURCE_FILE__ __LINE__
           
    let instr = bril !l
    if instr <> null then
        Instruction.Create(opc, instr)
    else null

let private ainstr = function
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
                    | Ldc_I4 n     -> Instruction.Create(OpCodes.Ldc_I4, n)
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
                    | Conv_I       -> Instruction.Create(OpCodes.Conv_I)
                    | Cpblk        -> Instruction.Create(OpCodes.Cpblk)
                    | Cpobj t      -> Instruction.Create(OpCodes.Cpobj, t)
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
                    | Resolved i   -> i

let metaToIlList = function 
       | InstructionList l -> l
       | InstructionSingleton s -> [s]
       // may be extended for inline variables in the future
       | _ -> failwith "IE not supported metaToIlList"

let private instr = function
                    | IlBrfalse i  -> brtoinstr i OpCodes.Brfalse
                    | IlBrtrue i   -> brtoinstr i OpCodes.Brtrue
                    | IlBr i       -> brtoinstr i OpCodes.Br
                    | IlBeq i      -> brtoinstr i OpCodes.Beq
                    | IlBgt i      -> brtoinstr i OpCodes.Bgt 
                    | IlBlt i      -> brtoinstr i OpCodes.Blt 
                    | IlBge i      -> brtoinstr i OpCodes.Bge 
                    | IlBle i      -> brtoinstr i OpCodes.Ble 
                    | IlResolved i -> i
                    | IlAtom i     -> let r = ainstr !i
                                      i := Resolved(r)
                                      r

let private emit (ilg : Cil.ILProcessor) inst =
    let appendIfNotNull i = if i <> null then ilg.Append i
    let appendIfNotNullReplace (leave: Instruction) (i: Instruction) =
        if i <> null then
            if i.OpCode = OpCodes.Ret then
                ilg.Append (Instruction.Create(OpCodes.Leave, leave))
            else
                ilg.Append i
    match inst with
    | DeclareLocal t -> t |> ilg.Body.Variables.Add
    | InstructionSingleton is -> instr is |> appendIfNotNull
    | InstructionList p -> p |> List.iter (instr >> appendIfNotNull)
    | HandleFinally (i, f, l, e) ->
        // TODO rewrite this for exit in functions, test code
        let processList (list: IlInstruction list) =
            let s = list.Head |> instr
            s |> appendIfNotNullReplace l
            list.Tail |> List.iter (instr >> appendIfNotNullReplace l)
            s
        let start = processList i
        ilg.Append(Instruction.Create(OpCodes.Leave, l))
        let startFinally = processList f
        ilg.Append l
        ExceptionHandler(ExceptionHandlerType.Finally,
                                       TryStart     = start,
                                       TryEnd       = startFinally,
                                       HandlerStart = startFinally,
                                       HandlerEnd   = ilg.Body.Instructions.Last())
        |> ilg.Body.ExceptionHandlers.Add
        ilg.Append(e)

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

// type may be pinned
let absVariableType (vt: TypeReference) =
    match vt with
    | :? PinnedType as pt -> pt.ElementType
    | _ -> vt

let derefType (t: TypeReference) =
    match t with
    | :? PointerType as pt -> pt.ElementType
    | _ -> failwith "Cannot dereference non pointer type"

let findSymbol (ctx: Ctx) (DIdent ident) =
    let mainSym = ident.Head |> function | PIName n -> ctx.FindSym n
    
    let rec findSym (ref: MemberReference) acc = function
    | (Designator.Array a)::t -> acc, Designator.Array(a)::t // TODO ?
    | PIName(h)::t ->
        // TODO ? here is solved auto records dereference (x.y instead of x^.y) and dereference for array elements
        let ref = match ref with | :? PointerType as pt -> pt.ElementType | _ -> ref :?> TypeReference
        let symbols = match ctx.findTypeInfoUnsafe ref with | TypeSymbols (d,_) -> d | _ -> failwith "IE"
        let sym = symbols.[h]
        findSym sym.FieldType (sym::acc) t
    | t -> acc, t
    
    let rec resolveTail acc (vt: TypeReference)  = function
        | [] -> (List.rev acc, vt)
        | h::t ->
            match h with
            | Deref ->
                // TODO check dereferencable
                let tref = vt :?> TypeSpecification
                resolveTail (DerefLoad::acc) tref.ElementType t
            | Ident _ -> 
                let sl, restOfTail = findSym vt [] (h::t)
                let vt = (List.last sl).FieldType
                resolveTail (StructLoad(sl)::acc) vt restOfTail
            | Designator.Array exprs ->
                let tref = vt :?> TypeDefinition
                let dims, elemTyp = match ctx.findTypeInfo tref with
                                    | Some(ArrayRange( dims , elemTyp )) -> dims, elemTyp
                                    | _ -> failwith "IE"
                // TODO rework this slow comparison
                if exprs.Length > dims.Length then failwith "IE"
                // TODO assign sub array parts?
                let elems, tail = List.splitAt exprs.Length dims
                let elemLoad = elems
                               |> List.zip exprs
                               |> fun dims -> ElemLoad (dims, elemTyp)
                // TODO validation of dims ?
                let subElemTyp = match List.tryHead tail with | Some h -> !h.selfType | _ -> elemTyp
                let newAcc = match ctx.findTypeInfo subElemTyp with
                             | Some(ArrayRange _) -> elemLoad::acc
                             | _ -> elemLoad::acc
                resolveTail newAcc subElemTyp t

    let varLoadChain vt dlist vd =
        let t = absVariableType vt
        let (tail, finalType) = resolveTail [] t dlist
        ChainLoad(VariableLoad(vd)::tail, finalType)

    let mainSym = defaultArg mainSym UnknownSym

    match mainSym with
    | VariableSym v -> LoadVar v |> varLoadChain (v.Type()) ident.Tail
    | WithSym (v, td) -> LoadVar v |> varLoadChain td ident
    | EnumValueSym(i) when ident.Tail = [] -> ChainLoad([ValueLoad(i)], ctx.details.moduleBuilder.TypeSystem.Int32)
    | MethodSym r ->  ChainLoad([CallableLoad r], r.ReturnType)
    | VariableParamSym ((i,t,r) as p) -> LoadParam p |> varLoadChain t ident.Tail
    | _ -> SymbolLoadError

let ilResolveInstr aii =
    let res = aii |> ainstr
    res, res |> IlResolved
let ilResolve = ainstr >> IlResolved
let ilResolveArray = ilResolve >> List.singleton

type LastTypePoint =
    | LTPVar of VariableKind * TypeReference
    | LTPParam of ParameterDefinition * TypeReference * bool
    | LTPDeref of TypeReference
    | LTPStruct of FieldDefinition
    | LTPNone
with
    member self.ToTypeRef =
        match self with
        | LTPVar(_,tr) -> tr
        | LTPParam(_,tr,_) -> tr
        | LTPDeref tr -> tr
        | LTPStruct fd -> fd.FieldType
        | _ -> failwith "IE"

let findFunction (ctx: Ctx) ident =
        let callChain = findSymbol ctx ident
        // TODO more advanced calls like foo().x().z^ := 10
        match callChain with
        | ChainLoad([CallableLoad cl], _) -> cl
        | _ -> failwith "Not supported"
        |> fun ms ->
           match ms with
           | Referenced mr -> ms, Some(Call(mr) |> ilResolve)
           | Intrinsic _ -> ms, None

type ValueKind = ValueKind of IlInstruction list * TypeReference

let rec exprToIl (ctx: Ctx) exprEl expectedType =
    let rec exprToMetaExpr el et =
        let add2OpIl a b i =
            match exprToMetaExpr a et with
            | ValueKind(a, at) ->
                match exprToMetaExpr b at with
                | ValueKind(b, bt) ->
                    ([
                        yield! a
                        yield! b
                        yield ilResolve i
                    ], at) |> ValueKind
        let inline add1OpIl a i =
            match exprToMetaExpr a et with
            | ValueKind(a, at) -> ValueKind([ yield! a; yield ilResolve i], ctx.details.moduleBuilder.TypeSystem.Int32)
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
            match add2OpIl a b Ceq with
            | ValueKind(o, ot) -> ([ yield! o; ilResolve (Ldc_I4(0)); ilResolve Ceq ], ctx.details.moduleBuilder.TypeSystem.Boolean) |> ValueKind
        | StrictlyLessThan(a, b) -> add2OpIl a b Clt
        | StrictlyGreaterThan(a, b) -> add2OpIl a b Cgt
        | LessThanOrEqual(a, b) ->
            match add2OpIl a b Cgt with
            | ValueKind(o, ot) -> ([ yield! o; ilResolve (Ldc_I4(0)); ilResolve Ceq ], ctx.details.moduleBuilder.TypeSystem.Boolean) |> ValueKind
        | GreaterThanOrEqual(a, b) ->
            match add2OpIl a b Clt with
            | ValueKind(o, ot) -> ([ yield! o; ilResolve (Ldc_I4(0)); ilResolve Ceq ], ctx.details.moduleBuilder.TypeSystem.Boolean) |> ValueKind
        | Addr(a) ->
            ([
                match a with
                | Value(VIdent i) -> yield! (findSymbolAndGetPtr ctx i |> fst)
                | _ -> failwith "IE"
            ], ctx.details.sysTypes.pointer) |> ValueKind
        | _ -> failwith "IE"

    match exprToMetaExpr exprEl expectedType with
    | ValueKind (a, at) -> a, at

and callParamToIl ctx cp idx (mr: MethodReference) =
    let param = if mr <> null then mr.Parameters.[idx].ParameterType else null
    match cp with
    | ParamExpr expr -> exprToIl ctx expr param
    | ParamIdent id ->
        if param <> null && param :? ByReferenceType then
            findSymbolAndGetPtr ctx id
        else
            findSymbolAndLoad ctx id

and doCall (ctx: Ctx) (CallExpr(ident, cp)) popResult =
    let mr, f = findFunction ctx ident
    match mr, f with
    | Referenced mr, Some f ->
        ([
            yield! cp
            |> List.mapi (fun i p -> fst <| callParamToIl ctx p i mr)
            |> List.concat
            yield f
            if popResult && mr.ReturnType.MetadataType <> MetadataType.Void then
                yield Pop |> ilResolve
         ], mr.ReturnType)
    | Intrinsic i, _ ->
        let deltaModify instr id =
            ([
             yield! findSymbolAndGetPtr ctx id |> fst
             Dup |> ilResolve
             Ldind(Ind_I4) |> ilResolve
             Ldc_I4 1 |> ilResolve
             instr |> ilResolve
             Stind(Ind_I4) |> ilResolve
            ], ctx.details.moduleBuilder.TypeSystem.Void)
        match i, cp with
        | IncProc, [ParamIdent(id)] -> deltaModify AddInst id
        | DecProc, [ParamIdent(id)] -> deltaModify MinusInst id
        | ExitProc, [] -> // TODO handle Exit(result);
            ([
                Ret |> ilResolve
             ], ctx.details.moduleBuilder.TypeSystem.Void)
        | WriteLnProc, _ ->
            let high = ref 0
            let cparams,str = cp |> List.mapi (fun i p -> incr high; callParamToIl ctx p i null, sprintf "{%d}" i) |> List.unzip
            let str = String.Concat(str)
            ([
                Ldstr str |> ilResolve
                Ldc_I4 !high |> ilResolve
                Newarr ctx.details.sysTypes.net_obj |> ilResolve
                yield! cparams
                       |> List.mapi (
                                       fun idx (i, t) ->
                                           let putArrayElem elem =
                                               (Dup |> ilResolve)::(Ldc_I4 idx |> ilResolve)::i @
                                               [elem ; Stelem Elem_Ref |> ilResolve]

                                           match t with
                                           | _ when t = ctx.details.sysTypes.string ->
                                               // TODO handle string https://stackoverflow.com/questions/144176/fastest-way-to-convert-a-possibly-null-terminated-ascii-byte-to-a-string
                                               Call ctx.details.sysProc.PtrToStringAnsi |> ilResolve
                                               |> putArrayElem
                                           | _ -> Box t |> ilResolve |> putArrayElem
                                    )
                       |> List.concat
                Call(ctx.details.sysProc.WriteLine) |> ilResolve
             ], ctx.details.moduleBuilder.TypeSystem.Void)
        | _ -> failwith "IE"
    | _ -> failwith "IE"

and valueToValueKind ctx v expectedType =
    match v with
    | VInteger i -> ValueKind(Ldc_I4(i) |> ilResolveArray, ctx.details.moduleBuilder.TypeSystem.Int32)
    | VIdent i -> findSymbolAndLoad ctx i |> ValueKind
    | VString s ->
        // TODO protect string as char interpretation when needed
        match expectedType = ctx.details.sysTypes.char with
        | true -> // handle char
            if s.Length <> 1 then failwith "IE"
            let cv = byte(s.Chars 0)
            ([
                Ldc_I4 (int cv) |> ilResolve
            ], ctx.details.sysTypes.char)
            |> ValueKind
        | false -> // handle string
            if s.Length >= 256 then failwith "IE"
            let strBytes = (s + (String.replicate (256-s.Length) "\000")) |> Encoding.ASCII.GetBytes
            ([
                strBytes
                |> ctx.details.AddBytesConst
                |> Ldsfld |> ilResolve
            ], ctx.details.sysTypes.string)
            |> ValueKind

// let _,v = ctx.EnsureVariable(ctx.details.sysTypes.string)
// TODO Ref count strin -> below some simple dynamic allocation for further rework
//            Ldc_I4 strBytes.Length |> ilResolve
//            Call ctx.details.GetMem |> ilResolve
//            Dup |> ilResolve
//            Stloc v |> ilResolve
//            strBytes
//            |> ctx.details.AddBytesConst
//            |> Ldsflda |> ilResolve
//            Ldc_I4 strBytes.Length |> ilResolve
//            // Unaligned 1uy |> ilResolve // ??? https://stackoverflow.com/questions/24122973/what-should-i-pin-when-working-on-arrays/24127524
//            Cpblk |> ilResolve
//            Ldloc v |> ilResolve
    | VCallResult(ce) -> doCall ctx ce false |> ValueKind
    | _ -> failwith "IE"

and chainReaderFactory asValue addr ltp =
    let valOrPtr v p (t: TypeReference) = (if asValue || (t :? PointerType && addr = false) then v else p) |> ilResolveArray
    match ltp with
    | LTPVar(v, vt) -> match v with
                       | LocalVariable v -> valOrPtr (Ldloc v) (Ldloca v) vt
                       | GlobalVariable v -> valOrPtr (Ldsfld v) (Ldsflda v) vt
    | LTPParam(i,t,r) -> match r with // handle by ref params
                         | false -> valOrPtr (Ldarg i) (Ldarga i) t
                         | true ->
                             [
                                Ldarg i |> ilResolve
                                if asValue then
                                    Ldobj(t) |> ilResolve
                                //if asValue then
                                //    yield! chainReaderFactory asValue addr (LTPDeref t)
                             ]
    | LTPStruct fld -> valOrPtr (Ldfld fld) (Ldflda fld) fld.FieldType
    | LTPDeref dt ->
        if dt.MetadataType = MetadataType.ValueType then []
        else
            let resolveMetadataType = function
                | MetadataType.Pointer -> Ldind(Ind_I)
                | MetadataType.SByte   -> Ldind(Ind_I1)
                | MetadataType.Int16   -> Ldind(Ind_I2)
                | MetadataType.Int32   -> Ldind(Ind_I4)
                | MetadataType.Int64   -> Ldind(Ind_I8)
                | MetadataType.Byte    -> Ldind(Ind_U1)
                | MetadataType.UInt16  -> Ldind(Ind_U2)
                | MetadataType.UInt32  -> Ldind(Ind_U4)
                | MetadataType.UInt64  -> Ldind(Ind_U8)
                | _ -> failwith "IE"
            match dt.MetadataType with
            | MetadataType.Class -> resolveMetadataType (dt.Resolve().BaseType.MetadataType)
            | _ -> resolveMetadataType dt.MetadataType
            |> ilResolveArray
    | LTPNone -> []

and chainWriterFactory (ctx: Ctx) = function
    | LTPVar(v, _) -> match v with
                      | LocalVariable v -> [Stloc v |> ilResolve]
                      | GlobalVariable v -> [Stsfld v |> ilResolve]
    | LTPParam(i, t, r) -> match r with // handle by ref params
                           | false -> [Starg i |> ilResolve]
                           | true -> chainWriterFactory ctx (LTPDeref t)
    | LTPStruct fld -> [fld |> Stfld |> ilResolve]
    | LTPDeref dt ->
        if dt.MetadataType = MetadataType.ValueType then
            let dt = dt :?> TypeDefinition
            let size = match ctx.findTypeInfo dt with
                       | Some(TypeSymbols (_, size)) -> size
                       | Some(SimpleType size) -> size
                       | _ -> failwith "IE"
            [
                Ldc_I4(size) |> ilResolve
//                if dt.PackingSize = 1s then
//                    Unaligned(byte(dt.PackingSize)) |> ilResolve
                Cpblk |> ilResolve
            ]
        else
            let resolveMetadataType = function
                | MetadataType.Pointer -> Stind(Ind_I)
                | MetadataType.SByte -> Stind(Ind_I1)
                | MetadataType.Int16 -> Stind(Ind_I2)
                | MetadataType.Int32 -> Stind(Ind_I4)
                | MetadataType.Int64 -> Stind(Ind_I8)
                | MetadataType.Byte -> Stind(Ind_U1)
                | MetadataType.UInt16 -> Stind(Ind_U2)
                | MetadataType.UInt32 -> Stind(Ind_U4)
                | MetadataType.UInt64 -> Stind(Ind_U8)
                | _ -> failwith "IE"
            match dt.MetadataType with
            | MetadataType.Class -> resolveMetadataType (dt.Resolve().BaseType.MetadataType)
            | _ -> resolveMetadataType dt.MetadataType
            |> ilResolveArray
    | LTPNone -> []

and derefLastTypePoint = function
    | LTPVar(_, t) -> derefType t
    | LTPDeref t -> derefType t
    | LTPStruct fd -> derefType fd.FieldType
    | _ -> failwith "cannot deref"

and findSymbolInternal value addr (ctx: Ctx) ident =
    let lastPoint = ref LTPNone
    let sl, t = findSymbol ctx ident
                |> chainToSLList
    sl
    |> List.collect (chainLoadToIl ctx lastPoint (chainReaderFactory false false))
    |> fun l -> l @ (chainReaderFactory value addr !lastPoint)
    ,t

and findSymbolAndLoad = findSymbolInternal true false

and findSymbolAndGetPtr = findSymbolInternal false true

and chainLoadToIl ctx lastType factory symload =
    let res = factory !lastType
    match symload with
    | VariableLoad vs ->
        lastType := match vs with
                    | LoadVar vs -> LTPVar(vs, absVariableType (vs.Type()))
                    | LoadParam p -> LTPParam p
        res
    | DerefLoad ->
        let dt = derefLastTypePoint !lastType
        lastType := LTPDeref dt
        res
    | ElemLoad (exprs, rt) ->
        lastType := LTPDeref rt
        [
            yield! res
            for e, d in exprs do
                // do not minus if not needed
                yield! fst <| exprToIl ctx (Multiply(Minus(e,d.low |> VInteger |> Value), d.elemSize |> VInteger |> Value)) rt
                yield AddInst |> ilResolve
        ]
    | StructLoad fds ->
        let instr, last, count = List.fold (fun (acc, _, c) f -> (Ldflda(f) |> ilResolve)::acc, f, c+1) ([],null,0) fds
        lastType := LTPStruct last
        res @ (List.take (count-1) instr)
    | ValueLoad evs ->
        lastType := LTPNone
        res @ (Ldc_I4(evs) |> ilResolveArray)
    | CallableLoad (Referenced mr) ->
        if mr.ReturnType.MetadataType = MetadataType.Void then failwith "IE"
        Call(mr) |> ilResolveArray

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

type ConstEvalResult =
    | CERString of string
    | CERInt of int
    | CERUnknown

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
    | UnaryPlus(e1)    -> eval1 e1 |> evalExprOp1 None (Some (~+) )
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

let stdIdent = PINameCreate >> List.singleton >> DIdent
let stdType  = stdIdent >> TIdIdent

let (|IlNotEqual|_|) (items: IlInstruction[]) =
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
        match last3 with
        | [|IlResolved(i1);IlResolved(i2);IlResolved(i3)|]
            when i1.OpCode = OpCodes.Ceq && i2.OpCode = OpCodes.Ldc_I4 && i3.OpCode = OpCodes.Ceq -> Some(IlNotEqual) 
        | _ -> None

type BuildScope =
    | MainScope of (string * TypeDefinition)
    | LocalScope of Ctx

type IlBuilder(moduleBuilder: ModuleDefinition) = class
    let mb = moduleBuilder
    let writeLineMethod = 
        typeof<System.Console>.GetMethod("WriteLine", [| typeof<string> ; typeof<obj array> |]) |> moduleBuilder.ImportReference
    let ptrToStringAnsi =
        typeof<System.Runtime.InteropServices.Marshal>.GetMethod("PtrToStringAnsi", [| typeof<nativeint> |]) |> moduleBuilder.ImportReference
    let allocMem =
        typeof<System.Runtime.InteropServices.Marshal>.GetMethod("AllocCoTaskMem")  |> moduleBuilder.ImportReference
    let freeMem =
        typeof<System.Runtime.InteropServices.Marshal>.GetMethod("FreeCoTaskMem")  |> moduleBuilder.ImportReference

    let rec stmtToIl (ctx: Ctx) sysLabels (s: Statement): (MetaInstruction * BranchLabel ref list) =
        let stmtToIlList = stmtListToIlList ctx
        let exprToIl = exprToIl ctx
        let getVar4ForLoop = findSymbol ctx >>
                             function
                             | ChainLoad([VariableLoad(LoadVar(LocalVariable vs))], _) -> vs
                             | _ -> failwith "IE"
        let getVar4Assign (ident: DIdent) expr =
             // add param for findSymbol to set purpose (like this `assign`)
             match findSymbol ctx ident with
             | ChainLoad(symbols,_) ->
                 let ltp = ref LTPNone
                 [
                     yield! match symbols.Head with // for non value types we need extra value on stack (for stind.x)
                            | VariableLoad(LoadParam (i,t,r)) when r && t.MetadataType <> MetadataType.ValueType ->
                                Ldarg i |> ilResolveArray
                            | _ -> []
                     yield! List.collect (chainLoadToIl ctx ltp (chainReaderFactory false false)) symbols
                     yield! (fst <| expr (!ltp).ToTypeRef)
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
                            let cl = List.collect (chainLoadToIl ctx ltp (chainReaderFactory false false)) symbols
                            let vt = match !ltp with // TODO allow structs only ? (ValueType as records/classes only)
                                     | LTPVar(_,t) -> t
                                     | LTPParam(_,t,_) -> t
                                     | LTPStruct f -> f.FieldType
                                     | LTPDeref dt when dt.MetadataType = MetadataType.ValueType -> dt
                                     | _ -> failwith "IE"
                            // TODO check type of vt for with ?
                            let vt = vt :?> TypeDefinition
                            let pvt = PointerType(vt)
                            let (_, vv) = ctx.EnsureVariable(pvt)
                            ([
                                yield! cl
                                yield! chainReaderFactory false true !ltp
                                Stloc(vv) |> ilResolve
                            ], (vv, vt, pvt))
                        | _ -> failwith "IE"

                    let newSymbols = Dictionary<string, Symbol>()
                    let (v, td, ptd) = snd loadVarW
                    for f in td.Fields do
                        newSymbols.Add(f.Name, WithSym(LocalVariable v, ptd))
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
                    let condition = fst <| exprToIl expr ctx.details.sysTypes.boolean
                    let (trueBranch, trueLabels) = stmtToIlList tb
                    let (falseBranch, falseLabels) = stmtToIlList fb
                    let hasFalseBranch = falseBranch.Length > 0
                    // TODO rethink LazyLabel here
                    let checkCondition = [|IlBrfalse(if hasFalseBranch then ref (LazyLabel(falseBranch.[0])) else firstEnfOfStm)|]
                    ([
                        yield! condition
                        yield! checkCondition
                        yield! trueBranch
                        yield IlBr(if hasFalseBranch then firstEnfOfStm else lastEndOfStm)
                        if hasFalseBranch then
                            yield! falseBranch
                            yield IlBr(lastEndOfStm)
                    ], List.concat [[firstEnfOfStm;lastEndOfStm];trueLabels;falseLabels])
                | LabelStm l -> ([],[ctx.findLabelUnsafe l])
                | GotoStm s -> ([IlBr(ctx.findLabelUnsafe s)],[]) // will do LazyLabel
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
                            c := LazyLabel i
                            omitCase := None
                        | _ -> ()
                    let casec =
                        [for (tocheck, stmt) in mainLabels do
                            let (caseBranch, caseLabels) = stmtToIlList stmt
                            yield
                                (
                                 [
                                    for l in tocheck do
                                        let beginOfCase = IlAtom(ref(Ldloc(var)))
                                        // for ranges we need to skip
                                        ensureOmitCase beginOfCase
                                        yield beginOfCase
                                        match l with
                                        | CaseExpr(ConstExpr(ce)) -> 
                                             yield! (fst <| exprToIl ce null) // TODO type handle
                                             yield IlBeq(ref(LazyLabel(caseBranch.[0])))
                                        | CaseRange(ConstExpr(ce1), ConstExpr(ce2)) ->
                                             // TODO check proper range
                                             // lower range
                                             let nextCase = ref ForwardLabel
                                             omitCase := Some nextCase
                                             yield! (fst <| exprToIl ce1 null) // TODO type handle
                                             yield IlBlt nextCase
                                             // higher range
                                             yield IlAtom(ref(Ldloc(var)))
                                             yield! (fst <| exprToIl ce2 null) //TODO type handle
                                             yield IlBgt nextCase
                                             yield IlBr(ref(LazyLabel(caseBranch.[0])))
                                        | _ -> failwith "IE";
                                    ], [yield! caseBranch ; IlBr(lastEndOfStm)], caseLabels)
                         let defaultCase = [yield! defBranch; IlBr(lastEndOfStm)]
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
                    let condition = fst <| exprToIl expr ctx.details.sysTypes.boolean
                    let conditionLabel = ref (LazyLabel(condition.Head))
                    let (whileBranch, whileLabels) = stmtToIlList stmt
                    Ctx.resolveSysLabels (List.tryHead condition) whileLabels
                    ([
                        yield IlBr(conditionLabel)
                        yield! whileBranch
                        yield! condition
                        yield IlBrtrue(ref (LazyLabel
                                                (match List.tryHead whileBranch with
                                                | Some h -> h
                                                | _ -> condition.Head)))
                    ],[])
                | RepeatStm (stmt, expr) ->
                    let condition = fst <| exprToIl expr ctx.details.sysTypes.boolean
                    // let conditionLabel = ref (LazyLabel(condition.[0]))
                    let (repeatBranch, whileLabels) = stmtToIlList stmt
                    Ctx.resolveSysLabels (List.tryHead condition) whileLabels
                    ([
                        yield! repeatBranch
                        yield! condition
                        yield IlBrfalse(ref (LazyLabel
                                                (match List.tryHead repeatBranch with
                                                | Some h -> h
                                                | _ -> condition.[0])))
                    ],[])
                | ForStm (ident, initExpr, delta, finiExpr, stmt) ->
                    let exprLabel = ref ForwardLabel
                    let var = getVar4ForLoop ident
                    let (name, varFinal) = ctx.EnsureVariable()
                    let (loopInit, _) = AssignStm(ident, initExpr) |> List.singleton |> stmtToIlList
                    // TODO optimization for simple values (dont store in var)
                    let (loopFinal, _) = AssignStm(stdIdent name, finiExpr) |> List.singleton |> stmtToIlList
                    let condition = [
                        Ldloc var |> ilResolve
                        Ldloc varFinal |> ilResolve;
                        match delta with
                                    | 1 -> IlBle exprLabel
                                    | -1 -> IlBge exprLabel
                                    | _ -> failwith "IE"]
                    let conditionLabel = ref <| LazyLabel(condition.Head)
                    let (incLoopVar, _) = AssignStm(ident, Add(Value(VIdent(ident)), Value(VInteger delta))) |> List.singleton |> stmtToIlList
                    let (forBranch, forLabels) = stmtToIlList stmt
                    exprLabel := (LazyLabel
                                                (match List.tryHead forBranch with
                                                | Some h -> h
                                                | _ -> condition.Head))
                    Ctx.resolveSysLabels (List.tryHead condition) forLabels
                    ([
                        yield! loopInit
                        yield! loopFinal
                        yield IlBr(conditionLabel)
                        yield! forBranch
                        yield! incLoopVar
                        yield! condition
                    ],[])
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
            ctx.variables.Head.Values
            |> Seq.collect
               (function
                | LocalVariable v ->
                     ctx.res.Add(DeclareLocal(v))
                     match v.VariableType with
                     | t when t = ctx.details.sysTypes.string -> []//stmtListToIlList ctx (IfStm())
                     | _ -> []
                | GlobalVariable v ->
                     match v.FieldType with
                     | t when t = ctx.details.sysTypes.string ->
                         [
//                             Ldsfld v |> ilResolve
//                             Call ctx.details.FreeMem |> ilResolve
                         ]
                     | _ -> []
                )
        let (instr, labels) = stmtListToIlList ctx sl

        match res with
        | null ->
            let lastInstr = Ret |> ilResolve
            Ctx.resolveSysLabels (Some lastInstr) labels
            ctx.res.Add(instr |> InstructionList)
            ctx.res.Add(lastInstr |> InstructionSingleton)
        | _ ->
            // TODO optimization possible if there is no exit ? Don't create try/finally
            let ret, _ = Ret |> ilResolveInstr
            let ress, _ = Ldloc res |> ilResolveInstr;
            let lastInstrSet = [yield! finalizeVariables; Endfinally |> ilResolve]
            Ctx.resolveSysLabels (Some lastInstrSet.Head) labels
            (instr, lastInstrSet, ress, ret )
            |> HandleFinally
            |> ctx.res.Add
        ctx.res

    let vt = mb.ImportReference(typeof<ValueType>)
    let ot = mb.ImportReference(typeof<obj>)

    let findType =
        typeIdToStr

    let createStdTypes ns (dt: Dictionary<TypeIdentifier, TypeReference>) (ti: Dictionary<TypeReference, TypeInfo>) =
        dt.Add(stdType "Int64", mb.TypeSystem.Int64)
        dt.Add(stdType "UInt64", mb.TypeSystem.UInt64)
        dt.Add(stdType "Integer", mb.TypeSystem.Int32)
        dt.Add(stdType "LongInt", mb.TypeSystem.Int32)
        dt.Add(stdType "LongWord", mb.TypeSystem.UInt32)
        dt.Add(stdType "SmallInt", mb.TypeSystem.Int16)
        dt.Add(stdType "Word", mb.TypeSystem.UInt16)
        dt.Add(stdType "ShortInt", mb.TypeSystem.SByte)
        dt.Add(stdType "Byte", mb.TypeSystem.Byte)
        let boolType = mb.TypeSystem.Boolean
        dt.Add(stdType "Boolean", boolType)
        let ptrType = PointerType(mb.TypeSystem.Void)
        dt.Add(stdType "Pointer", ptrType)
        let charType = ModuleDetails.NewSizedType ns mb mb.TypeSystem.Byte 1 ""
        dt.Add(stdType "Char", charType)
        let strType = (ModuleDetails.NewSizedType ns mb vt 256 "") :> TypeReference
        dt.Add(TIdString, strType)
        // allow to use string as array
        let strDim = {
                        low = 1
                        high = 256
                        size = 256
                        elemSize = 1
                        elemType = charType
                        selfType = ref strType
                     }
        ti.Add(strType, ArrayRange([strDim], charType))
        ti.Add(charType, SimpleType(1))
        {
            string = strType
            char = charType
            value = vt
            pointer = ptrType
            boolean = boolType
            net_obj = ot
        }

    member self.BuildIl(block: Block, buildScope, ?resVar) =
        let ctx = match buildScope with
                  | MainScope (ns, tb) ->
                    let typeInfo = Dictionary<TypeReference,_>()
                    let langCtx = LangCtx()
                    let newSymbols = Dictionary<_,_>(langCtx)
                    let defTypes = Dictionary<TypeIdentifier, TypeReference>()
                    let systemTypes = createStdTypes ns defTypes typeInfo
                    let systemProc = {
                        GetMem = allocMem
                        FreeMem = freeMem
                        WriteLine = writeLineMethod
                        PtrToStringAnsi = ptrToStringAnsi
                    }
                    newSymbols.Add("GetMem", Referenced allocMem |> MethodSym)
                    newSymbols.Add("FreeMem", Referenced freeMem |> MethodSym)
                    newSymbols.Add("Inc", Intrinsic IncProc |> MethodSym)
                    newSymbols.Add("Dec", Intrinsic DecProc |> MethodSym)
                    newSymbols.Add("WriteLn", Intrinsic WriteLnProc |> MethodSym)
                    newSymbols.Add("Exit", Intrinsic ExitProc |> MethodSym)
                    ModuleDetails.Create moduleBuilder ns tb systemTypes systemProc
                    |> Ctx.Create defTypes newSymbols typeInfo langCtx
                  | LocalScope ctx -> ctx
        let defTypes = ctx.types.Head
        let newSymbols = ctx.symbols.Head
        let result = match resVar with
                     | Some (name, Some(v)) ->
                        ctx.variables.Head.Add(name, v)
                        match v with
                        | LocalVariable v -> v
                        | _ -> null
                     | Some (_, None) -> null // no result (void)
                     | _ -> null // main program

        let rec sizeOf (tr: TypeReference) (td: TypeDefinition) =
            let sizeOfTD (td: TypeDefinition) = td.Fields |> Seq.sumBy (fun f -> sizeOf f.FieldType null)
            match tr.MetadataType with
            | MetadataType.SByte | MetadataType.Byte   -> 1
            | MetadataType.Int16 | MetadataType.UInt16 -> 2
            | MetadataType.Int32 | MetadataType.UInt32 -> 4
            | MetadataType.Int64  -> 8
            | MetadataType.Single -> 4
            | MetadataType.Double -> 8
            | MetadataType.Void   -> 8 // TODO 4 or 8 -> target dependent
            | MetadataType.ValueType ->
                match ctx.findTypeInfo tr with
                | Some(ArrayRange _) -> (tr :?> TypeDefinition).ClassSize
                | Some(TypeSymbols _) ->
                    // check mono_marshal_type_size -> https://github.com/dotnet/runtime/blob/487c940876b1932920454c44d2463d996cc8407c/src/mono/mono/metadata/marshal.c
                    // check mono_type_to_unmanaged -> https://github.com/dotnet/runtime/blob/aa6d1ac74e6291b3aaaa9da60249d8c327593698/src/mono/mono/metadata/metadata.c
                    sizeOfTD(tr :?> TypeDefinition)
                | _ when td <> null -> sizeOfTD td
                | _ -> failwith "IE"
            | _ -> failwith "IE"

        printfn "%A" block.decl
        block.decl
        |> List.iter 
               (function
               | Types types ->
                   (
                       for (name, decl) in types do
                           match decl with
                           | TypePtr (count, typeId) ->
                               let mutable pt = PointerType(defTypes.[typeId])
                               for i = 2 to count do pt <- PointerType(pt)
                               defTypes.Add(stdType name, pt)
                           | TypeEnum enumValues ->
                               enumValues |> List.iteri (fun i v -> newSymbols.Add (v, EnumValueSym(i)))
                               defTypes.Add(stdType name, mb.TypeSystem.Int32)
                           | Record (packed, fields) -> 
                                let td = TypeDefinition(ctx.details.ns, name, TypeAttributes.Sealed ||| TypeAttributes.BeforeFieldInit ||| TypeAttributes.SequentialLayout)
                                td.ClassSize <- 0
                                td.BaseType <- vt
                                td.PackingSize <- if packed then 1s else 0s
                                let fieldsMap = Dictionary<_,_>()
                                for (names, typeName) in fields do
                                  for name in names do
                                    let fd = FieldDefinition(name, FieldAttributes.Public, defTypes.[typeName])
                                    td.Fields.Add fd
                                    fieldsMap.Add(name, fd)
                                mb.Types.Add(td)
                                defTypes.Add(stdType name, td)
                                ctx.typeInfo.Head.Add(td, TypeSymbols(fieldsMap, sizeOf td td))
                           | Array(ArrayDef(_, dimensions, tname)) ->
                                let newSubType (dims, size) (typ, typSize) name =
                                    let size = size * typSize
                                    let at = ModuleDetails.NewSizedType ctx.details.ns mb vt size ""
                                    FieldDefinition("item0", FieldAttributes.Public, typ)
                                    |> at.Fields.Add
                                    ctx.typeInfo.Head.Add(at, ArrayRange(dims, typ))
                                    at

                                let rec doArrayDef dimensions tname dims name =

                                    let newDims, typAndSize, newTyp =
                                        match tname with
                                        | TIdArray(ArrayDef(_, dimensions, tname)) -> doArrayDef dimensions tname dims (name + "$a$")
                                        | TIdIdent _ ->
                                            let typ = defTypes.[tname]
                                            dims, (typ, sizeOf typ null), typ
                                        | _ -> failwith "IE"

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
                                                selfType = ref null
                                            }
                                            let dims = newDim::dims
                                            let totalSize = totalSize * size
                                            let dimsAndSize = (dims, totalSize)
                                            let newTyp = (newSubType dimsAndSize typAndSize (name + "$" + string(i))) :> TypeReference
                                            newDim.selfType := newTyp
                                            dimsAndSize, elemType, i+1, newTyp
                                        | _ -> failwith "IE"

                                    let (res, _, _, typ) = List.foldBack newArrayDim dimensions (newDims, null, 0, newTyp)
                                    res, typAndSize, typ
                                // final array
                                let _,_,typ = (doArrayDef dimensions tname ([], 1) name)
                                typ.Name <- name
                                defTypes.Add(stdType name, typ)
                                //(doArrayDef dimensions tname (ref []) name).Name <- name
                           | _ -> ()
                   )
               | Variables v ->
                   let addVar =
                        let addVar v vk = (v, vk) |> ctx.variables.Head.Add
                        match buildScope with
                        | LocalScope _ -> fun (v, t) ->
                                            VariableDefinition t
                                            |> LocalVariable
                                            |> fun vk ->
                                                newSymbols.Add(v, VariableSym vk)
                                                addVar v vk
                        | MainScope _ -> fun (v, t) ->
                                            let fd = FieldDefinition(v, FieldAttributes.Public ||| FieldAttributes.Static, t)
                                            fd |> GlobalVariable
                                            |> fun vk ->
                                                newSymbols.Add(v, VariableSym vk)
                                                ctx.details.tb.Fields.Add fd
                                                addVar v vk
                   v
                   |> List.collect (fun (l, t) -> l |> List.map (fun v -> (v, defTypes.[t])))
                   |> List.iter addVar
               | Labels labels -> (for l in labels do ctx.labels.Head.Add(l, ref (UserLabel l)))
               | ProcAndFunc((name, mRes, mPara), d) ->
                   let name = match name with
                              | Some n -> n
                              | _ -> failwith "name expected"
                   let newMethodSymbols = Dictionary<_,_>(ctx.lang)
                   let mRes, rVar = match mRes with
                                    | Some r ->
                                          let res = defTypes.[r]
                                          let resultVar = VariableDefinition res |> LocalVariable
                                          newMethodSymbols.Add("result", resultVar |> VariableSym)
                                          res, Some resultVar
                                    | _ -> moduleBuilder.TypeSystem.Void, None

                   let ps = defaultArg mPara []
                            |> List.collect
                               (fun (k, (ps, t)) ->
                                let t = defTypes.[t]
                                [for p in ps do
                                    let (typ, byref) = match k with
                                                       | Some(Var) -> ByReferenceType(t) :> TypeReference, true
                                                       | _ -> t, false
                                    let pd = ParameterDefinition(p, ParameterAttributes.None, typ)
                                    newMethodSymbols.Add(p, VariableParamSym(pd, t, byref))
                                    yield pd]
                               )
                   let methodBuilder =
                       match ctx.forward.TryGetValue name with
                       | true, md ->
                           // TODO check signature - must be identical
                           ctx.forward.Remove name |> ignore
                           md
                       | _ ->
                           let methodAttributes = MethodAttributes.Public ||| MethodAttributes.Static
                           let md = MethodDefinition(name, methodAttributes, mRes)
                           List.iter md.Parameters.Add ps
                           newSymbols.Add(name, Referenced (md :> MethodReference) |> MethodSym)
                           md
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
                       ctx.forward.Add(name, methodBuilder)
                   | _ -> failwith "no body def"
               | _ -> ())

        stmtListToIl block.stmt ctx result
end