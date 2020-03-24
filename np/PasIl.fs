module NP.PasIl

open System
open System.Collections
open System.Collections.Generic
open System.Linq.Expressions
open System.Linq
open System.Reflection.Metadata
open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks
open Microsoft.FSharp.Linq
open Mono.Cecil

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
    | Ldloc of VariableDefinition
    | Ldloca of VariableDefinition
    | Ldfld of FieldDefinition
    | Ldflda of FieldDefinition
    | Ldind of IndirectKind
    | Stloc of VariableDefinition
    | Stfld of FieldDefinition
    | Stind of IndirectKind
    | Conv_I
    | Cpblk
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
    | Ret
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

type Symbol =
    | VariableSym of VariableDefinition
    | EnumValueSym of int
    | WithSym of VariableDefinition * TypeDefinition

type ArrayDim = { low: int; high: int; size: int; elemSize: int; elemType: TypeReference; selfType: TypeReference ref }

type SymbolLoad =
    | DerefLoad
    | ElemLoad of (ExprEl * ArrayDim) list * TypeReference
    | StructLoad of FieldDefinition list
    | VariablePtrLoad of VariableDefinition
    | VariableLoad of VariableDefinition
    | ValueLoad of int

type ChainLoad =
    | ChainLoad of SymbolLoad list
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
    | TypeSymbols of Dictionary<string,FieldDefinition> * int
    | ArrayRange of ArrayDim list * TypeReference

type Ctx = {
        variables: Dictionary<string,VariableDefinition>
        labels: Dictionary<string, BranchLabel ref>
        symbols: Dictionary<string, Symbol> list
        typeInfo: Dictionary<TypeReference,TypeInfo>
        moduleBuilder: ModuleDefinition
        localVariables: int ref
        lang: LangCtx
        res: List<MetaInstruction>
    } with
    static member Create variables labels symbols typeInfo lang moduleBuilder =
        {
            variables = variables
            labels = labels
            symbols = symbols
            typeInfo = typeInfo
            moduleBuilder = moduleBuilder
            localVariables = ref 0
            lang = lang
            res = List<MetaInstruction>()
        }
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
        let value = VariableDefinition(
                                          defaultArg kind self.moduleBuilder.TypeSystem.Int32
                                      )
        self.res.Add(DeclareLocal(value))
        self.variables.Add(key, value)
        self.symbols.Head.Add(key, VariableSym(value))
        (key, value)
    
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
                    | Unaligned i  -> Instruction.Create(OpCodes.Unaligned, i)
                    | Ret          -> Instruction.Create(OpCodes.Ret)
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
    match inst with
    | DeclareLocal t -> t |> ilg.Body.Variables.Add
    | InstructionSingleton is -> instr is |> appendIfNotNull
    | InstructionList p -> p |> List.iter (instr >> appendIfNotNull) 

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
        let ref = match ref with | :? PointerType as pt -> pt.ElementType | _ -> ref :?> TypeReference
        let symbols = match ctx.typeInfo.[ref] with | TypeSymbols (d,_) -> d | _ -> failwith "IE"
        let sym = symbols.[h]
        findSym sym.FieldType (sym::acc) t
    | t -> acc, t
    
    let rec resolveTail acc (vt: TypeReference)  = function
        | [] -> List.rev acc
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
                let dims, elemTyp = match ctx.typeInfo.TryGetValue tref with
                                    | true, ArrayRange( dims , elemTyp ) -> dims, elemTyp
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
                let newAcc = match ctx.typeInfo.TryGetValue subElemTyp with
                             | true, ArrayRange _ -> elemLoad::acc
                             | _ -> elemLoad::acc
                resolveTail newAcc subElemTyp t

    match mainSym with
    | Some(VariableSym vd) ->
        let vt = absVariableType vd.VariableType
        ChainLoad <|
            let tail = resolveTail [] vt ident.Tail
            match List.tryHead tail with
            | Some(ElemLoad _) -> VariablePtrLoad(vd)
            | _ -> VariableLoad(vd)
            ::tail
    | Some(WithSym (vd, td)) ->
        let vt = absVariableType td
        let chain = resolveTail [] vt ident
        ChainLoad (VariableLoad(vd)::chain)
    | Some(EnumValueSym(i)) when ident.Tail = [] -> [ValueLoad(i)] |> ChainLoad
    | _ -> SymbolLoadError

let ilResolve = ainstr >> IlResolved
let ilResolveArray = ilResolve >> List.singleton

//let findSymbolAndGetPtr (ctx: Ctx) ident =
//    let sym = findSymbol ctx ident
//    let rec resolveSym acc = function
//        | VariablePtrLoad vd -> (vd |> Ldloca |> ilResolve)::acc
//        | VariableLoad vs -> (vs |> Ldloc |> ilResolve)::acc
//        | StructLoad fdl -> List.fold (fun acc f -> (Ldflda(f) |> ilResolve)::acc) acc fdl
//        | ChainLoad cl -> List.fold resolveSym acc cl
//        | _ -> failwith "IE"
//    resolveSym [] sym |> List.rev

type LastTypePoint =
    | LTPVar of VariableDefinition * TypeReference
    | LTPVarPtr of VariableDefinition * TypeReference
    | LTPDeref of TypeReference
    | LTPStruct of FieldDefinition
    | LTPNone

let rec exprToIl ctx exprEl =
    let inline add2OpIl a b i = [|yield! exprToIl ctx a; yield! exprToIl ctx b; yield ilResolve i|]
    let inline add1OpIl a i = [|yield! exprToIl ctx a; yield ilResolve i|]
    match exprEl with
    | Value v -> [|yield! valueToIl ctx v|]
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
    | NotEqual(a, b) -> [|yield! add2OpIl a b Ceq; ilResolve (Ldc_I4(0)); ilResolve Ceq|]
    | StrictlyLessThan(a, b) -> add2OpIl a b Clt
    | StrictlyGreaterThan(a, b) -> add2OpIl a b Cgt
    | LessThanOrEqual(a, b) -> [|yield! add2OpIl a b Cgt ; ilResolve (Ldc_I4(0)); ilResolve Ceq|]
    | GreaterThanOrEqual(a, b) -> [|yield! add2OpIl a b Clt; ilResolve (Ldc_I4(0)); ilResolve Ceq|]
    | Addr(a) ->
        [|
//            match a with
//            | Value(VIdent i) -> yield! findSymbolAndGetPtr ctx i
//            | _ -> failwith "IE"
        |]
    | _ -> failwith "IE"

and valueToIl ctx v =
    match v with
    | VInteger i -> Ldc_I4(i) |> ilResolve |> List.singleton
    | VIdent i -> findSymbolAndLoad ctx i
    | VString s -> Ldstr(s) |> ilResolve |> List.singleton
    | _ -> Unknown |> ilResolve |> List.singleton

and chainReaderFactory asValue = function
    | LTPVar(vs, _) -> [Ldloc vs |> ilResolve]
    | LTPVarPtr(vs, _) -> [Ldloca vs |> ilResolve]
    | LTPStruct fld -> [ fld |> (if asValue then Ldfld else Ldflda) |> ilResolve]
    | LTPDeref dt ->
        if dt.MetadataType = MetadataType.ValueType then []
        else
            match dt.MetadataType with
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
            |> ilResolveArray
    | LTPNone -> []

and chainWriterFactory (ctx: Ctx) = function
    | LTPVar(vs, _) -> [Stloc vs |> ilResolve]
    | LTPVarPtr _ -> failwith "IE" // [Stloc vs |> ilResolve]
    | LTPStruct fld -> [fld |> Stfld |> ilResolve]
    | LTPDeref dt ->
        if dt.MetadataType = MetadataType.ValueType then
            let dt = dt :?> TypeDefinition
            let size = match ctx.typeInfo.TryGetValue dt with | true, TypeSymbols (_, size) -> size | _ -> failwith "IE"
            [
                Ldc_I4(size) |> ilResolve
//                if dt.PackingSize = 1s then
//                    Unaligned(byte(dt.PackingSize)) |> ilResolve
                Cpblk |> ilResolve
            ]
        else
            match dt.MetadataType with
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
            |> ilResolveArray
    | LTPNone -> []

and derefLastTypePoint = function
    | LTPVar(_, t) -> derefType t
    | LTPVarPtr(_, t) -> derefType t
    | LTPDeref t -> derefType t
    | LTPStruct fd -> derefType fd.FieldType
    | _ -> failwith "cannot deref"

and findSymbolAndLoad (ctx: Ctx) ident =
    let lastPoint = ref LTPNone
    findSymbol ctx ident
    |> chainToSLList
    |> List.collect (chainLoadToIl ctx lastPoint (chainReaderFactory false))
    |> fun l -> l @ (chainReaderFactory true !lastPoint)

and chainLoadToIl ctx lastType factory symload =
    let res = factory !lastType
    match symload with
    | VariableLoad vs ->
        lastType := LTPVar(vs, absVariableType vs.VariableType)
        res
    | VariablePtrLoad vs ->
        lastType := LTPVarPtr(vs, absVariableType vs.VariableType)
        res
    | DerefLoad ->
        let dt = derefType <| derefLastTypePoint !lastType
        lastType := LTPDeref dt
        res
    | ElemLoad (exprs, rt) ->
        lastType := LTPDeref rt
        [
            yield! res
            for e, d in exprs do
                yield! exprToIl ctx (Multiply(e, d.elemSize |> VInteger |> Value))
                yield AddInst |> ilResolve
        ]
    | StructLoad fds ->
        let instr, last, count = List.fold (fun (acc, _, c) f -> (Ldflda(f) |> ilResolve)::acc, f, c+1) ([],null,0) fds
        lastType := LTPStruct last
        res @ (List.take (count-1) instr)
    | ValueLoad evs ->
        lastType := LTPNone
        res @ (Ldc_I4(evs) |> ilResolveArray)

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

let callParamToIl ctx cp = 
    match cp with
    | ParamExpr expr -> List.ofArray (exprToIl ctx expr)
    | ParamIdent id -> findSymbolAndLoad ctx id

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

type IlBuilder(moduleBuilder: ModuleDefinition) = class
    let mb = moduleBuilder
    let writeLineMethod = 
        typeof<System.Console>.GetMethod("WriteLine", [| typeof<int32> |]) |> moduleBuilder.ImportReference
    let writeLineSMethod = 
        typeof<System.Console>.GetMethod("WriteLine", [| typeof<string> |]) |> moduleBuilder.ImportReference     

    let findFunction (DIdent ident) =
        assert(ident.Length = 1)
        let h = ident.Head
        let f = match h with
                | PIName n when n = "WriteLn" -> writeLineMethod
                | PIName n when n = "WriteLnS" -> writeLineSMethod
                | _ -> null
        Call(f) |> ilResolve
            
    let rec stmtToIl (ctx: Ctx) sysLabels (s: Statement): (MetaInstruction * BranchLabel ref list) =
        let stmtToIlList = stmtListToIlList ctx
        let exprToIl = exprToIl ctx
        let getVar4ForLoop = findSymbol ctx >>
                             function
                             | ChainLoad([VariableLoad vs]) -> vs
                             | _ -> failwith "IE"
        let getVar4Assign (ident: DIdent) expr =
             // add param for findSymbol to set purpose (like this `assign`)
             match findSymbol ctx ident with
             | ChainLoad symbols ->
                 let ltp = ref LTPNone
                 [
                     yield! List.collect (chainLoadToIl ctx ltp (chainReaderFactory false)) symbols
                     yield! expr
                     yield! chainWriterFactory ctx !ltp
                 ]
             | _ -> failwith "IE"
//        let getVar4With idents =
//            let ils = List<_>()
//            List.fold
//                (fun symbols i ->
//                    let loadVarW =
//                        match findSymbol ctx i with
//                        | VariableStructLoad (v, fld) ->
//                            let vt = match v.VariableType with
//                                     | :? TypeDefinition as td ->
//                                         v.VariableType <- v.VariableType.MakePinnedType()
//                                         td
//                                     | :? PinnedType as pt -> pt.GetElementType() :?> TypeDefinition
//                                     | _ -> failwith "IE"
//                            let (_, vv) = ctx.EnsureVariable(ctx.moduleBuilder.TypeSystem.UIntPtr)
//                            // TODO optimize List.tryLast?
//                            // try last element in expr x.y.z (z is the right choice)
//                            let vt = match List.tryLast fld with | Some f -> f.FieldType :?> TypeDefinition | _ -> vt
//                            ([
//                                Ldloca(v) |> ilResolve
//                                yield! List.map (Ldflda >> ilResolve) fld
//                                Stloc(vv) |> ilResolve
//                            ],(vv, vt))
//                        | _ -> failwith "IE"
//
//                    let newSymbols = Dictionary<string, Symbol>()
//                    let (v, td) = snd loadVarW
//                    for f in td.Fields do
//                        newSymbols.Add(f.Name, WithSym(v, td))
//                    ils.Add(fst loadVarW)
//                    newSymbols::symbols
//                ) ctx.symbols idents, ils
                     
        let (instructions, newSysLabels) =
                match s with
                | CallStm(CallExpr(ident, cp)) ->
                    ([for p in cp do yield! callParamToIl ctx p; findFunction(ident)], [])
                | AssignStm(ident, expr) -> 
                    (getVar4Assign ident (exprToIl expr), [])
                | IfStm(expr, tb, fb) ->
                    // if logic
                    let firstEnfOfStm = ref ForwardLabel
                    let lastEndOfStm = ref ForwardLabel
                    let condition = exprToIl expr
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
                | LabelStm l -> ([],[ctx.labels.[l]])
                | GotoStm s -> ([IlBr(ctx.labels.[s])],[]) // will do LazyLabel
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
                                             yield! (exprToIl ce)
                                             yield IlBeq(ref(LazyLabel(caseBranch.[0])))
                                        | CaseRange(ConstExpr(ce1), ConstExpr(ce2)) ->
                                             // TODO check proper range
                                             // lower range
                                             let nextCase = ref ForwardLabel
                                             omitCase := Some nextCase
                                             yield! (exprToIl ce1)
                                             yield IlBlt nextCase
                                             // higher range
                                             yield IlAtom(ref(Ldloc(var)))
                                             yield! (exprToIl ce2)
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
                    let condition = exprToIl expr
                    let conditionLabel = ref (LazyLabel(condition.[0]))
                    let (whileBranch, whileLabels) = stmtToIlList stmt
                    Ctx.resolveSysLabels (Array.tryHead condition) whileLabels
                    ([
                        yield IlBr(conditionLabel)
                        yield! whileBranch
                        yield! condition
                        yield IlBrtrue(ref (LazyLabel
                                                (match List.tryHead whileBranch with
                                                | Some h -> h
                                                | _ -> condition.[0])))
                    ],[])
                | RepeatStm (stmt, expr) ->
                    let condition = exprToIl expr
                    // let conditionLabel = ref (LazyLabel(condition.[0]))
                    let (repeatBranch, whileLabels) = stmtToIlList stmt
                    Ctx.resolveSysLabels (Array.tryHead condition) whileLabels
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
//                | WithStm (idents, stmt) ->
//                    let (withSymbols, ils) = getVar4With idents
//                    let newCtx = { ctx with symbols = withSymbols }
//                    // TODO check if some part is unused in ils
//                    let (branch, labels) = stmtListToIlList newCtx stmt
//                    ([
//                        for i in ils do yield! i
//                        yield! branch
//                    ],labels)
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
    
    let stmtListToIl sl (ctx: Ctx) =
        for v in ctx.variables.Values do ctx.res.Add(DeclareLocal(v))
        let (instr, labels) = stmtListToIlList ctx sl
        ctx.res.Add(instr |> InstructionList)
        let ret = Ret |> ilResolve
        Ctx.resolveSysLabels (Some ret) labels
        ctx.res.Add(ret |> InstructionSingleton)
        ctx.res

    let vt = mb.ImportReference(typeof<ValueType>)

    let findType =
        typeIdToStr

    member _.BuildIl(block: Block, ns) =
        let langCtx = LangCtx()
        let variables = Dictionary<_,_>(langCtx)
        let labelsMap = Dictionary<_,_>(langCtx)
        let symbols = Dictionary<_,_>(langCtx)
        let typeInfo = Dictionary<TypeReference,_>()
        let defTypes = Dictionary<TypeIdentifier, TypeReference>()

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
                match typeInfo.TryGetValue tr with
                | true, ArrayRange _ -> (tr :?> TypeDefinition).ClassSize
                | true, TypeSymbols _ ->
                    // check mono_marshal_type_size -> https://github.com/dotnet/runtime/blob/487c940876b1932920454c44d2463d996cc8407c/src/mono/mono/metadata/marshal.c
                    // check mono_type_to_unmanaged -> https://github.com/dotnet/runtime/blob/aa6d1ac74e6291b3aaaa9da60249d8c327593698/src/mono/mono/metadata/metadata.c
                    sizeOfTD(tr :?> TypeDefinition)
                | _ when td <> null -> sizeOfTD td
                | _ -> failwith "IE"
            | _ -> failwith "IE"

        defTypes.Add(stdType "Int64", mb.TypeSystem.Int64)
        defTypes.Add(stdType "UInt64", mb.TypeSystem.UInt64)
        defTypes.Add(stdType "Integer", mb.TypeSystem.Int32)
        defTypes.Add(stdType "LongWord", mb.TypeSystem.UInt32)
        defTypes.Add(stdType "SmallInt", mb.TypeSystem.Int16)
        defTypes.Add(stdType "Word", mb.TypeSystem.UInt16)
        defTypes.Add(stdType "ShortInt", mb.TypeSystem.SByte)
        defTypes.Add(stdType "Byte", mb.TypeSystem.Byte)
        defTypes.Add(stdType "Boolean", mb.TypeSystem.Boolean)
        defTypes.Add(stdType "Pointer", mb.TypeSystem.Void)

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
                               enumValues |> List.iteri (fun i v -> symbols.Add (v, EnumValueSym(i))) 
                               defTypes.Add(stdType name, mb.TypeSystem.Int32)                          
                           | Record (packed, fields) -> 
                                let td = TypeDefinition(ns, name, TypeAttributes.Sealed ||| TypeAttributes.BeforeFieldInit ||| TypeAttributes.SequentialLayout)
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
                                typeInfo.Add(td, TypeSymbols(fieldsMap, sizeOf td td))
                           | Array(ArrayDef(_, dimensions, tname)) ->
                                let newSubType (dims, size) (typ, typSize) name =
                                    let size = size * typSize
                                    let attributes = TypeAttributes.Sealed ||| TypeAttributes.BeforeFieldInit ||| TypeAttributes.SequentialLayout
                                    let at = TypeDefinition(ns, name, attributes)
                                    at.ClassSize <- size
                                    at.PackingSize <- 0s;
                                    at.BaseType <- vt
                                    let fd = FieldDefinition("item0", FieldAttributes.Public, typ)
                                    at.Fields.Add fd
                                    typeInfo.Add(at, ArrayRange(dims, typ))
                                    mb.Types.Add(at)
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
                   v
                   |> List.collect (fun (l, t) -> l |> List.map (fun v -> (v, defTypes.[t])))
                   |> List.iter (fun (v, t) -> (v, VariableDefinition(t)) |> variables.Add)
               | Labels labels -> (for l in labels do labelsMap.Add(l, ref (UserLabel l)))
               | _ -> ())
               
        for kv in variables do symbols.Add(kv.Key, VariableSym(kv.Value))
        
        let ctx = Ctx.Create variables labelsMap [symbols] typeInfo langCtx moduleBuilder
        stmtListToIl block.stmt ctx
end