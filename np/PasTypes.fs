[<AutoOpen>]
module Pas.Types

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