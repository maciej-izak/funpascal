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

and PasType = {
      name: CompilerName
      kind: TypeKind
      raw : ITypeDefOrRef
    }
with
    static member NewPtr(pt, ?name) =
        let name = defaultArg name ""
        let ptrType = pt.raw.ToTypeSig() |> PtrSig |> TypeSpecUser
        {name=CompilerName.FromString name;raw=ptrType;kind=TkPointer(pt)}

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
        | TkArray _ -> int <| this.raw.ResolveTypeDefThrow().ClassSize
        | TkUnknown s -> s
        | TkSet _ -> 256

    member this.ResolveArraySelfType() =
        match this.kind with
        | TkArray(_,h::_,_) -> h.selfType := this
        | _ -> raise <| InternalError "2020102000"
        this

    member this.IndKind =
        match this.raw.ToTypeSig().ElementType with
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

and [<CustomEquality; NoComparison>]
    ArrayDim = { low: int; high: int; size: int; elemSize: int; elemType: PasType; selfType: PasType ref }
with
    override self.Equals a =
        match a with
        | :? ArrayDim as a -> self.low = a.low && self.high = a.high && self.size = a.size && self.elemType = a.elemType
        | _ -> false
    override self.GetHashCode() = HashCode.Combine(hash self.low, hash self.high, hash self.size, hash self.elemType)

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