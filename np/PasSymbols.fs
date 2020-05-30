[<AutoOpen>]
module NP.PasSymbols

open Mono.Cecil
open Mono.Cecil.Cil

[<AutoOpen>]
module ConstSymbols =

    type ConstEvalResult =
        | CERString of string
        | CERInt of int * PasType
        | CERFloat of single
        | CERBool of byte
        | CEROrdSet of byte[] * PasType
        | CERUnknown

    type ConstSym =
        | ConstString of string
        | ConstInt of int
        | ConstFloat of single
        | ConstBool of byte
        | ConstTempValue of byte[] * PasType
        | ConstValue of FieldDefinition * PasType

open System
open System.Collections.Generic

type MethodParam = {
    typ: PasType
    ref: bool
}

type MethodInfo = {
    paramList: MethodParam array
    result: PasType option
    raw: MethodReference
}

type VariableKind =
     | LocalVariable of VariableDefinition
     | GlobalVariable of FieldDefinition
     | ParamVariable of ParamRefKind * ParameterDefinition
with
    member self.Type() =
        match self with
        | LocalVariable v -> v.VariableType
        | GlobalVariable v -> v.FieldType
        | ParamVariable (_, v) -> v.ParameterType

type ReferencedDef = MethodInfo * (Symbol * Symbol) list ref

and MethodSym =
    | Referenced of ReferencedDef
    | Intrinsic of Intrinsic
with
    member self.ReturnType =
        match self with
        | Referenced({result = result},_) -> result
        | Intrinsic _ -> None

and Symbol =
    | VariableSym of (VariableKind * PasType)
    | MethodSym of MethodSym
    | TypeSym of PasType
    | EnumValueSym of int * PasType
    | WithSym of (VariableKind * PasType)
    | ConstSym of ConstSym
    | UnknownSym

let (|NestedRoutineId|) = function
    | VariableSym(LocalVariable vd, _), _ -> vd :> obj
    | VariableSym(ParamVariable (_, pd), _), _ -> pd :> obj

let (|NestedRoutineSym|_|) = function
    | VariableSym(LocalVariable _, t) | VariableSym(ParamVariable _, t) as vs -> Some(NestedRoutineSym(vs, t))
    | _ -> None

type ValueLoad =
    | ValueInt of int
    | ValueFloat of single

type SymbolLoad =
    | DerefLoad
    | ElemLoad of (ExprEl * ArrayDim) list * PasType
    | StructLoad of (FieldDefinition * PasType) list
    | VariableLoad of (VariableKind * PasType)
    | ValueLoad of ValueLoad
    | CallableLoad of MethodSym
    | TypeCastLoad of PasType

let (|VariableSymLoad|) = function | VariableSym((_,t) as vs), _ -> [VariableLoad vs], Some t

type ChainLoad =
    | ChainLoad of (SymbolLoad list * PasType option)
    | SymbolLoadError

let chainToSLList = function
    | ChainLoad sl -> sl
    | SymbolLoadError -> [],None // failwith "IE"

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