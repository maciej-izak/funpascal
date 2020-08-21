namespace Pas

open Mono.Cecil
open Mono.Cecil.Cil
open NP

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

[<AutoOpen>]
module IntrinsicSymbols =

    type IntrinsicSym =
        | IncProc
        | DecProc
        | SuccProc
        | PredProc
        | ExitProc
        | HaltProc
        | HaltAtLineProc
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
        | TruncFunc
        | RoundFunc
        | SizeOfFunc

[<AutoOpen>]
module Symbols =

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

    type LastTypePoint =
        | LTPVar of VariableKind * PasType
        | LTPDeref of PasType * bool
        | LTPStruct of FieldDefinition * PasType
        | LTPNone
    with
        member self.ToTypeRef =
            match self with
            | LTPVar(_,tr) -> tr
            | LTPDeref(tr, _) -> tr
            | LTPStruct(_, tr) -> tr
            | _ -> failwith "IE"

        member self.PasType =
            match self with
            | LTPVar(_, pt) -> pt
            | LTPDeref(pt, _) -> pt
            | LTPStruct(_, pt) -> pt
            | _ -> failwith "IE"

    type ReferencedDef = MethodInfo * (Symbol * Symbol) list ref

    and MethodSym =
        | Referenced of ReferencedDef
        | Intrinsic of IntrinsicSym
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
        | _ -> failwith "IE"

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

    let (|VariableSymLoad|) = function
        | VariableSym((_,t) as vs), _ -> [VariableLoad vs], Some t
        | _ -> failwith "IE"

    type ChainLoad = SymbolLoad list * PasType option

    let chainToSLList = function
        | Some sl -> sl
        | None -> [],None

    let caseSensitive = HashIdentity.Structural<CompilerName>
    let caseInsensitive =
        HashIdentity.FromFunctions
            (function
             | CompilerName ti -> (ti.ToString()).GetHashCode(StringComparison.InvariantCultureIgnoreCase)
             | _ -> raise(InternalError "2020082102"))
            (fun a b -> String.Equals(string a, string b, StringComparison.InvariantCultureIgnoreCase))

    type SymbolsDict<'T>() =
        inherit Dictionary<string, 'T>()