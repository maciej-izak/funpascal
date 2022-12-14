namespace Pas

open dnlib.DotNet
open dnlib.DotNet.Emit

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
        | ConstValue of FieldDef * PasType

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
    
    type LastTypePoint =
        | LTPVar of VariableKind * PasType
        | LTPDeref of PasType * bool
        | LTPStruct of FieldDef * PasType
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

    type GlobalBlock =
        | NormalBlock
        | InitializationBlock
        | FinalizationBlock
    
    type LabelRec = {
        mutable branch: BranchLabel ref
        mutable used: bool
        mutable set: bool
        mutable block: GlobalBlock
    }
    
    type NestedRoutineVar(outer, inner) = 
        //outerSym: Symbol
        //innerSym: Symbol
        // def: FieldDefUser
        member val Outer: Symbol = outer
        member val Inner: Symbol = inner
        
    and NestedRoutine = {
        // Type: TypeDefUser
        // Variables: Dictionary<obj, NestedRoutineVar>
        Params: Dictionary<obj, NestedRoutineVar>
        ParamsList: List<NestedRoutineVar> 
        // mutable Occurrences: {| call: IlInstruction list ref; paramsCount: int |}
    } with
        member self.Add id nrv = self.Params.Add(id, nrv); self.ParamsList.Add nrv
        static member Create() = { Params = Dictionary<_,_>(); ParamsList = List<_>() }
    
    and NestedRoutines = Dictionary<IMethod, NestedRoutine>
    
    and ReferencedDef = MethodInfo * NestedRoutines option ref

    and MethodSym =
        | Referenced of ReferencedDef
        | Intrinsic of IntrinsicSym
    with
        member self.ReturnType =
            match self with
            | Referenced({result = Some result},_) -> Some result.typ
            | _ -> None
        
        member self.MethodInfo =
            match self with
            | Referenced(mi,_) -> mi
            | _ -> raise(InternalError "2020111800")

    and Symbol =
        | VariableSym of (VariableKind * PasType)
        | MethodSym of MethodSym
        | TypeSym of PasType
        | EnumValueSym of int * PasType
        | WithSym of (VariableKind * PasType)
        | ConstSym of ConstSym
        | LabelSym of LabelRec
        | UnknownSym

    let (|NestedRoutineId|) = function
        | VariableSym(LocalVariable vd, _), _ -> vd :> obj
        | VariableSym(ParamVariable (_, pd), _), _ -> pd :> obj
        | _ -> raise(InternalError "2020112200")

    let (|NestedRoutineSym|_|) = function
        | VariableSym(LocalVariable _, t) | VariableSym(ParamVariable _, t) as vs -> Some(NestedRoutineSym(vs, t))
        | _ -> None

    type ValueLoad =
        | ValueInt of int
        | ValueFloat of single

    type SymbolLoad =
        | DerefLoad
        | ElemLoad of (ExprEl * ArrayDim) list * PasType
        | StructLoad of (FieldDef * PasType) list
        | VariableLoad of (VariableKind * PasType)
        | ValueLoad of ValueLoad
        | CallableLoad of MethodSym
        | TypeCastLoad of PasType

    let (|VariableSymLoad|) nestedRec (nr: NestedRoutineVar) =
        match (if nestedRec then nr.Inner else nr.Outer) with
        | VariableSym((_,t) as vs) -> [VariableLoad vs], Some t
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