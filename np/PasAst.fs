[<AutoOpen>]
module Pas.Ast

open Microsoft.FSharp.Core

exception InternalError of string
let doInternalError str () = InternalError str |> raise 

let equalsOn f x (yobj:obj) =
    match yobj with
    | :? 'T as y -> (f x = f y)
    | _ -> false

let hashOn f x =  hash (f x)

let compareOn f x (yobj: obj) =
    match yobj with
    | :? 'T as y -> compare (f x) (f y)
    | _ -> invalidArg "yobj" "cannot compare values of different types"

type DIdent = DIdent of Designator list
with
    static member FromString str = DIdent[Ident str]
    
    override self.ToString() =
        match self with
        | DIdent (id::ids) ->
            let start = match id with
                        | Ident i -> i
                        | _ -> failwith "IE"
            List.fold (fun s d -> s + match d with | Ident i -> "." + i | d -> d.ToString()) start ids
        | _ -> failwith "IE"

    member self.BoxPos =
        match self with
        | DIdent(h::_) -> h.BoxPos
        | _ -> failwith "IE"
        
    static member Singleton i = DIdent[i]

and CallExpr = CallExpr of DIdent * CallParam list
with
    override self.ToString() =
        match self with
        | CallExpr (i, cp) -> sprintf "%O(%s)" i ((List.fold (fun s i -> sprintf "%s, %O" s i) "" cp).Substring(2))

and SetAtom =
    | SValue of ExprEl
    | SRange of ExprEl * ExprEl
with
    override self.ToString() =
        match self with
        | SValue ee -> ee.ToString()
        | SRange(ee1, ee2) -> sprintf "%O..%O" ee1 ee2

and Value =
    | VFloat of float
    | VInteger of int
    | VString of string
    | VIdent of DIdent
    | VCallResult of CallExpr
    | VNil
    | VSet of SetAtom list
with
    override self.ToString() =
        match self with
        | VFloat f -> f.ToString()
        | VInteger i -> i.ToString()
        | VString s -> sprintf "\"%s\"" s
        | VIdent i -> i.ToString()
        | VCallResult cr -> cr.ToString()
        | VNil -> "nil"
        | VSet sa -> sprintf "[%s]" ((List.fold (fun s i -> sprintf "%s, %O" s i) "" sa).Substring(2))

and ExprEl =
    | Value of Value
    | Expr of ExprEl
    | Add of ExprEl * ExprEl
    | Multiply of ExprEl * ExprEl
    | Minus of ExprEl * ExprEl
    | Divide of ExprEl * ExprEl
    | And of ExprEl * ExprEl
    | Or of ExprEl * ExprEl
    | Xor of ExprEl * ExprEl
    | Mod of ExprEl * ExprEl
    | Div of ExprEl * ExprEl
    | Shl of ExprEl * ExprEl
    | Shr of ExprEl * ExprEl
    | Not of ExprEl
    | UnaryPlus of ExprEl
    | UnaryMinus of ExprEl
    | Equal of ExprEl * ExprEl
    | NotEqual of ExprEl * ExprEl
    | StrictlyLessThan of ExprEl * ExprEl
    | StrictlyGreaterThan of ExprEl * ExprEl
    | LessThanOrEqual of ExprEl * ExprEl
    | GreaterThanOrEqual of ExprEl * ExprEl
    | As of ExprEl * ExprEl
    | Addr of ExprEl
    | Is of ExprEl * ExprEl
    | In of ExprEl * ExprEl
    | TupleExpr of ExprEl list
with
    override self.ToString() =
        match self with
        | Value v -> v.ToString()
        | Expr e -> e.ToString()
        | Add(e1, e2) -> sprintf "%O + %O" e1 e2
        | Multiply(e1, e2) -> sprintf "%O * %O" e1 e2
        | Minus(e1, e2) -> sprintf "%O - %O" e1 e2
        | Divide(e1, e2) -> sprintf "%O / %O" e1 e2
        | And(e1, e2) -> sprintf "%O and %O" e1 e2
        | Or(e1, e2) -> sprintf "%O or %O" e1 e2
        | Xor(e1, e2) -> sprintf "%O xor %O" e1 e2
        | Mod(e1, e2) -> sprintf "%O mod %O" e1 e2
        | Div(e1, e2) -> sprintf "%O div %O" e1 e2
        | Shl(e1, e2) -> sprintf "%O shl %O" e1 e2
        | Shr(e1, e2) -> sprintf "%O shr %O" e1 e2
        | Not e -> sprintf "not %O" e
        | UnaryPlus e -> sprintf "+%O" e
        | UnaryMinus e -> sprintf "-%O" e
        | Equal(e1, e2) -> sprintf "%O = %O" e1 e2
        | NotEqual(e1, e2) -> sprintf "%O <> %O" e1 e2
        | StrictlyLessThan(e1, e2) -> sprintf "%O < %O" e1 e2
        | StrictlyGreaterThan(e1, e2) -> sprintf "%O > %O" e1 e2
        | LessThanOrEqual(e1, e2) -> sprintf "%O <= %O" e1 e2
        | GreaterThanOrEqual(e1, e2) -> sprintf "%O >= %O" e1 e2
        | As(e1, e2) -> sprintf "%O as %O" e1 e2
        | Addr e -> sprintf "@(%O)" e
        | Is(e1, e2) -> sprintf "%O is %O" e1 e2
        | In(e1, e2) -> sprintf "%O in %O" e1 e2
        | TupleExpr ht ->
            match ht with
            | h::t -> List.fold(fun s i -> s + ":" + i.ToString()) (h.ToString()) t
            | _ -> failwith "IE"

    member self.BoxPos = box self

and Designator =
    | Ident of string
    | Deref
    | Array of ExprEl list
with
    override self.ToString() =
        match self with
        | Ident i -> i
        | Deref -> "^"
        | Array a -> List.fold (fun s i -> sprintf "%s[%O]" s i) "" a

    member self.BoxPos =
        match self with
        | Ident _ -> box self
        | Deref -> failwith "IE"
        | Array _ -> failwith "IE"

and
    CallParam =
    | ParamExpr of ExprEl
    | ParamIdent of DIdent
with
    override self.ToString() =
        match self with
        | ParamExpr ee -> ee.ToString()
        | ParamIdent id -> id.ToString()

    member self.BoxPos =
        match self with
        | ParamExpr expr -> box expr
        | ParamIdent id -> id.BoxPos

type ConstExpr = 
    | ConstExpr of ExprEl
    | ConstConstr of ConstExpr list
    | ConstStructConstr of (string * ConstExpr) list
with
    override self.ToString() =
        match self with
        | ConstExpr ce -> ce.ToString()
        | ConstConstr cc -> cc |> List.map (sprintf "%O") |> String.concat "," |> sprintf "(%s)"
        | ConstStructConstr csc -> csc |> List.map (fun (name, expr) -> sprintf "%s: %O" name expr)
                                   |> String.concat ";" |> sprintf "(%s)"

type ConstExprRange = (ConstExpr * ConstExpr)

type ArrayDimension =
    | DimensionType of string
    | DimensionExpr of ConstExprRange
with
    override self.ToString() =
        match self with
        | DimensionType s -> s
        | DimensionExpr(l, h) -> sprintf "%O..%O" l h 

type ParamKind = Var | Const

type ArrayDef = ArrayDef of (bool * ArrayDimension list * TypeIdentifier)
with
    override self.ToString() =
        match self with
        | ArrayDef (p, ad, t) ->
            let dims = ad |> List.map (sprintf "%O") |> String.concat ", "
            sprintf "array[%s] of %O" dims t

and TypeIdentifier =
    | TIdString of unit
    | TIdFile of unit
    | TIdPointer of int * TypeIdentifier
    | TIdSet of packed: bool * TypeIdentifier
    | TIdIdent of DIdent
    | TIdArray of ArrayDef
with
    static member FromString str = TIdIdent(DIdent.FromString str)
    static member FromDIdent did = TIdIdent did
    
    override self.ToString() =
        match self with
        | TIdString _ -> "string"
        | TIdFile _ -> "file"
        | TIdPointer (_, t) -> "^" + t.ToString()
        | TIdSet (_, t) -> "set of " + t.ToString()
        | TIdIdent id -> id.ToString()
        | TIdArray ad -> ad.ToString()

    member self.BoxPos = box self

type ParamList = (ParamKind option * (string list * TypeIdentifier option)) list option
type ProcHeader = DIdent option * TypeIdentifier option * ParamList

type TypeDecl =
    | TypeRecord of (bool * (string list * TypeIdentifier) list)
    | TypeArray of ArrayDef
    | TypePtr of (int * TypeIdentifier)
    | TypeAlias of (bool * TypeIdentifier)
    | TypeSet of (bool * TypeIdentifier)
    | TypeEnum of string list
    | SimpleRange of ConstExprRange
    | ProcType of ProcHeader
    
type Type = (DIdent * TypeDecl)

type CaseLabel =
    | CaseExpr of ConstExpr
    | CaseRange of ConstExprRange

type Statement =
    | EmptyStm
    | AssignStm of (DIdent * ExprEl)
    | CallStm of CallExpr
    | IdentStm of DIdent
    | IfStm of (ExprEl * Statement list * Statement list)
    | CaseStm of (ExprEl * (CaseLabel list * Statement list) list * Statement list)
    | WhileStm of (ExprEl * Statement list)
    | RepeatStm of (Statement list * ExprEl)
    | ForStm of (DIdent * ExprEl * int * ExprEl * Statement list)
    | WithStm of (DIdent list * Statement list)
    | GotoStm of DIdent
    | LabelStm of DIdent

type ProcKind = Procedure | Function

type ProcBodyDeclr = Declarations list * Statement list

and ProcDeclaration =
    | ExternalDeclr of string * string
    | ForwardDeclr
    | BodyDeclr of ProcBodyDeclr

and ProcAndFuncRec = { head: ProcHeader; decl: ProcDeclaration }
with
    static member Create(head, decl) = { head = head; decl = decl }
    member self.BoxPos = box self

and Declarations =
    | Types of Type list
    | Variables of (DIdent list * TypeIdentifier) list
    | Constants of (DIdent * TypeIdentifier option * ConstExpr) list
    | Labels of DIdent list
    | ProcAndFunc of ProcAndFuncRec

type Program =
    | Unit of string
    | Program of string
    | Library of string
    
type MainBlockRec = {
    decl: Declarations list; stmt: Statement list
} with
    static member Create (decl, stmt) = { decl = decl; stmt = stmt }

type ModuleSection = { uses: DIdent list; decl: Declarations list }

type UnitModuleRec = {
    name: DIdent
    intf: ModuleSection
    impl: ModuleSection
    init: Statement list
    fini: Statement list
} with
    static member Create (name, ((intfUses, intfDecl), (implUses, implDecl), (init, fini))) =
        {
            name = name
            intf = { uses = intfUses; decl = intfDecl }
            impl = { uses = implUses; decl = implDecl }
            init = init
            fini = fini
        }
        
    member self.UnitName = self.name.ToString()

type MainModuleRec = {
    name: string option
    uses: DIdent list
    block: MainBlockRec
} with
    static member Create(name, uses, block) = { name = name; uses = uses; block = MainBlockRec.Create block }
    
    member self.ProgramName = defaultArg self.name "Program"


let (|UnitOp|_|) = function
    | TupleExpr[] -> Some UnitOp
    | _ -> None