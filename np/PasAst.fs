[<AutoOpen>]
module NP.PasAst

open FParsec
open System

let equalsOn f x (yobj:obj) =
    match yobj with
    | :? 'T as y -> (f x = f y)
    | _ -> false

let hashOn f x =  hash (f x)

let compareOn f x (yobj: obj) =
    match yobj with
    | :? 'T as y -> compare (f x) (f y)
    | _ -> invalidArg "yobj" "cannot compare values of different types"

[<CustomEquality; CustomComparison>]
type PIdent = 
    | PIdent of pos: Position * name: string

    static member Name(PIdent(name=n)) = n

    override self.Equals(a) =
        match a with
        | :? PIdent as i -> String.Equals((PIdent.Name self), (PIdent.Name i), StringComparison.InvariantCultureIgnoreCase) // equalsOn PIdent.Name self i
        | _ -> false

    override self.GetHashCode() = (PIdent.Name self).GetHashCode(StringComparison.InvariantCultureIgnoreCase) // hashOn PIdent.Name self

    interface System.IComparable with
      member self.CompareTo o = match o with
                                | :? PIdent as i -> compareOn PIdent.Name self i 
                                | _ -> invalidArg "o" "cannot compare values of different types"    

type DIdent = DIdent of Designator list 

and CallExpr = CallExpr of DIdent * CallParam list

and SetAtom =
    | SValue of ExprEl
    | SRange of ExprEl * ExprEl

and Value =
    | VFloat of float
    | VInteger of int
    | VString of string
    | VIdent of DIdent
    | VCallResult of CallExpr
    | VNil
    | VSet of SetAtom list

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

and Designator = 
    | Ident of PIdent
    | Deref
    | Array of ExprEl list

and CallParam =
    | ParamExpr of ExprEl
    | ParamIdent of DIdent
    
type ConstExpr = 
    | ConstExpr of ExprEl
    | ConstConstr of ConstExpr list
    | ConstStructConstr of (string * ConstExpr) list

type ConstExprRange = (ConstExpr * ConstExpr)

type ArrayDimension =
    | DimensionType of string
    | DimensionExpr of ConstExprRange

type ParamKind = Var | Const

type ArrayDef = ArrayDef of packed: bool * dimensions: ArrayDimension list * tname: TypeIdentifier

and TypeIdentifier =
    | TIdString
    | TIdFile
    | TIdPointer of int * TypeIdentifier
    | TIdSet of packed: bool * TypeIdentifier
    | TIdIdent of DIdent
    | TIdArray of ArrayDef

type ParamList = (ParamKind option * (string list * TypeIdentifier)) list option
type ProcHeader = string option * DIdent option * ParamList

type TypeDecl =
    | Record of packed: bool * fields: (string list * TypeIdentifier) list
    | Array of ArrayDef
    | TypePtr of int * TypeIdentifier
    | TypeAlias of strong: bool * origin: TypeIdentifier
    | TypeSet of packed: bool * TypeIdentifier
    | TypeEnum of string list
    | SimpleRange of ConstExprRange
    | ProcType of ProcHeader
    
type Type = (string * TypeDecl)

type CaseLabel =
    | CaseExpr of ConstExpr
    | CaseRange of ConstExprRange

type Statement =
    | EmptyStm
    | AssignStm of DIdent * ExprEl
    | CallStm of CallExpr
    | IdentStm of DIdent
    | IfStm of ExprEl * Statement list * Statement list
    | CaseStm of ExprEl * (CaseLabel list * Statement list) list * Statement list
    | WhileStm of ExprEl * Statement list
    | RepeatStm of Statement list * ExprEl
    | ForStm of DIdent * ExprEl * int * ExprEl * Statement list
    | WithStm of DIdent list * Statement list
    | GotoStm of string
    | LabelStm of string

type ProcKind = Procedure | Function

type ProcBodyDeclr = Declarations list * Statement list

and ProcDeclaration =
    | ExternalDeclr
    | ForwardDeclr of ProcBodyDeclr option ref
    | BodyDeclr of ProcBodyDeclr

and Declarations =
    | Types of Type list
    | Variables of (string list * TypeIdentifier) list
    | Consts of (string * TypeIdentifier option * ConstExpr) list
    | Labels of string list
    | ProcAndFunc of ProcHeader * ProcDeclaration

type Program =
    | Unit of string
    | Program of string
    | Library of string
    
type Block = {decl: Declarations list; stmt: Statement list}
     with 
       static member Create (decl: Declarations list, stmt: Statement list) = 
           {
               decl = decl
               stmt = stmt
           }

type ProgramAst = ProgramAst of name: string option * block: Block

let (|PIName|) = function
    | Ident(PIdent(name=n)) -> n
    | _ -> ""

let PIPosNameCreate = PIdent >> Ident
let PINameCreate name = PIPosNameCreate(null, name)
