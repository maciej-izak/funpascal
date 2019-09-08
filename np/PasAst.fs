[<AutoOpen>]
module np.PasAst

type Ident = string

type DIdent = DIdent of Designator list 

and Value =
    | Float of float
    | Integer of int
    | String of string
    | Ident of DIdent

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
    | As of ExprEl * ExprEl
    | Addr of ExprEl
    | Not of ExprEl
    | UnaryPlus of ExprEl
    | UnaryMinus of ExprEl
    | Equal of ExprEl * ExprEl
    | NotEqual of ExprEl * ExprEl
    | StrictlyLessThan of ExprEl * ExprEl
    | StrictlyGreaterThan of ExprEl * ExprEl
    | LessThanOrEqual of ExprEl * ExprEl
    | GreaterThanOrEqual of ExprEl * ExprEl
    | Is of ExprEl * ExprEl
    | In of ExprEl * ExprEl

and Designator =
    | Ident of string
    | Deref
    | Array of ExprEl list
    
type ConstExpr = ConstExpr of ExprEl

type ConstExprRange = (ConstExpr * ConstExpr)

type ArrayDimension =
    | DimensionType of string
    | DimensionExpr of ConstExprRange

type ParamKind = Var | Const

type TypeIdentifier =
    | String
    | File
    | Ident of DIdent

type ParamList = (ParamKind option * (string list * TypeIdentifier)) list option

type TypeDecl =
    | Record of packed: bool * fields: (string list * TypeIdentifier) list
    | Array of dimensions: ArrayDimension list * tname: string
    | TypePtr of TypeIdentifier
    | TypeAlias of strong: bool * origin: TypeIdentifier
    | SimpleRange of ConstExprRange
    | ProcType of result: DIdent option * paramList: ParamList
    
type Type = (string * TypeDecl)

type CaseLabel =
    | CaseExpr of ConstExpr
    | CaseRange of ConstExprRange

type Statement =
    | AssignStm of DIdent * ExprEl
    | IfStm of ExprEl * Statement list * Statement list
    | CaseStm of ExprEl * (CaseLabel list * Statement list) list * Statement list
    | WhileStm of ExprEl * Statement list
    | RepeatStm of Statement list * ExprEl
    | ForStm of DIdent * ExprEl * int * ExprEl * Statement list

type ProcKind = Procedure | Function

type Declarations =
    | Types of Type list
    | Variables of (string list * TypeIdentifier) list
    | Const of (string * ConstExpr) list

type Program =
    | Unit of Ident
    | Program of Ident
    | Library of Ident
    