[<AutoOpen>]
module np.PasAst

type Ident = string

type DIdent = DIdent of Designator list 

and CallExpr = CallExpr of DIdent * CallParam list

and SetAtom =
    | SValue of ExprEl
    | SRange of ExprEl * ExprEl

and Value =
    | Float of float
    | Integer of int
    | String of string
    | Ident of DIdent
    | CallResult of CallExpr
    | Nil
    | Set of SetAtom list

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
    | String
    | File
    | TypePtr of int * TypeIdentifier
    | TypeSet of packed: bool * TypeIdentifier
    | Ident of DIdent
    | Array of ArrayDef

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

type Declarations =
    | Types of Type list
    | Variables of (string list * TypeIdentifier) list
    | Consts of (string * TypeIdentifier option * ConstExpr) list
    | Labels of string list
    | ProcAndFunc of ProcHeader * (Declarations list * Statement list) option

type Program =
    | Unit of Ident
    | Program of Ident
    | Library of Ident
    
type ProgramAst = (string option * (Declarations list * Statement list))