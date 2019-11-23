module NP.PasIl

open System.Collections.Generic
open Mono.Cecil
open Mono.Cecil.Cil

type IlInstruction =
     | Unknown
     | Call of MethodReference
     | Ldc_I4 of int
     | Ldloc of int
     | Stloc of int
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
     | Brfalse of (int * Instruction)
     | Br of (int * Instruction)
     | Nop
     | Ldstr of string

type MetaInstruction =
     | DeclareLocal of TypeReference
     | InstructionSingleton of IlInstruction
     | InstructionList of IlInstruction list
     | IlInstructionList of Instruction list

type Ctx = Map<string,(int*TypeReference)>

let private instr = function
                    | AddInst        -> Instruction.Create(OpCodes.Add)
                    | MultiplyInst   -> Instruction.Create(OpCodes.Mul)
                    | MinusInst      -> Instruction.Create(OpCodes.Sub)
                    | DivideInst     -> Instruction.Create(OpCodes.Div)
                    | AndInst        -> Instruction.Create(OpCodes.And)
                    | OrInst         -> Instruction.Create(OpCodes.Or)
                    | XorInst        -> Instruction.Create(OpCodes.Xor)
                    | Rem            -> Instruction.Create(OpCodes.Rem)
                    | ShlInst        -> Instruction.Create(OpCodes.Shl)
                    | ShrInst        -> Instruction.Create(OpCodes.Shr)
                    | NotInst        -> Instruction.Create(OpCodes.Not)
                    | NegInst        -> Instruction.Create(OpCodes.Neg)
                    | Call mi        -> Instruction.Create(OpCodes.Call, mi)
                    | Ldc_I4 n when n = -1 -> Instruction.Create(OpCodes.Ldc_I4_M1)
                    | Ldc_I4 n when n >= 0 && n <= 8 -> 
                        match n with
                        | 0 -> Instruction.Create(OpCodes.Ldc_I4_0)
                        | 1 -> Instruction.Create(OpCodes.Ldc_I4_1)
                        | 2 -> Instruction.Create(OpCodes.Ldc_I4_2)
                        | 3 -> Instruction.Create(OpCodes.Ldc_I4_3)
                        | 4 -> Instruction.Create(OpCodes.Ldc_I4_4)
                        | 5 -> Instruction.Create(OpCodes.Ldc_I4_5)
                        | 6 -> Instruction.Create(OpCodes.Ldc_I4_6)
                        | 7 -> Instruction.Create(OpCodes.Ldc_I4_7)
                        | 8 -> Instruction.Create(OpCodes.Ldc_I4_8)
                        | _ -> null
                    | Ldc_I4 n when n >= -128 && n <= 127 -> Instruction.Create(OpCodes.Ldc_I4_S, sbyte n)
                    | Ldc_I4 n -> Instruction.Create(OpCodes.Ldc_I4, n)
                    | Ldloc i when i <= 3 -> 
                        match i with 
                        | 0 -> Instruction.Create(OpCodes.Ldloc_0)
                        | 1 -> Instruction.Create(OpCodes.Ldloc_1)
                        | 2 -> Instruction.Create(OpCodes.Ldloc_2)
                        | 3 -> Instruction.Create(OpCodes.Ldloc_3)
                        | _ -> null
                    | Ldloc i when i <= 255 -> Instruction.Create(OpCodes.Ldloc_S, byte i) 
                    | Ldloc i -> Instruction.Create(OpCodes.Ldloc, i)
                    | Stloc i when i <= 3 -> 
                        match i with 
                        | 0 -> Instruction.Create(OpCodes.Stloc_0)
                        | 1 -> Instruction.Create(OpCodes.Stloc_1)
                        | 2 -> Instruction.Create(OpCodes.Stloc_2)
                        | 3 -> Instruction.Create(OpCodes.Stloc_3)
                        | _ -> null
                    | Stloc i when i <= 255 -> Instruction.Create(OpCodes.Stloc_S, byte i) 
                    | Stloc i -> Instruction.Create(OpCodes.Stloc, i)
                    | Ret            -> Instruction.Create(OpCodes.Ret)
                    | Unknown        -> Instruction.Create(OpCodes.Nop)
                    | Ceq            -> Instruction.Create(OpCodes.Ceq)
                    | Brfalse (s, i) when s >= -128 && s <= 127 -> Instruction.Create(OpCodes.Brfalse_S, i)
                    | Brfalse (_, i) -> Instruction.Create(OpCodes.Brfalse, i)
                    | Br (s, i) when s >= -128 && s <= 127 -> Instruction.Create(OpCodes.Br_S, i)
                    | Br (_, i) -> Instruction.Create(OpCodes.Br, i)
                    | Nop -> Instruction.Create(OpCodes.Nop)
                    | Ldstr s -> Instruction.Create(OpCodes.Ldstr, s)

let private emit (ilg : Cil.ILProcessor) inst = 
    match inst with
    | DeclareLocal t -> VariableDefinition(t) |> ilg.Body.Variables.Add
    | InstructionSingleton i -> i |> instr |> ilg.Append
    | InstructionList p -> p |> List.iter (instr >> ilg.Append) 
    | IlInstructionList i -> i |> List.iter ilg.Append

let findVar (DIdent ident) (ctx: Ctx) =
    assert(ident.Length = 1)
    ident.Head |> function | PIName n -> ctx.Item n

let findVarAndLoad ident (ctx: Ctx) = (findVar ident ctx) |> fst |> Ldloc |> List.singleton

let valueToIl v ctx = 
    match v with
    | VInteger i -> [Ldc_I4(i)]
    | VIdent i -> findVarAndLoad i ctx
    | VString s -> [Ldstr(s)]
    | _ -> [Unknown] 

let rec exprToIl exprEl ctx =
    let inline add2OpIl a b i = exprToIl a ctx @ exprToIl b ctx @ [i]
    let inline add1OpIl a i = exprToIl a ctx @ [i]
    match exprEl with
    | Value v -> valueToIl v ctx
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
    | _ -> []

let callParamToIl ctx cp = 
    match cp with
    | ParamExpr expr -> exprToIl expr ctx
    | ParamIdent id -> findVarAndLoad id ctx

// type internal Marker = interface end
// let t = typeof<Marker>.DeclaringType
// let a = AssemblyDefinition.ReadAssembly(t.Assembly.Location);
// let methodToRef (m: System.Reflection.MethodInfo): MethodReference = a.MainModule.ImportReference(m)    

let compileBlock (methodBuilder: MethodDefinition) (typeBuilder : TypeDefinition) (instr: MetaInstruction list) = 
    let ilGenerator = methodBuilder.Body.GetILProcessor() |> emit
    typeBuilder.Methods.Add(methodBuilder)
    List.iter ilGenerator instr
    // ilGenerator (Call (methodToCall.GetElementMethod()))
    // if methodToCall.ReturnType <> null then
    //     ilGenerator (DeclareLocal methodToCall.ReturnType)
    //     ilGenerator Stloc_0
    //     ilGenerator (Ldloc_S 0uy)
    //     let tr = methodToCall.ReturnType
    //     let rt = System.Type.GetType(tr.FullName + ", " + tr.Scope.ToString()) // tr.Module.Assembly.FullName)
    //     let writeln = typeof<System.Console>.GetMethod("WriteLine", [| rt |]) |> moduleBuilder.ImportReference
    //     ilGenerator (Call(writeln))
    Ret |> InstructionSingleton |> ilGenerator
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
    | Divide(e1, e2)   -> eval2 e1 e2 |> evalExprOp2 None       None
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

let stdType name =
    (Unchecked.defaultof<_>, name)
    |> PINameCreate
    |> List.singleton
    |> DIdent
    |> TIdIdent

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
        [Call(f)]

    let rec stmtToIl ctx s = 
        let foldBackStmt stmt s = (stmtToIl ctx stmt)::s
        let metaToIlList = function 
                           | InstructionList l -> l |> List.map instr
                           | InstructionSingleton s -> [instr s]
                           | IlInstructionList l -> l
                           | _ -> []
        let stmtToIlList sl = List.foldBack foldBackStmt sl [] |> List.collect metaToIlList
        let getIlListSize = List.fold (fun s (i: Instruction) -> s + i.GetSize()) 0
        match s with
        | CallStm(CallExpr(ident, cp)) -> 
            [for p in cp do callParamToIl ctx p] @ [findFunction(ident)] |> List.concat |> InstructionList
        | AssignStm(ident, expr) -> 
            let var = findVar ident ctx 
            exprToIl expr ctx @ [Stloc(fst var)] |> InstructionList
        | IfStm(expr, tb, fb) ->
            let e = [instr Nop]
            let c = exprToIl expr ctx
            let it = stmtToIlList tb
            let t =  it @ [instr(Br(getIlListSize it, e.Head))]
            let mutable f = stmtToIlList fb
            let s = getIlListSize t
            if f.IsEmpty then f <- [instr Nop]
            let fh = f.Head
            List.concat [List.map instr c;[instr(Brfalse(s, fh))];t;f;e] |> IlInstructionList
        | _ -> [] |> InstructionList

    let stmtListToIl (vars: List<MetaInstruction>) sl ctx =
        [   
            for v in vars do v
            for s in sl do (stmtToIl ctx s) 
        ]

    let defTypes = Map.ofArray[|
            (stdType "Integer", mb.TypeSystem.Int32)
            (stdType "Byte", mb.TypeSystem.Byte)
            (stdType "Boolean", mb.TypeSystem.Boolean)
        |]

    let findType =
        typeIdToStr

    member _.BuildIl(block: Block) =
        let vars = List<MetaInstruction>(block.decl.Length)
        block.decl
        |> List.collect 
               (function
               | Variables(v) -> v |> List.collect (fun (l, t) -> l |> List.map (fun v -> (v, defTypes.[t])))
               | _ -> [])
        |> List.mapi (fun i (v, t) -> vars.Add(DeclareLocal(t)); (v, (i, t)))
        |> Map.ofList
        |> stmtListToIl vars block.stmt
end