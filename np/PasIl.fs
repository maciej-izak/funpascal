module NP.PasIl

open System.Collections.Generic
open Mono.Cecil
open Mono.Cecil.Cil

type BranchLabel =
    | LazyLabel of IlInstruction
    | Label of Instruction
    | EmptyLabel

and AtomIlInstruction =
    | Unknown
    | Call of MethodReference
    | Ldc_I4 of int
    | Ldloc of VariableDefinition
    | Stloc of VariableDefinition
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
    | Nop
    | Ldstr of string
    | Brfalse of Instruction
    | Br of Instruction
    | Resolved of Instruction

and IlInstruction =
    | IlAtom of AtomIlInstruction ref
    | IlBrfalse of BranchLabel ref
    | IlBr of BranchLabel ref
    | IlResolved of Instruction

type MetaInstruction =
    | DeclareLocal of VariableDefinition
    | InstructionSingleton of IlInstruction
    | InstructionList of IlInstruction list
    | IlInstructionList of Instruction list

type LabelRec = {
        branch: BranchLabel ref
        level: int
    }

type Ctx(variables: Map<string,VariableDefinition>) = class
    member val variables = variables
    member val labels = Stack<LabelRec>()
  end

let brli = function
           | Label l -> l
           | LazyLabel l -> match l with
                            | IlResolved i -> i
                            | _ -> failwithf "Internal error (%s:%s)" __SOURCE_FILE__ __LINE__
           | _ -> failwithf "Internal error (%s:%s)" __SOURCE_FILE__ __LINE__

let private ainstr = function
                    | AddInst      -> Instruction.Create(OpCodes.Add)
                    | MultiplyInst -> Instruction.Create(OpCodes.Mul)
                    | MinusInst    -> Instruction.Create(OpCodes.Sub)
                    | DivideInst   -> Instruction.Create(OpCodes.Div)
                    | AndInst      -> Instruction.Create(OpCodes.And)
                    | OrInst       -> Instruction.Create(OpCodes.Or)
                    | XorInst      -> Instruction.Create(OpCodes.Xor)
                    | Rem          -> Instruction.Create(OpCodes.Rem)
                    | ShlInst      -> Instruction.Create(OpCodes.Shl)
                    | ShrInst      -> Instruction.Create(OpCodes.Shr)
                    | NotInst      -> Instruction.Create(OpCodes.Not)
                    | NegInst      -> Instruction.Create(OpCodes.Neg)
                    | Call mi      -> Instruction.Create(OpCodes.Call, mi)
                    | Ldc_I4 n     -> Instruction.Create(OpCodes.Ldc_I4, n)
                    | Ldloc i      -> Instruction.Create(OpCodes.Ldloc, i)
                    | Stloc i      -> Instruction.Create(OpCodes.Stloc, i)
                    | Ret          -> Instruction.Create(OpCodes.Ret)
                    | Unknown      -> Instruction.Create(OpCodes.Nop)
                    | Ceq          -> Instruction.Create(OpCodes.Ceq)
                    | Nop          -> Instruction.Create(OpCodes.Nop)
                    | Ldstr s      -> Instruction.Create(OpCodes.Ldstr, s)
                    | Brfalse i    -> Instruction.Create(OpCodes.Brfalse, i)
                    | Br i         -> Instruction.Create(OpCodes.Br, i)
                    | Resolved i   -> i

let private instr = function
                    | IlBrfalse i  -> Instruction.Create(OpCodes.Brfalse, brli !i)
                    | IlBr i       -> Instruction.Create(OpCodes.Br, brli !i)
                    | IlResolved i -> i
                    | IlAtom i     -> let r = ainstr !i
                                      i := Resolved(r)
                                      r

let private emit (ilg : Cil.ILProcessor) inst = 
    match inst with
    | DeclareLocal t -> t |> ilg.Body.Variables.Add
    | InstructionSingleton i -> i |> instr |> ilg.Append
    | InstructionList p -> p |> List.iter (instr >> ilg.Append) 
    | IlInstructionList i -> i |> List.iter ilg.Append

let findVar (DIdent ident) (ctx: Ctx) =
    assert(ident.Length = 1)
    ident.Head |> function | PIName n -> ctx.variables.Item n

let findVarAndLoad ident (ctx: Ctx) = (findVar ident ctx) |> Ldloc |> ainstr |> IlResolved |> List.singleton

let valueToIl v ctx = 
    match v with
    | VInteger i -> Ldc_I4(i) |> ainstr |> IlResolved |> List.singleton
    | VIdent i -> findVarAndLoad i ctx
    | VString s -> Ldstr(s) |> ainstr |> IlResolved |> List.singleton
    | _ -> Unknown |> ainstr |> IlResolved |> List.singleton 

let rec exprToIl exprEl ctx =
    let inline add2OpIl a b i = exprToIl a ctx @ exprToIl b ctx @ [i |> ainstr |> IlResolved]
    let inline add1OpIl a i = exprToIl a ctx @ [i |> ainstr |> IlResolved]
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

let compileBlock (methodBuilder: MethodDefinition) (typeBuilder : TypeDefinition) (instr: List<MetaInstruction>) (labels: Stack<LabelRec>) = 
    let ilGenerator = methodBuilder.Body.GetILProcessor() |> emit
    typeBuilder.Methods.Add(methodBuilder)
    Seq.iter ilGenerator instr
    // ilGenerator (Call (methodToCall.GetElementMethod()))
    // if methodToCall.ReturnType <> null then
    //     ilGenerator (DeclareLocal methodToCall.ReturnType)
    //     ilGenerator Stloc_0
    //     ilGenerator (Ldloc_S 0uy)
    //     let tr = methodToCall.ReturnType
    //     let rt = System.Type.GetType(tr.FullName + ", " + tr.Scope.ToString()) // tr.Module.Assembly.FullName)
    //     let writeln = typeof<System.Console>.GetMethod("WriteLine", [| rt |]) |> moduleBuilder.ImportReference
    //     ilGenerator (Call(writeln))
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
        [Call(f) |> ainstr |> IlResolved]

    let resolveLabels (ctx: Ctx) last level =
        let i = match last with
                | IlResolved l -> l
                | _ -> failwithf "Internal error (%s:%s)" __SOURCE_FILE__ __LINE__
        let mutable l: LabelRec = Unchecked.defaultof<_>
        while ctx.labels.TryPeek(&l) && l.level > level do
            printfn "fill label > %A" l
            l.branch := Label(i)
            ctx.labels.Pop() |> ignore
    
    let rec stmtToIl ctx s level = 
        let foldBackStmt stmt s = (stmtToIl ctx stmt (level+1))::s
        let metaToIlList = function 
                           | InstructionList l -> l // |> List.map instr
                           | InstructionSingleton s -> [s] // [instr s]
                           | IlInstructionList l ->  l |> List.map (fun i -> IlResolved(i))
                           | _ -> []
        let stmtToIlList sl = List.foldBack foldBackStmt sl [] |> List.collect metaToIlList
        //let getIlListSize = List.fold (fun s (i: Instruction) -> s + i.GetSize()) 0
        let i = match s with
                | CallStm(CallExpr(ident, cp)) ->
                    [for p in cp do callParamToIl ctx p] @ [findFunction(ident)] |> List.concat
                | AssignStm(ident, expr) -> 
                    let var = findVar ident ctx 
                    exprToIl expr ctx @ [Stloc(var) |> ainstr |> IlResolved]
                | IfStm(expr, tb, fb) ->
                    let endOfStm = ref EmptyLabel
                    let condition = exprToIl expr ctx
                    let trueBranch = (stmtToIlList tb) @ [IlBr(endOfStm)]
                    let falseBranch = (stmtToIlList fb) @ [IlBr(endOfStm)]
                    let checkCondition = [IlBrfalse(ref (LazyLabel(falseBranch.Head)))]
                    ctx.labels.Push {branch = endOfStm; level = level}
                    List.concat [condition;checkCondition;trueBranch;falseBranch]
                | _ -> []
        if i.Length > 0 then resolveLabels ctx i.Head level
        i |> InstructionList

    let stmtListToIl (vars: List<MetaInstruction>) sl ctx =
        let res = List<MetaInstruction>(vars)
        //let q = Stack<
        for s in sl do 
            match stmtToIl ctx s 0 with 
            | IlInstructionList i -> ()
            | m -> res.Add(m)
        let ret = Ret |> ainstr |> IlResolved
        resolveLabels ctx ret -1 // resolve all possible labels
        res.Add(ret |> InstructionSingleton)
        res
        // [   
        //     for v in vars do v
        //     for s in sl do (stmtToIl ctx s) 
        // ]

    let defTypes = Map.ofArray[|
            (stdType "Integer", mb.TypeSystem.Int32)
            (stdType "Byte", mb.TypeSystem.Byte)
            (stdType "Boolean", mb.TypeSystem.Boolean)
        |]

    let findType =
        typeIdToStr

    member _.BuildIl(block: Block) =
        let vars = List<MetaInstruction>(block.decl.Length)
        let ctx = Ctx(block.decl
                    |> List.collect 
                           (function
                           | Variables(v) -> v |> List.collect (fun (l, t) -> l |> List.map (fun v -> (v, defTypes.[t])))
                           | _ -> [])
                    // ! context !
                    |> List.map (fun (v, t) -> 
                        let var = VariableDefinition(t)
                        vars.Add(DeclareLocal(var))
                        (v, var))
                    |> Map.ofList);
        (stmtListToIl vars block.stmt ctx, ctx.labels)
end