module NP.PasIl

open System
open System.Collections
open System.Collections.Generic
open System.Linq.Expressions
open System.Linq
open Mono.Cecil
open Mono.Cecil.Cil

type BranchLabel =
    | LazyLabel of IlInstruction
    | Label of Instruction
    | ForwardLabel
    | UserLabel of string
    | IgnoreLabel

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
    | Beq of Instruction
    | Resolved of Instruction

and IlInstruction =
    | IlAtom of AtomIlInstruction ref
    | IlBrfalse of BranchLabel ref
    | IlBr of BranchLabel ref
    | IlBeq of BranchLabel ref
    | IlResolved of Instruction

type MetaInstruction =
    | DeclareLocal of VariableDefinition
    | InstructionSingleton of IlInstruction
    | InstructionList of IlInstruction list

type SysLabelRec = {
        branch: BranchLabel ref
        mutable level: int
    }

type VarLabelRec = {
        name: string
    }

type Symbol =
    | VariableSym of VariableDefinition
    //| LabelSym
    | EnumValueSym of int

type Ctx(
            variables: Dictionary<string,VariableDefinition>,
            labels: Dictionary<string, BranchLabel ref>,
            symbols: Dictionary<string, Symbol>,
            moduleBuilder: ModuleDefinition
        ) = class
    let localVariables = ref 0
    member val variables = variables
    member val labels = labels
    member val symbols = symbols
    member val res = List<MetaInstruction>()
    member self.EnsureVariable() =
        let ikey = !localVariables
        let key = string ikey
        incr localVariables
        // TODO manager of variables for reuse
//        let result = match self.variables.TryGetValue key with
//                     | true, value ->
//                         value
//                     | _ ->
        let value = VariableDefinition(moduleBuilder.TypeSystem.Int32)
        self.res.Add(DeclareLocal(value))
        self.variables.Add(key, value)
        self.symbols.Add(key, VariableSym(value))
        (key, value)
    
    member self.resolveSysLabels head labels =
        match head with
        | Some h ->
            match h with
            | IlResolved il -> for l in labels do l := Label(il)
            | IlBr _ -> for l in labels do l := LazyLabel(h)
            | _ -> failwithf "Internal error (%s:%s)" __SOURCE_FILE__ __LINE__
        | _ -> ()
    
  end

let brtoinstr l opc =
    let rec bril = function
           | Label l -> l
           | LazyLabel l -> match l with
                            | IlResolved i -> i
                            | IlBrfalse i -> bril !i
                            | IlBr i -> bril !i
                            | IlBeq i -> bril !i
                            | _ -> failwithf "Internal error (%s:%s)" __SOURCE_FILE__ __LINE__
           | IgnoreLabel -> null
           | _ -> failwithf "Internal error (%s:%s)" __SOURCE_FILE__ __LINE__
           
    let instr = bril !l
    if instr <> null then
        Instruction.Create(opc, instr)
    else null

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
                    | Beq i        -> Instruction.Create(OpCodes.Beq, i)
                    | Resolved i   -> i

let private instr = function
                    | IlBrfalse i  -> brtoinstr i OpCodes.Brfalse
                    | IlBr i       -> brtoinstr i OpCodes.Br
                    | IlBeq i      -> brtoinstr i OpCodes.Beq
                    | IlResolved i -> i
                    | IlAtom i     -> let r = ainstr !i
                                      i := Resolved(r)
                                      r

let private emit (ilg : Cil.ILProcessor) inst =
    let appendIfNotNull i = if i <> null then ilg.Append i
    match inst with
    | DeclareLocal t -> t |> ilg.Body.Variables.Add
    | InstructionSingleton is -> instr is |> appendIfNotNull
    | InstructionList p -> p |> List.iter (instr >> appendIfNotNull) 

let findSymbol (DIdent ident) (ctx: Ctx) =
    assert(ident.Length = 1)
    ident.Head |> function | PIName n -> ctx.symbols.[n]

let findSymbolAndLoad ident (ctx: Ctx) =
    match findSymbol ident ctx with
    | VariableSym vs -> vs |> Ldloc |> ainstr |> IlResolved |> List.singleton
    | EnumValueSym evs -> Ldc_I4(evs) |> ainstr |> IlResolved |> List.singleton

let valueToIl v ctx = 
    match v with
    | VInteger i -> Ldc_I4(i) |> ainstr |> IlResolved |> List.singleton
    | VIdent i -> findSymbolAndLoad i ctx
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
    | ParamIdent id -> findSymbolAndLoad id ctx

// type internal Marker = interface end
// let t = typeof<Marker>.DeclaringType
// let a = AssemblyDefinition.ReadAssembly(t.Assembly.Location);
// let methodToRef (m: System.Reflection.MethodInfo): MethodReference = a.MainModule.ImportReference(m)    

let compileBlock (methodBuilder: MethodDefinition) (typeBuilder : TypeDefinition) (instr: List<MetaInstruction>) =
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

let stdIdent = PINameCreate >> List.singleton >> DIdent
let stdType  = stdIdent >> TIdIdent

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
            
    let emptyLabelRec = Unchecked.defaultof<SysLabelRec>
    let rec stmtToIl (ctx: Ctx) sysLabels s =
        // for ifs
        let stmtToIlList sl =
            let foldStmt s stmt =
                let sl = stmtToIl ctx (snd s) stmt
                ((fst sl)::(fst s), snd sl)
            let metaToIlList = function 
                               | InstructionList l -> l
                               | InstructionSingleton s -> [s]
                               | _ -> []
            List.fold foldStmt ([],[]) sl |> fun (i,l) -> (List.rev i |> List.collect metaToIlList, l) 
        
        let (instructions, newSysLabels) =
                match s with
                | CallStm(CallExpr(ident, cp)) ->
                    ([for p in cp do callParamToIl ctx p] @ [findFunction(ident)] |> List.concat, [])
                | AssignStm(ident, expr) -> 
                    let var = findSymbol ident ctx |> function | VariableSym vs -> vs | _ -> failwith "IE"  
                    (exprToIl expr ctx @ [Stloc(var) |> ainstr |> IlResolved], [])
                | IfStm(expr, tb, fb) ->
                    // if logic
                    let firstEnfOfStm = ref ForwardLabel
                    let lastEndOfStm = ref ForwardLabel
                    let condition = exprToIl expr ctx
                    let (trueBranch, trueLabels) = stmtToIlList tb
                    let (falseBranch, falseLabels) = stmtToIlList fb
                    let hasFalseBranch = falseBranch.Length > 0
                    // TODO rethink LazyLabel here
                    let checkCondition = [IlBrfalse(if hasFalseBranch then ref (LazyLabel(falseBranch.Head)) else firstEnfOfStm)]
                    (List.concat [
                        condition
                        checkCondition
                        trueBranch @ [IlBr(if hasFalseBranch then firstEnfOfStm else lastEndOfStm)]
                        if hasFalseBranch then falseBranch @ [IlBr(lastEndOfStm)] else []
                    ], List.concat [[firstEnfOfStm;lastEndOfStm];trueLabels;falseLabels])
                | LabelStm l -> ([],[ctx.labels.[l]])
                | GotoStm s -> ([IlBr(ctx.labels.[s])],[]) // will do LazyLabel
                | CaseStm (expr, mainLabels, stmt) ->
                    // let case = exprToIl expr
                    let (name, var) = ctx.EnsureVariable()
                    let lastEndOfStm = ref ForwardLabel
                    let (defBranch, defLabels) = stmtToIlList stmt
                    // TODO reduce creation of new var if we want to just read variable
                    let (setCaseVar, _) = AssignStm(stdIdent name, expr) |> List.singleton |> stmtToIlList
                    let casec =
                            //List.concat
                                [for (tocheck, stmt) in mainLabels do
                                    let (trueBranch, trueLabels) = stmtToIlList stmt
                                    yield
                                        (
                                         List.concat [
                                            for l in tocheck do
                                                yield [IlAtom(ref(Ldloc(var)))]
                                                match l with
                                                | CaseExpr ce -> match ce with
                                                                 | ConstExpr ce ->
                                                                     yield (exprToIl ce ctx)
                                                                     yield [IlBeq(ref(LazyLabel(trueBranch.Head)))]
                                                                 | _ -> failwith "IE" 
                                                | _ -> failwith "IE";
                                           ],
                                         trueBranch @ [IlBr(lastEndOfStm)], trueLabels)
                                 yield (defBranch @ [IlBr(lastEndOfStm)], [], defLabels)
                                ]
                    let (cases, casesbodies, labels) = List.unzip3 casec |||> (fun a b c -> (List.concat a, List.concat b, List.concat c))
                    (List.concat[
                        setCaseVar
                        cases
                        casesbodies
                    ] , List.concat[labels ; [lastEndOfStm]])
                | EmptyStm -> ([],[])
                | _ -> ([],[])
        
        ctx.resolveSysLabels (List.tryHead instructions) sysLabels
        (instructions |> InstructionList, newSysLabels)

    let stmtListToIl sl (ctx: Ctx) =
        for v in ctx.variables.Values do ctx.res.Add(DeclareLocal(v))
        let lastSysLabels = ref []
        let resSeq = seq {
            for s in sl do
                let (instructions, sysLabels) = stmtToIl ctx !lastSysLabels s
                lastSysLabels := sysLabels
                yield instructions
        }
        for i in resSeq do ctx.res.Add i
        let ret = Ret |> ainstr |> IlResolved
        ctx.resolveSysLabels (Some ret) !lastSysLabels
        ctx.res.Add(ret |> InstructionSingleton)
        ctx.res

    let defTypes = Dictionary<TypeIdentifier, TypeReference>()
    do
        defTypes.Add(stdType "Integer", mb.TypeSystem.Int32)
        defTypes.Add(stdType "Byte", mb.TypeSystem.Byte)
        defTypes.Add(stdType "Boolean", mb.TypeSystem.Boolean)
    
    let findType =
        typeIdToStr

    member _.BuildIl(block: Block) =
        let variables = Dictionary<_,_>()
        let labelsMap = Dictionary<_,_>()
        let symbols = Dictionary<_,_>()
        printfn "%A" block.decl
        block.decl
        |> List.iter 
               (function
               | Types types ->
                   (
                       for (name, decl) in types do
                           defTypes.Add(stdType name, mb.TypeSystem.Int32)
                           match decl with
                           | TypeEnum enumValues -> enumValues |> List.iteri (fun i v -> symbols.Add (v, EnumValueSym(i))) 
                           | _ -> ()
                   )
               | Variables v ->
                   v
                   |> List.collect (fun (l, t) -> l |> List.map (fun v -> (v, defTypes.[t])))
                   |> List.iter (fun (v, t) -> (v, VariableDefinition(t)) |> variables.Add)
               | Labels labels -> (for l in labels do labelsMap.Add(l, ref (UserLabel l)))
               | _ -> ())
               
        for kv in variables do symbols.Add(kv.Key, VariableSym(kv.Value))
        
        let ctx = Ctx(variables, labelsMap, symbols, moduleBuilder)
        stmtListToIl block.stmt ctx
end