module NP.ParsePas

open System.Collections.Generic

open FParsec
open FParsec.Primitives
open FParsec.CharParsers
//open FParsec.Pipes
//open FParsec.Pipes.Precedence

let inline (!^) (a: Parser<'a list option,_>) = a |>> Option.defaultValue []
let inline (!^^) (a: Parser<'a list option option,_>) = 
    a |>> function Some v -> defaultArg v [] | None -> []
let inline castAs f (a, b) = (f a, f b)
let inline toList a = [a]
let inline (!+) x a = (a, x)

let hasStr s = str_wsc s >>. preturn true <|>% false
let inline str_opt s v = opt (str_wsc s >>% Some v )

let ``do `` = str_wsc "do"
let ``if `` = str_wsc "if"
let ``of `` = str_wsc "of"
let ``to `` = str_wsc "to"
let ``end `` = str_wsc "end"
let ``for `` = str_wsc "for"
let ``nil `` = str_wsc "nil"
let ``set `` = str_wsc "set"
let ``var `` = str_wsc "var"
let ``case `` = str_wsc "case"
let ``else `` = str_wsc "else"
let ``file `` = str_wsc "file"
let ``goto `` = str_wsc "goto"
let ``then `` = str_wsc "then"
let ``type `` = str_wsc "type"
let ``with `` = str_wsc "with"
let ``array `` = str_wsc "array"
let ``begin `` = str_wsc "begin"
let ``const `` = str_wsc "const"
let ``label `` = str_wsc "label"
let ``until `` = str_wsc "until"
let ``while `` = str_wsc "while"
let ``downto `` = str_wsc "downto"
let ``record `` = str_wsc "record"
let ``repeat `` = str_wsc "repeat"
let ``string `` = str_wsc "string"
let ``program `` = str_wsc "program"
let ``function `` = str_wsc "function"
let ``procedure `` = str_wsc "procedure"
let ``^ `` = str_wsc "^"
let ``[ `` = str_wsc "["
let ``] `` = str_wsc "]"
let ``( `` = str_wsc "("
let ``) `` = str_wsc ")"
let ``. `` = str_wsc "."
let ``, `` = str_wsc ","
let ``: `` = str_wsc ":"
let ``; `` = str_wsc ";"
let ``= `` = str_wsc "="
let ``.. `` = str_wsc ".."
let ``:= `` = str_wsc ":="
let ``?type `` = hasStr "type"
let ``?packed `` = hasStr "packed"

let keywords = [
    "as"
    "do"
    "if"
    "in"
    "is"
    "of"
    "or"
    "to"
    "and"
    "div"
    "end"
    "for"
    "mod"
    "nil"
    "set"
    "shl"
    "shr"
    "var"
    "xor"
    "case"
    "else"
    "file"
    "goto"
    "then"
    "type"
    "with"
    "array"
    "begin"
    "const"
    "label"
    "while"
    "until"
    "downto"
    "packed"
    "repeat"
    "string"
    "program"
    "function"
    "procedure"
  ]

let expr = opp.ExpressionParser

let keywordsSet = HashSet<string>(keywords);

let isKeyword s = keywordsSet.Contains s

let numeralOrDecimal : Parser<_, PasState> =
    // note: doesn't parse a float exponent suffix
    numberLiteral NumberLiteralOptions.AllowFraction "number" 
    |>> function // raises an exception on overflow
        | v when v.IsInteger -> v.String |> int |> VInteger
        | v -> v.String |> float |> VFloat

let numeral : Parser<_, PasState> =
    // note: doesn't parse a float exponent suffix
    numberLiteral NumberLiteralOptions.None "integer" 
    |>> fun v ->  // raises an exception on overflow
          v.String |> int |> VInteger

let hexNumber =    
    pstring "$" >>. many1SatisfyL isHex "hex digit"
    |>> fun s -> System.Convert.ToInt32(s, 16) |> VInteger // raises an exception on overflow

// let binaryNumber =    
//     pstring "#b" >>. many1SatisfyL (fun c -> c = '0' || c = '1') "binary digit"
//     |>> fun hexStr -> 
//             // raises an exception on overflow
//             Binary(System.Convert.ToInt32(hexStr, 2))


let number =
    choiceL [numeral
             hexNumber]
            "number literal"

let numberOrDecimal =
    choiceL [numeralOrDecimal
             hexNumber]
            "number literal"


let stringLiteral =
    let normalCharSnippet = manySatisfy ((<>) ''')
    let escapedChar = pstring "'" >>? (pstring "'" >>% "'") 
    many1Strings ((between (pstring "'") (pstring "'") (stringsSepBy normalCharSnippet escapedChar))
                 <|> ((pstring "#" >>. number) >>= function
                                                   | VInteger i -> i |> char |> string |> preturn
                                                   | _ -> fail "integer expected"))

let tkIdentifier =
    let isProperFirstChar c = isLetter c || c = '_'
    let isProperChar c = isLetter c || c = '_' || isDigit c
    many1Satisfy2L isProperFirstChar isProperChar "ident"
    .>> wsc
    
let identifier : Parser<string, PasState> =
    let expectedIdentifier = expected "identifier"
    fun stream ->
        let state = stream.State
        let reply = tkIdentifier stream
        if reply.Status <> Ok || not (isKeyword reply.Result) then reply
        else // result is keyword, so backtrack to before the string
            stream.BacktrackTo(state)
            Reply(Error, expectedIdentifier)

let constRangeExpression =
    (expr .>> wsc)
    .>>. (``.. `` >>. expr  .>> wsc) 
    |>> (castAs ConstExpr >> ConstExprRange)
 
let constType =
    identifier |>> DimensionType

let arrayIndex =
    choice[constType;constRangeExpression |>> DimensionExpr ]

let arrayIndexes =
    ``[ `` >>. (sepEndBy1 arrayIndex ``, ``) .>> ``] ``

let designator = 
    pipe2   (getPosition .>>. identifier |>> PIPosNameCreate) 
            (manyTill
                (choice[
                        ``^ `` >>% Deref; 
                        (``[ `` >>. (sepEndBy1 expr ``, ``) .>> ``] ``)
                        |>> Designator.Array;
                        ``. `` >>. getPosition .>>. identifier |>> PIPosNameCreate
                      ])
                (lookAhead(followedBy (next2CharsSatisfy 
                              (fun c1 c2 ->
                                match (c1, c2) with
                                | '.', '.' -> true
                                | '^', _ | '[', _ | '.', _ -> false
                                | _, _ -> true)))))
            (fun h t -> h::t |> DIdent)

let typeIdentifier, typeIdentifierRef = createParserForwardedToRef()

let typePtrDef =
    many1(``^ ``) .>>. typeIdentifier |>> fun (r, t) -> (r.Length, t)

let arrayDecl =
    tuple3
        (``?packed ``)
        (``array `` >>. arrayIndexes)
        (``of `` >>. typeIdentifier)

let typeSetDef = 
  ``?packed `` .>>.? (``set `` >>. ``of `` >>. typeIdentifier)

typeIdentifierRef :=
    choice[
            typePtrDef |>> TIdPointer
            designator |>> TIdIdent
            ``string `` >>% TIdString
            ``file `` >>% TIdFile
            arrayDecl |>> (ArrayDef >> TIdArray)
            typeSetDef |>> TIdSet
          ]

let fieldListDecl p =
    p identifier ``, `` .>>. (``: `` >>. typeIdentifier)

let fieldsList  =
    fieldListDecl sepBy

let recordDef =
      ``?packed `` .>>.? (between ``record `` ``end `` (sepEndBy fieldsList ``; ``))
      
let structType =
    recordDef |>> Record
     
let typeAlias =
    ``?type `` .>>. typeIdentifier 
    |>> TypeAlias

let paramListDecl p =
    p identifier ``, `` .>>. opt(``: `` >>. typeIdentifier)

let paramList1 =
    paramListDecl sepBy1

let formalParam  = 
        opt ((``const `` >>. preturn ParamKind.Const) <|> (``var `` >>. preturn Var))
        .>>. paramList1
        
let formalParamsList =
    between ``( `` ``) `` (sepEndBy formalParam ``; ``)  

let procDecl = 
    tuple3
        ((``procedure `` >>. preturn Procedure) 
         <|> (``function `` >>. preturn Function)) 
        (opt identifier)
        (opt formalParamsList)
     >>= fun (k, n, p) -> match k with
                          | Function -> (``: `` >>. typeIdentifier) |>> fun i -> (n, Some(i), p)
                          | Procedure -> preturn(n, None, p)
                          
let typeProc =
    procDecl |>> ProcType
    
let typeRange =
    expr .>>. (``.. `` >>. expr) |>> ((castAs ConstExpr) >> SimpleRange)

let typeEnum = between ``( `` ``) `` (sepEndBy1 identifier ``, ``) |>> TypeEnum

let typeSet = typeSetDef |>> TypeSet

let arrayType = arrayDecl |>> (ArrayDef >> Array)

let typePtr = typePtrDef |>> TypePtr

let typeDeclarations =
    (``type `` >>.
        many1 (
            (identifier .>> ``= ``)
            .>>. ((choice[structType;attempt(arrayType);typePtr;attempt(typeRange);typeAlias;typeProc;typeSet;attempt(typeEnum)]).>> ``; ``)
            |>> Type)) 
    |>> Types
 
let exprInt =
    pint32 .>> wsc .>> lookAhead(followedBy (next2CharsSatisfy 
                                  (fun c1 c2 ->
                                    match (c1, c2) with
                                    | '.', '.' -> true
                                    | '.', _ -> false
                                    | _, _ -> true))) 
    |>> VInteger
    // 

let exprFloat =
    //lookAhead(pint32 .>> wsc .>> notFollowedBy (pstring "..")) >>. 
    //pfloat |>> Float
    numberOrDecimal

let exprString = 
    stringLiteral |>> VString
    
let exprIdent =
    designator |>> VIdent

let exprNil =
    ``nil `` >>% VNil

let exprSet =
    ``[ `` >>. (sepEndBy (attempt(expr .>>. (``.. `` >>. expr) |>> SRange) <|> (expr |>> SValue)) ``, ``) .>> ``] `` 
    |>> VSet
    
let callExpr =
    let actualParam =
        (expr |>> function 
                  | Value v -> match v with VIdent i -> ParamIdent(i) | v -> Value(v) |> ParamExpr
                  | e -> ParamExpr e)
    let actualParamsList =
        between ``( `` ``) `` (sepEndBy actualParam ``, ``)
    designator .>>.? actualParamsList 
    |>> CallExpr

let exprCall = 
    callExpr |>> VCallResult

let exprAtom =
    choice[exprCall; exprIdent; attempt(exprInt); exprFloat; exprString; exprSet; exprNil] |>> Value
    
let exprExpr =
    between ``( `` ``) `` expr 

let term = (exprExpr <|> exprAtom) .>> wsc
opp.TermParser <- term

addOperators()

let varDeclarations =
    ``var `` 
    >>. many1 (tuple2 ((sepEndBy1 identifier ``, ``) .>> ``: ``)
              (typeIdentifier .>> ``; ``)) 
    |>> Variables
    
let typedConstConstr, typedConstConstrRef = createParserForwardedToRef()

let constConstr =
    choice[
        attempt( expr |>> ConstExpr)
        attempt(``( `` >>. (sepEndBy typedConstConstr ``, ``) .>> ``) `` |>> ConstConstr)
        attempt(``( `` >>. (sepEndBy ((identifier .>> ``: ``) .>>. typedConstConstr) ``; ``) .>> ``) `` |>> ConstStructConstr)
    ]

typedConstConstrRef := constConstr

let constDeclarations =
    ``const `` 
    >>. many1 (
            tuple3
                (identifier)
                (opt(``: `` >>. typeIdentifier))
                (
                    ``= `` >>. constConstr .>> ``; ``
                )
            )
    |>> Constants


let labelDeclarations =
    ``label ``
    >>. sepBy1 identifier ``, `` .>> ``; ``
    |>> Labels

let expression =
    pint32

let emptyStatement =
    lookAhead(``; ``) >>% EmptyStm

let simpleStatement =
    designator .>>.? (``:= `` >>. expr) |>> AssignStm

let callStatement =
    callExpr |>> CallStm

let designatorStatement =
    designator |>> IdentStm

let compoundStatement, compoundStatementRef = createParserForwardedToRef()

let ifStatement =
    tuple3 (``if `` >>. expr .>> ``then ``)
           !^(opt compoundStatement)
           !^(opt(``else `` >>. !^(opt(compoundStatement)))) 
    |>> IfStm  

let caseLabel =
    sepBy1 ((attempt(constRangeExpression |>> CaseRange))
            <|>
            (expr |>> (ConstExpr >> CaseExpr))) ``, ``

let statementList, statementListRef = createParserForwardedToRef()

let innerStatementList = 
        (statementList
         .>>. many(identifier .>>? (``: `` .>> many ``; ``) |>> LabelStm) 
         |>> function
             | (s, []) -> s |> List.concat
             | (s, l) -> [l] |> List.append s |> List.concat
        )

let caseStatement =
    tuple3 (``case `` >>. expr .>> ``of ``)
           (sepEndBy(caseLabel
               .>>. (``: `` >>. !^(opt compoundStatement))) ``; ``)
           !^(opt (``else `` >>. innerStatementList) .>> ``end ``)
           |>> CaseStm

let forStatement =
    tuple5 
        (``for `` >>. designator)
        (``:= `` >>. expr)
        ((``to `` >>% 1) <|> (``downto `` >>% -1))
        expr
        (``do `` >>. !^(opt compoundStatement))
    |>> ForStm

let repeatStatement =
    (between 
        ``repeat `` 
        ``until `` 
        (sepEndBy compoundStatement ``; ``) |>> List.concat)
    .>>. expr
    |>> RepeatStm

let whileStatement =
    (``while `` >>. expr .>> ``do ``)
    .>>. !^(opt compoundStatement)
    |>> WhileStm

let withStatement =
    (``with `` >>. (sepEndBy designator ``, ``) .>> ``do ``)
    .>>. !^(opt compoundStatement)
    |>> WithStm

let gotoStatement =
    ``goto `` >>. identifier |>> GotoStm

let statement =
    many(identifier .>>? (lookAhead(notFollowedBy (pstring ":=")) >>. ``: ``) |>> LabelStm) 
    .>>.
    choice[
            simpleStatement
            callStatement
            designatorStatement
            ifStatement
            caseStatement
            forStatement
            repeatStatement
            whileStatement
            withStatement
            gotoStatement
            emptyStatement
        ] 
    |>> function
        | ([], s) -> [s]
        | (l, s) -> l @ [s]
    
let procFuncDeclarations, procFuncDeclarationsRef = createParserForwardedToRef()

let declarations =
    many (choice[typeDeclarations; varDeclarations; constDeclarations; labelDeclarations; procFuncDeclarations]) 

let beginEnd = 
    between ``begin `` ``end `` 
            (statementList
             .>>. many(identifier .>>? (``: `` .>> many ``; ``) |>> LabelStm) 
             |>> function
                 | (s, []) -> s |> List.concat
                 | (s, l) -> [l] |> List.append s |> List.concat
            )

statementListRef := (sepEndBy (statement <|> compoundStatement) (many1 ``; ``))
    
let block = 
    declarations .>>. beginEnd
    (*fun(stream: CharStream<PasState>) ->
        let reply = 
            ((opt declarations |>> 
                function
                | Some s -> 
                    let us = stream.UserState
                    for d in s do
                        match d with
                        | Types t -> ()
                        | Variables v -> ()
                        | Consts c -> ()
                        | Labels l -> 
                            for i in l do
                                (i, Label{name=i; stmtPoint=false})
                                |> us.moduled.block.symbols.Add
                | _ -> () 
            ) 
            .>>. beginEnd) stream
        reply*)
        

let stdCompoundStatement = beginEnd <|> statement

compoundStatementRef := stdCompoundStatement 

type FuncNonDeclaredKind =
    | ForwardKind
    | ExternalKind of (string * string)

let nonDeclKind =
    let strWsc = stringLiteral .>> wsc
    (str_wsc "forward" .>> ``; `` >>% ForwardKind)
    <|>
    (str_wsc "external" >>. (strWsc .>>. (str_wsc "name" >>. strWsc)) .>> ``; `` |>> ExternalKind)

procFuncDeclarationsRef :=
    ((procDecl .>>. (``; `` >>. (opt nonDeclKind)))
        >>= fun (d, f) ->
              match f with
              | Some ForwardKind -> preturn(d, ForwardDeclr)
              | Some (ExternalKind es) -> preturn(d, ExternalDeclr es)
              | None -> ((block .>> ``; ``) |>> fun b -> (d, BodyDeclr(b)))
    )        
    |>> ProcAndFunc

let program =
    (opt(``program `` >>. identifier .>> ``; ``))
    .>>.
    (block .>> pstring ".")        

let pascalModule =
    wsc >>. program .>> (skipManyTill skipAnyChar eof)
    
let pGrammar: Parser<_, PasState> =  // one type annotation is enough for the whole parser
    pascalModule