module np.ParsePas

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
let ``var `` = str_wsc "var"
let ``case `` = str_wsc "case"
let ``else `` = str_wsc "else"
let ``file `` = str_wsc "file"
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
    "or"
    "to"
    "and"
    "div"
    "end"
    "for"
    "mod"
    "nil"
    "shl"
    "shr"
    "var"
    "xor"
    "else"
    "file"
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
    "string"
    "repeat"
    "program"
    "function"
    "procedure"
  ]

let expr = opp.ExpressionParser

let keywordsSet = new HashSet<string>(keywords);

let isKeyword s = keywordsSet.Contains s

let numeralOrDecimal : Parser<_, PasState> =
    // note: doesn't parse a float exponent suffix
    numberLiteral NumberLiteralOptions.AllowFraction "number" 
    |>> function // raises an exception on overflow
        | v when v.IsInteger -> v.String |> int |> Integer
        | v -> v.String |> float |> Float

let hexNumber =    
    pstring "$" >>. many1SatisfyL isHex "hex digit"
    |>> fun s -> System.Convert.ToInt32(s, 16) |> Integer // raises an exception on overflow

// let binaryNumber =    
//     pstring "#b" >>. many1SatisfyL (fun c -> c = '0' || c = '1') "binary digit"
//     |>> fun hexStr -> 
//             // raises an exception on overflow
//             Binary(System.Convert.ToInt32(hexStr, 2))


let number =
    choiceL [numeralOrDecimal
             hexNumber]
            "number literal"


let stringLiteral =
    let normalCharSnippet = manySatisfy ((<>) ''')
    let escapedChar = pstring "'" >>? (pstring "'" >>% "'") 
    many1Strings ((between (pstring "'") (pstring "'") (stringsSepBy normalCharSnippet escapedChar))
                 <|> ((pstring "#" >>. number) >>= function
                                                   | Integer i -> i |> char |> string |> preturn
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
    pipe2   (identifier |>> Designator.Ident) 
            (many(choice[ 
                        ``^ `` >>% Deref; 
                        (``[ `` >>. (sepEndBy1 expr ``, ``) .>> ``] ``)
                        |>> Designator.Array;
                        ``. `` >>. identifier |>> Designator.Ident
                      ]))
            (fun h t -> h::t |> DIdent)

let typeIdentifier =
    choice[
            designator |>> Ident;
            ``string `` >>% String;
            ``file `` >>% File  
          ]    

let field =
    identifier .>>. (``: `` >>. identifier)
    
let fieldListDecl p =
    p identifier ``, `` .>>. (``: `` >>. typeIdentifier)

let fieldsList  =
    fieldListDecl sepBy 

let fieldsList1 =
    fieldListDecl sepBy1

let recordDef =
      ``?packed `` .>>. (between ``record `` ``end `` (sepEndBy fieldsList ``; ``))
      
let structType =
    recordDef |>> Record
     
let arrayType =
    (``array `` >>. arrayIndexes) .>>. (``of `` >>. identifier)
    |>> Array

let typeAlias =
    ``?type `` .>>. typeIdentifier 
    |>> TypeAlias

let formalParam  = 
        opt ((``const `` >>. preturn ParamKind.Const) <|> (``var `` >>. preturn Var))
        .>>. fieldsList1
        
let formalParamsList =
    between ``( `` ``) `` (sepEndBy formalParam ``; ``)  

let procIntfDecl = 
    (((``procedure `` >>. preturn Procedure) 
    <|> (``function `` >>. preturn Function)) .>>. (opt formalParamsList))
    >>= fun (k, p) -> match k with
                      | Function -> (``: `` >>. designator) |>> fun i -> (Some(i), p)
                      | Procedure -> preturn(None, p)
        
let typeProc =
    procIntfDecl |>> ProcType
    
let typePtr =
    ``^ `` >>. typeIdentifier |>> TypePtr

let typeRange =
    expr .>>. (``.. `` >>. expr) |>> ((castAs ConstExpr) >> SimpleRange)

let typeDeclarations =
    (``type `` >>.
        many1 (
            (identifier .>> ``= ``)
            .>>. ((choice[structType;arrayType;typePtr;typeAlias;typeProc;typeRange]).>> ``; ``)
            |>> Type)) 
    |>> Types
 
let exprInt =
    pint32 |>> Integer
    
let exprFloat =
    pfloat |>> Float

let exprString = 
    stringLiteral |>> Value.String
    
let exprIdent =
    designator |>> Value.Ident
    
let callExpr =
    let actualParam =
        (expr |>> function 
                  | Value v -> match v with Value.Ident i -> ParamIdent(i) | v -> Value(v) |> ParamExpr
                  | e -> ParamExpr e)
    let actualParamsList =
        between ``( `` ``) `` (sepEndBy actualParam ``, ``)
    designator .>>. actualParamsList 
    |>> CallExpr

let exprCall = 
    callExpr |>> CallResult

let exprAtom =
    choice[attempt(exprCall); exprInt; exprFloat; exprIdent; exprString] |>> Value
    
let exprExpr =
    between ``( `` ``) `` expr 

let term = (exprAtom <|> exprExpr) .>> wsc
opp.TermParser <- term

addOperators()

let varDeclarations =
    ``var `` 
    >>. many1 (tuple2 ((sepEndBy1 identifier ``, ``) .>> ``: ``)
              (typeIdentifier .>> ``; ``)) 
    |>> Variables
    
let constDeclarations =
    ``const `` 
    >>. many1 (identifier .>>. (``= `` >>. expr .>> ``; `` |>> ConstExpr))
    |>> Consts

let labelDeclarations =
    ``label ``
    >>. sepBy1 identifier ``, `` .>> ``; ``
    |>> Labels

let expression =
    pint32

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
           !^(opt(``else `` >>. compoundStatement)) 
    |>> IfStm  

let caseLabel =
    sepBy1 ((attempt(constRangeExpression |>> CaseRange))
            <|>
            (expr |>> (ConstExpr >> CaseExpr))) ``, ``

let caseStatement =
    tuple3 (``case `` >>. expr .>> ``of ``)
           (sepEndBy(caseLabel
               .>>. (``: `` >>. !^(opt compoundStatement))) ``; ``)
           !^^(opt (``else `` >>. opt compoundStatement) .>> ``end ``)
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

let statement =
    many ``; ``
    >>.
    (opt(identifier .>>? (``: ``) |>> LabelStm) .>> many ``; ``) 
    .>>.?
    (choice[
            simpleStatement
            attempt(callStatement)
            attempt((identifier .>>? ``: ``) |>> LabelStm)
            designatorStatement
            ifStatement
            caseStatement
            forStatement
            repeatStatement
            whileStatement
            withStatement
        ] <?> "") 
    |>> function
        | (None, s) -> [s]
        | (Some l, s) -> [l ; s]

let statementList =
    (sepEndBy (statement <|> compoundStatement) (many1 ``; ``))
    //.>>. opt((identifier .>>? ``: ``) |>> LabelStm) 
    //|>> function
    //    | (s, None) -> s |> List.concat
    //    | (s, Some l) -> [[l]] |> List.append s |> List.concat
    
let declarations =
    many (choice[typeDeclarations; varDeclarations; constDeclarations; labelDeclarations]) 

let beginEnd = 
    between ``begin `` ``end `` 
            (statementList
             .>>. opt((identifier .>>? ``: ``) |>> LabelStm) 
             |>> function
                 | (s, None) -> s |> List.concat
                 | (s, Some l) -> [[l]] |> List.append s |> List.concat
             .>> many ``; ``
            )
    

let block = 
    fun(stream: CharStream<PasState>) ->
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
        reply
        

let stdCompoundStatement = beginEnd <|> statement

compoundStatementRef := stdCompoundStatement 

let program =
    (opt(``program `` >>. identifier .>> ``; ``))
    .>>.
    (block .>> pstring ".")        

let pascalModule =
    wsc >>. program .>> (skipManyTill skipAnyChar eof)
    
let pGrammar: Parser<_, PasState> =  // one type annotation is enough for the whole parser
    pascalModule