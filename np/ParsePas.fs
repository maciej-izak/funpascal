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


let keywords = ["while"; "for"; "to"; "downto"; "repeat"; "until"; "begin"; "end"; "do"; "if"; "then"; "else"; "type"; "var"; "const"; "procedure"; "function"; "array"; "string"; "file";
    "xor"; "or"; "and"; "div"; "mod"; "shl"; "shr"; "as"; "in"; "is"; "nil"; "program"]

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

let has_str s = str_wsc s >>. preturn true <|>% false

let constRangeExpression =
    (expr .>> wsc)
    .>>. (str_wsc ".." >>. expr  .>> wsc) 
    |>> (castAs ConstExpr >> ConstExprRange)
 
let constType =
    identifier |>> DimensionType

let arrayIndex =
    choice[constType;constRangeExpression |>> DimensionExpr ]

let arrayIndexes =
    str_wsc "[" >>. (sepEndBy1 arrayIndex (str_wsc ",")) .>> str_wsc "]"

let designator = 
    pipe2   (identifier |>> Designator.Ident) 
            (many(choice[ 
                        str_wsc "^" >>% Deref; 
                        (str_wsc "[" >>. (sepEndBy1 expr (str_wsc ",")) .>> str_wsc "]")
                        |>> Designator.Array;
                        str_wsc "." >>. identifier |>> Designator.Ident
                      ]))
            (fun h t -> h::t |> DIdent)

let typeIdentifier =
    choice[
            designator |>> Ident;
            str_wsc "string" >>% String;
            str_wsc "file" >>% File  
          ]    

let field =
    identifier .>>. (str_wsc ":" >>. identifier)
    
let fieldListDecl p =
    p identifier (str_wsc ",") .>>. (str_wsc ":" >>. typeIdentifier)

let fieldsList  =
    fieldListDecl sepBy 

let fieldsList1 =
    fieldListDecl sepBy1
    
let inline str_opt s v = 
        opt (str_wsc s >>% Some v )

let recordDef =
      has_str "packed" // str_opt "packed"
      .>>. (between
              (str_wsc "record")
              (str_wsc "end")
              (sepEndBy fieldsList (str_wsc ";")))
      
let structType =
    recordDef |>> Record
     
let arrayType =
    (str_wsc "array" >>. arrayIndexes)
    .>>. (str_wsc "of" >>. identifier)
    |>> Array

let typeAlias =
    has_str "type" .>>. typeIdentifier 
    |>> TypeAlias

let formalParam  = 
        opt ((str_wsc "const" >>. preturn ParamKind.Const) <|> (str_wsc "var" >>. preturn Var))
        .>>. fieldsList1
        
let formalParamsList =
    between (str_wsc "(") (str_wsc ")") (sepEndBy formalParam (str_wsc ";"))  

let procIntfDecl = 
    (((str_wsc "procedure" >>. preturn Procedure) 
    <|> (str_wsc "function" >>. preturn Function)) .>>. (opt formalParamsList))
    >>= fun (k, p) -> match k with
                      | Function -> (str_wsc ":" >>. designator) |>> fun i -> (Some(i), p)
                      | Procedure -> preturn(None, p)
        
let typeProc =
    procIntfDecl |>> ProcType
    
let typePtr =
    str_wsc "^" >>. typeIdentifier |>> TypePtr

let typeRange =
    expr .>>. (str_wsc ".." >>. expr) |>> ((castAs ConstExpr) >> SimpleRange)

let typeDeclarations =
    (str_wsc "type" >>.
        many1 (
            (identifier .>> str_wsc "=")
            .>>. ((choice[structType;arrayType;typePtr;typeAlias;typeProc;typeRange]).>> str_wsc ";")
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
        between (str_wsc "(") (str_wsc ")") (sepEndBy actualParam (str_wsc ","))
    designator .>>. actualParamsList 
    |>> CallExpr

let exprCall = 
    callExpr |>> CallResult

let exprAtom =
    choice[attempt(exprCall); exprInt; exprFloat; exprIdent; exprString] |>> Value
    
let exprExpr =
    between (str_wsc "(") (str_wsc ")") expr 

let term = (exprAtom <|> exprExpr) .>> wsc
opp.TermParser <- term

addOperators()

let varDeclarations =
    str_wsc "var" 
    >>. many1 (tuple2 ((sepEndBy1 identifier (str_wsc ",")) .>> str_wsc ":")
              (typeIdentifier .>> str_wsc ";")) 
    |>> Variables
    
let constDeclarations =
    (str_wsc "const" >>.
        many1 ( 
            identifier .>>. (str_wsc "=" >>. expr .>> str_wsc ";" |>> ConstExpr)))
            |>> Const

let expression =
    pint32

let simpleStatement =
    designator .>>.? (str_wsc ":=" >>. expr) |>> AssignStm

let callStatement =
    callExpr |>> CallStm

let designatorStatement =
    designator |>> IdentStm

let compoundStatement, compoundStatementRef = createParserForwardedToRef()

let ifStatement =
    tuple3 (str_wsc "if" >>. expr .>> str_wsc "then")
           (compoundStatement)
           !^(opt(str_wsc "else" >>. compoundStatement)) 
    |>> IfStm  

let caseLabel =
    sepBy1 ((attempt(constRangeExpression |>> CaseRange))
            <|>
            (expr |>> (ConstExpr >> CaseExpr))) (str_wsc ",")

let caseStatement =
    tuple3 (str_wsc "case" >>. expr .>> str_wsc "of")
           (sepEndBy(caseLabel
               .>>. (str_wsc ":" >>. !^(opt compoundStatement))) (str_wsc ";"))
           !^^(opt (str_wsc "else" >>. opt compoundStatement) .>> str_wsc "end")
           |>> CaseStm

let forStatement =
    tuple5 
        (str_wsc "for" >>. designator)
        (str_wsc ":=" >>. expr)
        ((str_wsc "to" >>% 1) <|> (str_wsc "downto" >>% -1))
        expr
        (str_wsc "do" >>. !^(opt compoundStatement))
    |>> ForStm

let repeatStatement =
    (between 
        (str_wsc "repeat") 
        (str_wsc "until") 
        (sepEndBy compoundStatement (str_wsc ";")) |>> List.concat)
    .>>. expr
    |>> RepeatStm  

let whileStatement =
    (str_wsc "while" >>. expr .>> str_wsc "do")
    .>>. !^(opt compoundStatement)
    |>> WhileStm

let statement =
    choice[
            simpleStatement
            attempt(callStatement)
            designatorStatement
            ifStatement
            caseStatement
            forStatement
            repeatStatement
            whileStatement
    ] <?> ""

let statementList =
    (sepEndBy ((statement |>> toList) <|> (compoundStatement)) (str_wsc ";")) 
    |>> List.concat
    
let declarations =
    many (choice[typeDeclarations; varDeclarations; constDeclarations]) 

let block =
    opt declarations .>>. between (str_wsc "begin") (str_wsc "end") statementList

compoundStatementRef := 
    between
        (str_wsc "begin")
        (str_wsc "end")
        statementList <|> (statement |>> toList)

let program =
    (opt(str_wsc "program" >>. identifier .>> str_wsc ";"))
    .>>.
    (block .>> pstring ".")        

let pascalModule =
    wsc >>. program .>> (skipManyTill skipAnyChar eof)
    
let pGrammar: Parser<_, PasState> =  // one type annotation is enough for the whole parser
    pascalModule