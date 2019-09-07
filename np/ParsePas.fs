module np.ParsePas

open FParsec
open FParsec
open FParsec.Primitives
open FParsec.CharParsers
//open FParsec.Pipes
//open FParsec.Pipes.Precedence
open PasAst
open System.Collections.Generic
open np

let inline (!^) (a: 'a list option) = if a.IsSome then a.Value else []
let inline (!^^) (a: 'a list option option) = if a.IsSome then (if a.Value.IsSome then a.Value.Value else []) else []
let inline cast f (a, b) = (f a, f b)

let ws = spaces

let commentBlock =
    skipChar '{' .>> skipManyTill skipAnyChar (skipChar '}')
        
let starCommentBlock =
    skipString "(*" .>> skipManyTill skipAnyChar (skipString "*)")
    
    
//    parse {
//        stringReturn
//    } (skipString "{") (skipManyTill (pchar "}")) (fun _ _ -> () )*)

let wsc = skipMany (choice[spaces1; commentBlock; starCommentBlock] <?> "")

let str_wsc s =
    pstring s .>> wsc

let keywords = ["while"; "for"; "to"; "downto"; "repeat"; "until"; "begin"; "end"; "do"; "if"; "then"; "else"; "type"; "var"; "const"; "procedure"; "function"; "array"; "string"; "file";
    "xor"; "or"; "and"; "div"; "mod"; "shl"; "shr"; "as"; "in"; "is"; "nil"]

let opp = new OperatorPrecedenceParser<_,_,_>()
let expr = opp.ExpressionParser

let keywordsSet = new HashSet<string>(keywords);

let isKeyword s = keywordsSet.Contains s

let numeralOrDecimal : Parser<_, unit> =
    // note: doesn't parse a float exponent suffix
    numberLiteral NumberLiteralOptions.AllowFraction "number" 
    |>> fun num -> 
            // raises an exception on overflow
            if num.IsInteger then Integer(int num.String)
            else Float(float num.String)

let hexNumber =    
    pstring "$" >>. many1SatisfyL isHex "hex digit"
    |>> fun hexStr -> 
            // raises an exception on overflow
            Integer(System.Convert.ToInt32(hexStr, 16)) 

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
    let normalCharSnippet = manySatisfy (fun c -> c <> ''')
    let escapedChar = pstring "'" >>? (pstring "'" >>% "'") 
                                    //    <|> ())
    many1Strings ((between (pstring "'") (pstring "'") (stringsSepBy normalCharSnippet escapedChar))
                 <|> ((pstring "#" >>. number) >>= function
                                                   | Integer i -> preturn (string (char i))
                                                   | _ -> fail "integer expected"))

let tkIdentifier =
    let isProperFirstChar c = isLetter c || c = '_'
    let isProperChar c = isLetter c || c = '_' || isDigit c
    many1Satisfy2L isProperFirstChar isProperChar "ident"
    .>> wsc
    
let identifier : Parser<string, unit> =
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
    pipe2 (expr .>> wsc)
          (str_wsc ".." >>. expr  .>> wsc) 
          (fun l h -> ConstExprRange(ConstExpr(l), ConstExpr(h)))
 
let constType =
    identifier |>> fun s -> DimensionType(s)

let arrayIndex =
    choice[constType;constRangeExpression |>> fun e -> DimensionExpr(e) ]

let arrayIndexes =
    str_wsc "[" >>. (sepEndBy1 arrayIndex (str_wsc ",")) .>> str_wsc "]"

let designator = 
    identifier 
    .>>.(many(choice[ 
                str_wsc "^" >>% Deref; 
                (str_wsc "[" >>. (sepEndBy1 expr (str_wsc ",")) .>> str_wsc "]")
                |>> fun l -> Designator.Array(l);
                str_wsc "." >>. identifier |>> Designator.Ident
              ]))
    |>> ((fun t -> Designator.Ident(fst t)::(snd t)) >> DIdent)

let typeIdentifier =
    choice[
            designator |>> (Ident);
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
    //opt (str_wsc "type" >>% true) .>>. identifier |>> fun (s, n) -> TypeAlias({strong=s.IsSome; origin=n})
    has_str "type" .>>. typeIdentifier |>> fun (t, n) -> TypeAlias({strong=t; origin=n})

let formalParam  = 
        opt ((str_wsc "const" >>. preturn ParamKind.Const) <|> (str_wsc "var" >>. preturn ParamKind.Var))
        .>>. fieldsList1
        //  IdentList [":" ["array" "of"] TypeIdent] .
        
let formalParamsList =
    between (str_wsc "(") (str_wsc ")") (sepEndBy formalParam (str_wsc ";"))  


let procIntfDecl = 
    (((str_wsc "procedure" >>. preturn Procedure) 
    <|> (str_wsc "function" >>. preturn Function)) .>>. (opt formalParamsList))
    >>= fun r -> match fst r with
                 | Function -> (str_wsc ":" >>. designator) |>> fun i -> (Some(i), snd r)
                 | Procedure -> preturn(None, snd r)
        
let typeProc =
    procIntfDecl |>> ProcType
    
let typePtr =
    str_wsc "^" >>. typeIdentifier |>> TypePtr

let typeRange =
    expr .>>. (str_wsc ".." >>. expr) |>> ((cast ConstExpr) >> SimpleRange)

let typeDeclarations =
    (str_wsc "type" >>.
        many1 (
            (identifier .>> str_wsc "=")
            .>>. ((choice[structType;arrayType;typePtr;typeAlias;typeProc;typeRange]).>> str_wsc ";")
            |>> Type)) |>> Types
 
let exprInt =
    pint32 |>> Integer
    
let exprFloat =
    pfloat |>> Float

let exprString = 
    stringLiteral |>> Value.String
    
let exprIdent =
    designator |>> Value.Ident

    //|>> List.reduce (+)
    
let exprAtom =
    choice[exprInt; exprFloat; exprIdent; exprString] |>> Value
    
let exprExpr =
    between (str_wsc "(") (str_wsc ")") expr 

let term = (exprAtom <|> exprExpr) .>> wsc
opp.TermParser <- term

opp.AddOperator(InfixOperator("=", wsc, 1, Associativity.Left, fun x y -> Equal(x, y)))
opp.AddOperator(InfixOperator("<>", wsc, 1, Associativity.Left, fun x y -> NotEqual(x, y)))
opp.AddOperator(InfixOperator("<", wsc, 1, Associativity.Left, fun x y -> StrictlyLessThan(x, y)))
opp.AddOperator(InfixOperator(">", wsc, 1, Associativity.Left, fun x y -> StrictlyGreaterThan(x, y)))
opp.AddOperator(InfixOperator("<=", wsc, 1, Associativity.Left, fun x y -> LessThanOrEqual(x, y)))
opp.AddOperator(InfixOperator(">=", wsc, 1, Associativity.Left, fun x y -> GreaterThanOrEqual(x, y)))
opp.AddOperator(InfixOperator("in", wsc, 1, Associativity.Left, fun x y -> In(x, y)))
opp.AddOperator(InfixOperator("is", wsc, 1, Associativity.Left, fun x y -> Is(x, y)))
opp.AddOperator(InfixOperator("+", wsc, 2, Associativity.Left, fun x y -> Add(x, y)))
opp.AddOperator(InfixOperator("-", wsc, 2, Associativity.Left, fun x y -> Minus(x, y)))
opp.AddOperator(InfixOperator("or", wsc, 2, Associativity.Left, fun x y -> Or(x, y)))
opp.AddOperator(InfixOperator("xor", wsc, 2, Associativity.Left, fun x y -> Xor(x, y)))
opp.AddOperator(InfixOperator("*", wsc, 3, Associativity.Left, fun x y -> Multiply(x, y)))
opp.AddOperator(InfixOperator("/", wsc, 3, Associativity.Left, fun x y -> Divide(x, y)))
opp.AddOperator(InfixOperator("div", wsc, 3, Associativity.Left, fun x y -> Div(x, y)))
opp.AddOperator(InfixOperator("mod", wsc, 3, Associativity.Left, fun x y -> Mod(x, y)))
opp.AddOperator(InfixOperator("and", wsc, 3, Associativity.Left, fun x y -> And(x, y)))
opp.AddOperator(InfixOperator("shl", wsc, 3, Associativity.Left, fun x y -> Shl(x, y)))
opp.AddOperator(InfixOperator("shr", wsc, 3, Associativity.Left, fun x y -> Shr(x, y)))
opp.AddOperator(InfixOperator("as", wsc, 3, Associativity.Left, fun x y -> As(x, y)))
opp.AddOperator(PrefixOperator("@", wsc, 4, true, fun x -> Addr(x)))
opp.AddOperator(PrefixOperator("not", wsc, 4, true, fun x -> Not(x)))
opp.AddOperator(PrefixOperator("-", wsc, 4, true, fun x -> UnaryMinus(x)))
opp.AddOperator(PrefixOperator("+", wsc, 4, true, fun x -> UnaryAdd(x)))

let varDeclarations =
    (str_wsc "var" >>.
        many1 (pipe2 ((sepEndBy1 identifier (str_wsc ",")) .>> str_wsc ":")
            (typeIdentifier .>> str_wsc ";")
            (fun n t -> {names = n; vtype = t}))) |>> fun v -> Variables(v)
    
let constDeclarations =
    (str_wsc "const" >>.
        many1 ( 
            (identifier .>>. (str_wsc "=" >>. expr .>> str_wsc ";") |>> fun t -> {name = fst t; value = ConstExpr(snd t)} )))
            |>> fun c -> Const(c)

let expression =
    pint32

let simpleStatement =
    designator .>>. (str_wsc ":=" >>. expr) |>> AssignStm

let compoundStatement, compoundStatementRef = createParserForwardedToRef()

let ifStatement =
      pipe3 (str_wsc "if" >>. expr .>> str_wsc "then")
            (compoundStatement)
            (opt (str_wsc "else" >>. compoundStatement))
            (fun e a b -> IfStm(e, a, !^b))

let caseLabel =
    sepBy1 ((attempt(constRangeExpression |>> CaseRange))
            <|>
            (expr |>> (ConstExpr >> CaseExpr))) (str_wsc ",")

let caseStatement =
      pipe3 (str_wsc "case" >>. expr .>> str_wsc "of")
            (sepEndBy(caseLabel
                .>>. (str_wsc ":" >>. (opt compoundStatement |>> (!^)))) (str_wsc ";"))
            (opt (str_wsc "else" >>. opt compoundStatement) .>> str_wsc "end")
            (fun ex c el -> CaseStm(ex, c, !^^el))

let forStatement =
    pipe4 
        (str_wsc "for" >>. designator)
        (str_wsc ":=" >>. expr)
        (((str_wsc "to" >>% 1) <|> (str_wsc "downto" >>% -1)) .>>. expr)
        (str_wsc "do" >>. opt compoundStatement)
        (fun i s f b -> ForStm(i, s, fst f, snd f, !^b)) 

let repeatStatement =
    (between (str_wsc "repeat") (str_wsc "until") (sepEndBy compoundStatement (str_wsc ";")))
    .>>. expr
    |>> fun (b, e) -> RepeatStm(List.concat b, e)  

let whileStatement =
    (str_wsc "while" >>. expr .>> str_wsc "do")
    .>>. (opt compoundStatement)
    |>> fun (e, b) -> WhileStm(e, !^b)  

let statement =
    choice[
            simpleStatement;
            ifStatement;
            caseStatement;
            forStatement;
            repeatStatement;
            whileStatement
          ]

let statementList =
    (sepEndBy ((statement |>> fun x -> [x]) <|> (compoundStatement)) (str_wsc ";")) 
    |>> List.concat
    
let declarations =
    many (choice[typeDeclarations; varDeclarations; constDeclarations]) 

let block =
    opt declarations .>>. compoundStatement

compoundStatementRef := 
    between
        (str_wsc "begin")
        (str_wsc "end")
        statementList <|> (statement |>> (fun s -> [s]))

let program =
    (opt(str_wsc "program" >>. identifier .>> str_wsc ";"))
    .>>.
    (block .>> str_wsc ".")        

let pascalModule =
    wsc >>. program .>> (skipManyTill skipAnyChar eof)
    
let pGrammar: Parser<_, unit> =  // one type annotation is enough for the whole parser
    pascalModule