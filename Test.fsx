[<AutoOpen>]
module Test

#I __SOURCE_DIRECTORY__
#r @"np/bin/Debug/netcoreapp3.0/FParsecCS.dll"
#r @"np/bin/Debug/netcoreapp3.0/FParsec.dll"
//#r @"np/bin/Debug/netcoreapp2.2/FParsec-Pipes.dll"

#load "np/PasAst.fs"
#load "np/PasVar.fs"
#load "np/BasicParsers.fs"
#load "np/PasOperators.fs"
#load "np/ParsePas.fs"

open System.Text
open System.Runtime
open np.PasVar
open np.PasAst
open np.BasicParsers
open np.ParsePas
open FParsec
open FParsec.Primitives
open FParsec.CharParsers

(*
    let (|Default|) onNone value =
    match value with
    | None -> onNone
    | Some e -> e
*)

(*let test p s =
    run p s

// let testAll s =
//     test pascalModule s

let testProcDecl s = 
    test procIntfDecl s

let testProcTypeDecl s =
    test typeDeclarations s

let testStmt s =
    test compoundStatement s

let testDesignator s =
    test designator s

let testIfThen s =
    test compoundStatement s

let testCaseStmt s =
    test caseStatement s

let testString s =
    test stringLiteral s

let testExpr s =
    test expr s

let testCD s =
    test constDeclarations s

let testFor s =
    test forStatement s

let testRS s =
    test repeatStatement s
 
let testWS s = 
    test whileStatement s

let testCall s =
    test callStatement s*)

//let testDirective s =
//    test ((str_wsc "if") -. (str_wsc "then")) s

let applyParser (parser: Parser<'Result,'UserState>) (stream: CharStream<'UserState>) =
    let reply = parser stream
    if reply.Status = Ok then
        Success(reply.Result, stream.UserState, stream.Position)
    else
        let error = ParserError(stream.Position, stream.UserState, reply.Error)
        Failure(error.ToString(stream), error, stream.UserState)

let testPas p s = 
    let us = PasState.Create(new PasStream(s))
    use stream1 = new CharStream<PasState>(us.stream, Encoding.Unicode)
    stream1.UserState <- us
    stream1.Name <- "some code"
    let result = applyParser pass1Parser stream1
    match result with
    | Success (_,s,_) -> s.stream.SaveToFile()
    | Failure (_,_,s) -> s.stream.SaveToFile()
    us.handleInclude := pass2IncludeHandler
    printfn ">>> SECOND PASS"
    use stream2 = new CharStream<PasState>(us.stream, Encoding.Unicode)
    stream2.UserState <- us
    stream2.Name <- "some code"
    applyParser p stream2

let testAll s =
    printfn ">%A" s
    testPas pascalModule s

// let testAllFromFile f =
//     let s = StringBuilder
//     testPas pascalModule s

(*
type Expr =
    | Int of int

type Ident =
    | Ident of string

type Stmt =
    | AssignStmt of Ident * Expr

type Block =
    | Block of Stmt list

type Program =
    | Program of name: string * block: Block 

let ws = spaces

(*let prog =
    pstring " " >>. anyString .>> pstring " " *)
    
// let floatBetweenBrackets = pfloat .>> pstring ","

// let floatList = many floatBetweenBrackets

// let backtrackingSepBy1 p sep = pipe2 p (many (sep >>? p)) (fun hd tl -> hd::tl)

// //let ap = sepBy1 (pstring "a") (pstring "b")
// let ap = backtrackingSepBy1 (pstring "a") (pstring "b")
// let bp = tuple2 ap (pstring "bc")
// let test_ab() = run bp "abababc"

// let test_i() = run tkIdentifier "Helllo_9"
// do printfn "%A" (test_i())
// let pGrammarTest: Parser<_, unit> =  // one type annotation is enough for the whole parser
//     floatList
// *)    
(*

    
let test =
    run floatList "1.0,2.0,"

let y = 9;
let test_ =
    run commentLine "//[] foo\n\r"

test

// testy

let processInput value input =
    let newValue = match input with
                   | "A" -> value + 1
                   | "M" -> value * 2
                   | _ -> value
    printf "%d\n" newValue
    newValue

let inputs = seq {
    while true do
        yield System.Console.ReadLine()
}

let startingState = 0
let finishingState =
    inputs
    |> Seq.map (fun input -> input.ToUpper())
    |> Seq.takeWhile (fun input -> not(input = "Q"))
    |> Seq.fold processInput startingState
    
type Union =
    | A of int
    | B of int
    
let foo() =
    let test = B 42
    match test with
    | A _ -> None
    | B a when a > 41 -> Some(a) // the guard applies to both patterns
    | _ -> Some(1)
    
*)