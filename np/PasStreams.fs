module np.PasStreams

open System.Text
open System.IO
open System.Runtime
open System.Text.Json
open System.Text.Json.Serialization
open np.PasVar
open np.PasAst
open np.BasicParsers
open np.ParsePas
open FParsec
open FParsec.Primitives
open FParsec.CharParsers

let applyParser (parser: Parser<'Result,'UserState>) (stream: CharStream<'UserState>) =
    let reply = parser stream
    if reply.Status = Ok then
        Success(reply.Result, stream.UserState, stream.Position)
    else
        let error = ParserError(stream.Position, stream.UserState, reply.Error)
        Failure(error.ToString(stream), error, stream.UserState)

let testPas p s i = 
    let us = PasState.Create (new PasStream(s)) i
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
    let strToStream (s: string) = s |> Encoding.Unicode.GetBytes |> fun s -> new MemoryStream(s)
    testPas pascalModule (strToStream s) ""

let testFile f =
    let s = File.ReadAllText(f)
    let strToStream (s: string) = s |> Encoding.Unicode.GetBytes |> fun s -> new MemoryStream(s)
    let result = testPas pascalModule (strToStream s) (Path.GetDirectoryName f)
    File.WriteAllText(Path.GetDirectoryName f + string Path.DirectorySeparatorChar + "out.ast", (sprintf "%A" result))