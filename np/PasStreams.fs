module NP.PasStreams

open System.Collections.Generic
open System.Text
open System.IO
open Pas
open FParsec
open Microsoft.FSharp.Reflection

let applyParser (parser: Parser<'Result,'UserState>) (stream: CharStream<'UserState>) =
    let reply = parser stream
    if reply.Status = Ok then
        Success(reply.Result, stream.UserState, stream.Position)
    else
        let error = ParserError(stream.Position, stream.UserState, reply.Error)
        Failure(error.ToString(stream), error, stream.UserState)

let testPas p s i fn doTests =
    let us = PasState.Create (new PasStream(s)) i doTests
    use stream1 = new CharStream<PasState>(us.stream, Encoding.Unicode)
    stream1.UserState <- us
    stream1.Name <- fn
    us.stream.AddInc "system.inc" @"C:\_projects\newpascal\np\npcli\test\xdpw"
    let result = applyParser pass1Parser stream1
//    match result with
//    | Success (_,s,_) -> s.stream.SaveToFile()
//    | Failure (_,_,s) -> s.stream.SaveToFile()
    let us =
        { us with
            pass = 2
            handleInclude = redirectParserTo
            handleMacro = pass2MacroHandler
        }
    printfn ">>> SECOND PASS"
    use stream2 = new CharStream<PasState>(us.stream, Encoding.Unicode)
    stream2.UserState <- us
    stream2.Name <- fn
    applyParser p stream2

let testAll fn doTests s =
    let strToStream (s: string) = s |> Encoding.Unicode.GetBytes |> fun s -> new MemoryStream(s)
    let ast = testPas pascalModule (strToStream s) @"C:\_projects\newpascal\np\npcli\test\xdpw" fn doTests
    match ast with
    | Success (r,u,_) ->
        match Ctx.BuildModule(ProgramAst(fst r, Block.Create(snd r))) u with
        | Microsoft.FSharp.Core.Ok ad ->
            let outName = Path.ChangeExtension(fn, ".dll")
            ad.Write(outName)
            Microsoft.FSharp.Core.Ok(outName, u.warnings)
        | Microsoft.FSharp.Core.Error e ->
            Microsoft.FSharp.Core.Error(u.warnings, e)
    | Failure(s,_,u) -> Microsoft.FSharp.Core.Error(u.warnings, [s] |> List<_>)

let toString (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

//let testFile f =
//    let s = File.ReadAllText(f)
//    let strToStream (s: string) = s |> Encoding.Unicode.GetBytes |> fun s -> new MemoryStream(s)
//    let dir = Path.GetDirectoryName f
//    let result = testPas pascalModule (strToStream s) dir
//    let xmlSerializer = FsPickler.CreateXmlSerializer(indent = true)
//    use sw = new StreamWriter(path=dir + string Path.DirectorySeparatorChar + "out.ast")
//    match result with
//    | Success (r,_,_) -> xmlSerializer.Serialize(sw, r)
//    | _ -> ()
//    sw.Close()
//    ()

    // use sw = new StreamWriter(path=dir + string Path.DirectorySeparatorChar + "out.ast")
    // let b = StringBuilder()
    // bprintf b "%O" result
    // sw.Close()