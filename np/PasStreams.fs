module NP.PasStreams

open System.Collections.Generic
open System.Text
open System.IO
open Pas
open FParsec
open Microsoft.FSharp.Reflection

type CompilerPassResult =
    | InitialPassResult of ParserResult<unit, PasState>
    | MainPassResult of ParserResult<ModuleAst, PasState>

let applyParser (parser: Parser<'Result,'UserState>) (stream: CharStream<'UserState>) =
    let reply = parser stream
    if reply.Status = Ok then
        Success(reply.Result, stream.UserState, stream.Position)
    else
        let error = ParserError(stream.Position, stream.UserState, reply.Error)
        Failure(error.ToString(stream), error, stream.UserState)

let doPasStream s i fn doTests =
    let us = PasState.Create (InitialPass()) (new PasStream(s)) i doTests
    use stream1 = new CharStream<PasState>(us.stream, Encoding.Unicode)
    stream1.UserState <- us
    stream1.Name <- fn
    us.stream.AddInc "system.inc" @"C:\_projects\newpascal\np\npcli\test\xdpw"
    match applyParser initialPassParser stream1 with
    | Success _ -> // Do second pass
        let us = { us with pass = MainPass() }
        use stream2 = new CharStream<PasState>(us.stream, Encoding.Unicode)
        stream2.UserState <- us
        stream2.Name <- fn
        applyParser mainPassParser stream2 |> MainPassResult
    | res -> InitialPassResult res

let doPas fn doTests s =
    let strToStream (s: string) = s |> Encoding.Unicode.GetBytes |> fun s -> new MemoryStream(s)
    let ast = doPasStream (strToStream s) @"C:\_projects\newpascal\np\npcli\test\xdpw" fn doTests
    match ast with
    | MainPassResult(Success(pasModule,u,_)) ->
        match Ctx.BuildModule pasModule u with
        | Microsoft.FSharp.Core.Ok ad ->
            let outName = Path.ChangeExtension(fn, ".dll")
            ad.Write(outName)
            Microsoft.FSharp.Core.Ok(outName, u)
        | Microsoft.FSharp.Core.Error() ->
            Microsoft.FSharp.Core.Error u
    | MainPassResult(Failure(s,_,u)) | InitialPassResult(Failure(s,_,u)) ->
        u.messages.errors.Add s
        Microsoft.FSharp.Core.Error u
    | _ -> raise (InternalError "2020110601")

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