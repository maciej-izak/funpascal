module NP.PasStreams

open System.Collections.Generic
open System.Text
open System.IO
open Pas
open FParsec
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Core

type PascalProject = {
    File: string
    FileName: string
    FilePath: string
    Exe: string option
    Name: string
    Test: bool
} with
    static member Create(mainFile, test) =
        {
            File = mainFile
            FileName = Path.GetFileName mainFile
            FilePath = Path.GetDirectoryName mainFile
            Exe = None
            Name = Path.GetFileNameWithoutExtension mainFile
            Test = test
        }

let applyParser (parser: Parser<'Result,'UserState>) (stream: CharStream<'UserState>) =
    let reply = parser stream
    if reply.Status = FParsec.Primitives.Ok then
        Success(reply.Result, stream.UserState, stream.Position)
    else
        let error = ParserError(stream.Position, stream.UserState, reply.Error)
        FParsec.CharParsers.Failure(error.ToString(stream), error, stream.UserState)

let doPasStream s i fn =
    let addParserError (us: PasState) parserError =
        us.messages.errors.Add parserError
        Error us
    let us = PasState.Create (InitialPass()) (new PasStream(s)) i
    use stream1 = new CharStream<PasState>(us.stream, Encoding.Unicode)
    stream1.UserState <- us
    stream1.Name <- fn
    us.stream.AddInc "system.inc" @"C:\_projects\newpascal\np\npcli\test\xdpw"
    match applyParser initialPassParser stream1 with
    | Success _ when not us.messages.HasError -> // Do second pass, parsing success may means failure in AST
        let us = { us with pass = MainPass() }
        use stream2 = new CharStream<PasState>(us.stream, Encoding.Unicode)
        stream2.UserState <- us
        stream2.Name <- fn
        match applyParser mainPassParser stream2 with
        | Success(ast, _, _) when not us.messages.HasError -> Ok(ast, us)
        | FParsec.CharParsers.Failure(s, _, _) -> addParserError us s
        | _ -> Error us
    | FParsec.CharParsers.Failure(s, _, _) -> addParserError us s
    | _ -> Error us

let doPas proj s =
    let strToStream (s: string) = s |> Encoding.Unicode.GetBytes |> fun s -> new MemoryStream(s)

    let result = doPasStream (strToStream s) @"C:\_projects\newpascal\np\npcli\test\xdpw" proj.FileName
    match result with
    | Ok(ast, us) ->
        match Ctx.BuildModule ast us with
        | Ok asmDef ->
            let outName = Path.ChangeExtension(proj.File, ".dll")
            asmDef.Write(outName)
            Ok(outName, us.messages, us.testEnv)
        | Error() -> Error(us.messages, us.testEnv)
    | Error us -> Error(us.messages, us.testEnv)

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