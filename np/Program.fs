// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Collections.Generic
open Fake.Core
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open NP.CommandLineHandle
open NP.PasStreams
open Pas
open Argu
open NP

let writeRuntimeConfig proj =
    File.WriteAllText(proj.OutPath </> proj.Name + ".runtimeconfig.json",
                                  """
{
  "runtimeOptions": {
    "tfm": "netcoreapp3.1",
    "framework": {
      "name": "Microsoft.NETCore.App",
      "version": "3.1.3"
    }
  }
}
                                  """)


let handleTest proj testCase isError =
    match isError, testCase.FailExpected with
    | false, false | true, true -> Ok()
    | true, false -> Error("compiler error")
    | false, true -> Error("compiler success")
    |> if testCase.FailExpected then id
       else
        function
        | Ok() ->
            if proj.Exe.IsNone then raise (InternalError "2020061301")
            let result =
                CreateProcess.fromRawCommand @"C:\_projects\newpascal\core32\dotnet.exe" ["exec"; proj.Exe.Value]
                |> CreateProcess.redirectOutput
                |> Proc.run
            File.WriteAllText(proj.OutPath </> proj.Name + testCase.DefSuffix + ".elg", result.Result.Output)
            if result.ExitCode = testCase.Result then
                Ok()
            else
                Error <| sprintf "exit code = %d (expected %d)" result.ExitCode testCase.Result
        | id -> id
    |>  function
        | Ok() -> printfn "OK\t%s%s" proj.Name testCase.DefSuffix
        | Error(msg) -> printfn "FAIL\t%s%s\t\t%s" proj.Name testCase.DefSuffix msg

let doFullCompilation proj (testCase: TestCase option) logh =
    let handleTest(proj, isError) =
        if testCase.IsSome then
            handleTest proj testCase.Value isError
    System.IO.File.ReadAllText(proj.File)
    |> PasStreams.doPas proj
    |> function
       | Ok(outName, msg) ->
           Seq.iter (fprintfn logh "%s") msg.Warnings
           fprintfn logh "Compilation success!"
           writeRuntimeConfig proj
           { proj with Exe = Some outName}, false
       | Error(msg) ->
           Seq.iter (fprintfn logh "%s") msg.Warnings
           Seq.iter (fprintfn logh "%s") msg.Errors
           fprintfn logh "[Fatal Error] Cannot compile module '%s'" proj.FileName
           proj, true
    |> handleTest

let tryCompileFile doTest mainFile =
    let proj = PascalProject.Create(mainFile, doTest)
    let handle defSuffix =
        if doTest then
            let logFile = proj.OutPath </> sprintf "%s%s.log" proj.Name defSuffix
            new StreamWriter(path=logFile) :> TextWriter
        else stdout
    if doTest then
        let doCompile testCase =
            let newDefs = if testCase.Define.IsSome then testCase.Define.Value::proj.Defines else proj.Defines
            let proj = { proj with Defines = newDefs}
            using (handle testCase.DefSuffix) (doFullCompilation proj (Some testCase))
        match prepareTest proj with
        | Ok cases -> cases |> List.iter (doCompile >> ignore) // first defines has meaning see handleTest
        | _ -> printfn "TEST ERROR : %s" proj.Name
    else doFullCompilation proj None stdout

[<EntryPoint>]
let main argv =
    // command line arguments
    let results =
        let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
        ArgumentParser
            .Create<CLIArguments>(programName="NewPascal", errorHandler=errorHandler)
            .ParseCommandLine(inputs = argv, raiseOnUsage = true)

    match results.TryGetResult(Files) with
    | Some pasFiles ->
        match pasFiles with
        | [f] -> f
        | _ -> failwith "Multiply files not supported"
        |> tryCompileFile false
    | None ->
        // only proper for tests
        match results.TryGetResult(Test) with
        | Some testDir -> !! (testDir </> "*.pas") ++ (testDir </> "*.pp") |> Seq.iter (tryCompileFile true)
        | _ -> failwith "No proper command found"
    0