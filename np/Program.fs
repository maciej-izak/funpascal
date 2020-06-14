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


let handleTest proj (testEnv: TestEnvDict) isError =
    let failExpected = testEnv.ContainsKey "FAIL"
    // TODO handle conversion errors ?
    let runResult = match testEnv.TryGetValue "RESULT" with
                    | true, [v] ->
                        match Int32.TryParse v with
                        | true, v -> v
                        | _ -> 0
                    | _ -> 0
    match isError, failExpected with
    | false, false | true, true -> Ok()
    | true, false -> Error("compiler error")
    | false, true -> Error("compiler success")
    |> if failExpected then id
       else
        function
        | Ok() ->
            if proj.Exe.IsNone then raise (InternalError "2020061301")
            let result =
                CreateProcess.fromRawCommand @"C:\_projects\newpascal\core32\dotnet.exe" ["exec"; proj.Exe.Value]
                |> CreateProcess.redirectOutput
                |> Proc.run
            File.WriteAllText(proj.OutPath </> proj.Name + ".elg", result.Result.Output)
            if result.ExitCode = runResult then
                Ok()
            else
                Error <| sprintf "exit code = %d (expected %d)" result.ExitCode runResult
        | id -> id
    |>  function
        | Ok() -> printfn "OK\t%s" proj.Name
        | Error(msg) -> printfn "FAIL\t%s\t\t%s" proj.Name msg
    // for further tests (only first iteration has meaning)
    match testEnv.TryGetValue "DEFINES" with
    | true, defs -> defs
    | _ -> []


let doFullCompilation proj td logh =
    let handleTest(proj, testEnv, isError) =
       if proj.Test then
           handleTest proj testEnv isError
        else []
    System.IO.File.ReadAllText(proj.File)
    |> PasStreams.doPas proj td
    |> function
       | Ok(outName, msg, testEnv) ->
           Seq.iter (fprintfn logh "%s") msg.warnings
           fprintfn logh "Compilation success!"
           writeRuntimeConfig proj
           { proj with Exe = Some outName}, testEnv, false
       | Error(msg, testEnv) ->
           Seq.iter (fprintfn logh "%s") msg.warnings
           Seq.iter (fprintfn logh "%s") msg.errors
           fprintfn logh "[Fatal Error] Cannot compile module '%s'" proj.FileName
           proj, testEnv, true
    |> handleTest

let tryCompileFile doTest mainFile =
    let proj = PascalProject.Create(mainFile, doTest)
    let handle def =
        if doTest then
            let logSuffix = match def with | Some def -> "-" + def | _ -> ""
            let logFile = proj.OutPath </> sprintf "%s%s.log" proj.Name logSuffix
            new StreamWriter(path=logFile) :> TextWriter
        else stdout
    let doCompile ho = using (handle ho) (doFullCompilation proj ho)
    doCompile None |> List.iter (Some >> doCompile >> ignore) // first defines has meaning see handleTest

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
        | Some testDir -> !! (testDir </> "*.pas") |> Seq.iter (tryCompileFile true)
        | _ -> failwith "No proper command found"
    0