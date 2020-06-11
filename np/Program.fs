// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Collections.Generic
open Fake.Core
open NP.CommandLineHandle
open Pas
open Argu
open NP

let writeRuntimeConfig outName =
    File.WriteAllText(Path.GetFileNameWithoutExtension(outName: string) + ".runtimeconfig.json",
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


let handleTest exeName (testEnv: Dictionary<string, string list>) isError =
    let shortName = Path.GetFileName (exeName: string) |> Path.GetFileNameWithoutExtension
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
            let result =
                CreateProcess.fromRawCommand @"C:\_projects\newpascal\core32\dotnet.exe" ["exec"; exeName]
                |> CreateProcess.redirectOutput
                |> Proc.run
            File.WriteAllText(Path.ChangeExtension(exeName, ".elg"), result.Result.Output)
            if result.ExitCode = runResult then
                Ok()
            else
                Error <| sprintf "exit code = %d (expected %d)" result.ExitCode runResult
        | id -> id
    |>  function
        | Ok() -> printfn "OK\t%s" shortName
        | Error(msg) -> printfn "FAIL\t%s\t\t%s" shortName msg

let tryCompileFile doTest mainFile =
    let mainFileName = Path.GetFileName(mainFile: string)
    let handle =
        if doTest then Some(new StreamWriter(path=Path.ChangeExtension(mainFile,".log")))
        else None
    let mprintfn fmt = if doTest then fprintfn handle.Value fmt else printfn fmt
    try
        System.IO.File.ReadAllText(mainFile)
        |> PasStreams.doPas mainFileName doTest
        |> function
           | Ok(outName, msg, testEnv) ->
               Seq.iter (mprintfn "%s") msg.warnings
               mprintfn "Compilation success!"
               if doTest then handleTest outName testEnv false
           | Error(msg, testEnv) ->
               Seq.iter (mprintfn "%s") msg.warnings
               Seq.iter (mprintfn "%s") msg.errors
               mprintfn "[Fatal Error] Cannot compile module '%s'" mainFileName
               if doTest then handleTest mainFile testEnv true
    finally
        handle.Value.Close()

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
        | Some testDir ->
            for testFile in Directory.GetFiles(testDir, "*.pas") do
                tryCompileFile true testFile
        | _ -> failwith "No proper command found"
    0