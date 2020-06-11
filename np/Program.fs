// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Collections.Generic
open Fake.Core
open NP.CommandLineHandle
open Pas
open Argu
open NP

let tryCompileFile doTest mainFile =
    let testResult (testEnv: Dictionary<string, string list>) isError =
        let ok() = printfn "TEST %s OK" mainFile
        let fail() = printfn "FAIL: %s" mainFile
        if doTest then
           match isError, testEnv.ContainsKey "FAIL" with
           | false, false | true, true -> ok()
           | true, false | false, true -> fail()

    let mainFileName = Path.GetFileName(mainFile: string)
    System.IO.File.ReadAllText(mainFile)
    |> PasStreams.doPas mainFileName doTest
    |> function
       | Ok(outName, msg, testEnv) ->
           Seq.iter (printfn "%s") msg.warnings
           printfn "Compilation success!"
           File.WriteAllText(Path.GetFileNameWithoutExtension(outName) + ".runtimeconfig.json",
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
           testResult testEnv false
           Some outName
       | Error(msg, testEnv) ->
           Seq.iter (printfn "%s") msg.warnings
           Seq.iter (printfn "%s") msg.errors
           printfn "[Fatal Error] Cannot compile module '%s'" mainFileName
           testResult testEnv true
           None

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
        |> ignore
    | None ->
        // only proper for tests
        match results.TryGetResult(Test) with
        | Some testDir ->
            for testFile in Directory.GetFiles(testDir, "*.pas") do
                match tryCompileFile true testFile with
                | Some binFile ->
                    let result =
                        CreateProcess.fromRawCommand @"C:\_projects\newpascal\core32\dotnet.exe" ["exec"; binFile]
                        |> Proc.run
                    if result.ExitCode = 0 then
                        printfn "TEST RUN OK (%s)" binFile
                    else
                        printfn "FAIL RUN = %d (%s)" result.ExitCode binFile
                    ()
                | None -> ()
        | _ -> failwith "No proper command found"
    0