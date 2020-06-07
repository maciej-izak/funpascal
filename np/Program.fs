// Learn more about F# at http://fsharp.org

open System
open System.IO
open Fake.Core
open NP.CommandLineHandle
open Pas
open Argu
open NP

let tryCompileFile doTest mainFile =
    let mainFileName = Path.GetFileName(mainFile: string)
    System.IO.File.ReadAllText(mainFile)
    |> PasStreams.testAll mainFileName doTest
    |> function
       | Ok(outName, w) ->
           Seq.iter (printfn "%s") w
           printfn "Compilation success!"
           Some outName
       | Error (w, e) ->
           Seq.iter (printfn "%s") w
           Seq.iter (printfn "%s") e
           printfn "[Fatal Error] Cannot compile module '%s'" mainFileName
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
        | Some testFile ->
            match tryCompileFile true testFile with
            | Some binFile ->
                File.WriteAllText(Path.GetFileNameWithoutExtension(binFile) + ".runtimeconfig.json",
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
                let result =
                    CreateProcess.fromRawCommand @"C:\_projects\newpascal\core32\dotnet.exe" ["exec"; binFile]
                    |> Proc.run
                printfn "test result = %A" result.ExitCode
                ()
            | None -> ()
        | _ -> failwith "No proper command found"

    0