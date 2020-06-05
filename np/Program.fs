// Learn more about F# at http://fsharp.org

open System
open System.IO
open NP.CommandLineHandle
open Pas
open Argu
open NP

let tryCompileFile mainFile =
    let mainFileName = Path.GetFileName(mainFile: string)
    System.IO.File.ReadAllText(mainFile)
    |> PasStreams.testAll mainFileName
    |> function
       | Ok() -> printfn "Compilation success!"
       | Error e ->
           Seq.iter (printfn "%s") e
           printfn "[Fatal Error] Cannot compile module '%s'" mainFileName

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
        |> tryCompileFile
    | None ->
        // only proper for tests
        if not(results.Contains(Test)) then failwith "No proper command found"

    0