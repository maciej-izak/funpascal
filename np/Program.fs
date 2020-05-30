// Learn more about F# at http://fsharp.org

open System
open System.IO
open NP.CommandLineHandle
open NP.PasParser
open Argu
open FParsec
open NP

[<EntryPoint>]
let main argv =
    // command line arguments
    let results =
        let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
        ArgumentParser
            .Create<CLIArguments>(programName="NewPascal", errorHandler=errorHandler)
            .ParseCommandLine(inputs = argv, raiseOnUsage = true)

    let pasFiles = results.GetResult(Files)
    let mainFile =
        match pasFiles with
        | [f] -> f
        | _ -> failwith "Multiply files not supported"

    System.IO.File.ReadAllText(mainFile)
    |> PasStreams.testAll (Path.GetFileName(mainFile))
    |> function
       | Success _ -> printfn "Compilation success!"
       | Failure(s,_,_) -> printfn "%s" s
    0