// Learn more about F# at http://fsharp.org

open System
open np.CommandLineHandle
open np.ParsePas
open Argu
open FParsec

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<NpArguments>(programName = "ls", errorHandler = errorHandler)
    let results = parser.ParseCommandLine argv
    let files = results.GetResult(Files, [])
    for f in files do
        printfn "%A" f
    run commentLine "// foo"
    0 // return an integer exit code
