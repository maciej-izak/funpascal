// Learn more about F# at http://fsharp.org

open System
open NP.CommandLineHandle
open NP.ParsePas
open Argu
open FParsec
open NP

[<EntryPoint>]
let main argv =
    PasStreams.testAll
      """program wow;
      type
        TA = array[0..10] of byte;
        TB = array[0..10, 0..5] of integer;
        //TC = array[0..10] of array[0..5] of integer;
        //TD = array[0..10, 0..5, 0..0, -1..2] of integer;
        //TE = array[0..10, 0..2] of array[0..5, 1..2] of integer;
      var
        a: TA;
        b: TB;
      begin
        a[0] := 1;
        b[10,4] := 1;
        b[10][5] := 2;
        a[1] := 2;
        //WriteLn(a[0]);
        //WriteLn(a[1]);
        //WriteLn(a[2]);
      end.
      """
    |> printfn "%A"
    0
    (*let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<NpArguments>(programName = "ls", errorHandler = errorHandler)
    let results = parser.ParseCommandLine argv
    let files = results.GetResult(Files, [])
    for f in files do
        printfn "%A" f
    //run commentLine "// foo"
    0 // return an integer exit code*)
