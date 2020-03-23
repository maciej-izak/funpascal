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
        TSubA = array[0..0] of integer;
        TFoo = packed record
          x: byte;
          s: TSubA;
          y: integer;
        end;
        TA = array[0..10] of TFoo;
      var
        a: TA;
        i: integer;
      begin
        //a[0].x := 1;
        //a[1].s[0] := 2;
        a[10].s[0] := 69;
        for i := 0 to 10 do
          WriteLn(a[i].s[0]);
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
