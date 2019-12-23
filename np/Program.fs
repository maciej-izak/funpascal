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
      var
        x: Integer;
      begin
        x := 8;
        if (x = 8) and (x mod 3 = 0) then
          WriteLn(x)
        else if x = 7 + 1 then begin
          WriteLnS('1');
          if x = 9 then
            WriteLnS('foo');
          WriteLnS('foo2');
        end else
          WriteLnS('O_o');
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
