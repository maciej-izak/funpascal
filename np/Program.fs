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
        TFoo = (f1, f2, f3);
      var i: Integer;
      begin
        for i := 0 to 3 do
          WriteLn(i);
        for i := 3 downto 0 do
          WriteLn(i);
        while i < 9 do begin
          case f2 of
            f2..f3: WriteLn(1);
          else
            WriteLnS('f1 or unknown');
          end;
          i := i + 1
        end;
        i := 0;
        repeat WriteLnS(':D'); i := i + 2 until i >= 9
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
