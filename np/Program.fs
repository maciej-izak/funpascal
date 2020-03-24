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
        a[1].x := 2;
        a[1].s[0] := 2;
        a[1].y := 2;
        a[9].x := $FF;
        a[9].s[0] := $FFFFFFFF;
        a[9].y := $FFFFFFFF;
        a[10].x := 1;
        a[10].s[0] := 1;
        a[10].y := 1;
        a[0] := a[9];
        for i := 0 to 10 do begin
          WriteLn(a[i].x);
          WriteLn(a[i].s[0]);
          WriteLn(a[i].y);
        end
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
