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
      """
{$I system.inc}
var
  r: Real;
  c: integer;
  s: string;
begin
  InitSystem;
  c := 12;
  WriteLine(StdOutputHandle);
  r := 3.14;
  Str(r,s,2);
  //WriteLine(s);
  //WriteLn(s);
  //WriteLn(3.14:0:2);
  //WriteLn(12:4);
  //WriteLn('c':4);
  //Val('3.14', r, c);
  //WriteLn(r:1:2);
  //WriteLn(Succ(byte(255)), '  ', Pred(0));
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
