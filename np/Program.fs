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

        function foo: string;
        begin
          Result := 'Hello :D';
          exit;
          Result := ' in hell :P';
        end;

        procedure LoopTest;
        var
          i: integer;
        begin
          for i := 0 to 10 do
            if i > 5 then begin
              WriteLn('? :D');
              break;
            end else
              WriteLn('i = ', i);
          i := 0;
        end;

        begin
          LoopTest;
          WriteLn(foo);
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
