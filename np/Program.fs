﻿// Learn more about F# at http://fsharp.org

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

      function Length(const s: string): Integer;
      begin
        Result := 0;
        while s[Result + 1] <> #0 do Inc(Result);
      end;

      procedure AppendStr(var Dest: string; const Source: string);
      var
        DestLen, i: Integer;
      begin
        DestLen := Length(Dest);
        i := 0;
        repeat
          Inc(i);
          Dest[DestLen + i] := Source[i];
        until Source[i] = #0;
      end;

      var
        s: string;
      begin
        s := 'hello';
        AppendStr(s, ' Maciej');
        WriteLn(Length(s));
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
