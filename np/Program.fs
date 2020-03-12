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
        PI = ^Integer;
        PPI = ^^Integer;
        PPPI = ^^^Integer;
      var
        i: Integer;
        ip: PI;
        ipp: PPI;
        ippp: PPPI;
      begin
        i := 1;
        ip := @i;
        WriteLn(i);
        ip^ := 2;
        WriteLn(ip^);
        ipp := @ip;
        ipp^^ := 3;
        WriteLn(ipp^^);
        ippp := @ipp;
        ippp^^^ := 4;
        WriteLn(ippp^^^);
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
