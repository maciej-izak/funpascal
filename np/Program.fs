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

        procedure foo(a: ShortInt);
        begin
          WriteLn(a);
          a := a - 1;
          WriteLn(a);
        end;

        type TFoo = packed record
          a, b : integer;
        end;

        type TA = array[0..2] of TFoo;

        procedure foo2(var f: TFoo);
        begin
          WriteLn(f.a);
          WriteLn(f.b);
          f.a := f.a * 10;
          f.b := f.b * 10;
        end;

        procedure foo3(var a: TA);
        begin
          WriteLn(a[1].a);
          WriteLn(a[1].b);
          a[1].a := a[1].a * 10;
          a[1].b := a[1].b * 10;
        end;

      var
        x: byte;
        f: TFoo;
        a: TA;
      begin
        x := -1;
        foo(x);
        foo(x);
        f.a := 1;
        f.b := 2;
        foo2(f);
        foo2(f);
        a[1].a := 3;
        a[1].b := 4;
        WriteLn(a[1].a);
        WriteLn(a[1].b);
        foo3(a);
        foo3(a);
        WriteLn(a[1].a);
        WriteLn(a[1].b);
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
