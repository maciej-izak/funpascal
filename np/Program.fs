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

type
  TTest = (a1, a2, a3);
  TTests = set of TTest;
  TA = array[0..1] of byte;

//const
//  Arr: TA = ($4D, $5A);

const
  Digits:    set of Char = ['0'..'9', 'Z'];

var
  r: Real;
  s: string;
  c: char;
  sp: ^string;
  t: set of TTest;
  d: set of char;
begin
  d := Digits;
  InitSystem;
  ReadLn(c);
  WriteLn(c);
  WriteLn(3.14:4:4);
  case c of
    'p': begin
        WriteLn(3.14:4:4);
        Halt(1)
      end;
    'w':
      begin
        WriteLn('w');
        Halt(2)
      end;
  else
    WriteLn('Something else :)');
  end;
  New(sp);
  sp^ := 'fooo';
  WriteLn(sp^, Length(sp^));
  Dispose(sp);
  Halt;
  WriteLn('end')
  //WriteLn(12:4);
  //WriteLn('c':4);
  //Val('3.14', r, c);
  //WriteLn(r:1:2);
  //WriteLn(Succ(byte(255)), '  ', Pred(0));
end.
      """
    |> function
       | Success _ -> printfn "Compilation success!"
       | Failure(s,_,_) -> printfn "%s" s
    0
    (*let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<NpArguments>(programName = "ls", errorHandler = errorHandler)
    let results = parser.ParseCommandLine argv
    let files = results.GetResult(Files, [])
    for f in files do
        printfn "%A" f
    //run commentLine "// foo"
    0 // return an integer exit code*)
