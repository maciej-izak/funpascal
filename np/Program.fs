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
procedure DoC(const c: char);
begin
  case c of
    '0'..'9': WriteLn('IS num   (', c, ') ', Ord(c));
    'a'..'z': WriteLn('IS small (', c, ') ', Ord(c));
    'A'..'Z': WriteLn('IS big   (', c, ') ', Ord(c));
  else
    WriteLn('unknown (', c, ') ', Ord(c));
  end;
end;

var
  c: char;
begin
  for c := #0 to #255 do if c < #32 then continue else doC(c);
  for c := #255 downto #0 do doC(c);
  for c := #0 to #255 do ;
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
