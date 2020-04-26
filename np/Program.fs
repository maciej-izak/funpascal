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
function Length(const s: string): Integer;
begin
Result := 0;
while s[Result + 1] <> #0 do Inc(Result);
end;

procedure SetLength(var s: string; NewLength: Integer);
begin
if NewLength >= 0 then s[NewLength + 1] := #0;
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

procedure ConcatStr(const s1, s2: string; var s: string);
begin
s := s1;
AppendStr(s, s2);
end;

function CompareStr(const s1, s2: string): Integer;
var
  i: Integer;
begin
Result := 0;
i := 0;
repeat
  Inc(i);
  Result := Integer(s1[i]) - Integer(s2[i]);
until (s1[i] = #0) or (s2[i] = #0) or (Result <> 0);
end;

procedure Move(const Source; var Dest; Count: Integer);
var
  S, D: ^string;
  i: Integer;
begin
S := @Source;
D := @Dest;

if S = D then Exit;

for i := 1 to Count do
  D^[i] := S^[i];
end;

var
a, b, c, d: string;
  begin
    a := 'a';
    b := 'b';
    d := 'ab';
    ConcatStr(a, b, c);
    WriteLn('a + b = ', c);
    WriteLn('compare = ', CompareStr(b, a));
    WriteLn('compare = ', CompareStr(a, b));
    WriteLn('compare = ', CompareStr(c, d));
    a := ':DDD';
    Move(a[1], b[1], 4);
    WriteLn(b);
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
