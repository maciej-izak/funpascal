{%fail}
{%results=F1,F2,OK:+}

type
  PCells = array [0..3] of Byte;
  TPArray = array [0..0] of PCells;
  PTPArray = ^TPArray;
 
var
 i, j: Integer;
 p: PTPArray;
begin
 GetMem(p, 10 * SizeOf(PCells));
 i := 7;
 j := 1;
 p^[i][j] := 4;
 for i := 1 to 9 do
   p^[i] := p^[i-1];
 {$IFDEF F1}
 Writeln(p^[2]^);
 {$ENDIF}
 {$IFDEF F2}
 Writeln(p^[2]);
 {$ENDIF}
 {$IFDEF OK}
 Writeln(p^[2][0]);
 {$ENDIF}
end.