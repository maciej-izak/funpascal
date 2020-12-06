{%results=OK1:2}

type
  TProc = procedure(x: byte);
  
var
  b: byte;

procedure Test(v: byte);
begin
  b := v;
end;

var
  p: TProc;
  pp: Pointer;
begin
  b := 0;
  {$IFDEF OK1}
  pp := @Test;
  TProc(pp)(2);
  {$ENDIF}
  Halt(b);
end.