{%result=1}
{%results=OK1,OK2,OK3,OK4,OK5,F1:-,F2:-}

type
  TProc = procedure;
  
var
  b: byte;

procedure Test;
begin
  b := 1;
end;

var
  p: TProc;
  pp: Pointer;
begin
  b := 0;
  {$IFDEF OK1}
  p := Test;
  p;
  {$ENDIF}
  {$IFDEF OK2}
  p := @Test;
  p;
  {$ENDIF}
  {$IFDEF OK3}
  p := Test;
  p();
  {$ENDIF}
  {$IFDEF OK4}
  pp := @Test;
  TProc(pp);
  {$ENDIF}
  {$IFDEF OK5}
  pp := @Test;
  TProc(pp)();
  {$ENDIF}
  {$IFDEF F1}
  p := Test();
  p;
  {$ENDIF}
  {$IFDEF F2}
  pp := Test;
  {$ENDIF}
  Halt(b);
end.