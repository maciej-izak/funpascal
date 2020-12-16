{%results=OK1:10,OK2:1,OK3:4,OK4:4,F1:-}

type
  TProc = procedure(x: byte);
  TProc1 = procedure;
  TProc2 = function: TProc1;
  TProc3 = function: TProc2;
  TProc4 = function: TProc3;
  
var
  b: byte;

procedure Test(v: byte);
begin
  b := v;
end;

procedure Proc1;
begin
  b := 1;
end;

function Proc2: TProc1;
begin
  b := 2;
  Result := @Proc1;
end;

function Proc3: TProc2;
begin
  b := 3;
  Result := @Proc2;
end;

function Proc4: TProc3;
begin
  b := 4;
  Result := @Proc3;
end;

var
  p: TProc;
  pp: Pointer;
begin
  b := 0;
  {$IFDEF OK1}
  pp := @Test;
  TProc(pp)(10);
  {$ENDIF}
  {$IFDEF OK2}
  pp := @Proc4;
  TProc4(pp)()()()();
  {$ENDIF}
  {$IFDEF OK3}
  pp := @Proc4;
  TProc4(pp)();
  {$ENDIF}  
  {$IFDEF OK4}
  pp := @Proc4;
  TProc4(pp);
  {$ENDIF}
  {$IFDEF F1}
  pp := @Proc4;
  TProc4(pp)()()()()();
  {$ENDIF}
  Halt(b);
end.