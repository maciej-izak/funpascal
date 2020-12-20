{%results=OK1:1,OK2:4,OK3:4,F1:-}

type
  TProc1 = procedure;
  TProc2 = function: TProc1;
  TProc3 = function: TProc2;
  
var
  b: byte;

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

begin
  b := 0;
  {$IFDEF OK1}
  Proc4()()()();
  {$ENDIF}
  {$IFDEF OK2}
  Proc4();
  {$ENDIF}  
  {$IFDEF OK3}
  Proc4;
  {$ENDIF}
  {$IFDEF F1}
  Proc4()()()()();
  {$ENDIF}
  Halt(b);
end.