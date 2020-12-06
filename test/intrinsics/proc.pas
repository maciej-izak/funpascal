{%results=OK1,OK2,F1:-,F2:-,F3:-}

procedure Test(v: byte);
begin
end;

var x: integer;

procedure ok;
begin
  x := 10;
end;

begin
  x := 0;
  {$IFDEF OK1}
  ok();
  {$ENDIF}
  {$IFDEF OK2}
  ok;
  {$ENDIF}
  {$IFDEF F1}
  Test(1,2);
  {$ENDIF}
  {$IFDEF F2}
  OK(());
  {$ENDIF}
  {$IFDEF F3}
  OK(0);
  {$ENDIF}
  if x <> 10 then HaltAtLine;
  end.