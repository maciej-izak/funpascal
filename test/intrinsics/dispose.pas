{%results=OK1,OK2,OK3,FAIL1:-,FAIL2:-,FAIL3:-}

type
  PInteger = ^Integer;

var
  i: ^integer;
  i2: PInteger;
begin
  {$IFDEF OK1}
  New(i);
  Dispose(i);
  {$ENDIF}
  {$IFDEF OK2}
  New(i2);
  Dispose(i2);
  {$ENDIF}
  {$IFDEF OK3}
  i2 := New(PInteger);
  Dispose(i2);
  {$ENDIF}
  {$IFDEF FAIL1}
  Dispose(PInteger);
  {$ENDIF}
  {$IFDEF FAIL2}
  Dispose();
  {$ENDIF}
  {$IFDEF FAIL3}
  Dispose;
  {$ENDIF}
end.