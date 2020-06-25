{%results=OK1,OK2,OK3,FAIL1:-,FAIL2:-}

type
  PInteger = ^Integer;

var
  i: ^integer;
  i2: PInteger;
begin
  {$IFDEF OK1}
  New(i);
  {$ENDIF}
  {$IFDEF OK2}
  New(i2);
  {$ENDIF}
  {$IFDEF OK3}
  i2 := New(PInteger);
  {$ENDIF}
  {$IFDEF FAIL1}
  New(PInteger);
  {$ENDIF}
  {$IFDEF FAIL2}
  i2 := New(i);
  {$ENDIF}
end.