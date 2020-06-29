{%results=OK,FAIL1:-,FAIL2:-,FAIL3:-}
var
  x: char;
  y: single;
begin
  {$IFDEF OK}
  if SizeOf(byte) <> 1 then HaltAtLine;
  if SizeOf(integer) <> 4 then HaltAtLine;
  if SizeOf(x) <> 1 then HaltAtLine;
  if SizeOf(y) <> 4 then HaltAtLine;
  {$ENDIF}
  {$IFDEF FAIL1}
  SizeOf(byte);
  {$ENDIF}
  {$IFDEF FAIL2}
  if SizeOf(1) <> 4 then ;
  {$ENDIF}
  {$IFDEF FAIL3}
  if SizeOf() <> 4 then ;
  {$ENDIF}
end.