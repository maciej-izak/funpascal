{%results=OK1,FAIL1:-,FAIL2:-,FAIL3:-}
begin
  {$IFDEF OK1}
  if ord(' ') <> 32 then HaltAtLine;
  {$ENDIF}
  {$IFDEF FAIL1}
  ord(1);
  {$ENDIF}
  {$IFDEF FAIL2}
  if ord('  ') = 1 then ;
  {$ENDIF}
  {$IFDEF FAIL3}
  if ord(' ', 2) = 32 then ;
  {$ENDIF}
end.