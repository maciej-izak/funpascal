{%results=OK1,FAIL1:-,FAIL2:-,FAIL3:-}
begin
  {$IFDEF OK1}
  if chr(32) <> ' ' then HaltAtLine;
  {$ENDIF}
  {$IFDEF FAIL1}
  chr(1);
  {$ENDIF}
  {$IFDEF FAIL2}
  if chr('1') = '1' then ;
  {$ENDIF}
  {$IFDEF FAIL3}
  if chr(1, 2) = ' ' then ;
  {$ENDIF}
end.