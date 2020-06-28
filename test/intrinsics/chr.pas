{%results=OK1,FAIL1:-}
begin
  {$IFDEF OK1}
  if chr(32) <> ' ' then HaltAtLine;
  {$ENDIF}
  {$IFDEF FAIL1}
  chr(1);
  {$ENDIF}
end.