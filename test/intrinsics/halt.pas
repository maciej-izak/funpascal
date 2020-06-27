{%results=OK1,OK2,OK3:10,FAIL1:-,FAIL2:-,FAIL3:-,FAIL4:-}
begin
  {$IFDEF OK1}
  Halt;
  {$ENDIF}
  {$IFDEF OK2}
  Halt(0);
  {$ENDIF}
  {$IFDEF OK3}
  HaltAtLine;
  {$ENDIF}
  {$IFDEF FAIL1}
  Halt();
  {$ENDIF}
  {$IFDEF FAIL2}
  Halt(0,0);
  {$ENDIF}
  {$IFDEF FAIL3}
  HaltAtLine();
  {$ENDIF}
  {$IFDEF FAIL4}
  HaltAtLine(0);
  {$ENDIF}
end.
