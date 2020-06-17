{%results=F1:-,F2:-,F3:-,F4:-,F5:-,OK1:20,OK2:41}
var
  i: integer;
begin
  {$IFDEF F1}
  Break;
  {$ENDIF}
  {$IFDEF F2}
  Break();
  {$ENDIF}
  {$IFDEF F3}
  Break(0);
  {$ENDIF}
  {$IFDEF OK1}
  for i := 0 to 1 do begin
    if i = 1 then HaltAtLine
    else Break;
    HaltAtLine;
  end;
  HaltAtLine;
  {$ENDIF}
  {$IFDEF F4}
  for i := 0 to 1 do begin
    if i = 1 then HaltAtLine
    else Break();
    HaltAtLine;
  end;
  HaltAtLine;
  {$ENDIF}
  {$IFDEF F5}
  for i := 0 to 1 do begin
    if i = 1 then HaltAtLine
    else Break(0);
    HaltAtLine;
  end;
  HaltAtLine;
  {$ENDIF}
  {$IFDEF OK2}
  for i := 0 to 1 do begin
    if i = 1 then Break;
    HaltAtLine;
  end;
  HaltAtLine;
  {$ENDIF}
end.