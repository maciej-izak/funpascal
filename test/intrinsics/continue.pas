{%results=F1:-,F2:-,F3:-,F4:-,F5:-,OK1:16,OK2:38}
var
  i: integer;
begin
  {$IFDEF F1}
  Continue;
  {$ENDIF}
  {$IFDEF F2}
  Continue();
  {$ENDIF}
  {$IFDEF F3}
  Continue(0);
  {$ENDIF}
  {$IFDEF OK1}
  for i := 0 to 1 do begin
    if i = 1 then HaltAtLine
    else Continue;
    HaltAtLine;
  end;
  {$ENDIF}
  {$IFDEF F4}
  for i := 0 to 1 do begin
    if i = 1 then HaltAtLine
    else Continue();
    HaltAtLine;
  end;
  {$ENDIF}
  {$IFDEF F5}
  for i := 0 to 1 do begin
    if i = 1 then HaltAtLine
    else Continue(0);
    HaltAtLine;
  end;
  {$ENDIF}
  {$IFDEF OK2}
  for i := 0 to 1 do begin
    if i = 2 then Continue;
    HaltAtLine;
  end;
  {$ENDIF}
end.