{%result=6}
begin
  {$DEFINE X}
  {$IFNDEF X}
  {$ELSE}
  HaltAtLine;
  {$ENDIF}
end.