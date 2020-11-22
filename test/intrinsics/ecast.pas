{%results=OK:+,F:-}
var
  b: byte;
begin
{$IFDEF F}
  b := byte(1, 1)
{$ENDIF}
{$IFDEF OK}
  b := byte(1)
{$ENDIF}
end.