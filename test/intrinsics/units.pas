{%results=OK1:1,OK2:2}
{$IFDEF OK1}
uses units_u2, units_u1;
{$ENDIF}
{$IFDEF OK2}
uses units_u1, units_u2;
{$ENDIF}
begin
  Halt(uvalue);
end.