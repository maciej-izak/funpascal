unit labelsmodule;

label x;

var r: byte;

{$IFDEF OK3}
begin
  r := 1;
  goto x;
  r := 2;
  x:
  Halt(r)
end.
{$ENDIF}

{$IFDEF F3}
initialization
  goto x;
finalization
  x:
end.
{$ENDIF}

{$IFDEF F4}
initialization
  x:
finalization
  goto x;
end.
{$ENDIF}
