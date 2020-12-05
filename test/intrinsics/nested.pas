{%results=:,OK2}
procedure foo(var x: integer);

var
  y: integer;

  procedure foo2;
  begin
    x := x + 1;
    {$IFDEF OK2}
    if x < 2 then foo2;
    {$ENDIF}
    y := x + 1;
  end;

begin
  foo2;
  x := x + y;
end;

var
  z: integer;
begin
  z := 0;
  foo(z);
  {$IFDEF OK2}
  if z <> 5 then HaltAtLine;
  {$ELSE}
  if z <> 3 then HaltAtLine;
  {$ENDIF}
end.