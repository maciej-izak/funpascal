{ %fail }
{ %results=F1,F2,F3,F4 }

function foo: single;
begin
  result := 0;
end;

procedure bar;
begin
end;

var
  i: integer;
begin
{$IFDEF F1}
  i := foo;
{$ENDIF}
{$IFDEF F2}
  i := foo();
{$ENDIF}
{$IFDEF F3}
  i := bar;
{$ENDIF}
{$IFDEF F4}
  i := bar();
{$ENDIF}
end.