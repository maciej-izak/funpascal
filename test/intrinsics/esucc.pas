{%fail}
{%results=F1,F2,F3}

var
  i: Integer;
begin
{$IFDEF F1}
  Succ(1);
{$ENDIF}
{$IFDEF F2}
  i := Succ(1.0);
{$ENDIF}
{$IFDEF F3}
  i := Succ(1, 1.0);
{$ENDIF}
end.