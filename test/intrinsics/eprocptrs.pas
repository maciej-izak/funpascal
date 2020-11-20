{%fail}
{%results=F1,F2,F3,F4}

type
  TProcWithParam = procedure(a: byte);

procedure Test;
begin
end;

var
  p: TProc2;
  b: byte;
begin
  {$IFDEF F1}
  lol; // test paramless calls of non existing symbol
  {$ENDIF}
  {$IFDEF F2}
  p := Test;
  {$ENDIF}
  {$IFDEF F3}
  p;
  {$ENDIF}
  {$IFDEF F4}
  b;
  {$ENDIF}
end.