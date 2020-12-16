{%fail}
{%results=F1,F2,F3,OK1:+,OK2:+}

type TFoo2 = function: byte;
type TFoo3 = procedure(x: byte);

procedure Test;
begin
end;

var
  x: integer;
  f2: TFoo2;
  f3: TFoo3;
begin
{$IFDEF F1}
  x := 0 + Test;
{$ENDIF}  
{$IFDEF F2}
  f2 := Test;
{$ENDIF}  
{$IFDEF F3}
  f3 := Test;
{$ENDIF}  
{$IFDEF OK1}
  f2 := @Test;
{$ENDIF}  
{$IFDEF OK2}
  f3 := @Test;
{$ENDIF}  
end.