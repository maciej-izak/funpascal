{%results=F1:-,OK1:+}

var 
  x: pointer;
begin
{$IFDEF F1}
  x := @0;
{$ENDIF}
{$IFDEF OK1}
  x := Pointer(0);
{$ENDIF}
end.