{%fail}
{%results=F1,F2,F3,F4,F5}

var
  x: byte;

{$IFDEF F1}
var x: integer;
{$ENDIF}

{$IFDEF F2}
type x = byte;
{$ENDIF}

{$IFDEF F3}
const x = 1;
{$ENDIF}

{$IFDEF F4}
procedure x;
begin
end;
{$ENDIF}

{$IFDEF F5}
label x;
{$ENDIF}

begin
end.