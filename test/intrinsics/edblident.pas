{%fail}
{%results=F1,F2,F3}

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

begin
end.