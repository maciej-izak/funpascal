{%fail}
{%results=T1,T2,T3,T4,T5,T6,T7,T8}

{$IFDEF T1}
var s: singleee;
{$ENDIF}
{$IFDEF T2}
function foo: byteee;
begin
end;
{$ENDIF}
{$IFDEF T3}
procedure foo(a: inteeeger);
begin
end;
{$ENDIF}
{$IFDEF T4}
type x = set of byyyte;
{$ENDIF}
{$IFDEF T5}
// TODO extreme metaprogramming - validator of error messages fot this case
type x = set of string;
type y = set of string; // intended ! for some internal checks - duplicated position 
{$ENDIF}
{$IFDEF T6}
type x = set of ^char;
{$ENDIF}
{$IFDEF T7}
type x = set of array[0..1] of byte;
{$ENDIF}
{$IFDEF T8}
type x = set of file;
{$ENDIF}

begin
end.