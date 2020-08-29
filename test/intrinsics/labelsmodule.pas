unit labelsmodule;

label x;

var r: byte;

begin
  r := 1;
  goto x;
  r := 2;
  x:
  Halt(r)
end.

