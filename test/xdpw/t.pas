program t;

var
  s: PStream;
  c: ^PChar;
  o: Pointer;
begin
  s := PStream(nil);
  c := @s^.Data;
  o := @s^.Index;
  WriteLn(Integer(o));
end.