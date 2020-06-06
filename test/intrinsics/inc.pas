{ # }
var
  i: integer;
  s: single;
begin
  i := 0;
  Inc(i);
  if i <> 1 then halt(1);
  if Inc(i) <> 2 then halt(2);
  if Dec(i) <> 1 then halt(3);
  Dec(i);
  if i <> 0 then halt(4);
  if Pred(i) <> -1 then Halt(5);
  if Succ(i) <> 1 then Halt(6);
  if Pred(i, 2) <> -2 then Halt(7);
  if Succ(i, 2) <> 2 then Halt(8);
  if Inc(i,2) <> 2 then halt(9);
  if Dec(i,2) <> 0 then halt(10);

  s := 0;
  Inc(s);
  if s <> 1 then halt(11);
  if Inc(s) <> 2 then halt(12);
  if Dec(s) <> 1 then halt(13);
  Dec(s);
  if s <> 0 then halt(14);
  if Pred(s) <> -1 then Halt(15);
  if Succ(s) <> 1 then Halt(16);
  if Pred(s, 2) <> -2 then Halt(17);
  if Succ(s, 2) <> 2 then Halt(18);
  if Inc(s,2) <> 2 then halt(19);
  if Dec(s,2) <> 0 then halt(20);
  if Pred(s, 2.5) <> -2.5 then Halt(21);
  if Succ(s, 2.5) <> 2.5 then Halt(22);
  if Inc(s, 2.5) <> 2.5 then halt(23);
  if Dec(s, 2.5) <> 0.0 then halt(24);
end.