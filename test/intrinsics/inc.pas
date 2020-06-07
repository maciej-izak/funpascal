{ # }
var
  i: integer;
  s: single;
begin
  i := 0;
  Inc(i);
  if i <> 1 then HaltAtLine;
  if Inc(i) <> 2 then HaltAtLine;
  if Dec(i) <> 1 then HaltAtLine;
  Dec(i);
  if i <> 0 then HaltAtLine;
  if Pred(i) <> -1 then HaltAtLine;
  if Succ(i) <> 1 then HaltAtLine;
  if Pred(i, 2) <> -2 then HaltAtLine;
  if Succ(i, 2) <> 2 then HaltAtLine;
  if Inc(i,2) <> 2 then HaltAtLine;
  if Dec(i,2) <> 0 then HaltAtLine;

  s := 0;
  Inc(s);
  if s <> 1 then HaltAtLine;
  if Inc(s) <> 2 then HaltAtLine;
  if Dec(s) <> 1 then HaltAtLine;
  Dec(s);
  if s <> 0 then HaltAtLine;
  if Pred(s) <> -1 then HaltAtLine;
  if Succ(s) <> 1 then HaltAtLine;
  if Pred(s, 2) <> -2 then HaltAtLine;
  if Succ(s, 2) <> 2 then HaltAtLine;
  if Inc(s,2) <> 2 then HaltAtLine;
  if Dec(s,2) <> 0 then HaltAtLine;
  if Pred(s, 2.5) <> -2.5 then HaltAtLine;
  if Succ(s, 2.5) <> 2.5 then HaltAtLine;
  if Inc(s, 2.5) <> 2.5 then HaltAtLine;
  if Dec(s, 2.5) <> 0.0 then HaltAtLine;
end.