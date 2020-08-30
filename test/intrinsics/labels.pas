{%results=OK1:8,OK2:21,F1:-,F2:-,OK3:1}

{$IFDEF OK1}
label 1;
begin
  goto 1;
  HaltAtLine;
  1: HaltAtLine;
end.
{$ENDIF}

{$IFDEF OK2}
procedure foo;
label 2,a3;
begin
  goto 2;
  HaltAtLine;
2:;;a3:;;end; // extra test for empty instructions + labels
begin
  foo;
  HaltAtLine;
end.
{$ENDIF}

{$IFDEF F1}
label X;
procedure foo;
begin
  goto X;
end;
begin
  X:
end.
{$ENDIF}

{$IFDEF F2}
procedure foo;
begin
  goto X;
end;
begin
end.
{$ENDIF}

{$IFDEF OK3}
uses labelsmodule;
begin
end.
{$ENDIF}