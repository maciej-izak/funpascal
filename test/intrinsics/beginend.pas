{%results=OK1:1,OK2:2,OK3:3}

var
  varr: byte;
  beginner: byte;
  ender: byte;
begin
{$IFDEF OK1}
  varr := 1;
  Halt(varr);
{$ENDIF}
{$IFDEF OK2}
  beginner := 2;
  Halt(beginner);
{$ENDIF}
{$IFDEF OK3}
  ender := 3;
  Halt(ender);
{$ENDIF}
end.