{ %fail }
{ %defines=noparams,noparams2 }
var
  i: integer;
begin
  i := 0;
  {$IFDEF NOPARAMS}
  Inc();
  {$ELSE}
  {$IFDEF NOPARAMS2}
  Inc;
  {$ELSE}
  Inc(i, 2.0);
  {$ENDIF}
  {$ENDIF}  
end.