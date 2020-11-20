Test directives
---------------

The compiler allows special comments, detected as test directives at the top of the test file, the following pseudo directives can be used to
determine result of test:

+ `FAIL` - compiler is expected to fail
+ `RESULT` - expected exit code of test
+ `RESULTS` - allows to do many tests at once (compilation with different defines), it allows mix of `FAIL` and `RESULT`, so part of tests can pass and some other tests can fail

Examples of `RESULTS` usage:

------------

Define 2 tests, the first without extra define, expected exit code is 4, the second with defined X, expected exit code is 6
```pascal
{%results=:4,X:6}
begin
  {$IFNDEF X}
  HaltAtLine;
  {$ELSE}
  HaltAtLine;
  {$ENDIF}
end.
```

------------

All test cases are supposed to fail.
```pascal
{%fail}
{%results=F1,F2}

{$IFDEF F1}
procedure;
begin
end;
{$ENDIF}

{$IFDEF F2}
function: byte;
begin
end;
{$ENDIF}

begin
end.
```

------------
Part of tests are supposed to fail and part to success:
```pascal
{%results=OK1,FAIL1:-,FAIL2:-,FAIL3:-}
begin
  {$IFDEF OK1}
  if chr(32) <> ' ' then HaltAtLine;
  {$ENDIF}
  {$IFDEF FAIL1}
  chr(1);
  {$ENDIF}
  {$IFDEF FAIL2}
  if chr('1') = '1' then ;
  {$ENDIF}
  {$IFDEF FAIL3}
  if chr(1, 2) = ' ' then ;
  {$ENDIF}
end.
```
the `%results` for above example can be written as:
```pascal
{%results=OK1:+,FAIL1:-,FAIL2:-,FAIL3:-}
```
the `+` can be used instead of `-` or expected exit code. For the `+` the default expected exit code is `0` or the code defined in `%result`:
```pascal
{%result=1}
{%results=OK1:+,FAIL1:-,FAIL2:-,FAIL3:-}
```

------------
Success expected with exit code 6 for two first cases and 7 for the last:
```pascal
{%result=6}
{%results=OK1,OK2,OK3:7}
begin
  {$IFDEF OK1}
  Halt(6);
  {$ENDIF}
  {$IFDEF OK2}
  Halt(6);
  {$ENDIF}
  {$IFDEF OK3}
  Halt(7);
  {$ENDIF}
end.
```