#load "Test.fsx"
open np.BasicParsers
open FParsec

// testAll """
//     program FsIsTheBest;
//     { WOWWWW } {W} (* LOL *)
//     type
//         TFoo {}=(**) packed record
//             wiek: integer;
//             f2, f4: integer
//         end;
//         TArray = array[char] of byte;
//         TArray2 = array[0..10, 0..1] of byte;
//         TArrayPtr = ^TArray2;
//         TArrayAlias = TArrayPtr;
//         TMyInt = type Integer;   
//         TTest2 = record
//         end;
//     const 
//       MY_TEST = 10 + 9; 
//     var
//       foo, boo: Integer;
//       s: string;
//     begin
//       foo := 1 + 2 * 3 / 2 + 9;
//     end.
//                                """

//testProcDecl "procedure(var test, foo: integer; const x: byte; i, j, k: integer)"

//testProcDecl "function(var test, foo: integer; const x: byte; i, j, k: integer): byte"

// testProcTypeDecl 
//   """type 
//       TFun = function(var test, foo: integer; const x: byte; i, j, k: integer): byte;
//       TProc = procedure;
//       TProc2 = procedure(var Foo: byte);
//       TFun2 = function(): byte;
//   """

//testAll @"type MyStrin = ^string; TFoo = byte; TRange = 1..3; begin end."

//testStmt @"begin x := 10; end"

// testStmt @"begin x := 10 end"

// testStmt @"begin if true then x := 10 end"

// testStmt @"begin if not(1+2*someVar = 9) then x := 10 end"

// testDesignator "foo^"
// testDesignator "foo[1,2,3].io^"

// testIfThen "if true then else x := 10"
// testStmt "begin if true then begin a := 9; b := 7 end else x := 10 end"

// testStmt "begin if true then a := 9; b := 7 end"

// testCaseStmt "case 9 + b.foo^[9*i] of 1*9: i := 10; end"

// testCaseStmt "case 9 + b.foo^[9*i] of 1*9: i := 10; else if x then d^^ := 5 end"

// testCaseStmt "case a of 0..9: ; end"

// testCaseStmt "case a of 0..9, 8: ; 1,2,3..(4+2): ; else end"

// testString "'fooo'"
// testString "'fooo'''#01D5''''"

// testExpr "'Hell'#32'''o' + #70 + -5 + variable^"
// testExpr "'Hell'#32'''o' + #70 + -5 + @variable^"

// testCD "const TEST = 1+2*foo;"

// testFor "for i := 9 downto 0 do x := i;"

// testFor "for i := 9 downto 0 do begin x := i; begin begin end end; begin y := j end end"

// testRS "repeat x := 5; begin y := 10; end; begin end; z := 'Heloł' until x = 10"

// testWS "while (true * 9) - 9 do begin x := 7; x := 9 end"

// testCall "foo(a,b,10*2,1,2,3,4,5,6,7,8,9)"
// testCall "foo"
// testCall "foo( )"
// testCall "foo( 1 + x(y), x(1+2) )"

// testStmt "x;"

// testStmt "x();"

// testStmt "begin x := 999; end"

//testDirective "if {$I foo.inc}"

// run (between (str_wsc "x")(str_wsc "x")((str_wsc "if") .-. (str_wsc "then")) .>> eof) "x if {$I foo.inc}then x"

// testAll 
//     @"program foo;
// {$I foo.inc}
// {$APPTYPE CONSOLE}
// (*$I foo.inc*){}
// {$I bad.inc}
//   i, f: string;
// begin
//   if x = 0 then
//     WriteLn();
//   with x do begin
//     ReadLn();
//   end;
//   {.$I bad.inc}
// end."

// testAll
//   """
//   var
//     a, b, c: byte;
//   label
//     my_label, xxx, z;
//   begin
//     my_label:if true then xxx:foo(); z:
//   end.
//  """

// testAll
//   """
//   var
//     a, b, c: byte;
//   label
//     my_label, xxx, z;
//   begin
//     with x,y.z do if x then else writeln()
//     z() // good error :)
//   end.
//  """

// testAll // about ignoring ;
//   """
//   var
//     a, b, c: byte;
//   label
//     my_label, xxx, z;
//   begin
//     with x,y.z do if x then else begin ;;;;writeln();;end;;;
//     ;;z:;;;;
//   end.
//  """

// testAll 
//   """
//   const 
//     D: string = 'foo';
//   begin end.
//  """

testAll // about ignoring ;
  """
  const 
    D: string = 'foo';
  begin end.
 """

// TO CHECK
// {$I bad.inc} if 0 then ;