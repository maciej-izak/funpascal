#load "Test.fsx"
open np.BasicParsers
open FParsec

// """
// {$I system.inc}
// {$I Common.inc}
// {.$I Scanner.inc}

// type
//   TTest = (a1, a2, a3);
//   TTests = set of TTest;
//   TA = array[0..1] of byte;

// const
//   Arr: TA = ($4D, $5A);

// const
//   Digits:    set of Char = ['0'..'9', 'Z'];

// var
//   r: Real;
//   s: string;
//   c: char;
//   sp: ^string;
//   t: set of TTest;
//   d: set of char;
//   a: TA;
// begin
//   InitSystem;
//   d := Digits;
//   a := Arr;
//   WriteLn('z' in Digits);
//   WriteLn('A');
//   WriteLn('b');
//   WriteLn('A' + 'b');
//   WriteLn(chr(a[0]), chr(a[1]));
//   WriteLn('AB' + 'CD');
//   ReadLn(c);
//   WriteLn(c);
//   WriteLn(3.14:4:4);
//   case c of
//     'p': begin
//         WriteLn(3.14:4:4);
//         Halt(1)
//       end;
//     'w':
//       begin
//         WriteLn('w');
//         Halt(2)
//       end;
//   else
//     WriteLn('Something else :)');
//   end;
//   New(sp);
//   sp^ := 'fooo';
//   WriteLn(sp^, Length(sp^));
//   Dispose(sp);
//   Halt;
//   WriteLn('end')
//   //WriteLn(12:4);
//   //WriteLn('c':4);
//   //Val('3.14', r, c);
//   //WriteLn(r:1:2);
//   //WriteLn(Succ(byte(255)), '  ', Pred(0));
// end.
// """


// """
// procedure DoC(const c: char);
// begin
//   case c of
//     '0'..'9': WriteLn('IS num   (', c, ') ', Ord(c));
//     'a'..'z': WriteLn('IS small (', c, ') ', Ord(c));
//     'A'..'Z': WriteLn('IS big   (', c, ') ', Ord(c));
//   else
//     WriteLn('unknown (', c, ') ', Ord(c));
//   end;
// end;

// var
//   c: char;
// begin
//   for c := #0 to #255 do if c < #32 then continue else doC(c);
//   for c := #255 downto #0 do doC(c);
//   for c := #0 to #255 do ;
// end.
// """

//       """
// var
//   c: char;
// begin
//   repeat
//     if c < 'A' then begin
//       Inc(c);
//       Continue;
//     end;
//     WriteLn(Ord(c), ' = ', c);
//     if c = 'Z' then Break;
//     Inc(c);
//   until c = 'a';
// end.
//       """



// """
// type
//   PChar = ^Char;
//   LongInt = Integer;


//   TFileRec = record
//     Name: string;
//     Handle: LongInt;
//   end;

//   PFileRec = ^TFileRec;

//   TStream = record
//     Data: PChar;
//     Index: Integer;
//   end;

//   PStream = ^TStream;


// procedure ReadCh(var F: file; P: PStream; var ch: Char);
// var
//   Len: Integer;
//   Dest: PChar;
//   FileRecPtr: PFileRec;
// begin
//   FileRecPtr := @F;
//   if P <> nil then                                      // String stream input
//     begin
//     Dest := PChar(Integer(P^.Data) + P^.Index);
//     ch := Dest^;
//   end;
// end;

// var
//   f: file;
//   s: TStream;
//   c: array[0..1] of char;
//   cr: char;
//   b: byte;
// begin
//   c[0] := 'a';
//   c[1] := 'b';
//   s.Data := @c[0];
//   s.Index := 1;
//   ReadCh(f, @s, cr);
//   WriteLN(cr);
// end.
// """

//         """
// function Length(const s: string): Integer;
// begin
// Result := 0;
// while s[Result + 1] <> #0 do Inc(Result);
// end;

// procedure SetLength(var s: string; NewLength: Integer);
// begin
// if NewLength >= 0 then s[NewLength + 1] := #0;
// end;

// procedure AppendStr(var Dest: string; const Source: string);
// var
//   DestLen, i: Integer;
// begin
// DestLen := Length(Dest);
// i := 0;
// repeat
//   Inc(i);
//   Dest[DestLen + i] := Source[i];
// until Source[i] = #0;
// end;

// procedure ConcatStr(const s1, s2: string; var s: string);
// begin
// s := s1;
// AppendStr(s, s2);
// end;

// function CompareStr(const s1, s2: string): Integer;
// var
//   i: Integer;
// begin
// Result := 0;
// i := 0;
// repeat
//   Inc(i);
//   Result := Integer(s1[i]) - Integer(s2[i]);
// until (s1[i] = #0) or (s2[i] = #0) or (Result <> 0);
// end;

// procedure Move(const Source; var Dest; Count: Integer);
// var
//   S, D: ^string;
//   i: Integer;
// begin
// S := @Source;
// D := @Dest;

// if S = D then Exit;

// for i := 1 to Count do
//   D^[i] := S^[i];
// end;

// var
// a, b, c, d: string;
//   begin
//     a := 'a';
//     b := 'b';
//     d := 'ab';
//     ConcatStr(a, b, c);
//     WriteLn('a + b = ', c);
//     WriteLn('compare = ', CompareStr(b, a));
//     WriteLn('compare = ', CompareStr(a, b));
//     WriteLn('compare = ', CompareStr(c, d));
//     a := ':DDD';
//     Move(a[1], b[1], 4);
//     WriteLn(b);
//   end.
//   """



//         """
// type
//   PChar = ^Char;
//   TStream = record
//     Data: PChar;
//     Index: Integer;
//   end;
// begin
// end.

// """


        // """program wow;

        // function foo: string;
        // begin
        //   Result := 'Hello :D';
        //   exit;
        //   Result := ' in hell :P';
        // end;

        // procedure LoopTest;
        // var
        //   i: integer;
        // begin
        //   for i := 0 to 10 do
        //     if i > 5 then begin
        //       WriteLn('? :D');
        //       break;
        //     end else
        //       WriteLn('i = ', i);
        //   i := 0;
        // end;

        // begin
        //   LoopTest;
        //   WriteLn(foo);
        // end.
        // """


        // """program wow;
        // var i: integer;
        //     s: string;
        // begin
        //   s := '(But previous value was 9!)';
        //   i := 9;
        //   Dec(i);
        //   WriteLn('Result is : ', i + 2, ' ', s);
        // end.
        // """


        // """program wow;
        // var i: integer;
        // begin
        //   i := 9;
        //   Dec(i);
        //   WriteLn(i);
        // end.
        // """

      // """program wow;

      // procedure foo; forward;

      // procedure boo;
      // begin
      //   WriteLn(1);
      //   foo;
      // end;

      // procedure foo;
      // begin
      //   WriteLn(2);
      // end;

      // function GetProcessHeap: Int64; external 'KERNEL32.DLL' name 'GetProcessHeap';
      // function GetLastError: LongInt; external 'KERNEL32.DLL' name 'GetLastError';

      // begin
      //   WriteLn64(GetProcessHeap());
      //   boo;
      //   foo();
      // end.
      // """


      //       """program wow;

      // function Length(const s: string): Integer;
      // begin
      //   Result := 0;
      //   while s[Result + 1] <> #0 do Inc(Result);
      // end;

      // procedure AppendStr(var Dest: string; const Source: string);
      // var
      //   DestLen, i: Integer;
      // begin
      //   DestLen := Length(Dest);
      //   i := 0;
      //   repeat
      //     Inc(i);
      //     Dest[DestLen + i] := Source[i];
      //   until Source[i] = #0;
      // end;

      // var
      //   s: string;
      // begin
      //   s := 'hello';
      //   AppendStr(s, ' Maciej');
      //   WriteLn(Length(s));
      // end.
      // """

      // program wow;

      // function foo(a: byte): integer;
      // begin
      //   WriteLnS('Hello from foo!');
      //   result := 88 + a;
      // end;

      // begin
      //   foo(3);
      //   WriteLn(foo(2));
      // end.


      // program wow;
      //   var x: byte;
      //
      //   procedure foo(a: ShortInt);
      //   begin
      //     x := 101;
      //     WriteLn(a);
      //     a := a - 1;
      //     WriteLn(a);
      //   end;
      //
      //   type TFoo = packed record
      //     a, b : integer;
      //   end;
      //
      //   type TA = array[0..2] of TFoo;
      //
      //   procedure foo2(var f: TFoo);
      //   begin
      //     x := 102;
      //     WriteLn(f.a);
      //     WriteLn(f.b);
      //     f.a := f.a * 10;
      //     f.b := f.b * 10;
      //   end;
      //
      //   procedure foo3(var a: TA);
      //   begin
      //     x := 103;
      //     WriteLn(a[1].a);
      //     WriteLn(a[1].b);
      //     a[1].a := a[1].a * 10;
      //     a[1].b := a[1].b * 10;
      //   end;
      //
      // var
      //   f: TFoo;
      //   a: TA;
      // begin
      //   x := -1;
      //   WriteLn(x);
      //   foo(x);
      //   foo(x);
      //   WriteLn(x);
      //   f.a := 1;
      //   f.b := 2;
      //   foo2(f);
      //   foo2(f);
      //   WriteLn(x);
      //   with a[1] do begin
      //     a := 3;
      //     b := 4;
      //   end;
      //   WriteLn(a[1].a);
      //   WriteLn(a[1].b);
      //   foo3(a);
      //   foo3(a);
      //   WriteLn(a[1].a);
      //   WriteLn(a[1].b);
      //   WriteLn(x);
      // end.

      // program wow;
      // type
      //   TSubA = array[0..0] of integer;
      //   TFoo = packed record
      //     x: byte;
      //     s: TSubA;
      //     y: integer;
      //   end;
      //   PFoo = ^TFoo;
      //   PPFoo = ^PFoo;
      //   TA = array[0..10] of TFoo;
      // var
      //   a: TA;
      //   i: integer;
      //   f: PFoo;
      // begin
      //   //a[0].x := 1;
      //   //a[1].s[0] := 2;
      //   a[1].x := 2;
      //   a[1].s[0] := 2;
      //   a[1].y := 2;
      //   a[9].x := $FF;
      //   a[9].s[0] := $FFFFFFFF;
      //   a[9].y := $FFFFFFFF;
      //   a[10].x := 1;
      //   a[10].s[0] := 1;
      //   a[10].y := 1;
      //   a[0] := a[9];
      //   f := @a[0];
      //   f^.x := 69;
      //   for i := 0 to 10 do begin
      //     WriteLn(a[i].x);
      //     WriteLn(a[i].s[0]);
      //     WriteLn(a[i].y);
      //   end;
      //   f.x := 70;
      //   WriteLn(f.x);
      //   with f^ do
      //     WriteLn(x);
      // end.

      // TODO
      // type
      //   TA = array[0..10] of byte;
      //   TB = array[0..10, 0..5] of integer;
      //   TC = array[0..10] of array[0..5] of integer;
      //   TD = array[0..10, 0..5, 0..0, -1..2] of integer;
      //   TE = array[0..10, 0..2] of array[0..5, 1..2] of integer;
      // var
      //   a: TA;
      //   b: TB;
      // begin
      //   a[0] := 1;
      //   b[10,4] := 1;
      //   b[10][5] := 2;
      //   a[1] := 2;
      //   WriteLn(a[0]);
      //   WriteLn(a[1]);
      //   WriteLn(a[2]);
      //   WriteLn(b[10][5]);
      //   WriteLn(b[0,5]);
      // end.

      // """program wow;
      // type
      //   PI = ^Integer;
      //   PPI = ^^Integer;
      //   PPPI = ^^^Integer;
      // var
      //   i: Integer;
      //   ip: PI;
      //   ipp: PPI;
      //   ippp: PPPI;
      // begin
      //   i := 1;
      //   ip := @i;
      //   WriteLn(i);
      //   ip^ := 2;
      //   WriteLn(ip^);
      //   ipp := @ip;
      //   ipp^^ := 3;
      //   WriteLn(ipp^^);
      //   ippp := @ipp;
      //   ippp^^^ := 4;
      //   WriteLn(ippp^^^);
      // end.
      // """

      // TODO case sensitive
      // program wow;
      // type
      //   TBoo = record
      //     x: Integer;
      //   end;
      //   TFoo = record
      //     b: TBoo;
      //     x,y: Integer;
      //   end;
      // var
      //   F: TFOO;
      // begin
      //   f.b.x := 10;
      //   f.x := 5;
      //   with f, f.b do
      //     WriteLn(b.x);
      //   with f, f.b do
      //     WriteLn(x);
      //   with f, f.b do
      //     WriteLn(f.b.x);
      // end.


      // for optimizations ?
      // type
      //   TFoo = (f1, f2, f3);
      // var i: Integer;
      // begin
      //   for i := 0 to 3 do
      //     WriteLn(i);
      //   for i := 3 downto 0 do
      //     WriteLn(i);
      //   while i < 9 do begin
      //     case f2 of
      //       f2..f3: WriteLn(1);
      //     else
      //       WriteLnS('f1 or unknown');
      //     end;
      //     i := i + 1
      //   end;
      //   i := 0;
      //   repeat WriteLnS(':D'); i := i + 2 until i >= 9

      // peephole opt needed for IL C
      // """program wow;
      // type
      //   TFoo = (f1, f2, f3);
      // begin
      //   case f2 of
      //     f2..f3: WriteLn(1);
      //   //else
      //   end;  WriteLnS('f1 or unknown');
      //   //end;
      // end.
      // """

      // """program wow;
      // type
      //   TFoo = (f1, f2, f3);
      // var
      //   x: TFoo;
      // label f1, f2;
      // begin
      //   x := f3;
      //   case x of
      //     f1: f2:WriteLn(1);
      //     f2: begin f1:WriteLn(2); goto f2; end;
      //   else
      //     WriteLnS('f3 or unknown');
      //     goto f1;
      //   end;
      // end.
      // """

    // PasStreams.testAll
    //   """program wow;
    //   var
    //     x: Integer;
    //   label a;
    //   begin
    //     x := 8;
    //     goto a;
    //     if (x = 8) and (x mod 3 = 0) then
    //       WriteLn(x)
    //     else if x = 7 + 1 then begin
    //       WriteLnS('1');
    //       if x = 123 then
    //       else ;
    //       if x = 9 then
    //         if x = 8 then
    //           WriteLnS('foo');
    //       WriteLnS('foo2');
    //     end else
    //       a: WriteLnS('O_o');
    //     if 6=7 then
    //       WriteLnS('^^');
    //   end.
    //   """

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

// testAll 
//   """
//   type
//     TFoo = (one + 10)..60;
//     TFoo = (one, two, three);
//     TFoos = set of TFoo;
//     TPA = packed array[0..10] of byte;
//     TPFoo = packed set of TFoo;
//     TR = packed record end;
//   begin end.
//  """

// testAll 
//   """
//   label x;
//   begin goto x; x:end.
//  """

// testAll 
//   """
//   const A: TArray = (1,2,3);
//   const R: TRec = (a: 12; b:(c:'aaa'));
//   type f = a..f;
//   const S: TS = [aa..bb, cc, dd, ee..ff];
//   begin 
//     b:c:;e:a := [1..b];d:e:f:x:=2;d:d:
//   end.
//  """

// testAll 
//   """
//   begin 
//     if true then else x:
//   end.
//  """

// testAll // in Delphi the last y is not supported without ; 
//   """ 
//   begin 
//     if true then z: else begin x: end y:
//   end.
//  """

// testAll // in Delphi the last y is not supported without ; 
//   """ 
//   begin 
//     if true then z: else begin y: foo() z: end
//   end.
//  """

// testAll  
//   """ 
//   begin
//     x := nil + 1 * nil / x + [a(f)..e, g+1] 
//   end.
//  """

// testAll  
//   """ 
//   begin
//     for X := 10 to 11 do
//     case X of
//       A..B,C: F:;
//     else
//     end;
//   end.
//  """

// testAll  
//   """
//   procedure foo;
//   var
//     x: integer;

//     function boo(var x: string): integer;
//     type TMyPRoc = procedure foo; { foo name is accepted }
//     const X = 9;
//     begin
//     end;

//   begin
//   end;

//   begin
//   end.
//  """

testAll  
  """
  begin
    // x := 10;
    y := 10; // Some stuff
  end.
 """