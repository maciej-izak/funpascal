#load "Test.fsx"
open np.BasicParsers
open FParsec

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