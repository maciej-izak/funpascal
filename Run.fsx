#load "Test.fsx"

open NP.BasicParsers
open FParsec
open NP.PasStreams
open System.Reflection
open System.Text.Json

// testAll  
//   """
//   begin
//     // x := 10;
//     y := 10; // Some stuff
//   end.
//  """

// testAll // ! really bad error msg !  -> now is good :D
//   """
//   begin
//     Gen(Byte(w a nd $F + F));
//   end.
//  """

//testFile @"C:\_projects\newpascal\xdpw\source\XDPW.pas"

// testAll 
//     """program wow;
//     var
//       y: Byte;
//       x: Integer;
//     begin
//       y := 10;
//       x := (1+1)*y / 7;
//       y := x;
//       WriteLn(x+8+y-12);
//       WriteLn(3 mod 2 shl 4);
//     end.
//     """

testAll 
    """program wow;
    var
      x: Integer;
    begin
      x := 1*8;
      if x = 8 then 
        WriteLn(1)
    end.
    """