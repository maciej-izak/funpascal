#load "Test.fsx"

open np.BasicParsers
open FParsec
open np.PasStreams
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
