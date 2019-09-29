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

testAll 
  """
  const A: TArray = (1,2,3);
  const R: TRec = (a: 12; b:(c:'aaa'));
  type f = a..f;
  const S: TS = [aa..bb, cc, dd, ee..ff];
  begin end.
 """
  //