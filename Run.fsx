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

// testAll 
//     """program wow;
//     var
//       x: Integer;
//     begin
//       x := 8;
//       if (x = 8) then 
//         WriteLn(x)
//     end.
//     """

// testAll 
//     """program wow;
//     var
//       x: Integer;
//     begin
//       x := 8;
//       if (x = 8) and (x mod 3 = 0) then // bitwise and is used :O
//         WriteLn(x)
//     end.
//     """

// testAll 
//     """program wow;
//     var
//       x: Integer;
//     begin
//       x := 8;
//       if (x = 8) and (x mod 3 = 0) then
//         WriteLn(x)
//       else if x = 7 + 1 then begin
//         WriteLnS('1');
//         WriteLnS('foo');
//       end else
//         WriteLnS('O_o');
//     end.
//     """


(*
.method public static 
	void Main () cil managed 
{
	// Method begins at RVA 0x2050
	// Code size 83 (0x53)
	.maxstack 3
	.entrypoint
	.locals init (
		[0] int32
	)

	// int num = 8;
	IL_0000: ldc.i4.8
	IL_0001: stloc.0
	// if ((num == 8) & (num % 3 == 0))
	IL_0002: ldloc.0
	IL_0003: ldc.i4.8
	IL_0004: ceq
	IL_0006: ldloc.0
	IL_0007: ldc.i4.3
	IL_0008: rem
	IL_0009: ldc.i4.0
	IL_000a: ceq
	IL_000c: and
	// (no C# code)
	IL_000d: brfalse.s IL_0015

	// Console.WriteLine(num);
	IL_000f: ldloc.0
	IL_0010: call void [System.Console]System.Console::WriteLine(int32)

	// if (num == 7 + 1)
	IL_0015: ldloc.0
	IL_0016: ldc.i4.7
	IL_0017: ldc.i4.1
	IL_0018: add
	IL_0019: ceq
	// (no C# code)
	IL_001b: brfalse.s IL_0046

	// Console.WriteLine("1");
	IL_001d: ldstr "1"
	IL_0022: call void [System.Console]System.Console::WriteLine(string)
	// if (num == 9)
	IL_0027: ldloc.0
	IL_0028: ldc.i4.s 9
	IL_002a: ceq
	// (no C# code)
	IL_002c: brfalse.s IL_003a

	// Console.WriteLine("foo");
	IL_002e: ldstr "foo"
	IL_0033: call void [System.Console]System.Console::WriteLine(string)
	// (no C# code)
	IL_0038: br.s IL_003a

	// Console.WriteLine("foo2");
	IL_003a: ldstr "foo2"
	IL_003f: call void [System.Console]System.Console::WriteLine(string)
	// }
	IL_0044: br.s IL_0052

	// Console.WriteLine("O_o");
	IL_0046: ldstr "O_o"
	IL_004b: call void [System.Console]System.Console::WriteLine(string)
	// (no C# code)
	IL_0050: br.s IL_0052

	IL_0052: ret
} // end of method wow::Main
*) // ^^^ IL_0038 ^^^ can be optimized
testAll
    """program wow;
    var
      x: Integer;
    begin
      x := 8;
      if (x = 8) and (x mod 3 = 0) then
        WriteLn(x)
      else if x = 7 + 1 then begin
        WriteLnS('1');
        if x = 9 then
          WriteLnS('foo');
        WriteLnS('foo2');
      end else
        WriteLnS('O_o');
    end.
    """