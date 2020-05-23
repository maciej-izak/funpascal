// Learn more about F# at http://fsharp.org

open System
open NP.CommandLineHandle
open NP.ParsePas
open Argu
open FParsec
open NP

[<EntryPoint>]
let main argv =

    // !!! GetIdentUnsafe
    // !!! CompileDirective

    PasStreams.testAll
      (*"""
      {.$I system.inc}
  type
  LongInt = Integer;
  TPESectionHeader = packed record
    Name: array [0..7] of Char;
    VirtualSize: LongInt;
    VirtualAddress: LongInt;
    SizeOfRawData: LongInt;
    PointerToRawData: LongInt;
    PointerToRelocations: LongInt;
    PointerToLinenumbers: LongInt;
    NumberOfRelocations: Word;
    NumberOfLinenumbers: Word;
    Characteristics: LongInt;
  end;

  THeaders = packed record
    CodeSectionHeader: TPESectionHeader;
  end;

const
  IMGBASE = 12;

var
  h: THeaders;
begin
  WriteLine(IMGBASE + h.CodeSectionHeader.VirtualAddress);
end.
     """*)
      """
{$I system.inc}
{$I Common.inc}
{$I Scanner.inc}
{$I CodeGen.inc}
{$I Linker.inc}
{$I Parser.inc}

procedure ChangeExt(const InStr, Ext: TString; var OutStr: TString);
var
  i, DotPos: Integer;
begin
DotPos := 0;

for i := Length(InStr) downto 1 do
  if InStr[i] = '.' then
    begin
    DotPos := i;
    Break;
    end;

OutStr := InStr;
if DotPos > 0 then SetLength(OutStr, DotPos);
OutStr := OutStr + Ext;
end;




var
  ProgramName, ExeName: TString;
  OutFile: TOutFile;

begin
  InitSystem;
  WriteLn();
  WriteLn('XD Pascalx for Windows ', VERSIONMAJOR, '.', VERSIONMINOR);
  WriteLn('Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov');

  if ParamCount < 3 then
  begin
    WriteLn('Usage: xdpw <file.pas>');
    Halt(1);
  end;

  ProgramName := ParamStr(3);


  // Compile
  WriteLn('Compiling ', ProgramName);

  FillOperatorSets;
  FillTypeSets;

  IsConsoleProgram := 1;  // Console program by default

  ZeroAll;
  FillChar(ImportSection, SizeOf(ImportSection), #0);

  InitializeScanner(ProgramName);
  CompileProgram;
  FinalizeScanner;

  FillHeaders(CodeSize, InitializedGlobalDataSize, UninitializedGlobalDataSize);

  Relocate(IMGBASE + Headers.CodeSectionHeader.VirtualAddress,
           IMGBASE + Headers.DataSectionHeader.VirtualAddress,
           IMGBASE + Headers.DataSectionHeader.VirtualAddress + InitializedGlobalDataSize);


  // Write output file
  ChangeExt(ProgramName, 'exe', ExeName);
  Assign(OutFile, ExeName);
  Rewrite(OutFile, 1);

  if IOResult <> 0 then
    Error('Unable to open output file ' + ExeName);

  BlockWrite(OutFile, Headers, SizeOf(Headers));
  Pad(OutFile, SizeOf(Headers), FILEALIGN);

  BlockWrite(OutFile, ImportSection, SizeOf(ImportSection));
  Pad(OutFile, SizeOf(ImportSection), FILEALIGN);

  BlockWrite(OutFile, InitializedGlobalData, InitializedGlobalDataSize);
  Pad(OutFile, InitializedGlobalDataSize, FILEALIGN);

  BlockWrite(OutFile, Code, CodeSize);
  Pad(OutFile, CodeSize, FILEALIGN);

  Close(OutFile);


  WriteLn('Compilation complete. Code size: ', CodeSize, ' bytes. Data size: ', InitializedGlobalDataSize + UninitializedGlobalDataSize, ' bytes.');

end.
      """

    |> function
       | Success _ -> printfn "Compilation success!"
       | Failure(s,_,_) -> printfn "%s" s
    0
    (*let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<NpArguments>(programName = "ls", errorHandler = errorHandler)
    let results = parser.ParseCommandLine argv
    let files = results.GetResult(Files, [])
    for f in files do
        printfn "%A" f
    //run commentLine "// foo"
    0 // return an integer exit code*)
