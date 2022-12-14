// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov

// Main program



program XDPW;


{$APPTYPE CONSOLE}
{$I-}
{$H-}


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
WriteLn;
WriteLn('XD Pascal for Windows ', VERSIONMAJOR, '.', VERSIONMINOR);
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

