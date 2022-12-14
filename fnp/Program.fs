// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Collections.Generic
open Fake.Core
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open FNP.CommandLineHandle
open FNP.PasStreams
open Pas
open Argu
open FNP

let writeRuntimeConfig (proj: PascalProject) =
    File.WriteAllText(proj.OutName + ".runtimeconfig.json",
                                  """
{
  "runtimeOptions": {
    "tfm": "net7.0",
    "rollForward": "LatestMinor",
    "framework": {
      "name": "Microsoft.NETCore.App",
      "version": "7.0.0"
    }
  }
}
                                  """)

[<Literal>]
// TODO move to config
let ROOT_PATH = @"..\..\..\..\"
[<Literal>]
let DOTNET32_FILENAME = ROOT_PATH + @"paket-files\download.visualstudio.microsoft.com\dotnet32\dotnet.exe"
[<Literal>]
let RTL_DIR = ROOT_PATH + @"rtl"
[<Literal>]
let INC_DIR = ROOT_PATH + @"test\xdpw"

let handleTest proj testCase isError =
    match isError, testCase.FailExpected with
    | false, false | true, true -> Ok()
    | true, false -> Error("compiler error")
    | false, true -> Error("compiler success")
    |> if testCase.FailExpected then id
       else
        function
        | Ok() ->
            if proj.Exe.IsNone then raise (InternalError "2020061301")
            let result =
                CreateProcess.fromRawCommand DOTNET32_FILENAME ["exec"; proj.Exe.Value]
                |> CreateProcess.redirectOutput
                |> Proc.run
            File.WriteAllText(proj.OutName + ".elg", result.Result.Output)
            if result.ExitCode = testCase.Result then
                Ok()
            else
                Error <| sprintf "exit code = %d (expected %d)" result.ExitCode testCase.Result
        | id -> id
    |>  function
        | Ok() -> sprintf "OK\t%s%s" proj.Name testCase.DefSuffix
        | Error(msg) -> sprintf "FAIL\t%s%s\t\t%s" proj.Name testCase.DefSuffix msg

let doFullCompilation proj (testCase: TestCase option) logh =
    let handleTest(proj, isError) =
        if testCase.IsSome then
            handleTest proj testCase.Value isError
        else ""
    let proj = {proj with NameSuffix = if testCase.IsSome then testCase.Value.DefSuffix else ""}
    doPas proj proj.File
    |> function
       | Some outName ->
           Seq.iter (fprintfn logh "%s") proj.Warnings
           fprintfn logh "Compilation success!"
           writeRuntimeConfig proj
           { proj with Exe = Some outName}, false
       | None ->
           proj.AddFatal (sprintf "Cannot compile project '%s'" proj.FileName)
           Seq.iter (fprintfn logh "%s") proj.Warnings
           Seq.iter (fprintfn logh "%s") proj.Errors
           proj, true
    |> handleTest

let tryCompileFile doTest mainFile =
    let rtl = [Path.GetFullPath RTL_DIR]
    let inc = [Path.GetFullPath INC_DIR]
    let proj = PascalProject.Create(mainFile, rtl, inc, None)
    let handle defSuffix =
        if doTest then
            let logFile = proj.OutPath </> sprintf "%s%s.log" proj.Name defSuffix
            new StreamWriter(path=logFile) :> TextWriter
        else stdout
    if doTest then
        let doCompile testCase =
            let proj = PascalProject.Create(mainFile, rtl, inc, testCase.Define)
            lazy using (handle testCase.DefSuffix) (doFullCompilation proj (Some testCase))
        match prepareTest proj with
        | Ok(Some cases) ->
            cases
            |> List.map doCompile // first defines has meaning see handleTest
            |> Some
        | Ok None -> None // ignore, probably unit
        | _ -> Some [lazy sprintf "TEST ERROR : %s" proj.Name]
    else doFullCompilation proj None stdout |> ignore; None
    
module Async =
    let ParallelThrottle throttle workflows = Async.Parallel(workflows, throttle)

[<EntryPoint>]
let main argv =
    // command line arguments
    let results =
        let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
        ArgumentParser
            .Create<CLIArguments>(programName="funpascal", errorHandler=errorHandler)
            .ParseCommandLine(inputs = argv, raiseOnUsage = true)

    match results.TryGetResult(Files) with
    | Some pasFiles ->
        match pasFiles with
        | [f] -> f
        | _ -> failwith "Multiply files not supported"
        |> tryCompileFile false |> ignore
    | None ->
        // only proper for tests
        if results.Contains(TestAll) then
            match results.TryGetResult(TestAll) with
            | Some testDir ->
                let sw = System.Diagnostics.Stopwatch()
                sw.Start()
                !! (testDir </> "*.pas") ++ (testDir </> "*.pp")
                |> Seq.map (fun t -> async { return tryCompileFile true t })
                |> Async.ParallelThrottle Environment.ProcessorCount
                |> Async.RunSynchronously
                |> Array.choose id
                |> List.ofArray
                |> List.concat
                |> List.map (fun l -> async { return l.Value })
                |> Async.ParallelThrottle Environment.ProcessorCount
                |> Async.RunSynchronously
                |> Array.filter String.isNotNullOrEmpty // possible for modules
                |> Array.sort
                |> Array.partition (String.startsWith "OK")
                |> fun(ok, fails) ->
                    let logDir = testDir </> ".."
                    File.WriteAllLines(logDir </> "OK.log", ok)
                    match fails.Length with
                    | 0 -> printfn "No regression found! Yay :D"
                    | _ -> fails |> Array.iter (printfn "%s")
                    File.WriteAllLines(logDir </> "ERROR.log", fails)
                sw.Stop()
                printfn "[TIME ELAPSED '%O']" (sw.Elapsed) 
            | _ -> failwith "No proper command found"
        else if results.Contains(Test) then
            match results.TryGetResult(Test) with
            | Some testFile -> tryCompileFile true testFile |> ignore
            | _ -> failwith "No proper command found"
        else if results.Contains(TestParser) then
            let proj = PascalProject.Create("test.pas", [], [], None)
            FParsec.CharParsers.runParserOnString statement (PasState.Create (TestPass(proj)) null proj "test.pas") "test"
                "TFoo(pp)()(10)('fooo',9.0);"
//                """
//uses Test;
//
//type
//  TFoo = record
//  end;
//
//begin
//end.
//"""
//              """
//unit x;
//
//interface
//
//uses
//  System.SysUtils;
//
//type
//  TFoo = record
//  end;
//
//function foo: integer;
//begin
//end;
//
//implementation
//
//var
//  x: integer;
//finalization
//  if true then
//end.
//"""
            |> printfn "%A"
    0