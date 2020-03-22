module NP.PasStreams

open System
open System.Text
open System.IO
open System.Runtime
open System.Text.Json
open System.Text.Json.Serialization
open NP.PasVar
open NP.PasAst
open NP.BasicParsers
open NP.ParsePas
open NP.PasIl
open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open Microsoft.FSharp.Reflection
open System.Runtime.Serialization
open System.Runtime.Serialization.Json
open MBrace.FsPickler
open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks

let applyParser (parser: Parser<'Result,'UserState>) (stream: CharStream<'UserState>) =
    let reply = parser stream
    if reply.Status = Ok then
        Success(reply.Result, stream.UserState, stream.Position)
    else
        let error = ParserError(stream.Position, stream.UserState, reply.Error)
        Failure(error.ToString(stream), error, stream.UserState)

let testPas p s i = 
    let us = PasState.Create (new PasStream(s)) i
    use stream1 = new CharStream<PasState>(us.stream, Encoding.Unicode)
    stream1.UserState <- us
    stream1.Name <- "some code"
    let result = applyParser pass1Parser stream1
    match result with
    | Success (_,s,_) -> s.stream.SaveToFile()
    | Failure (_,_,s) -> s.stream.SaveToFile()
    us.handleInclude := pass2IncludeHandler
    printfn ">>> SECOND PASS"
    use stream2 = new CharStream<PasState>(us.stream, Encoding.Unicode)
    stream2.UserState <- us
    stream2.Name <- "some code"
    applyParser p stream2

let private compileModule (ProgramAst(name, block)) = //, methods: Method list) =
    let moduleName = match name with | Some n -> n | None -> "Program"
    let moduleNameWithoutExtension = System.IO.Path.GetFileNameWithoutExtension moduleName
    let assemblyBuilder =
        let assemblyName = AssemblyNameDefinition(moduleName, Version(0,0,0,0))
        AssemblyDefinition.CreateAssembly(assemblyName, moduleNameWithoutExtension, ModuleKind.Console)
    let moduleBuilder = assemblyBuilder.MainModule

    let typeBuilder =
        let className = moduleName
        let typeAttributes =
                TypeAttributes.Public
                ||| TypeAttributes.Abstract
                ||| TypeAttributes.Sealed
                ||| TypeAttributes.AutoLayout
                ||| TypeAttributes.AnsiClass
                ||| TypeAttributes.BeforeFieldInit
        TypeDefinition(moduleName, className, typeAttributes, moduleBuilder.TypeSystem.Object)
    moduleBuilder.Types.Add(typeBuilder)
    let methodBuilder = 
        let methodAttributes = MethodAttributes.Public ||| MethodAttributes.Static 
        let methodName = "Main"
        MethodDefinition(methodName, methodAttributes, moduleBuilder.TypeSystem.Void)
    let ilBuilder = IlBuilder(moduleBuilder)
    let bb = ilBuilder.BuildIl(block, moduleName) 
    printfn "bb = %A" bb
    let mainBlock = compileBlock methodBuilder typeBuilder bb
    //mainBlock.Body.InitLocals <- true
    // https://github.com/jbevain/cecil/issues/365
    mainBlock.Body.OptimizeMacros()
    assemblyBuilder.EntryPoint <- mainBlock
    //printfn "%A" 
    (*let methodBuilders = 
        methods 
        |> List.map (compileMethod typeBuilder)
        |> Map.ofList
    let entryPoint = compileEntryPoint moduleBuilder typeBuilder methodBuilders.["main"]
    assemblyBuilder.EntryPoint <- entryPoint
    let v = moduleBuilder.ImportReference(typeof<TargetFrameworkAttribute>.GetConstructor([|typeof<string>|]));
    let c = CustomAttribute(v);
    let sr = moduleBuilder.ImportReference(typeof<string>)
    let ca = CustomAttributeArgument(sr, box ".NETCoreApp,Version=v3.0")
    c.ConstructorArguments.Add(ca)
    assemblyBuilder.CustomAttributes.Add(c)*)
    assemblyBuilder

let testAll s =
    let strToStream (s: string) = s |> Encoding.Unicode.GetBytes |> fun s -> new MemoryStream(s)
    let ast = testPas pascalModule (strToStream s) ""
    match ast with
    | Success (r,_,_) -> 
                        let ad = compileModule(ProgramAst(fst r, Block.Create(snd r)))
                        ad.Write("test.dll")
                        Some(r)
    | _ -> None


let toString (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

let testFile f =
    let s = File.ReadAllText(f)
    let strToStream (s: string) = s |> Encoding.Unicode.GetBytes |> fun s -> new MemoryStream(s)
    let dir = Path.GetDirectoryName f
    let result = testPas pascalModule (strToStream s) dir
    let xmlSerializer = FsPickler.CreateXmlSerializer(indent = true)
    use sw = new StreamWriter(path=dir + string Path.DirectorySeparatorChar + "out.ast")
    match result with
    | Success (r,_,_) -> xmlSerializer.Serialize(sw, r)
    | _ -> ()
    sw.Close()
    ()
    // use sw = new StreamWriter(path=dir + string Path.DirectorySeparatorChar + "out.ast")
    // let b = StringBuilder()
    // bprintf b "%O" result
    // sw.Close()