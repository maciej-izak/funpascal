[<AutoOpen>]
module Pas.Var

open System
open System.IO
open System.Text
open Microsoft.FSharp.Collections
open System.Collections.Generic
open FParsec
open Fake.IO
open Fake.IO.FileSystemOperators
open dnlib.DotNet

[<Literal>]
let DefaultBlockSize = 4096L;//196608L // 3*2^16 = 200k
[<Literal>]
let DefaultByteBufferLength = 4096L;
[<Literal>]
let MinimumByteBufferLength = 128L

type PasSubStream = {index: int; length: int}

type CompilerMessages() =

    let errorFmt = sprintf "[Error] %s(%d,%d) %s"
    let warningFmt = sprintf "[Warning] %s(%d,%d) %s"
    let fatalFmt = sprintf "[Fatal Error] %s"

    member val Errors = List<string>()
    member val Warnings = List<string>()
    member val PosMap = Dictionary<obj, Position>(HashIdentity.Reference)

    member self.HasError = self.Errors.Count > 0
    member self.GetPos(o: obj): Position option =
        match self.PosMap.TryGetValue o with
        | true, v -> Some v
        | _ -> None

    member self.AddMsg (pos: Position) msg =
        match msg with
        | MsgError(_, msg) -> self.Errors.Add(errorFmt pos.StreamName (int pos.Line) (int pos.Column) msg)
        | MsgWarning(_, _, msg) -> self.Warnings.Add(warningFmt pos.StreamName (int pos.Line) (int pos.Column) msg)

    member self.AddFatal msg = self.Errors.Add(fatalFmt msg)

type LabelDef = {name: string; mutable stmtPoint: bool}

type SymbolDef =
    | Label of LabelDef

type BlockDef() = class
    member val symbols: Dictionary<string, SymbolDef> = Dictionary<_,_>() with get, set
  end

type ModuleDef() = class
    member val block = BlockDef()
  end

type Macro =
    | CompilerInfoStr of string
    | CompilerInfoInt of int

type MacroId =
    { name: string; line: int; column: int }
    override self.ToString() = sprintf "%s (%d, %d)" self.name self.line self.column

type AppType = | Console | GUI

type IfDefBranch =
    | IfDefBranch
    | ElseBranch
    | EndIfBranch

    override self.ToString() =
        match self with
        | IfDefBranch -> "IFDEF"
        | ElseBranch -> "ELSE"
        | EndIfBranch -> "ENDIF"

type IfDefPos = {Pos: Position ; Defined: bool; Branch: IfDefBranch }

type Directive =
     | Include of string
     | IOCheck of bool
     | LongString of bool
     | IfDef of IfDefPos
     | EndIf
     | Else
     | AppType of AppType

type Comment =
     | Directive of Directive
     | TestEnvVar of string * string list
     | Regular
     | Macro of (MacroId * Macro)

type CompilerPassId =
    | InitialPassId
    | MainPassId
    | TestPassId

type ICompilerPassGeneric =
    abstract member Defines: HashSet<string>

type TestEnvDict = Dictionary<string, string list>

type PasTestState = {
    isUnit: bool 
    testEnv: TestEnvDict
}   with
    // TODO as parser ?
    static member HandleComment _ = preturn()
    static member Create() = { isUnit = false; testEnv = TestEnvDict(StringComparer.OrdinalIgnoreCase) }

type PascalModule = {
    Messages: CompilerMessages
    Obj: obj option
    Init: IMethod
    Fini: IMethod
}

module PascalModule =
    let invalid m = { Messages = m; Obj = None; Init = null; Fini = null }
    let main = invalid
    let unit m (ctx, init, fini) = { Messages = m; Obj = Some(box ctx); Init = init; Fini = fini }

type PascalModules = {
    Order: List<string>
    Items: Dictionary<string, PascalModule>
} with
    static member Create() = { Order = List<_>(); Items = Dictionary<_,_>() }
    member self.Clear() = self.Order.Clear() ; self.Items.Clear()
    member self.Add f m =
        match self.Items.TryAdd(f, m) with
        | true -> self.Order.Add f
        | false -> raise (InternalError "2020083001")

    member private self.DoMessages cm = self.Ordered |> Seq.collect(fun m -> cm m.Messages)
    member self.Warnings = self.DoMessages (fun cm -> cm.Warnings)
    member self.Errors = self.DoMessages (fun cm -> cm.Errors)
    member self.Ordered = seq { for f in self.Order do self.Items.[f] }
    
type UnitSearchResult =
    | UnitCompiled of obj
    | UnitFound of string
    | UnitNotFound
    
type PascalProject = {
    File: string
    FileName: string
    FilePath: string
    OutPath: string
    Exe: string option
    Name: string
    NameSuffix: string
    Defines: string list
    UnitFiles: string list
    IncludeFiles: string list
    Modules: PascalModules
}
 with
    member self.OutName = self.OutPath </> self.Name + self.NameSuffix
    static member Create(mainFile, unitFiles, includeFiles, define) =
        let file, filePath, fileName =
            let fileName = Path.GetFileName (mainFile: string)
            let p = Path.GetDirectoryName mainFile
            let filePath = if Path.IsPathFullyQualified p then p else Path.Combine(Directory.GetCurrentDirectory(), p)
            Path.Combine(filePath, fileName), filePath, fileName
        let outPath = filePath </> "out"
        let mapDirectories = List.map (fun (p: string) -> if Path.IsPathFullyQualified p then p else Path.combine filePath p)
        Directory.ensure outPath
        {
            File = file
            FileName = fileName
            FilePath = filePath
            OutPath = outPath
            Exe = None
            Name = Path.GetFileNameWithoutExtension mainFile
            NameSuffix = ""
            Defines = Option.toList define
            UnitFiles = ""::unitFiles |> List.rev |> mapDirectories // "" = search in module directory
            IncludeFiles = ""::includeFiles |> List.rev |> mapDirectories // "" = search in module directory
            Modules = PascalModules.Create()
        }

    // TODO some map cache / optimization?
    member self.FindInc name = self.IncludeFiles |> List.tryPick (Directory.tryFindFirstMatchingFile name) 
    member self.FindUnit name =
        let unitFile = self.UnitFiles |> List.tryPick (Directory.tryFindFirstMatchingFile name)
        match unitFile with
        | Some f ->
            match self.Modules.Items.TryGetValue f with
            | true, m ->
                match m.Obj with
                | Some o -> UnitCompiled o
                | _ -> raise(InternalError "2020083002") // possible only for main pascal file, so raise IE
            | _ -> UnitFound f
        | _ -> UnitNotFound
        
    member self.AddModule f m = self.Modules.Add f m
        
    member self.Warnings = self.Modules.Warnings
    member self.Errors = self.Modules.Errors
    member self.HasError = Seq.exists (fun v -> v.Messages.HasError) self.Modules.Items.Values
    member self.AddFatal msg =
        match self.Modules.Items.TryGetValue self.File with
        | true, m -> m.Messages.AddFatal msg
        | _ -> raise(InternalError "2020083000") // fatal error should be added at the end of compilation process

type ICompilerPass =
    inherit ICompilerPassGeneric
    abstract member Id: CompilerPassId
    abstract member HandleComment: CharStream<PasState> * Comment -> unit

and PasState = {
    pass: ICompilerPass
    proj: PascalProject
    fileName: string
    stream: PasStream
    incStack: (int64 * string * CharStreamState<PasState>) Stack
    defGoto: Dictionary<Position, Position>
    moduled: ModuleDef
    messages: CompilerMessages
}   with
    // TODO as parser ?
    static member HandleComment c = fun (stream: CharStream<PasState>) ->
        stream.UserState.pass.HandleComment(stream, c)
        Reply(())

    member private self.NewMessage (o: obj) =
        match o with
        | :? Position -> self.messages.AddMsg(unbox<Position> o)
        | _ ->
            match self.messages.GetPos o with
            | Some pos -> self.messages.AddMsg pos
            | _ -> raise (InternalError "2020061001")

    member self.NewMsg (o: obj) msg = self.NewMessage o msg

    member self.HasError = self.messages.Errors.Count > 0 || self.proj.HasError

and IncludeHandle = string -> CharStream<PasState> -> Reply<unit>
and MacroHandle = MacroId * Macro -> CharStream<PasState> -> Reply<unit>

(*
  ref (
    fun (s: string) 
      -> (
          fun (stream: CharStream<PasState>) ->
            stream.UserState.stream.AddInc s
            Reply(())
         )
  ) 
*)

and [<AllowNullLiteral>]
    PasStream(s: Stream) = class 
    inherit Stream()
    let mainStream = s
    let mutable lastSubStream = {index=0;length=int mainStream.Length / 2};
    let mutable stream = mainStream
    let finalStream = new MemoryStream(int mainStream.Length)
    let mutable read: (byte[] * int * int) -> int = 
        fun (buffer, offset, count) ->
            let readed = stream.Read(buffer, offset, count)
            finalStream.Write(buffer, offset, readed)
            readed
   
    let streams: Dictionary<string, PasSubStream> = new Dictionary<_,_>()

    override _.get_CanRead() : bool = true
    override _.get_CanWrite() : bool = false
    override _.get_CanSeek() : bool = true
    override _.get_Length() : int64 = stream.Length 
    override _.get_Position() : int64 = stream.Position
    override _.set_Position(value: int64) : unit =
        stream.Position <- value

    override _.Flush() : unit = stream.Flush()

    override _.Seek(offset: int64, origin: SeekOrigin) : int64 =
        stream.Seek(offset, origin)
        
    override _.SetLength(value: int64) : unit = ()
    override _.Read(buffer: byte [], offset: int, count: int) : int = read(buffer, offset, count)

    override _.Write(buffer: byte [], offset: int, count: int) : unit = () 

    override _.Close() =
        if stream = mainStream then
            finalStream.WriteByte(0uy)
            finalStream.WriteByte(0uy)
            finalStream.Flush()
            stream.Close()
            stream <- finalStream
            read <- fun (buffer, offset, count) -> stream.Read(buffer, offset, count)
            stream.Seek(0L, SeekOrigin.Begin) |> ignore
        else
            stream.Close()

    member self.AddInc filePath fileName = File.ReadAllText filePath |> self.AddStr fileName

    member _.AddStr strName str =
        //printfn "AddStr %s" str
        let addInc() =
            let pos = finalStream.Position
            finalStream.Seek(finalStream.Length, SeekOrigin.Begin) |> ignore
            let str = "\013" + str + "\000"
            let bytes = Encoding.Unicode.GetBytes(str)
            finalStream.Write(bytes, 0, bytes.Length)
            let subStream =
                {
                    index = lastSubStream.index + lastSubStream.length + 2// if lastSubStream.index = 0 then 1 else 2
                    length = str.Length - 2
                }
            streams.Add(strName, subStream)
            lastSubStream <- subStream
            finalStream.Seek(pos, SeekOrigin.Begin) |> ignore
            //printfn "check %s = %A (%A)" strName (streams.ContainsKey(strName)) subStream
        if streams.ContainsKey(strName) then
            () // TODO should be InternalError for macros
        else addInc()

    member _.FindStream s = streams.[s]

    member _.SaveToFile() =
        use f = new FileStream("final.pas", FileMode.Create, FileAccess.Write)
        finalStream.WriteTo(f)

    member _.EndInc() = () 
  end
  
type GenericPass(proj) =
    let defines =
        let defs = HashSet<_>(StringComparer.OrdinalIgnoreCase)
        proj.Defines
        |> List.filter ((<>) "")
        |> List.iter (defs.Add >> ignore)
        defs

    interface ICompilerPassGeneric with
        member _.Defines = defines

type InitialPass(proj) =
    inherit GenericPass(proj)
    let id = InitialPassId
    let ifDefStack = Stack<IfDefPos>()

    let includeFile comment (stream: CharStream<PasState>) fileName =
        let us = stream.UserState
        match us.proj.FindInc fileName with
        | Some filePath -> us.stream.AddInc filePath fileName
        | _ -> ``Error: Cannot find file '%O'`` fileName |> us.NewMsg (box comment)
        
    let macro us (mId, m) =
        let addStr = us.stream.AddStr (mId.ToString())
        match m with
        | CompilerInfoInt i -> addStr (string i)
        | CompilerInfoStr s -> addStr s

    let ifDef = ifDefStack.Push

    let endIfElse comment (stream: CharStream<PasState>) branch =
        let us = stream.UserState
        let handleElse defined =
            let pos = us.messages.GetPos(box comment).Value
            ifDefStack.Push{ Pos=pos; Defined=defined; Branch=ElseBranch }
        let doError() = ``Error: Unbalanced '{$%O}'`` branch |> us.NewMsg (box comment)
        match ifDefStack.TryPop() with
        | true, ifDefPos ->
            match branch with
            | EndIfBranch ->
                // for Defined do nothing
                if not ifDefPos.Defined then us.defGoto.Add(ifDefPos.Pos, stream.Position)
            | ElseBranch ->
                match ifDefPos with
                | {Branch=IfDefBranch; Defined=defined; Pos=pos} ->
                    if not defined then us.defGoto.Add(pos, stream.Position)
                    handleElse (not defined)
                | {Branch=ElseBranch} ->
                    ifDefStack.Push ifDefPos // try generate less errors
                    doError()
                | _ -> doError()
            | _ -> raise (InternalError "202006150021")
        | _ -> doError()

    member self.IfDefStack = ifDefStack

    interface ICompilerPass with
        member _.Id = id

        member _.HandleComment(stream, comment) =
            match comment with
            | Directive d ->
                match d with
                | Include f -> includeFile comment stream f
                | IfDef notDefined -> ifDef notDefined
                | EndIf -> endIfElse comment stream EndIfBranch
                | Else -> endIfElse comment stream ElseBranch
                | _ -> ()
            | Macro m -> macro stream.UserState m
            | _ -> ()

type MainPass(proj) =
    inherit GenericPass(proj)
    let id = MainPassId
    let ifDefStack = Stack<bool>()

    let redirectParserTo (stream: CharStream<PasState>) name =
        let offset = stream.Position.Index;
        stream.UserState.incStack.Push(offset, name, CharStreamState(stream))
        name
        |> stream.UserState.stream.FindStream
        |> fun pss ->
                    stream.Seek(int64 pss.index)
                    stream.Name <- name
                    stream.SetLineBegin_WithoutCheckAndWithoutIncrementingTheStateTag 0L
                    stream.SetLine_WithoutCheckAndWithoutIncrementingTheStateTag 0L
                    stream.RegisterNewline()

    let ifDef (stream: CharStream<PasState>) = function
        | {Pos=pos; Defined=false} ->
            let gotoPos = stream.UserState.defGoto.[pos]
            stream.Seek(gotoPos.Index) // seek will not update Line, do it below
            let lineOffset = (gotoPos.Line - pos.Line) - 1L
            if lineOffset > 0L then
                stream.RegisterNewlines(lineOffset, gotoPos.Column - 1L)
        | _ -> ()

    interface ICompilerPass with
        member _.Id: CompilerPassId = id

        member _.HandleComment(stream, comment) =
            let commentPos() = match stream.UserState.messages.GetPos (box comment) with
                               | Some v -> v
                               | None -> raise (InternalError "2020061402")
            match comment with
            | Directive d ->
                match d with
                | Include f -> redirectParserTo stream f
                | IfDef defPos ->
                    ifDefStack.Push defPos.Defined
                    ifDef stream defPos
                | Else ->
                    match ifDefStack.TryPop() with
                    | true, defined -> ifDef stream { Pos=commentPos(); Defined=not defined; Branch=ElseBranch }
                    | _ -> raise (InternalError "2020061401") // lack should be found in pass1
                | _ -> ()
            | Macro (mId, _) -> redirectParserTo stream (mId.ToString())
            | _ -> ()

type TestPass(proj) =
    inherit GenericPass(proj)
    let id = TestPassId

    let unsupported() = raise (Exception "part of directives are not supported in test pass")

    interface ICompilerPass with
        member _.Id: CompilerPassId = id

        member _.HandleComment(_, comment) =
            match comment with
            | Directive d ->
                match d with
                | Include _ | IfDef _ | Else -> unsupported()
                | _ -> ()
            | Macro _ -> unsupported()
            | _ -> ()

type PasState with
    static member Create pass s proj fileName =
        {
            pass = pass
            proj = proj
            fileName = fileName
            stream = s
            incStack = Stack()
            defGoto = Dictionary<_,_>()
            moduled = ModuleDef()
            messages = CompilerMessages()
        }

let opp = OperatorPrecedenceParser<ExprEl,unit,PasState>()
let popp = OperatorPrecedenceParser<ExprEl,unit,PasState>()