[<AutoOpen>]
module Pas.Var

open System
open System.IO
open System.Text
open System.Collections.Generic
open FParsec

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

    member val Errors = List<string>()
    member val Warnings = List<string>()
    member val PosMap = Dictionary<obj, Position>()

    member self.HasError = self.Errors.Count > 0
    member self.GetPos(o: obj): Position option =
        match self.PosMap.TryGetValue o with
        | true, v -> Some v
        | _ -> None

    member self.AddMsg (pos: Position) msg =
        match msg with
        | MsgError(_, msg) -> self.Errors.Add(errorFmt pos.StreamName (int pos.Line) (int pos.Column) msg)
        | MsgWarning(_, _, msg) -> self.Warnings.Add(warningFmt pos.StreamName (int pos.Line) (int pos.Column) msg)

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

[<ReferenceEquality>]
type Comment =
     | Directive of Directive
     | TestEnvVar of string * string list
     | Regular
     | Macro of (MacroId * Macro)

type CompilerPassId =
    | InitialPassId
    | MainPassId

exception InternalError of string

type ICompilerPassGeneric =
    abstract member Defines: HashSet<string>

type TestEnvDict = Dictionary<string, string list>

type PasTestState = {
    testEnv: TestEnvDict
}   with
    // TODO as parser ?
    static member HandleComment _ = preturn()
    static member Create() = { testEnv = TestEnvDict(StringComparer.OrdinalIgnoreCase) }

type ICompilerPass =
    inherit ICompilerPassGeneric
    abstract member Id: CompilerPassId
    abstract member HandleComment: CharStream<PasState> * Comment -> unit

and PasState = {
    pass: ICompilerPass
    stream: PasStream
    incPath: string
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

    member self.NewMsg (o: obj) msg =
        match self.pass.Id with
        | InitialPassId -> self.NewMessage o msg
        | _ -> ()

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

and PasStream(s: Stream) = class 
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

    member self.AddInc fileName searchPath =
        //printfn "AddInc %A %A" fileName searchPath
        if File.Exists fileName then fileName else Path.Combine(searchPath, fileName)
        |> File.ReadAllText
        |> self.AddStr fileName

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

type PascalProject = {
    File: string
    FileName: string
    FilePath: string
    OutPath: string
    Exe: string option
    Name: string
    Defines: string list
}

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

    let includeFile us file = us.stream.AddInc file us.incPath

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
                | Include f -> includeFile stream.UserState f
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

type PasState with
    static member Create pass s ip =
        {
            pass = pass
            stream = s
            incPath = ip
            incStack = Stack()
            defGoto = Dictionary<_,_>()
            moduled = ModuleDef()
            messages = CompilerMessages()
        }

let opp = OperatorPrecedenceParser<ExprEl,unit,PasState>()
let popp = OperatorPrecedenceParser<ExprEl,unit,PasState>()