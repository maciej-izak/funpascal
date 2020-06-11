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

let errorFmt = sprintf "[Error] %s(%d,%d) %s"
let warningFmt = sprintf "[Warning] %s(%d,%d) %s"
let addMsg (items: List<string>) fmt msg (pos: Position) = items.Add(fmt pos.StreamName (int pos.Line) (int pos.Column) msg)

type PasSubStream = {index: int; length: int}

type CompilerMessages = {
    errors: List<string>
    warnings: List<string>
}   with
    member self.HasError = self.errors.Count > 0

type LabelDef = {name: string; mutable stmtPoint: bool}

type SymbolDef =
    | Label of LabelDef

type BlockDef() = class
    member val symbols: Dictionary<string, SymbolDef> = new Dictionary<_,_>() with get, set
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

type Directive =
     | Include of string
     | IOCheck of bool
     | LongString of bool
     | IfDef of (int64 * int64) option
     | EndIf
     | AppType of AppType

[<ReferenceEquality>]
type Comment =
     | Directive of Directive
     | TestEnv of string * string list
     | Regular
     | Macro of (MacroId * Macro)

type CompilerPassId =
    | InitialPassId
    | MainPassId

exception InternalError of string

type ICompilerPassGeneric =
    abstract member Defines: HashSet<string>
    abstract member PosMap: Dictionary<obj, Position>
    abstract member GetPos: obj -> Position option

type ICompilerPass =
    inherit ICompilerPassGeneric
    abstract member Id: CompilerPassId
    abstract member HandleComment: CharStream<PasState> * Comment -> unit

and PasState = {
    pass: ICompilerPass
    stream: PasStream
    incPath: string
    incStack: (int64 * string * CharStreamState<PasState>) Stack
    ifDefGoto: Dictionary<int64 * int64, int64>
    moduled: ModuleDef
    messages: CompilerMessages
    testEnv: Dictionary<string, string list>
}   with
    // TODO as parser ?
    static member HandleComment c = fun (stream: CharStream<PasState>) ->
        stream.UserState.pass.HandleComment(stream, c)
        Reply(())

    member self.NewMessage (items: List<string>) fmt (o: obj) msg =
        let addMsg = addMsg items fmt msg
        match o with
        | :? Position -> unbox<Position> o |> addMsg
        | _ ->
            match self.pass.GetPos o with
            | Some pos -> addMsg pos
            | _ -> raise (InternalError "2020061001")

    member self.NewInitialPassMessage (items: List<string>) fmt (o: obj) msg =
        match self.pass.Id with
        | InitialPassId -> self.NewMessage items fmt o msg
        | _ -> ()

    member self.NewError = self.NewInitialPassMessage self.messages.errors errorFmt
    member self.NewWarning = self.NewInitialPassMessage self.messages.warnings warningFmt

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

type GenericPass() =
    let defines = HashSet<_>(StringComparer.OrdinalIgnoreCase)
    let posMap = Dictionary<_,_>()
    interface ICompilerPassGeneric with
        member _.Defines = defines
        member _.PosMap = posMap
        member _.GetPos o =
            match posMap.TryGetValue o with
            | true, v -> Some v
            | _ -> None

type InitialPass() =
    inherit GenericPass()
    let id = InitialPassId
    let ifDefStack = Stack<(int64 * int64) option>()

    let includeFile us file = us.stream.AddInc file us.incPath

    let macro us (mId, m) =
        let addStr = us.stream.AddStr (mId.ToString())
        match m with
        | CompilerInfoInt i -> addStr (string i)
        | CompilerInfoStr s -> addStr s

    let ifDef = ifDefStack.Push

    let endIf comment (stream: CharStream<PasState>) =
        let us = stream.UserState
        match ifDefStack.TryPop() with
        | true, Some pos -> us.ifDefGoto.Add(pos, stream.Index)
        | true, _ -> ()
        | _ -> us.NewError (box comment) "Unbalanced '{$ENDIF}'"

    interface ICompilerPass with
        member _.Id = id

        member _.HandleComment(stream, comment) =
            match comment with
            | Directive d ->
                match d with
                | Include f -> includeFile stream.UserState f
                | IfDef notDefined -> ifDef notDefined
                | EndIf -> endIf comment stream
                | _ -> ()
            | Macro m -> macro stream.UserState m
            | _ -> ()

type MainPass() =
    inherit GenericPass()
    let id = MainPassId

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
        | Some pos -> stream.Seek(stream.UserState.ifDefGoto.[pos])
        | _ -> ()

    interface ICompilerPass with
        member _.Id: CompilerPassId = id

        member _.HandleComment(stream, comment) =
            match comment with
            | Directive d ->
                match d with
                | Include f -> redirectParserTo stream f
                | IfDef notDefined -> ifDef stream notDefined
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
            ifDefGoto = Dictionary<_,_>()
            moduled = ModuleDef()
            messages = { errors = List<string>(); warnings = List<string>() }
            testEnv = Dictionary<_,_>(StringComparer.OrdinalIgnoreCase)
        }

let opp = OperatorPrecedenceParser<ExprEl,unit,PasState>()
let popp = OperatorPrecedenceParser<ExprEl,unit,PasState>()