[<AutoOpen>]
module NP.PasVar

open System.IO
open System.Text
open System.Collections.Generic
open FParsec
open PasAst

[<Literal>]
let DefaultBlockSize = 4096L;//196608L // 3*2^16 = 200k
[<Literal>]
let DefaultByteBufferLength = 4096L;
[<Literal>]
let MinimumByteBufferLength = 128L

type PasSubStream = {index: int; length: int} 

type LabelDef = {name: string; mutable stmtPoint: bool}

type SymbolDef =
    | Label of LabelDef

type BlockDef() = class
    member val symbols: Dictionary<string, SymbolDef> = new Dictionary<_,_>() with get, set
  end

type ModuleDef() = class
    member val block = BlockDef()
  end

type PasState = {
  stream: PasStream
  incPath: string
  incStack: (int64 * string * CharStreamState<PasState>) Stack
  handleInclude: IncludeHandle ref
  moduled: ModuleDef
} 

and IncludeHandle = string -> CharStream<PasState> -> Reply<unit>

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

    member _.AddInc fileName searchPath =
        printfn "AddInc %A %A" fileName searchPath
        let addInc() =    
            let pos = finalStream.Position
            finalStream.Seek(finalStream.Length, SeekOrigin.Begin) |> ignore
            let fn= if File.Exists fileName then fileName else Path.Combine(searchPath, fileName)
            let str = "\013" + File.ReadAllText(fn) + "\000"
            let bytes = Encoding.Unicode.GetBytes(str)
            finalStream.Write(bytes, 0, bytes.Length)
            let subStream = 
                {
                    index = lastSubStream.index + lastSubStream.length + 2// if lastSubStream.index = 0 then 1 else 2
                    length = str.Length - 2 
                }
            streams.Add(fileName, subStream)
            lastSubStream <- subStream 
            finalStream.Seek(pos, SeekOrigin.Begin) |> ignore
            printfn "check %s = %A (%A)" fileName (streams.ContainsKey(fileName)) subStream
        if streams.ContainsKey(fileName) then
            ()
        else addInc()

    member _.FindStream s = streams.[s]

    member _.SaveToFile() =
        use f = new FileStream("final.pas", FileMode.Create, FileAccess.Write)
        finalStream.WriteTo(f)

    member _.EndInc() = () 
  end

let pass1IncludeHandler s =  
    fun (stream: CharStream<PasState>) ->
        stream.UserState.stream.AddInc s stream.UserState.incPath
        Reply(())

let pass2IncludeHandler s =  
    fun (stream: CharStream<PasState>) ->
        //stream.Line
        let offset = stream.Position.Index;
        stream.UserState.incStack.Push(offset, s, CharStreamState(stream))
        s
        |> stream.UserState.stream.FindStream
        |> fun pss -> 
                    stream.Seek(int64 pss.index)
                    stream.Name <- s
                    stream.SetLineBegin_WithoutCheckAndWithoutIncrementingTheStateTag 0L
                    stream.SetLine_WithoutCheckAndWithoutIncrementingTheStateTag 0L 
                    stream.RegisterNewline()
        Reply(())

type PasState with
    static member Create s ip =
        {
            stream = s
            incPath = ip
            incStack = Stack()
            handleInclude = {contents = pass1IncludeHandler}
            moduled = ModuleDef()
        }

let opp = OperatorPrecedenceParser<ExprEl,unit,PasState>()