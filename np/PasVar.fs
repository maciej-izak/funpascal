[<AutoOpen>]
module np.PasVar

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

type PasState = {
  stream: PasStream
  incStack: (int64 * string * CharStreamState<PasState>) Stack
  handleInclude: IncludeHandle ref
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

and PasStream(s: string) = class 
    inherit Stream()
    let strToStream (s: string) = s |> Encoding.Unicode.GetBytes |> fun s -> new MemoryStream(s)
    (*let calcNextOffset size = 
        if size < DefaultBlockSize then
            DefaultBlockSize
        else
            ((size / DefaultBlockSize) * DefaultBlockSize) + (size % DefaultBlockSize)*)
    let mainStream = strToStream s
    let mutable lastSubStream = {index=0;length=s.Length};
    let mutable stream = mainStream
    let finalStream = new MemoryStream(int mainStream.Length)

    (*let mutable nextFreeOffset: int64 = calcNextOffset mainStream.Size
    let computeNextFreeOffset (s: Stream) =
        let result = nextFreeOffset
        nextFreeOffset <- nextFreeOffset + calcNextOffset s.Length
        result*)
    
    let streams: Dictionary<string, PasSubStream> = new Dictionary<_,_>()

    let mutable offsetsMap: string [] = [||]

    override this.get_CanRead() : bool = true
    override this.get_CanWrite() : bool = false
    override this.get_CanSeek() : bool = true
    override this.get_Length() : int64 = stream.Length 
    override this.get_Position() : int64 = stream.Position
    override this.set_Position(value: int64) : unit =
        stream.Position <- value

    override this.Flush() : unit = stream.Flush()

    override this.Seek(offset: int64, origin: SeekOrigin) : int64 = 
        stream.Seek(offset, origin)
        
    override this.SetLength(value: int64) : unit = ()
    override this.Read(buffer: byte [], offset: int, count: int) : int =
        let readed = stream.Read(buffer, offset, count)
        finalStream.Write(buffer, offset, readed)
        readed

    override this.Write(buffer: byte [], offset: int, count: int) : unit = () 

    override t.Close() =
        if stream = mainStream then
            finalStream.WriteByte(0uy)
            finalStream.WriteByte(0uy)
            stream.Close()
            stream <- finalStream
            stream.Seek(0L, SeekOrigin.Begin) |> ignore
        else
            stream.Close()

    member this.AddInc fileName =
        let addInc() =    
            let pos = finalStream.Position
            finalStream.Seek(finalStream.Length, SeekOrigin.Begin) |> ignore
            let str = "\013" + File.ReadAllText(fileName) + "\000"
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

    member t.FindStream s = streams.[s]

    member t.SaveToFile() =
        use f = new FileStream("final.pas", FileMode.Create, FileAccess.Write)
        finalStream.WriteTo(f)

    member this.EndInc() =
        () 
  end

let pass1IncludeHandler s =  
    fun (stream: CharStream<PasState>) ->
        stream.UserState.stream.AddInc s
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
                    printfn "register new lines %A" (stream.RegisterNewline())
        Reply(())

type PasState with
    static member Create s =
        {
            stream = s
            incStack = Stack()
            handleInclude = {contents = pass1IncludeHandler}
        }

let opp = new OperatorPrecedenceParser<ExprEl,unit,PasState>()