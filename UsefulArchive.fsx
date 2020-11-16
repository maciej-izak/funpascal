// IMPORTANT LNKS :
// about align !
//     - https://stackoverflow.com/questions/24122973/what-should-i-pin-when-working-on-arrays/24127524
// Equality dsyme
//     - https://docs.microsoft.com/pl-pl/archive/blogs/dsyme/equality-and-comparison-constraints-in-f
// To get sizes
//     check mono_marshal_type_size:
//         - https://github.com/dotnet/runtime/blob/487c940876b1932920454c44d2463d996cc8407c/src/mono/mono/metadata/marshal.c
//     check mono_type_to_unmanaged:
//         - https://github.com/dotnet/runtime/blob/aa6d1ac74e6291b3aaaa9da60249d8c327593698/src/mono/mono/metadata/metadata.c
// Ignore errors:
//     - https://stackoverflow.com/questions/9248426/basic-error-recovery-with-fparsec/9249678
// Compiler design :
//     - http://www.itu.dk/people/sestoft/plc/
// referenceequals
// https://stackoverflow.com/questions/39217116/how-do-i-check-for-reference-equality-in-f
// TODO ;)
//     - https://www.elementscompiler.com/elements/oxygene/evolution.aspx
// active patterns tutorial
// https://github.com/sgoguen/journal/blob/master/2019/12/fsharp-advent/05-active-patterns.md
// Analiza CE : https://stackoverflow.com/questions/23122639/how-do-i-write-a-computation-expression-builder-that-accumulates-a-value-and-als
// FnPtr : https://stackoverflow.com/questions/27195502/what-is-the-fnptr-type-and-how-to-create-it

// Custom comparers : HashIdentity.Reference

// HOWTO implicity include inc file :

(*
1. add to stream

    let doPasStream proj parser stream =
        let addParserError (us: PasState) parserError =
            us.messages.Errors.Add parserError
            Error us
        let us = PasState.Create (InitialPass proj) (new PasStream(stream)) proj
        use stream1 = new CharStream<PasState>(us.stream, Encoding.Unicode)
        stream1.UserState <- us
        stream1.Name <- proj.FileName
        // !!! -> HERE
        match proj.FindInc "system.inc" with
        | Some fileName ->  us.stream.AddInc fileName "system.inc"
        | _ -> raise (InternalError "2020081800")

2. must be part of paring

let include_system_inc =
    PasState.HandleComment(Include "system.inc" |> Directive) >>. wsc

3. included in some part of parse process

let mainModule =
    tuple3
        (opt(``program `` >>. identifier .>> ``; ``))
        uses
        (include_system_inc >>. block .>> pstring ".")
    |>> MainModuleRec.Create


*)


// meta trick to get assembly methods reference:

(*
type internal Marker = interface end
let t = typeof<Marker>.DeclaringType
let a = AssemblyDefinition.ReadAssembly(t.Assembly.Location);
let methodToRef (m: System.Reflection.MethodInfo): MethodReference = a.MainModule.ImportReference(m)
*)

(* comparers

[<CustomEquality; CustomComparison>]
type PIdent =
    | PIdent of string

    member self.Name = match self with | PIdent n -> n

    override self.Equals(a) =
        match a with
        | :? PIdent as i -> String.Equals(self.Name, i.Name, StringComparison.InvariantCultureIgnoreCase) // equalsOn PIdent.Name self i
        | _ -> false

    override self.GetHashCode() = self.Name.GetHashCode(StringComparison.InvariantCultureIgnoreCase) // hashOn PIdent.Name self

    interface System.IComparable with
      member self.CompareTo o = match o with
                                | :? PIdent as i -> compare self.Name i.Name
                                | _ -> invalidArg "o" "cannot compare values of different types"    


*)

// In place parser:
(*
let block = // declarations .>>. beginEnd
    fun(stream: CharStream<PasState>) ->
        let reply =
            ((opt declarations |>>
                function
                | Some s ->
                    let us = stream.UserState
                    for d in s do
                        match d with
                        | Types t -> ()
                        | Variables v -> ()
                        | Consts c -> ()
                        | Labels l ->
                            for i in l do
                                (i, Label{name=i; stmtPoint=false})
                                |> us.moduled.block.symbols.Add
                | _ -> ()
            )
            .>>. beginEnd) stream
        reply
*)

//        let rec sizeOf (typ: PasType) (td: TypeDefinition) =
//            let sizeOfTD (td: TypeDefinition) = td.Fields |> Seq.sumBy (fun f -> sizeOf f.FieldType null)
//            match typ.raw.MetadataType with
//            | MetadataType.SByte | MetadataType.Byte | MetadataType.Boolean  -> 1
//            | MetadataType.Int16 | MetadataType.UInt16 -> 2
//            | MetadataType.Int32 | MetadataType.UInt32 -> 4
//            | MetadataType.Int64  -> 8
//            | MetadataType.Single -> 4
//            | MetadataType.Double -> 8
//            | MetadataType.Void   -> ptrSize // TODO 4 or 8 -> target dependent
//            | MetadataType.Pointer-> ptrSize // TODO 4 or 8 -> target dependent
//            | MetadataType.ValueType ->
//                match ctx.FindType typ with
//                | Some(ArrayRange _) -> (tr :?> TypeDefinition).ClassSize
//                | Some(TypeSymbols _) ->
//                    sizeOfTD(tr :?> TypeDefinition)
//                | Some(SimpleType(s,_)) -> s
//                | _ when td <> null -> sizeOfTD td
//                | _ -> failwith "IE"
//            | _ -> failwith "IE"

// let rec typeIdToStr (ctx: Ctx) = function
//     | TIdString -> "$s"
//     | TIdFile -> "$f"
//     | TIdPointer(i, t) -> "$" + (String.replicate i "^") + (typeIdToStr ctx t)
//     | TIdSet(p, t) -> "$S" + packedToStr(p) + (typeIdToStr ctx t)
//     | TIdIdent(DIdent di) -> simplifiedDIdent di |> String.concat "$" |> (+) "$i"
//     | TIdArray(ArrayDef(p, d, t)) -> "$a" + packedToStr(p) + (dimenstionsToStr ctx d |> String.concat ",") + "$" + typeIdToStr ctx t


// Test C# code for sharplab.io

(*

        int a = 0;
        int* p = &a;
        {
        // p is pinned as well as object, so create another pointer to show incrementing it.
            int** p2 = &p;
            Console.WriteLine( **p2 );
        }


        using System;
using System.Runtime.InteropServices;
public class C {

[DllImport("kernel32.dll")]
static extern void GetSystemTime(SystemTime systemTime);

[StructLayout(LayoutKind.Sequential)]
class SystemTime {
    public ushort Year;
    public ushort Month;
    public ushort DayOfWeek;
    public ushort Day;
    public ushort Hour;
    public ushort Minute;
    public ushort Second;
    public ushort Milsecond;
}

public static void Main(string[] args) {
    SystemTime st = new SystemTime();
    GetSystemTime(st);
    Console.WriteLine(st.Year);
}

    [StructLayout(LayoutKind.Sequential)]
public struct MyStruct {
   [MarshalAs(UnmanagedType.ByValArray, SizeConst=128)] public short[] s1;
}

    public struct InPlaceArray
{
[MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)]
public int[] values;
}

    public unsafe void M() {
        InPlaceArray x = new InPlaceArray();
        x.values[3] = 9;
    }
}

*)