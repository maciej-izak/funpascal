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

// meta trick to get assembly methods reference:

(*
type internal Marker = interface end
let t = typeof<Marker>.DeclaringType
let a = AssemblyDefinition.ReadAssembly(t.Assembly.Location);
let methodToRef (m: System.Reflection.MethodInfo): MethodReference = a.MainModule.ImportReference(m)
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