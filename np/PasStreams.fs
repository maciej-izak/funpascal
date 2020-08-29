module NP.PasStreams

open System.Text
open System.IO
open Pas
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Core
open Fake.IO.FileSystemOperators

let doPas proj =
    loadAndDoFile proj parseMainModule
    >>
    function // TODO very similar code to Ctx.BuildUnit, might be worth to merge
    | Ok (_, us as res) ->
        match Ctx.BuildMainModule res with
        | Some asmDef ->
            let outName = proj.OutPath </> proj.Name + ".dll"
            asmDef.Write(outName)
            proj.AddCompilerMessages proj.File us.messages
            Some outName
        | None ->
            proj.AddCompilerMessages proj.File us.messages
            None
    | Error us ->
        proj.AddCompilerMessages proj.File us.messages
        None

let toString (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

//let testFile f =
//    let s = File.ReadAllText(f)
//    let strToStream (s: string) = s |> Encoding.Unicode.GetBytes |> fun s -> new MemoryStream(s)
//    let dir = Path.GetDirectoryName f
//    let result = testPas pascalModule (strToStream s) dir
//    let xmlSerializer = FsPickler.CreateXmlSerializer(indent = true)
//    use sw = new StreamWriter(path=dir + string Path.DirectorySeparatorChar + "out.ast")
//    match result with
//    | Success (r,_,_) -> xmlSerializer.Serialize(sw, r)
//    | _ -> ()
//    sw.Close()
//    ()

    // use sw = new StreamWriter(path=dir + string Path.DirectorySeparatorChar + "out.ast")
    // let b = StringBuilder()
    // bprintf b "%O" result
    // sw.Close()