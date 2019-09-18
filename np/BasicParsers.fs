[<AutoOpen>]
module np.BasicParsers

open System.Text
open System.IO
open FParsec
open FParsec.Error

type Comment =
     | InsertCode of string
     | Regular

exception InternalErrorException of string

let ws = spaces
let str s = pstring s
let mws: Parser<unit, PasState> = skipMany spaces1
let strWsc s =
    str s .>> mws

// TODE escape for ~ in inc files ?

let manySatisfyInclude = 
    let was0 = ref false
    fun (stream: CharStream<_>) ->
      let parser = manySatisfy (function '}' -> false | '\000' -> was0 := true; false | _ -> true)
      let reply = parser stream
      if reply.Status = Ok && not !was0 then
        reply
      else
        Reply(Error, messageError "Unexpected end of file")

let insertCode = 
    between (strWsc "{$I") (pstring "}") manySatisfyInclude |>> InsertCode

let commentBlock: Parser<unit,PasState> =
    between (strWsc "{") (strWsc "}") manySatisfyInclude >>% ()
    // skipChar '{' .>> skipManyTill skipAnyChar (skipChar '}')
        
//let starCommentBlock: Parser<unit,PasState> =
//    between (strWsc "(*") (strWsc "*)") manySatisfyInclude >>% () 
    // skipString "(*" .>> skipManyTill skipAnyChar (skipString "*)")

(*let spacesOr0: Parser<unit,PasState> = 
    let parser = (spaces1 >>% true)<|>(pchar '\000' >>% false)
    fun (stream: CharStream<_>) ->
      let reply = parser stream
      if reply.Status = Ok then
        if not reply.Result then
          let us = stream.UserState
          if us.incStack.Count > 0 then
            let pos, name = us.incStack.Pop()
            let i = us.stream.FindStream name
            if i.index + i.length + 1 <> int stream.Index then 
              raise (InternalErrorException("201909150"))
            stream.Seek (pos+1L)
            // important to omit FParsec bug i.e. {$I foo.inc}i f
            // otherwaise the following error is raised
            // The current position of the stream must not lie before the position corresponding to the given CharStreamIndexToken/CharStreamState.
            let r = (pchar '}') stream //|> ignore
            //Reply(r.Status, r.Error)
            printfn "status %A %A" r.Status r.Result
            Reply(()) 
          else 
            Reply(Error, messageError "Unexpected char: #0")
        else
          Reply(())
      else
        Reply(reply.Status, reply.Error)
*)

let c0 =
    fun (stream: CharStream<_>) ->
      let reply = (pchar '\000') stream
      if reply.Status = Ok then
        let us = stream.UserState
        if us.incStack.Count > 0 then
          let pos, name, s = us.incStack.Pop()
          let i = us.stream.FindStream name
          printfn "%s %i = %i" name (i.index + i.length + 1) stream.Index
          if i.index + i.length + 1 <> int stream.Index then //todo different 1 or 2
            raise (InternalErrorException("201909150"))
          stream.Seek pos
          // important to omit FParsec bug i.e. {$I foo.inc}i f
          // otherwaise the following error is raised
          // The current position of the stream must not lie before the position corresponding to the given CharStreamIndexToken/CharStreamState.
          stream.BacktrackTo(s)
          //printfn "%A" r.Status
          Reply(())
        else 
          stream.Seek (stream.Index-1L) 
          Reply(Error, messageError "Unexpected end of file")
      else
        Reply(reply.Status, reply.Error)

let comments =
    choice[
      spaces1 >>% Regular
      c0 >>% Regular     
      attempt(insertCode)
      commentBlock >>% Regular
      //starCommentBlock >>% Regular
    ] <?> ""
    >>= function
        | InsertCode (s) -> 
          fun stream -> !stream.UserState.handleInclude s stream
        | _ -> preturn()
    
// let std_ws =
//   choice[spaces1; commentBlock; starCommentBlock] <?> ""

let wsc: Parser<unit, PasState> = skipMany comments

let str_wsc s =
    pstring s .>> wsc

(*
  let (.-.) (p: Parser<'a,unit>) (q: Parser<'b,unit>): Parser<'a*'b,unit> =
    let parser = comments
    fun stream ->
        let reply = p stream
        if reply.Status = Ok then
          printfn "ok %A" reply.Result
          let stateTag1 = stream.StateTag
          // Reply(reply.Status, reply.Result, reply.Error) 
          let reply2 = parser stream
          if reply2.Status = Ok then
            let s = match reply2.Result with
                    | InsertCode s -> s
                    | _ -> ""
            printfn "s is equal %s" s
            if s <> "" then
              let res = runParserOnFile (many(comments) >>. opt(q) .>> eof) () s System.Text.Encoding.Default
              match res with
              | Success (s, _, _) -> 
                  printfn ">Success";
                  match s with
                  | Some v -> Reply(Ok,(reply.Result, v),NoErrorMessages)
                  | None -> 
                            printfn "None branch"
                            let reply3 = q stream
                            let error = if stateTag1 <> stream.StateTag then reply3.Error
                                        else mergeErrors reply3.Error reply2.Error
                            let result = if reply3.Status = Ok then (reply.Result, reply3.Result)
                                         else Unchecked.defaultof<_>
                            Reply(reply3.Status, result, error)
              | Failure (_, f, _) -> printfn ">Failure"; Reply(Error, f.Messages)
            else
              printfn "s is nil?";
              Reply()
          else
            Reply(reply2.Status, reply2.Error)
        else
          Reply(reply.Status, reply.Error)
          *)


                    //CharStream.ParseString ("{}", 0, 2, stream, "string", "foo")
                    // runParserOnString 
                    // stream
                    // let reply = runParserOnString p () "foo" "{}"
                    // if reply.Status = Ok then
                    //     Reply(())
                    // else
                    //     reply.
//many (attempt(directiveBlock) <|> std_ws)


