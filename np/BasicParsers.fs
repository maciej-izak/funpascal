[<AutoOpen>]
module np.BasicParsers

open System.Text
open System.IO
open FParsec
open FParsec.Error

type AppType = | Console | GUI

type Directive =
     | Include of string
     | AppType of AppType

type Comment =
     | Directive of Directive
     | Regular

exception InternalErrorException of string

let ws = spaces
let str s = pstring s
let mws: Parser<unit, PasState> = skipMany spaces1
let strWsc s =
    str s .>> mws

let directiveIdentifier =
    let isProperFirstChar c = isLetter c || c = '_'
    let isProperChar c = isLetter c || c = '_' || isDigit c
    many1Satisfy2L isProperFirstChar isProperChar "directive ident"
    .>> mws

// TODE escape for ~ in inc files ?

let manySatisfyWith0 (commentParser: Parser<_,_>) = 
    fun (stream: CharStream<_>) ->
      if stream.Skip '$' then
        let idReply = directiveIdentifier stream
        if idReply.Status = Ok then
          let inReply = commentParser stream
          let r = if inReply.Status = Ok then inReply.Result
                  else Unchecked.defaultof<_>
          match idReply.Result with
          | "I" -> Some(Include(r))
          | "A" | "APPTYPE" -> 
            match r with
            | "CONSOLE" -> Some(AppType(Console))
            | "GUI" -> Some(AppType(GUI))
            | _ -> None
          | _ -> None
          |> function
             | Some d -> Reply(inReply.Status, Directive(d), inReply.Error)
             | _ -> 
               let e = sprintf "Invalid '%s' directive declaration" idReply.Result
                       |> messageError 
                       |> mergeErrors inReply.Error
               Reply(Error, e)
        else
          Reply(Error, Unchecked.defaultof<_>, idReply.Error)
      else
        let inReply = commentParser stream
        Reply(inReply.Status, Regular, inReply.Error)
        
let commentBlock =
    let was0 = ref false
    let internalCommentParser (stream: CharStream<_>) =
      let parser = manySatisfy (function '}' -> false | '\000' -> was0 := true; false | _ -> true)
      let reply = parser stream
      if reply.Status = Ok && not !was0 then
        reply
      else
        Reply(Error, messageError "Unexpected end of file")
    between (pchar '{') (strWsc "}") (manySatisfyWith0 internalCommentParser) 
        
let starCommentBlock =
    let was0 = ref false
    let internalCommentParser (stream: CharStream<_>) =
      let endOfComment =
        next2CharsSatisfy (
          fun c1 c2 ->
            match (c1, c2) with
            | '*', ')' -> true 
            | '\000', _ -> was0 := true; true
            | _, _ -> false
        )
      let parser = manyCharsTill anyChar endOfComment
      let reply = parser stream
      if reply.Status = Ok && not !was0 then
        reply
      else
        Reply(Error, messageError "Unexpected end of file")
    between (pstring "(*") (strWsc "*)") (manySatisfyWith0 internalCommentParser) 

let c0 =
    fun (stream: CharStream<_>) ->
      let reply = (pchar '\000') stream
      if reply.Status = Ok then
        let us = stream.UserState
        if us.incStack.Count > 0 then
          let pos, name, s = us.incStack.Pop()
          let i = us.stream.FindStream name
          printfn "%s %i = %i" name (i.index + i.length + 1) stream.Index
          if i.index + i.length + 1 <> int stream.Index then
            raise (InternalErrorException("201909150"))
          stream.Seek pos
          // important to omit FParsec bug i.e. {$I foo.inc}i f
          // otherwaise the following error is raised
          // The current position of the stream must not lie before the position corresponding to the given CharStreamIndexToken/CharStreamState.
          stream.BacktrackTo(s)
          Reply(Ok,Regular,reply.Error)
        else 
          stream.Seek (stream.Index-1L) 
          Reply(Error,messageError "Unexpected end of file")
      else
        Reply(reply.Status,reply.Error)

let comments =
    choice[
      spaces1 >>% Regular
      commentBlock
      starCommentBlock
      c0
    ] <?> ""
    >>= function
        | Directive d ->
          match d with
          | Include f -> fun stream -> !stream.UserState.handleInclude f stream
          | AppType _ -> preturn()
        | _ -> preturn()
    
let wsc: Parser<unit, PasState> = skipMany comments

let pass1Parser =
    many(
          (skipMany1 comments) 
          <|> (skipMany1Till anyChar (
                next2CharsSatisfy (
                  fun c1 c2 ->
                    match (c1, c2) with
                    | '{', _ | '(', '*' | '\000', _ -> true
                    | _, _ -> false
          ))))

let str_wsc s =
    pstring s .>> wsc