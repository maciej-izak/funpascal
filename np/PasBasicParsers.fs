[<AutoOpen>]
module Pas.BasicParsers

open FParsec

type AppType = | Console | GUI

type Directive =
     | Include of string
     | IOCheck of bool
     | LongString of bool
     | AppType of AppType

type Comment =
     | Directive of Directive
     | TestEnv of string * string list
     | Regular
     | Macro of (MacroId * Macro)

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
    .>>. (pchar '+' <|> pchar '-' <|> (mws >>% ' '))

let testEnvVar =
    let isProperFirstChar c = isLetter c || c = '_'
    let isProperChar c = isLetter c || c = '_' || isDigit c
    let isProperValChar c = isProperChar c || c = '-' || c = ' '
    let simpleIdentName = many1Satisfy2L isProperFirstChar isProperChar "test ident"
    let simpleIdent = many1SatisfyL isProperValChar "test value"
    simpleIdentName .>>.
    (mws >>. opt( pchar '=' >>. sepEndBy simpleIdent (pchar ','))
     >>= function
         | Some x -> preturn x
         | _ -> preturn []
     )

// TODO: escape for ~ in inc files ?

let manySatisfyWith0 (commentParser: Parser<_,_>) =
    fun (stream: CharStream<_>) ->
      let commentPos = stream.Position;
      if stream.Skip '$' then
        let idReply = directiveIdentifier stream
        if idReply.Status = Ok then
          let inReply = commentParser stream
          let r: string = (if inReply.Status = Ok then inReply.Result else "").Trim()
          match idReply.Result with
          | "I", (' ' as firstIChar) | "I", ('%' as firstIChar) ->
              // `{$I%` or `{$I %` or `{$I   %` is correct
              if (firstIChar = '%' && r.Length > 0) || (r.Length > 1 && r.Chars(0) = '%') then
                  // TODO optimize parsing
                  // follow FPC logic, allow all below :
                  // {$I %IDENT} -> 'IDENT'
                  // {$I %IDENT%} -> 'IDENT'
                  // {$I %IDENT %} -> 'IDENT'
                  // {$I %IDENT% %} -> 'IDENT'
                  // {$I %IDENT%X} -> 'IDENT%X'
                  // {$I %%IDENT%%%} -> '%IDENT%%'
                  let r =
                      // {$I %IDENT%} version
                      let r = if (firstIChar = ' ' && r.Chars(0) = '%') then r.Substring(1) else r
                      let eof = r.IndexOf(' ')
                      let r = if eof > -1 then r.Substring(0, eof) else r
                      if r.Chars(r.Length-1) = '%' then
                          r.Substring(0, r.Length-1)
                      else r
                  let macro = match r.ToUpper() with
                              | "LINENUM" -> int stream.Line |> CompilerInfoInt
                              // TODO warnings about lack of env variable
                              | _ -> "" |> CompilerInfoStr
                  let macroId =
                      {
                        name = sprintf "%s : Compiler Info %s" (commentPos.StreamName) r
                        line = commentPos.Line
                        column = commentPos.Column
                      }
                  Macro(macroId, macro)
              else
                  // TODO handle bad file names
                  r |> Include |> Directive
              |> Some
          | "I", c when c = '+' || c = '-' -> c = '+' |> IOCheck |> Directive |> Some
          | "H", c when c = '+' || c = '-' -> c = '+' |> LongString |> Directive |> Some
          | "APPTYPE", ' ' ->
            match r with
            | "CONSOLE" -> Console |> AppType |> Directive |> Some
            | "GUI" -> GUI |> AppType |> Directive |> Some
            | _ -> None
          | _ -> None
          |> function
             | Some d -> Reply(inReply.Status, d, inReply.Error)
             | _ -> 
               let e = sprintf "Invalid '%s' directive declaration" (fst idReply.Result)
                       |> messageError 
                       |> mergeErrors inReply.Error
               Reply(Error, e)
        else
          Reply(Error, Unchecked.defaultof<_>, idReply.Error)
      else
        if stream.UserState.testsEnv <> null then
            mws stream |> ignore
            if stream.Skip '#' then
                mws stream |> ignore
                let idReply = testEnvVar stream
                if idReply.Status = Ok then
                    let inReply = commentParser stream
                    if inReply.Status = Ok then
                        Reply(inReply.Status, TestEnv(idReply.Result), inReply.Error)
                    else
                        let e = sprintf "Invalid '%s' directive declaration" (fst idReply.Result)
                               |> messageError
                               |> mergeErrors inReply.Error
                        Reply(Error, e)
                else
                    let inReply = commentParser stream
                    Reply(inReply.Status, Regular, inReply.Error)
            else
                let inReply = commentParser stream
                Reply(inReply.Status, Regular, inReply.Error)
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
          // otherwise the following error is raised
          // The current position of the stream must not lie before the position corresponding to the given CharStreamIndexToken/CharStreamState.
          stream.BacktrackTo(s)
          Reply(Ok,Regular,reply.Error)
        else 
          stream.Seek (stream.Index-1L) 
          Reply(Error,messageError "Unexpected end of file")
      else
        Reply(reply.Status,reply.Error)

let doubleSlashComment =
    pstring "//" >>. restOfLine true

let comments =
    choice[
      spaces1 >>% Regular
      commentBlock
      starCommentBlock
      doubleSlashComment >>% Regular
      c0
    ] <?> ""
    >>= function
        | Directive d ->
          match d with
          | Include f -> fun stream -> !stream.UserState.handleInclude f stream
          | AppType _ -> preturn()
          | IOCheck _ -> preturn()
          | LongString _ -> preturn()
        | Macro m -> fun stream -> !stream.UserState.handleMacro m stream
        | _ -> preturn()
    
let wsc: Parser<unit, PasState> = skipMany comments

let pass1Parser =
    many(
          (skipMany1 comments) 
          <|> (skipMany1Till anyChar (
                next2CharsSatisfy (
                  fun c1 c2 ->
                    match (c1, c2) with
                    | '{', _ | '(', '*' | '/', '/' | '\000', _ -> true
                    | _, _ -> false
          ))))

let str_wsc s =
    pstringCI s .>> wsc
let include_system_inc = (fun stream -> !stream.UserState.handleInclude "system.inc" stream) >>. wsc
let wrd_wsc s =
    pstringCI s .>> (notFollowedBy (choice[letter; digit; pchar '_']) .>> wsc)