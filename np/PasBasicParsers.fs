[<AutoOpen>]
module Pas.BasicParsers

open FParsec
open Fake.Runtime.Environment

exception InternalErrorException of string

let ws = spaces
let str s = pstring s
let mws: Parser<unit, 'u> = fun stream -> skipMany spaces1 stream
let strWsc s =
    str s .>> mws

let manySatisfyWith0 (startLen, eofCommentParser: Parser<_,_>) parseComments =
    fun (stream: CharStream<_>) ->
        let cp = stream.Position
        let commentPos = Position(cp.StreamName, cp.Index - startLen, cp.Line, cp.Column - startLen)
        parseComments commentPos stream eofCommentParser

let commentBlock perseComment =
    let was0 = ref false
    let internalCommentParser (stream: CharStream<_>) =
      let parser = manySatisfy (function '}' -> false | '\000' -> was0 := true; false | _ -> true)
      let reply = parser stream
      if reply.Status = Ok && not !was0 then
        reply
      else
        Reply(Error, messageError "Unexpected end of file")
    between (pchar '{') (strWsc "}") <| manySatisfyWith0(1L, internalCommentParser) perseComment

let starCommentBlock perseComment =
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
    between (pstring "(*") (strWsc "*)") <| manySatisfyWith0(2L, internalCommentParser) perseComment

let simpleIdentifier msg =
    let isProperFirstChar c = isLetter c || c = '_'
    let isProperChar c = isLetter c || c = '_' || isDigit c
    many1Satisfy2L isProperFirstChar isProperChar msg

let directiveIdentifier =
    simpleIdentifier "directive ident"
    .>>. (pchar '+' <|> pchar '-' <|> (mws >>% ' '))

let ifDefIdentifier = mws >>. simpleIdentifier "ifdef identifier" .>> skipMany(anyChar) .>> eof

// TODO: escape for ~ in inc files ?

let parseDirective  (commentPos: Position) (stream: CharStream<PasState>) (eofCommentParser: Parser<_,_>) =
    let us = stream.UserState
    let pass = us.pass

    let idReply = directiveIdentifier stream
    if idReply.Status = Ok then
        let inReply = eofCommentParser stream
        let restOfComment: string = (if inReply.Status = Ok then inReply.Result else "").Trim()

        let parseDefineId specIdName action =
            match runParserOnString ifDefIdentifier us "" restOfComment with
            | Success(id,_,_) -> action id |> Some
            | _ ->
                ``Error: Invalid %s identifier`` specIdName
                |> us.NewMsg (box commentPos)
                None

        let doIfDef defined =
            let spec = if defined then "IFDEF" else "IFNDEF"
            IfDef{
                Pos = commentPos
                Defined =
                    parseDefineId spec (pass.Defines.Contains >> if defined then id else not)
                    |> Option.defaultValue false
                Branch = IfDefBranch}
            |> Directive |> Some

        match idReply.Result with
        | "I", (' ' as firstIChar) | "I", ('%' as firstIChar) ->
            // `{$I%` or `{$I %` or `{$I   %` is correct
            if (firstIChar = '%' && restOfComment.Length > 0) || (restOfComment.Length > 1 && restOfComment.Chars(0) = '%') then
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
                    let r = if (firstIChar = ' ' && restOfComment.Chars(0) = '%') then restOfComment.Substring(1) else restOfComment
                    let eof = r.IndexOf(' ')
                    let r = if eof > -1 then r.Substring(0, eof) else r
                    if r.Chars(r.Length-1) = '%' then
                        r.Substring(0, r.Length-1)
                    else r
                let macroId =
                    {
                      name = sprintf "%s : Compiler Info %s" (commentPos.StreamName) r
                      line = int commentPos.Line
                      column = int commentPos.Column
                    }
                let macro = match r.ToUpper() with
                            | "LINENUM" -> int stream.Line |> CompilerInfoInt
                            | var ->
                                match environVarOrNone var with
                                | Some value -> CompilerInfoStr value
                                | None ->
                                    // report once
                                    // TODO rework as handler ?
                                    ``Warning: Cannot find enviroment variable '%s'`` var
                                    |> us.NewMsg (box commentPos)
                                    CompilerInfoStr ""
                Macro(macroId, macro)
            else
                // TODO handle bad file names
                restOfComment |> Include |> Directive
            |> Some
        | "I", c when c = '+' || c = '-' -> c = '+' |> IOCheck |> Directive |> Some
        | "H", c when c = '+' || c = '-' -> c = '+' |> LongString |> Directive |> Some
        | "APPTYPE", ' ' ->
            match restOfComment with
            | "CONSOLE" -> Console |> AppType |> Directive |> Some
            | "GUI" -> GUI |> AppType |> Directive |> Some
            | _ -> None
        | "DEFINE", ' ' ->
            parseDefineId "DEFINE" pass.Defines.Add |> ignore
            Some Regular
        | "UNDEF", ' ' ->
            parseDefineId "UNDEF" pass.Defines.Remove |> ignore
            Some Regular
        | "IFDEF", ' ' -> doIfDef true
        | "IFNDEF", ' ' -> doIfDef false
        | "ENDIF", ' ' -> EndIf |> Directive |> Some
        | "ELSE", ' ' -> Else |> Directive |> Some
        | _ -> None
        |>  function
            | Some comment ->
                us.messages.PosMap.TryAdd(box comment, commentPos) |> ignore
                Reply(inReply.Status, comment, inReply.Error)
            | _ ->
                ``Error: Invalid directive declaration``
                |> us.NewMsg (box commentPos)
                Reply(inReply.Status, Regular, inReply.Error)
    else
        Reply(Error, Unchecked.defaultof<_>, idReply.Error)


let stdParseComments (commentPos: Position) (stream: CharStream<PasState>) (eofCommentParser: Parser<_,_>) =
    if stream.Skip '$' then
        parseDirective commentPos stream eofCommentParser
    else
        let inReply = eofCommentParser stream
        Reply(inReply.Status, Regular, inReply.Error)

let c0 =
    fun (stream: CharStream<_>) ->
      let reply = (pchar '\000') stream
      if reply.Status = Ok then
        let us = stream.UserState
        if us.incStack.Count > 0 then
          let pos, name, s = us.incStack.Pop()
          let i = us.stream.FindStream name
          // printfn "%s %i = %i" name (i.index + i.length + 1) stream.Index
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

let doubleSlashComment: Parser<string, 'u> = fun stream -> (pstring "//" >>. restOfLine true) stream

type CommentsParser<'u> = Position -> CharStream<'u> -> Parser<string,'u> -> Reply<Comment>
type CommentsHandler<'u> = Comment -> Parser<unit,'u>
type C0<'u> = (CharStream<'u> -> Reply<Comment>) option

// (Position -> CharStream<PasState> -> Parser<string,'a> -> Reply<Comment>) -> () -> (CharStream<PasState> -> Reply<Comment>) option -> 'b -> Parser<'a,PasState>
let comments<'u>
    (commentsParser: CommentsParser<'u>)
    (commentsHandler: CommentsHandler<'u>)
    (c0: C0<'u>)
    : Parser<unit, 'u> =
    fun (stream: CharStream<'u>) ->
        (choice[
          spaces1 >>% Regular
          commentBlock commentsParser
          starCommentBlock commentsParser
          doubleSlashComment >>% Regular
          if c0.IsSome then c0.Value
        ] <?> ""
        >>= commentsHandler) stream

let stdComments = comments<PasState> stdParseComments PasState.HandleComment (Some c0)

let pass1Parser comments endParser =
    skipMany(
          (skipMany1 comments)
          <|> (skipMany1Till anyChar (
                next2CharsSatisfy (
                  fun c1 c2 ->
                    match (c1, c2) with
                    | '{', _ | '(', '*' | '/', '/' | '\000', _ -> true
                    | _, _ -> false)
                <|> endParser
          )))

let initialPassParser =
    let checkIfDefBalance us =
        let ifDefStack = (us.pass :?> InitialPass).IfDefStack
        if ifDefStack.Count > 0 then
            let pos = ifDefStack.Pop()
            ``Error: Unfinished '{$%O}' block detected`` pos.Branch
            |> us.NewMsg (pos.Pos |> box)
    pass1Parser stdComments (eof .>> (getUserState |>> checkIfDefBalance))

let wsc: Parser<unit, PasState> = skipMany stdComments
let str_wsc s =
    pstringCI s .>> wsc
let include_system_inc =
    PasState.HandleComment(Include "system.inc" |> Directive) >>. wsc
let wrd_wsc s =
    pstringCI s .>> (notFollowedBy (choice[letter; digit; pchar '_']) .>> wsc)