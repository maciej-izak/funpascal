[<AutoOpen>]
module Pas.BasicParsers

open FParsec
open Fake.Runtime.Environment

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
        let commentPos = stream.Position
        let us = stream.UserState
        let pass = us.pass
        if stream.Skip '$' then
            let idReply = directiveIdentifier stream
            if idReply.Status = Ok then
                let strDef (str: string) = //for $IFDEF - comment all after first ident
                    if System.String.IsNullOrEmpty str then
                        ""
                    else
                        let idx = str.IndexOfAny([|' ';'\t';'\r';'\n'|])
                        if idx < 0 then str
                        else str.Remove(idx)
                let doIfDef r defined =
                    IfDef{
                        Line = commentPos.Line
                        Column = commentPos.Column
                        Defined = strDef r |> pass.Defines.Contains && defined}
                    |> Directive |> Some
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
                                            sprintf "Cannot find enviroment variable '%s'" var
                                            |> us.NewWarning (box commentPos)
                                            CompilerInfoStr ""
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
                | "DEFINE", ' ' ->
                    pass.Defines.Add(strDef r) |> ignore
                    Some Regular
                | "UNDEF", ' ' ->
                    pass.Defines.Remove(strDef r) |> ignore
                    Some Regular
                | "IFDEF", ' ' -> doIfDef r true
                | "IFNDEF", ' ' -> doIfDef r false
                | "ENDIF", ' ' -> EndIf |> Directive |> Some
                | "ELSE", ' ' -> Else |> Directive |> Some
                | _ -> None
                |>  function
                    | Some comment ->
                        pass.PosMap.Add(box comment, commentPos)
                        Reply(inReply.Status, comment, inReply.Error)
                    | _ ->
                        "Invalid directive declaration"
                        |> us.NewError (box commentPos)
                        Reply(inReply.Status, Regular, inReply.Error)
            else
                Reply(Error, Unchecked.defaultof<_>, idReply.Error)
        else
            match stream.UserState.pass.Id with
            | InitialPassId ->
                mws stream |> ignore
                if stream.Skip '%' then
                    let inReply = commentParser stream
                    let r: string = (if inReply.Status = Ok then inReply.Result else "").Trim()
                    // TODO raise exception for duplicated test env ?
                    match runParserOnString testEnvVar us "" r with
                    | Success((key, value), _, _) -> us.testEnv.TryAdd(key, value) |> ignore
                    | _ -> () // sprintf "Invalid '%s' test env declaration" (fst idReply.Result)
                    Reply(inReply.Status, Regular, inReply.Error)
                else
                    let inReply = commentParser stream
                    Reply(inReply.Status, Regular, inReply.Error)
            | _ ->
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
    >>= PasState.HandleComment
    
let wsc: Parser<unit, PasState> = skipMany comments

let initialPassParser =
    skipMany(
          (skipMany1 comments) 
          <|> (skipMany1Till anyChar (
                next2CharsSatisfy (
                  fun c1 c2 ->
                    match (c1, c2) with
                    | '{', _ | '(', '*' | '/', '/' | '\000', _ -> true
                    | _, _ -> false)
                <|> eof
          )))

let str_wsc s =
    pstringCI s .>> wsc
let include_system_inc =
    PasState.HandleComment(Include "system.inc" |> Directive) >>. wsc
let wrd_wsc s =
    pstringCI s .>> (notFollowedBy (choice[letter; digit; pchar '_']) .>> wsc)