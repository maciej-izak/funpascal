[<AutoOpen>]
module Pas.Test

open System
open FParsec.CharParsers

module TestParser =

    open FParsec

    let testEnvVar =
        let isProperFirstChar c = isLetter c || c = '_'
        let isProperChar c = isLetter c || c = '_' || isDigit c
        let isProperValChar c = isProperChar c || c = '-' || c = ' ' || c = ':'
        let simpleIdentName = many1Satisfy2L isProperFirstChar isProperChar "test ident"
        let simpleIdent = many1SatisfyL isProperValChar "test value"
        simpleIdentName .>>.
        (mws >>. opt( pchar '=' >>. sepEndBy simpleIdent (pchar ','))
         >>= function
             | Some x -> preturn x
             | _ -> preturn []
         )

    let testParseComments (_: Position) (stream: CharStream<PasTestState>) (eofCommentParser: Parser<_,_>) =
        let us = stream.UserState
        mws stream |> ignore
        if stream.Skip '%' then
            let inReply = eofCommentParser stream
            let r: string = (if inReply.Status = Ok then inReply.Result else "").Trim()
            // TODO raise exception for duplicated test env ?
            match runParserOnString testEnvVar us "" r with
            | Success((key, value), _, _) -> us.testEnv.TryAdd(key, value) |> ignore
            | _ -> () // sprintf "Invalid '%s' test env declaration" (fst idReply.Result)
            Reply(inReply.Status, Regular, inReply.Error)
        else
            let inReply = eofCommentParser stream
            Reply(inReply.Status, Regular, inReply.Error)

    let testComments = comments<PasTestState> testParseComments PasTestState.HandleComment None

    let testPassParser = pass1Parser testComments eof


type TestCase = {
    Result: int
    Define: string option
    FailExpected: bool
} with
    member self.DefSuffix = match self.Define with | Some "" -> "" | Some def -> "-" + def | _ -> ""

let prepareTest proj =
    runParserOnFile TestParser.testPassParser (PasTestState.Create()) (proj.File) (System.Text.Encoding.UTF8)
    |>  function
        | Success (_, us, _) ->
            let result = match us.testEnv.TryGetValue "RESULT" with | true, [r] -> int r | _ -> 0
            let defCase = { Result=result; FailExpected=(us.testEnv.ContainsKey "FAIL"); Define=None }
            let strToTestCase (s: string) =
                match s.Split(':', 2) with
                | [|d|] -> {defCase with Define=Some d}
                | [|d;"-"|] -> {defCase with FailExpected=true; Define=Some d}
                | [|d;r|] ->
                    match Int32.TryParse r with
                    | true, r -> {defCase with Result=r; Define=Some d}
                    | _ -> {defCase with Define=Some d}
                | _ -> defCase
            match us.testEnv.TryGetValue "RESULTS" with | true, v -> v | _ -> []
            |>  function
                | [] -> [defCase]
                | results -> List.map strToTestCase results
            |> Ok
        | Failure _ -> Result.Error()