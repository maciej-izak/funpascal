[<AutoOpen>]
module Pas.Test

open System
open FParsec.CharParsers

type TestCase = {
    Result: int
    Define: string option
    FailExpected: bool
} with
    member self.DefSuffix = match self.Define with | Some "" -> "" | Some def -> "-" + def | _ -> ""

let prepareTest proj =
    runParserOnFile testPassParser (PasTestState.Create()) (proj.File) (System.Text.Encoding.UTF8)
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