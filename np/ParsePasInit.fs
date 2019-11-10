[<AutoOpen>]
module NP.ParsePasInit

open System.IO
open System.Text
open FParsec
open FParsec.CharParsers

let phase1 s =
    runParserOnStream s Encoding.Unicode