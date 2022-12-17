module FNP.CommandLineHandle

open Argu

type CLIArguments =
    | [<MainCommand>] Files of FILES:string list
    | [<EqualsAssignment>] TestAll of PATH: string option
    | Test of string
    | TestParser
with
    interface IArgParserTemplate with
        member arg.Usage =
            match arg with
            | Files _ -> "source code files to parse"
            | TestAll _ -> "run compiler tests"
            | Test _ -> "run compiler test"
            | TestParser -> "test some parser"