module NP.CommandLineHandle

open Argu

type CLIArguments =
    | [<MainCommand>] Files of FILES:string list
    | Test of string
with
    interface IArgParserTemplate with
        member arg.Usage =
            match arg with
            | Files _ -> "source code files to parse"
            | Test _ -> "run compiler tests"