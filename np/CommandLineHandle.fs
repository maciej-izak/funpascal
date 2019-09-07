module np.CommandLineHandle

open Argu

type NpArguments =
    | [<MainCommand;Mandatory>] Files of FILES:string list
with
    interface IArgParserTemplate with
        member arg.Usage =
            match arg with
            | Files _ -> "source code files to parse"