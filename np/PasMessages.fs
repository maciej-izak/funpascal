[<AutoOpen>]
module Pas.Messages


let ``Error: %O intrinsic cannot be used outside loop`` o =
    (0, sprintf "%O intrinsic cannot be used outside loop" o)
