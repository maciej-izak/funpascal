namespace Pas

type WarningKind =
    | SevereWarning
    | NormalWarning
    | HintWarning

type MessageKind =
    | MsgError of (int option * string)
    | MsgWarning of (int option * WarningKind * string)

[<AutoOpen>]
module Errors =

    let ``Error: Unbalanced '{$%O}'`` o =
        MsgError(Some 1, sprintf "Unbalanced '{$%O}'" o)

    let ``Error: Invalid %s identifier`` s =
        MsgError(Some 2, sprintf "Invalid %s identifier" s)

    let ``Error: Invalid directive declaration`` =
        MsgError(Some 3, "Invalid directive declaration")

    let ``Error: Unfinished '{$%O}' block detected`` o =
        MsgError(Some 4, sprintf "Unfinished '{$%O}' block detected" o)

    let ``Error: Cannot find type identifier '%O'`` o =
        MsgError(Some 5, sprintf "Cannot find type identifier \"%O\"" o)

    let ``Error: Cannot find symbol '%O'`` o =
        MsgError(Some 6, sprintf "Cannot find symbol '%O'" o)

    let ``Error: Incompatible types ('%O' and '%O') for %d parameter`` o1 o2 d =
        MsgError(Some 7, sprintf "Incompatible types ('%O' and '%O') for %d parameter" o1 o2 d)

    let ``Error: Unknown type kind of expression for Write/WriteLn: %O`` o =
        MsgError(Some 8, sprintf "Unknown type kind of expression for Write/WriteLn: %O" o)

    let ``Error: Expected %s type but '%O' found`` s o =
        MsgError(Some 9, sprintf "Expected %s type but '%O' found" s o)

    let ``Error: Expected %s type parameter but nothing found`` s =
        MsgError(Some 10, sprintf "Expected %s type parameter but nothing found" s)

    let ``Error: Expected ident parameter but expression found`` =
        MsgError(Some 11, sprintf "Expected ident parameter but expression found")

    let ``Error: Expected ident parameter but nothing found`` =
        MsgError(Some 12, sprintf "Expected ident parameter but nothing found")

    let ``Error: Expected integer type but '%O' found`` o =
        MsgError(Some 13, sprintf "Expected integer type but '%O' found" o)

    let ``Error: Unexpected parameter`` =
        MsgError(Some 14, "Unexpected parameter")

    let ``Error: %O intrinsic cannot be used outside loop`` o =
        MsgError(Some 15, sprintf "%O intrinsic cannot be used outside loop" o)

    let ``Error: Incompatible types ('%O' and '%O') for '%O'`` o1 o2 o3 =
        MsgError(Some 16, sprintf "Incompatible types ('%O' and '%O') for \"%O\"" o1 o2 o3)

    let ``Error: %s expected`` s =
        MsgError(Some 17, sprintf "%s expected" s)

    let ``Error: %O type expected but '%O' found`` o1 o2 =
        MsgError(Some 18, sprintf "%O type expected but '%O' found" o1 o2)

    let ``Error: Improper expression`` =
        MsgError(Some 19, "Improper expression")

    let ``Error: More parameters expected`` =
        MsgError(Some 20, "More parameters expected")
        
    let ``Error: Cannot find file '%O'`` o =
        MsgError(Some 21, sprintf "Cannot find file '%O'" o)

[<AutoOpen>]
module Warnings =
    let ``Warning: Cannot find enviroment variable '%s'`` s =
        MsgWarning(Some 1, SevereWarning, sprintf "Cannot find enviroment variable '%s'" s)