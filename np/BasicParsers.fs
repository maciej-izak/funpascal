[<AutoOpen>]
module np.BasicParsers

open FParsec

let ws = spaces

let commentBlock: Parser<unit,unit> =
    skipChar '{' .>> skipManyTill skipAnyChar (skipChar '}')
        
let starCommentBlock: Parser<unit,unit> =
    skipString "(*" .>> skipManyTill skipAnyChar (skipString "*)")
    
let wsc: Parser<unit, unit> = skipMany (choice[spaces1; commentBlock; starCommentBlock] <?> "")
