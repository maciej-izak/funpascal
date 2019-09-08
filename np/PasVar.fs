[<AutoOpen>]
module np.PasVar

open FParsec
open PasAst

let opp = new OperatorPrecedenceParser<ExprEl,unit,unit>()
