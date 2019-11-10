[<AutoOpen>]
module NP.PasOperators

open FParsec
open PasAst

type InfixOperators = {
    priority: int
    operators: (string * (ExprEl * ExprEl -> ExprEl)) list
}

type PrefixOperators = {
    priority: int
    operators: (string * (ExprEl -> ExprEl)) list
}

let infixOperators: InfixOperators list = [ 
                {   priority = 1
                    operators = [
                        ("=",  Equal);
                        ("<>", NotEqual);
                        ("<",  StrictlyLessThan);
                        (">",  StrictlyGreaterThan);
                        ("<=", LessThanOrEqual);
                        (">=", GreaterThanOrEqual);
                        ("in", In);
                        ("is", Is); 
                    ]};
                {   priority = 2 
                    operators = [
                        ("+",  Add);
                        ("-", Minus);
                        ("or",  Or);
                        ("xor",  Xor);
                    ]};
                {   priority = 3 
                    operators = [
                        ("*",  Multiply);
                        ("/", Divide);
                        ("div",  Div);
                        ("mod",  Mod);
                        ("and",  And);
                        ("shl",  Shl);
                        ("shr",  Shr);
                        ("as",  As);
                    ]};
]

let prefixOperators = {
    priority = 4
    operators = [
        ("@", Addr)
        ("not", Not)
        ("-", UnaryMinus)
        ("+", UnaryPlus)
    ]}

let addInfixOperators (ops: InfixOperators) =
    let imap o = fun (x: ExprEl) (y: ExprEl) -> o(x, y) 
    for s, op in ops.operators do
        InfixOperator(s, wsc, ops.priority, Associativity.Left, imap op)
        |> opp.AddOperator

let addPrefixOperators ops =
    let pmap o = fun (x: ExprEl) -> o(x) 
    for s, op in ops.operators do
        PrefixOperator(s, wsc, ops.priority, true, pmap op) 
        |> opp.AddOperator

let addOperators() =
    for ops in infixOperators do addInfixOperators ops
    addPrefixOperators prefixOperators
