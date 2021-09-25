#if INTERACTIVE
#r "nuget: FSharpPlus"
#r "nuget: FParsec"
#else
namespace TableParser
#endif

module TableParsing =
    open FSharpPlus
    open FParsec
   
    type TableState = {
        Columns: int option
    }
    with static member Empty = { Columns = None}


    let pWall = pchar '|'
    let pEscape = pipe2 (pchar '\\') (anyOf "\\|\n") (fun a b -> [a;b])
    let pNonEscape = noneOf "\\\|\n"
    let pCellChar = (pNonEscape |>> (fun x -> [x])) <|> pEscape
    let pCell = many pCellChar |>> (List.concat >> List.toArray) |>> (fun x -> new string(x))
    let pRow u = spaces >>. pWall >>. (sepBy1 pCell pWall) .>> skipRestOfLine false |>> (fun s -> s.[..^1]) 
                >>= (fun cs -> cs |> (u << List.length) >>. preturn cs)
    let pTable = 
        let setCol cs = updateUserState (fun u -> { u with Columns = Some cs })
        let checkCol cs =  userStateSatisfies (fun {Columns = c} -> c |> exists ((=) cs) ) <?> "Matching column count"
        spaces >>. pRow setCol .>> spaces .>>. sepEndBy1 (pRow checkCol) spaces 
        |>> (fun (first, rest) -> first::rest) 

module DomainParsing =
    open FParsec


module Testing =
    open FParsec
    open TableParsing

    [<Literal>]
    let Table =
            """
            |Component   	|100    | 100       	|	101		| 100   |   101 |
            |ColumnName  	|a      |  b        	| Parent	| Id    |   \\  |
            |ColumnType  	|int    |string			| parent	| id    |   \|  |
            |---------------|-------|---------------|-----------|-------|-------|
            |				| 10    | abba          |           | 1..10 |       |
            |               |       |               |     1     |       |   2   |
            """

    let test = runParserOnString pTable TableState.Empty "" Table
