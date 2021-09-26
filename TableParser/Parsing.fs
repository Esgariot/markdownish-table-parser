#if INTERACTIVE
#r "nuget: FSharpPlus"
#r "nuget: FParsec"
#else
namespace TableParser
#endif

module Parsing =
    open Microsoft.FSharp.Reflection
    open FParsec
    let pUnionCase c =
        match FSharpValue.GetUnionFields(c, c.GetType()) with
        | case, _ -> case.Name |> pstringCI

type TokenTable = private {
    Header: string [,]
    Body: string [,]
}
//    with member this.Whole withDelimiter =
//        let mutable result = Array2D.zeroCreateBased Array2D.

//module TokenTable =
    

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
    let pTokenTable =
        let setCol cs = updateUserState (fun u -> { u with Columns = Some cs })
        let checkCol cs =  userStateSatisfies (fun {Columns = c} -> c |> exists ((=) cs) ) <?> "Matching column count"
        spaces >>. pRow setCol .>> spaces .>>. sepEndBy1 (pRow checkCol) spaces 
        |>> (fun (first, rest) -> first::rest) // TODO: To TokenTable

module Domain =
    type ComponentId = ComponentId of int

    type ColumnType =
        | Attribute
        | Rate
        | Volume
        | TimeSeries
        | Meta

    type Header =
        | Component of ComponentId
        | DisplayGroup of string
        | ColumnType

    type HeaderColumn = {
        Component: ComponentId
        ColumnType: ColumnType
    }

module DomainParsing =
    open FParsec
    open Domain

    let pComponent = puint16 |>> (int >> ComponentId)
    let pColumnName = many anyChar |>> (fun x -> (new string(List.toArray x)).Trim())

//    let makeHeader {Header = header} =


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
            |		    	| 10    | abba          |           | 1..10 |       |
            |               |       |               |     1     |       |   2   |
            """

    let test = runParserOnString pTokenTable TableState.Empty "" Table
