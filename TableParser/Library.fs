#if  INTERACTIVE
#r "nuget: FSharpPlus"
#r "nuget: FParsec"
#else
namespace TableParser
#endif

module Parsing =
    open FSharp.Reflection
    open FSharpPlus
    open FParsec

    let inline pUnion<'a> () : Parser<'a, 'unit> =
        match FSharpType.IsUnion typeof<'a> with
            | true -> FSharpType.GetUnionCases typeof<'a>
            | false -> [||]
        |> map (fun x -> (FSharpValue.MakeUnion(x, [||]) :?> 'a, x.Name))
        |> map (fun (case, name) -> pstringCI name >>% case)
        |> choice

    let pUnionCase c =
        match FSharpValue.GetUnionFields(c, c.GetType()) with
        | case, _ -> case.Name |> pstringCI

    // let pWord = many1Satisfy isAsciiLetter

    let spaceNoNewLine = [|'\t';' '|] |> map pchar |> choice
    let whitespaceNoNewLine = many spaceNoNewLine

module TableParsing =
    open FParsec
    open Parsing
    
    let pWall = pchar '|'
    
    let pHeaderRow pFirst pRest pColumnState =
        let wall = between whitespaceNoNewLine whitespaceNoNewLine pWall
        (wall >>? pFirst .>> wall)
        .>>.
        (sepEndBy pRest wall >>= (fun x -> x |> List.length |> pColumnState >>% x))
        .>> (opt newline)

    type TableState<'columnType> = {
        Columns: int option
        Types: 'columnType list
    }
    with static member Empty = {Columns = None;Types = []}
    
module Domain =
    type Header =
        | Component
        | DisplayGroup
        | Id
        | Parent

    type Component = { ID: int }

    type ColumnType =
        | Attribute

    type HeaderRow =
        |Component of Component list
        |ColumnType of ColumnType list


module DomainParsing =
    open FSharpPlus
    open TableParsing
    open Parsing
    open FParsec
    open Domain

    let pComponent = puint16 |>> (fun x -> { ID = x |> int })

    let pColumnType = pUnion<ColumnType>()

    let pDomainHeaderRow u =
        let r first rest  = pHeaderRow (pUnionCase first) rest u >>= (snd >> first)
        let (~%) x = x >> preturn
        let setColumnTypes cs = updateUserState ( fun u -> {u with Types = cs})
        choice [
            r   %Component                                                 pComponent 
            r   (fun cs -> cs |> %ColumnType .>> (cs |> setColumnTypes))   pColumnType
        ]

    let pInitialHeaderRow p =
        p (fun c-> updateUserState ( fun u -> {u with Columns = Some c}))

    let pSubsequentHeaderRow p=
        p (fun c-> userStateSatisfies ( fun u-> u.Columns |> exists((=)c)) <?> "column count equal to that of the first row")

    let pSeparatorRow u =
        let line = pchar '-' |> many
        pHeaderRow line line u |>> ignore


    let pHeader = parse {
        do! spaces
        let! firstRow = pInitialHeaderRow pDomainHeaderRow
        let! restOfRows = (pSubsequentHeaderRow pDomainHeaderRow) |> many
        do! pSubsequentHeaderRow pSeparatorRow
        return firstRow::restOfRows
    }


[<AutoOpen>]
module Test = 
    open DomainParsing
    open TableParsing
    open Domain
    open FParsec

    [<Literal>]
    let Table = 
            """
            |Component   	|100    | 100       	|	101		| 100   |   101 |
            |ColumnName  	|a      |  b        	| Parent	| Id    |   Id  |
            |ColumnType  	|int    |string			| parent	| id    |   id  |
            |---------------|-------|---------------|-----------|-------|-------|
            |				| 10    | abba          |           | 1..10 |       |
            |               |       |               |     1     |       |   2   |
            """

    let result = runParserOnString pHeader TableState<ColumnType>.Empty "" Table