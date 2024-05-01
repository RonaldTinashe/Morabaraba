// For more information see https://aka.ms/fsharp-console-apps
module Morabaraba.Console.Program

open Morabaraba
open ExecutionService

let layoutTemplate =
    """
MIND SPORT SOUTH AFRICA MORABARABA

E1  -   -   -   E2  -   -   -   E3
|   \           |           /    |
|       A1  -   A2  -   A3       |
|       |   \       /    |       |
|       |    R1 R2 R3    |       |
E8  -   A8   R8    R4   A4  -   E4
|       |    R7 R6 R5    |       |
|       |   /       \    |       |
|       A7  -   A6  -   A5       |
|   /           |           \    |
E7  -   -   -   E6  -   -   -   E5

Player

Status

"""

let promptMessage board =
    if board.Status = Playing then
        "Move: "
    else
        "Enter any key to quit"

let parseInput input =
    try
        match String.length input with
        | 2 ->
            Some
                { Source = None
                  Destination = Junction input }
        | 4 ->
            Some
                { Source = Some(Junction input.[0..1])
                  Destination = Junction input.[2..3] }
        | _ -> None
    with :? System.ArgumentException ->
        None

let renderCow =
    function
    | Dark -> "DC"
    | Light -> "LC"

let renderPlayer { Shade = s; Hand = h } =
    $"{s} competitor with {h} cows to spare"

let renderStatus: Status -> string = sprintf "%A"

let renderValue placeholder value =
    match value with
    | Some value -> value
    | None -> placeholder

let junctionString (Junction j) = j

let extractValues board =
    let occupationValues =
        Map.toList board.Occupants
        |> List.map (fun (junction, occupant) -> junctionString junction, renderCow occupant)

    let values =
        ("Player", renderPlayer board.Player)
        :: ("Status", renderStatus board.Status)
        :: occupationValues

    values

let renderBoard board =
    let valuesToRender = extractValues board
    let renderValue (layout: string) (placeholder: string, value: string) = layout.Replace(placeholder, value)
    let renderedBoard = List.fold renderValue layoutTemplate valuesToRender
    renderedBoard

let rec runGame board =
    let () = System.Console.Clear()
    let renderedBoard = renderBoard board
    let () = stdout.Write renderedBoard
    let () = stdout.Write(promptMessage board)
    let input = parseInput (stdin.ReadLine())

    match board.Status, input with
    | Playing, Some action ->
        let board = execute board action
        runGame board
    | _, None ->
        let () = System.Console.Clear() |> ignore
        let () = stdout.Write "Invalid input. Enter any key to quit."
        System.Console.ReadKey() |> ignore
    | _, _ -> System.Console.ReadKey() |> ignore

[<EntryPoint>]
let main _ = let () = runGame initialBoard in 0
