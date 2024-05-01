[<AutoOpen>]
module Morabaraba.Pervasives

type Shade =
    | Dark
    | Light

type Junction = Junction of string

let Junction coordinates =
    let letters = [ 'A'; 'E'; 'R' ]
    let numbers = [ 1..8 ]

    let coordinatesCollection =
        List.collect (fun letter -> List.map (fun number -> $"{letter}{number}") numbers) letters

    if List.contains coordinates coordinatesCollection then
        Junction coordinates
    else
        let rulesUrl =
            "https://web.archive.org/web/20231121003005\
/https://esportscommentator.blogspot.com/2021/05/generally-accepted-rules-gar-for.html"

        invalidArg "coordinates" $"Find the legal coordinates via {rulesUrl}"

type Competitor = { Shade: Shade; Hand: int }

type Action =
    { Source: option<Junction>
      Destination: Junction }

type Status =
    | Won
    | Lost
    | Drew
    | Playing

type Board =
    { Player: Competitor
      Opponent: Competitor
      Occupants: Map<Junction, Shade>
      History: list<Action>
      Status: Status }
