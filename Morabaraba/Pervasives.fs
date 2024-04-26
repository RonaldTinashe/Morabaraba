[<AutoOpen>]
module Morabaraba.Pervasives

type Shade =
    | Dark
    | Light

type Junction = Junction of string

type Competitor = { Shade: Shade; Hand: int }

type Board =
    { Player: Competitor
      Opponent: Competitor
      Occupants: Map<Junction, Shade> }

type Action =
    { Source: option<Junction>
      Destination: Junction }

type Game = { Board: Board; History: list<Action> }
