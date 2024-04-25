module Morabaraba

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

type Executor =
    | Executor of (Game -> Action -> option<Game>) * child: Executor * failed: Executor
    | NoExecutor

let initialGame =
    let player = { Shade = Dark; Hand = 12 }
    let opponent = { player with Shade = Light }

    let board =
        { Player = player
          Opponent = opponent
          Occupants = Map.empty }

    { History = []; Board = board }

let executor =
    let placement game action =
        let board = game.Board
        let updatedOccupants = Map.add action.Destination board.Player.Shade board.Occupants

        let updatedBoard =
            { board with
                Occupants = updatedOccupants }

        Some { game with Board = updatedBoard }

    Executor(placement, NoExecutor, NoExecutor)

let execute game action =
    match executor with
    | (Executor(placement, _, _)) -> placement game action
    | NoExecutor -> None


[<EntryPoint>]
let main _ = 0
