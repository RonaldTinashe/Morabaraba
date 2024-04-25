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

    let switchTurns game _ =
        let board = game.Board
        let player, opponent = board.Player, board.Opponent

        let updatedBoard =
            { board with
                Player = opponent
                Opponent = player }

        Some { game with Board = updatedBoard }

    let decreaseHand game _ =
        let board = game.Board
        let opponent = game.Board.Opponent

        let updatedOpponent =
            { opponent with
                Hand = opponent.Hand - 1 }

        let updatedBoard =
            { board with
                Opponent = updatedOpponent }

        Some { game with Board = updatedBoard }

    Executor(placement, Executor(switchTurns, Executor(decreaseHand, NoExecutor, NoExecutor), NoExecutor), NoExecutor)

let execute game action =
    let rec innerExecute executor game action =
        match executor with
        | (Executor(placement, child, _)) ->
            let placedGame = placement game action

            match child with
            | NoExecutor -> placedGame
            | _ -> Option.bind (fun game -> innerExecute child game action) placedGame
        | NoExecutor -> None

    innerExecute executor game action

[<EntryPoint>]
let main _ = 0
