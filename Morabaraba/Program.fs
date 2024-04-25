module Morabaraba

[<RequireQualifiedAccess>]
module BinaryTree =
    type Node<'T> =
        | Node of value: 'T * left: Node<'T> * right: Node<'T>
        | NoValue

    let rec fold folder state tree =
        match tree with
        | NoValue -> state
        | Node(value, left, _) ->
            let state = folder state value
            fold folder state left

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

type Executor = Executor of BinaryTree.Node<Game -> Action -> option<Game>>

let initialGame =
    let player = { Shade = Dark; Hand = 12 }
    let opponent = { player with Shade = Light }

    let board =
        { Player = player
          Opponent = opponent
          Occupants = Map.empty }

    { History = []; Board = board }

let executor =
    let checkPlacingHand game _ =
        let player = game.Board.Player
        if player.Hand > 0 then Some game else None

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

    BinaryTree.Node(
        checkPlacingHand,
        BinaryTree.Node(
            placement,
            BinaryTree.Node(
                switchTurns,
                BinaryTree.Node(decreaseHand, BinaryTree.NoValue, BinaryTree.NoValue),
                BinaryTree.NoValue
            ),
            BinaryTree.NoValue
        ),
        BinaryTree.NoValue
    )
    |> Executor

let execute game action =
    let executionFolder gameOption executionValue =
        Option.bind (fun gameValue -> executionValue gameValue action) gameOption

    match executor with
    | Executor e -> BinaryTree.fold executionFolder (Some game) e

[<EntryPoint>]
let main _ = 0
