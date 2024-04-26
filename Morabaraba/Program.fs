module Morabaraba

[<RequireQualifiedAccess>]
module BinaryTree =
    type Node<'T> =
        | Node of value: 'T * left: Node<'T> * right: Node<'T>
        | NoValue

    let rec fold folder shouldSelectLeft state tree =
        match tree with
        | NoValue -> state
        | Node(value, left, right) ->
            let state2 = folder state value

            if shouldSelectLeft state2 then
                let state3 = fold folder shouldSelectLeft state2 left
                state3
            else
                match right with
                | NoValue -> state2
                | Node _ ->
                    let state4 = fold folder shouldSelectLeft state right
                    state4

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

    let checkPlacingDestination game action =
        let occupants = game.Board.Occupants

        if Map.containsKey action.Destination occupants then
            None
        else
            Some game

    let place game action =
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
        let player = game.Board.Player

        let updatedPlayer = { player with Hand = player.Hand - 1 }

        let updatedBoard = { board with Player = updatedPlayer }

        Some { game with Board = updatedBoard }

    let checkPlayerMill game _ =
        let lines =
            let flip a b c = a c b
            let createJunction (letter: char) (number: int) = Junction $"{letter}{number}"

            let sameLetterLines =
                let numbersLines = [ [ 1; 2; 3 ]; [ 7; 6; 5 ]; [ 1; 8; 7 ]; [ 3; 4; 5 ] ]
                let letters = [ 'E'; 'A'; 'R' ]
                List.collect (fun l -> List.map (fun nl -> List.map (createJunction l) nl) numbersLines) letters

            let sameNumberLines =
                [ 1..8 ]
                |> List.map (fun number -> List.map ((flip createJunction) number) [ 'E'; 'A'; 'R' ])

            sameLetterLines @ sameNumberLines

        let isAMill line =
            let player = game.Board.Player
            let occupants = game.Board.Occupants
            List.forall (fun junction -> Map.tryFind junction occupants = Some player.Shade) line

        if List.exists isAMill lines then Some game else None

    let checkShootingTargetShade game action =
        let { Board = { Occupants = occupants
                        Opponent = opponent } } =
            game

        let isShadeAppropriate =
            Map.tryFind action.Destination occupants = Some opponent.Shade

        if isShadeAppropriate then Some game else None

    let shoot game action =
        let updatedOccupants = Map.remove action.Destination game.Board.Occupants

        Some
            { game with
                Board =
                    { game.Board with
                        Occupants = updatedOccupants } }

    let saveAction game action =
        Some
            { game with
                History = action :: game.History }

    BinaryTree.Node(
        checkPlacingDestination,
        BinaryTree.Node(
            checkPlacingHand,
            BinaryTree.Node(
                place,
                BinaryTree.Node(
                    saveAction,
                    BinaryTree.Node(
                        decreaseHand,
                        BinaryTree.Node(
                            checkPlayerMill,
                            BinaryTree.NoValue,
                            BinaryTree.Node(switchTurns, BinaryTree.NoValue, BinaryTree.NoValue)
                        ),
                        BinaryTree.NoValue
                    ),
                    BinaryTree.NoValue
                ),
                BinaryTree.NoValue
            ),
            BinaryTree.NoValue
        ),
        BinaryTree.Node(
            checkPlayerMill,
            BinaryTree.Node(
                checkShootingTargetShade,
                BinaryTree.Node(
                    shoot,
                    BinaryTree.Node(
                        saveAction,
                        BinaryTree.Node(switchTurns, BinaryTree.NoValue, BinaryTree.NoValue),
                        BinaryTree.NoValue
                    ),
                    BinaryTree.NoValue
                ),
                BinaryTree.NoValue
            ),
            BinaryTree.NoValue
        )
    )
    |> Executor

let execute game action =
    let executionFolder gameOption ruleExecution =
        Option.bind (fun gameValue -> ruleExecution gameValue action) gameOption

    match executor with
    | Executor e -> BinaryTree.fold executionFolder Option.isSome (Some game) e

[<EntryPoint>]
let main _ = 0
