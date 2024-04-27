module Morabaraba.Execution

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

let isAMill shade occupants line =
    List.forall (fun junction -> Map.tryFind junction occupants = Some shade) line

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

let checkPlayerLastOccupied game _ =
    List.tryHead game.History
    |> Option.map (fun { Destination = d } ->
        Option.map
            (fun cowShade ->
                if game.Board.Player.Shade = cowShade then
                    Some game
                else
                    None)
            (Map.tryFind d game.Board.Occupants))
    |> Option.flatten
    |> Option.flatten

let checkPlayerMill game _ =
    let shade = game.Board.Player.Shade
    let occupants = game.Board.Occupants

    if List.exists (isAMill shade occupants) lines then
        Some game
    else
        None

let checkShootingTargetShade game action =
    let { Board = { Occupants = occupants
                    Opponent = opponent } } =
        game

    let isShadeAppropriate =
        Map.tryFind action.Destination occupants = Some opponent.Shade

    if isShadeAppropriate then Some game else None

let checkShootingTargetNotInMill game action =
    let shade = game.Board.Opponent.Shade
    let occupants = game.Board.Occupants

    let isDestinationInMill =
        List.filter (isAMill shade occupants) lines
        |> List.exists (List.contains action.Destination)

    if isDestinationInMill then None else Some game

let checkAllOpponentCowsAreInMills game _ =
    let opponentShade = game.Board.Opponent.Shade

    let opponentOccupants =
        Map.filter (fun _ shade -> shade = opponentShade) game.Board.Occupants

    let junctionsInOpponentMills =
        List.filter (isAMill opponentShade opponentOccupants) lines
        |> List.concat
        |> List.distinct

    if List.length junctionsInOpponentMills = Map.count opponentOccupants then
        Some game
    else
        None

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

let executorTree =
    let shootExecution =
        BinaryTree.Node(
            shoot,
            BinaryTree.Node(
                saveAction,
                BinaryTree.Node(switchTurns, BinaryTree.NoValue, BinaryTree.NoValue),
                BinaryTree.NoValue
            ),
            BinaryTree.NoValue
        )

    let shoot' =
        BinaryTree.Node(
            checkShootingTargetShade,
            BinaryTree.Node(
                checkShootingTargetNotInMill,
                shootExecution,
                BinaryTree.Node(checkAllOpponentCowsAreInMills, shootExecution, BinaryTree.NoValue)
            ),
            BinaryTree.NoValue
        )

    let checkMillOrSwitch =
        BinaryTree.Node(
            checkPlayerMill,
            BinaryTree.NoValue,
            BinaryTree.Node(switchTurns, BinaryTree.NoValue, BinaryTree.NoValue)
        )

    let place' =
        BinaryTree.Node(
            place,
            BinaryTree.Node(
                saveAction,
                BinaryTree.Node(decreaseHand, checkMillOrSwitch, BinaryTree.NoValue),
                BinaryTree.NoValue
            ),
            BinaryTree.NoValue
        )

    let placeOrMill =
        BinaryTree.Node(
            checkPlayerLastOccupied,
            shoot',
            BinaryTree.Node(
                checkPlacingDestination,
                BinaryTree.Node(checkPlacingHand, place', BinaryTree.NoValue),
                BinaryTree.NoValue
            )
        )

    placeOrMill

let initialGame =
    let player = { Shade = Dark; Hand = 12 }
    let opponent = { player with Shade = Light }

    let board =
        { Player = player
          Opponent = opponent
          Occupants = Map.empty }

    { History = []; Board = board }

let execute game action =

    let executionFolder gameOption ruleExecution =
        Option.bind (fun gameValue -> ruleExecution gameValue action) gameOption

    BinaryTree.fold executionFolder Option.isSome (Some game) executorTree
