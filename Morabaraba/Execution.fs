module Morabaraba.Execution

// Helpers
let createJunction (letter: char) (number: int) = Junction $"{letter}{number}"
let boardNumbers = [ 1..8 ]

let lines =
    let flip a b c = a c b

    let sameLetterLines =
        let numbersLines = [ [ 1; 2; 3 ]; [ 7; 6; 5 ]; [ 1; 8; 7 ]; [ 3; 4; 5 ] ]
        let letters = [ 'E'; 'A'; 'R' ]
        List.collect (fun l -> List.map (fun nl -> List.map (createJunction l) nl) numbersLines) letters

    let sameNumberLines =
        boardNumbers
        |> List.map (fun number -> List.map ((flip createJunction) number) [ 'E'; 'A'; 'R' ])

    sameLetterLines @ sameNumberLines

let isAMill shade occupants line =
    List.forall (fun junction -> Map.tryFind junction occupants = Some shade) line

let occupantsByShade competitorShade occupants =
    Map.filter (fun _ shade -> shade = competitorShade) occupants

let junctionsInMillsByShade competitorShade occupants =
    let competitorOccupants = occupantsByShade competitorShade occupants

    List.filter (isAMill competitorShade competitorOccupants) lines
    |> List.concat
    |> List.distinct

// Executors
let checkPlacingHand board _ =
    let player = board.Player
    if player.Hand > 0 then Some board else None

let checkPlacingDestination board action =
    let occupants = board.Occupants

    if Map.containsKey action.Destination occupants then
        None
    else
        Some board

let place board action =
    let updatedOccupants = Map.add action.Destination board.Player.Shade board.Occupants

    Some
        { board with
            Occupants = updatedOccupants }

let switchTurns board _ =
    let player, opponent = board.Player, board.Opponent

    Some
        { board with
            Player = opponent
            Opponent = player }

let decreaseHand board _ =
    let player = board.Player

    let updatedPlayer = { player with Hand = player.Hand - 1 }

    Some { board with Player = updatedPlayer }

let checkShootingTargetShade board action =
    let isShadeAppropriate =
        Map.tryFind action.Destination board.Occupants = Some board.Opponent.Shade

    if isShadeAppropriate then Some board else None

let checkShootingTargetNotInMill board action =
    let shade = board.Opponent.Shade
    let occupants = board.Occupants

    let isDestinationInMill =
        List.filter (isAMill shade occupants) lines
        |> List.exists (List.contains action.Destination)

    if isDestinationInMill then None else Some board

let checkAllOpponentCowsAreInMills board _ =
    let opponentOccupants = occupantsByShade board.Opponent.Shade board.Occupants

    let junctionsInOpponentMills =
        junctionsInMillsByShade board.Opponent.Shade board.Occupants

    if List.length junctionsInOpponentMills = Map.count opponentOccupants then
        Some board
    else
        None

let checkPlayerMillIsNew board _ =
    let junctionsInPlayerMills =
        junctionsInMillsByShade board.Player.Shade board.Occupants

    List.tryHead board.History
    |> Option.map (fun { Destination = d } ->
        if List.contains d junctionsInPlayerMills then
            Some board
        else
            None)
    |> Option.flatten

let shoot board action =
    let updatedOccupants = Map.remove action.Destination board.Occupants

    Some
        { board with
            Occupants = updatedOccupants }

let checkMovingJunctions board action =
    let sameLetterNeighbours =
        let numbers = [ 1, 2; 2, 3; 7, 6; 6, 5; 1, 8; 8, 7; 3, 4; 4, 5 ]
        let letters = [ 'E'; 'A'; 'R' ]

        List.collect
            (fun letter -> List.map (fun (n1, n2) -> (createJunction letter n1), (createJunction letter n2)) numbers)
            letters

    let sameNumberNeigbours =
        let letterNeighbours = [ 'E', 'A'; 'A', 'R' ]

        List.collect
            (fun (letter1, letter2) ->
                List.map (fun n -> (createJunction letter1 n), (createJunction letter2 n)) boardNumbers)
            letterNeighbours

    let neighbours = sameLetterNeighbours @ sameNumberNeigbours

    let areJunctionsNeighbours =
        match action.Source with
        | Some source ->
            List.contains (source, action.Destination) neighbours
            || List.contains (action.Destination, source) neighbours
        | None -> false

    if areJunctionsNeighbours then Some board else None

let checkLegalMillFormation board action =
    let occupantsWithBrokenMill =
        Map.add action.Destination board.Player.Shade board.Occupants

    let wasInAMill =
        let millJunctions =
            junctionsInMillsByShade board.Player.Shade occupantsWithBrokenMill

        List.contains action.Destination millJunctions

    let isInAMill =
        let millJunctions = junctionsInMillsByShade board.Player.Shade board.Occupants

        List.exists (fun junction -> Some junction = action.Source) millJunctions

    let wasOpponentLastActionAShot =
        let lastOpponentAction = List.tryHead board.History

        match lastOpponentAction with
        | Some { Source = None; Destination = d } -> not <| Map.containsKey d board.Occupants
        | _ -> false

    let isActionReverseOfPrevious =
        // Action indexed 0 is the opponent's last action
        // Action indexed 1 is the player's last shot action
        // Action indexed 2 might the player's last non-action action without a shot
        // Action indexed 3 might be the player last action before a shot
        let lastNonShotActionIndex = if wasOpponentLastActionAShot then 3 else 2
        let lastNonShotAction = List.tryItem lastNonShotActionIndex board.History

        match lastNonShotAction with
        | Some { Source = Some previousSource
                 Destination = previousDestination } ->
            action = { Source = Some previousDestination
                       Destination = previousSource }
        | _ -> false

    if wasInAMill && isInAMill && isActionReverseOfPrevious then
        None
    else
        Some board

let move board action =
    Option.map (fun source -> Map.remove source board.Occupants) action.Source
    |> Option.map (Map.add action.Destination board.Player.Shade)
    |> Option.map (fun occupants -> { board with Occupants = occupants })

let saveAction board action =
    Some
        { board with
            History = action :: board.History }

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
            checkPlayerMillIsNew,
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

    let moveExecution =
        BinaryTree.Node(move, BinaryTree.Node(saveAction, checkMillOrSwitch, BinaryTree.NoValue), BinaryTree.NoValue)

    let move' =
        BinaryTree.Node(
            checkMovingJunctions,
            BinaryTree.Node(checkLegalMillFormation, moveExecution, BinaryTree.NoValue),
            BinaryTree.NoValue
        )

    let placeOrMill =
        BinaryTree.Node(
            checkPlayerMillIsNew,
            shoot',
            BinaryTree.Node(
                checkPlacingDestination,
                BinaryTree.Node(checkPlacingHand, place', move'),
                BinaryTree.NoValue
            )
        )

    placeOrMill

let initialBoard =
    let player = { Shade = Dark; Hand = 12 }
    let opponent = { player with Shade = Light }

    { Player = player
      Opponent = opponent
      Occupants = Map.empty
      History = [] }

let execute board action =

    let executionFolder boardOption ruleExecution =
        Option.bind (fun boardValue -> ruleExecution boardValue action) boardOption

    BinaryTree.fold executionFolder Option.isSome (Some board) executorTree
