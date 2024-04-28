module Morabaraba.Tests

open System
open Xunit
open Morabaraba
open Execution

[<Fact>]
let ``Initial player's shade is Dark`` () =
    let player = initialBoard.Player
    Assert.Equal(Dark, player.Shade)

[<Fact>]
let ``Initial opponent's shade is Light`` () =
    let opponent = initialBoard.Opponent
    Assert.Equal(Light, opponent.Shade)

[<Fact>]
let ``Initial player has 12 cows`` () =
    let player = initialBoard.Player
    Assert.Equal(12, player.Hand)

[<Fact>]
let ``Initial opponent has 12 cows`` () =
    let opponent = initialBoard.Opponent
    Assert.Equal(12, opponent.Hand)

[<Fact>]
let ``Initial player occupies A1`` () =
    let action =
        { Source = None
          Destination = Junction "A1" }

    let board = execute initialBoard action |> Option.get
    let occupants = board.Occupants
    Assert.Equal(Dark, occupants.[Junction "A1"])

[<Fact>]
let ``Player is light after initial player has occupied A1`` () =
    let action =
        { Source = None
          Destination = Junction "A1" }

    let board = execute initialBoard action |> Option.get
    Assert.Equal(Light, board.Player.Shade)

[<Fact>]
let ``Initial dark competitor should have 11 cows after occupying A1`` () =
    let action =
        { Source = None
          Destination = Junction "A1" }

    let board = execute initialBoard action |> Option.get
    Assert.Equal(11, board.Opponent.Hand)

[<Fact>]
let ``Light player cannot place with an empty hand`` () =
    let action =
        { Source = None
          Destination = Junction "A1" }

    let player = { Shade = Light; Hand = 0 }
    let occupants = List.init 5 (fun i -> Junction $"E{i + 1}", Light) |> Map

    let board =

        { Occupants = occupants
          Player = player
          Opponent = { player with Shade = Dark }
          History = [] }

    Assert.Equal(None, execute board action)

[<Fact>]
let ``Light player cannot place on E1 if E1 is occupied`` () =
    let action =
        { Source = None
          Destination = Junction "E1" }

    let player = { Shade = Light; Hand = 7 }
    let occupants = List.init 5 (fun i -> Junction $"E{i + 1}", Dark) |> Map

    let board =
        { Occupants = occupants
          Player = player
          Opponent = { player with Shade = Dark }
          History = [] }

    Assert.Equal(None, execute board action)

[<Fact>]
let ``Save action after first action is executed`` () =
    let action =
        { Source = None
          Destination = Junction "A1" }

    let board = execute initialBoard action |> Option.get
    Assert.Equal<list<Action>>([ action ], board.History)

[<Fact>]
let ``Do not switch turns after dark player forms a mill`` () =
    [ "A1"; "R1"; "A2"; "R2"; "A3" ]
    |> List.fold
        (fun boardState junction ->
            Option.bind
                (fun board ->
                    execute
                        board
                        { Source = None
                          Destination = Junction junction })
                boardState)
        (Some initialBoard)
    |> Option.get
    |> fun { Player = player } -> Assert.Equal(Dark, player.Shade)

[<Fact>]
let ``Allow dark player to remove cow if they have a mill`` () =
    let target = "R1"

    [ "A1"; target; "A2"; "R2"; "A3"; target ]
    |> List.fold
        (fun boardState junction ->
            Option.bind
                (fun board ->
                    execute
                        board
                        { Source = None
                          Destination = Junction junction })
                boardState)
        (Some initialBoard)
    |> Option.get
    |> fun { Occupants = occupants } -> Assert.False(Map.containsKey (Junction target) occupants)

[<Fact>]
let ``Prevent light player from removing light cow`` () =
    let target = "R1"

    [ "A1"; target; "A2"; "R2"; "E3"; "R3"; target ]
    |> List.fold
        (fun boardState junction ->
            Option.bind
                (fun board ->
                    execute
                        board
                        { Source = None
                          Destination = Junction junction })
                boardState)
        (Some initialBoard)
    |> fun boardOption -> Assert.Equal(None, boardOption)

[<Fact>]
let ``Light player cannot shoot an empty junction`` () =
    let target = "A8"

    [ "A1"; "R1"; "A2"; "R2"; "E3"; "R3"; target ]
    |> List.fold
        (fun boardState junction ->
            Option.bind
                (fun board ->
                    execute
                        board
                        { Source = None
                          Destination = Junction junction })
                boardState)
        (Some initialBoard)
    |> fun boardOption -> Assert.Equal(None, boardOption)

[<Fact>]
let ``Dark player cannot shoot a light cow in a mill`` () =
    [ "E1"; "R4"; "A1"; "R1"; "A2"; "R2"; "A8"; "R3"; "A8"; "A3"; "R3" ]
    |> List.fold
        (fun boardState junction ->
            Option.bind
                (fun board ->
                    execute
                        board
                        { Source = None
                          Destination = Junction junction })
                boardState)
        (Some initialBoard)
    |> fun boardOption -> Assert.Equal(None, boardOption)

[<Fact>]
let ``Dark player can shoot a light cow if all light cows are in a mill`` () =
    [ "A1"; "R1"; "A2"; "R2"; "A8"; "R3"; "A8"; "A3"; "R3" ]
    |> List.fold
        (fun boardState junction ->
            Option.bind
                (fun board ->
                    execute
                        board
                        { Source = None
                          Destination = Junction junction })
                boardState)
        (Some initialBoard)
    |> fun boardOption -> Assert.NotEqual(None, boardOption)

[<Fact>]
let ``Dark player can only shoot with a newly formed mill`` () =
    [ "A1"; "R1"; "A2"; "R2"; "A3"; "R2"; "R2"; "A4"; "R2" ] // Mill "A1;A2;A3" cannot be reused
    |> List.fold
        (fun boardState junction ->
            Option.bind
                (fun board ->
                    execute
                        board
                        { Source = None
                          Destination = Junction junction })
                boardState)
        (Some initialBoard)
    |> fun boardOption -> Assert.Equal(None, boardOption)

let boardAfterMovementSimulation =
    [ "A1" // Dark places
      "R1" // Light places
      "A2" // Dark places
      "R2" // Light places
      "A3" // Dark places forming a mill
      "R2" // Dark shoots Light
      "R2" // Light replaces shot cow
      "A4" // Dark places
      "R3" // Light places forming a mill
      "A4" // Light shoots Dark
      "A4" // Dark replaces shot cow
      "R4" ] // Light places
    |> List.fold
        (fun boardState junction ->
            Option.bind
                (fun board ->
                    execute
                        board
                        { Source = None
                          Destination = Junction junction })
                boardState)
        (Some initialBoard)
    |> Option.map (fun board ->
        { board with
            Player = { board.Player with Hand = 0 }
            Opponent = { board.Opponent with Hand = 0 } })
    |> Option.bind (fun board ->
        execute
            board
            { Source = Some(Junction "A4")
              Destination = Junction "E4" })

[<Fact>]
let ``Movement removes source junction occupant, placing them on the destination`` () =
    match boardAfterMovementSimulation with
    | None -> Assert.Fail(sprintf "Failed to move cow. Current board state is %A" boardAfterMovementSimulation)
    | Some { Occupants = o } ->
        let sourceJunctionOccupant = Map.tryFind (Junction "A4") o
        let destinationJunctionOccupant = Map.tryFind (Junction "E4") o
        Assert.Equal((None, Some Dark), (sourceJunctionOccupant, destinationJunctionOccupant))

[<Fact>]
let ``Movements are saved`` () =
    Option.bind (fun { History = h } -> List.tryHead h) boardAfterMovementSimulation
    |> fun action ->
        Assert.Equal(
            Some
                { Source = Some(Junction "A4")
                  Destination = Junction "E4" },
            action
        )

[<Fact>]
let ``After dark player moves cow and does not form a mill, the next player is light`` () =
    match boardAfterMovementSimulation with
    | Some { Player = { Shade = s } } -> Assert.Equal(Light, s)
    | None -> Assert.Fail(sprintf "Failed to move cow. Current board state is %A" boardAfterMovementSimulation)

[<Fact>]
let ``Player cannot fly with more than three cows on the board`` () =
    boardAfterMovementSimulation
    |> Option.bind (fun board ->
        execute
            board
            { Source = Some(Junction "R1")
              Destination = Junction "R7" })
    |> fun illegalBoard -> Assert.Equal(None, illegalBoard)

[<Fact>]
let ``Restrict broken mill recreation after opponent's non-shot action`` () =
    let movingPlayer = { Shade = Dark; Hand = 0 } // Hand has 0 for moving phase

    let breakingMovement =
        { Source = Some(Junction "E6")
          Destination = Junction "A6" }

    let illegalMovement =
        { Source = Some breakingMovement.Destination
          Destination = Option.get breakingMovement.Source }

    [ "E7" // Dark places
      "E1" // Light places
      "A7" // Dark places
      "A1" // Light places
      "E5" // Dark places
      "E3" // Light places
      "A5" // Dark places
      "A3" // Light places
      "E6" // Dark places and forms mill
      "E1" // Dark shoots Light
      "E1" ] // Light replaces shot cow
    |> List.fold
        (fun boardState junction ->
            let action =
                { Source = None
                  Destination = Junction junction }

            Option.bind (fun board -> execute board action) boardState)
        (Some initialBoard)
    |> Option.map (fun board -> { board with Player = movingPlayer })
    |> Option.bind (fun board -> execute board breakingMovement) // break mill
    |> Option.bind (fun board ->
        execute
            board
            { Source = None
              Destination = Junction "E1" }) // Dark shoots Light
    |> Option.bind (fun board ->
        execute
            board
            { Source = None
              Destination = Junction "E1" }) // Light places
    |> Option.bind (fun board -> execute board illegalMovement)
    |> fun board -> Assert.Equal(None, board)

[<Fact>]
let ``Restrict broken mill recreation after opponent's shot action`` () =
    let movingPlayer = { Shade = Dark; Hand = 0 } // Hand has 0 for moving phase

    let breakingMovement =
        { Source = Some(Junction "E6")
          Destination = Junction "A6" }

    let illegalMovement =
        { Source = Some breakingMovement.Destination
          Destination = Option.get breakingMovement.Source }

    [ "E7" // Dark places
      "E1" // Light places
      "A7" // Dark places
      "A1" // Light places
      "E5" // Dark places
      "E3" // Light places
      "R4" // Dark places
      "A4" // Light places
      "A5" // Dark places
      "A3" // Light places
      "E6" // Dark places and forms mill
      "E1" // Dark shoots Light
      "E1" ] // Light replaces shot cow
    |> List.fold
        (fun boardState junction ->
            let action =
                { Source = None
                  Destination = Junction junction }

            Option.bind (fun board -> execute board action) boardState)
        (Some initialBoard)
    |> Option.map (fun board -> { board with Player = movingPlayer })
    |> Option.bind (fun board -> execute board breakingMovement) // break mill
    |> Option.bind (fun board ->

        execute
            board
            { Source = None
              Destination = Junction "E1" }) // Dark shoots Light
    |> Option.bind (fun board ->
        execute
            board
            { Source = None
              Destination = Junction "A2" }) // Light places
    |> Option.bind (fun board ->
        execute
            board
            { Source = None
              Destination = Junction "R4" }) // Light shoots
    |> Option.bind (fun board -> execute board illegalMovement)
    |> fun board -> Assert.Equal(None, board)

[<Fact>]
let ``Player with three cows on the board and an empty hand can fly`` () =
    let player = { Shade = Dark; Hand = 0 }

    let occupants =
        [ "A1", Dark; "A2", Light; "A3", Dark; "E3", Light; "A6", Dark; "E5", Light ]
        |> List.map (fun (junction, shade) -> (Junction junction), shade)
        |> Map

    let movement =
        { Source = Some(Junction "A1")
          Destination = Junction "E7" }

    let board =
        { Player = player
          Opponent = { player with Shade = Light }
          History = []
          Occupants = occupants }

    let boardAfterExecution = execute board movement
    Assert.NotEqual(None, boardAfterExecution)

[<Fact>]
let ``Disallow placement on non-existent junctions`` () =
    let createIllegalJunction = System.Action(fun () -> ignore <| Junction "X9")
    Assert.Throws<ArgumentException>(createIllegalJunction)
