module Morabaraba.Tests

open System
open Xunit
open Morabaraba
open Execution

[<Fact>]
let ``Initial player's shade is Dark`` () =
    let player = initialGame.Board.Player
    Assert.Equal(Dark, player.Shade)

[<Fact>]
let ``Initial opponent's shade is Light`` () =
    let opponent = initialGame.Board.Opponent
    Assert.Equal(Light, opponent.Shade)

[<Fact>]
let ``Initial player has 12 cows`` () =
    let player = initialGame.Board.Player
    Assert.Equal(12, player.Hand)

[<Fact>]
let ``Initial opponent has 12 cows`` () =
    let opponent = initialGame.Board.Opponent
    Assert.Equal(12, opponent.Hand)

[<Fact>]
let ``Initial player occupies A1`` () =
    let action =
        { Source = None
          Destination = Junction "A1" }

    let game = execute initialGame action |> Option.get
    let occupants = game.Board.Occupants
    Assert.Equal(Dark, occupants.[Junction "A1"])

[<Fact>]
let ``Player is light after initial player has occupied A1`` () =
    let action =
        { Source = None
          Destination = Junction "A1" }

    let game = execute initialGame action |> Option.get
    Assert.Equal(Light, game.Board.Player.Shade)

[<Fact>]
let ``Initial dark competitor should have 11 cows after occupying A1`` () =
    let action =
        { Source = None
          Destination = Junction "A1" }

    let game = execute initialGame action |> Option.get
    Assert.Equal(11, game.Board.Opponent.Hand)

[<Fact>]
let ``Light player cannot place with an empty hand`` () =
    let action =
        { Source = None
          Destination = Junction "A1" }

    let player = { Shade = Light; Hand = 0 }
    let occupants = List.init 5 (fun i -> Junction $"E{i}", Light) |> Map

    let game =
        { Board =
            { Occupants = occupants
              Player = player
              Opponent = { player with Shade = Dark } }
          History = [] }

    Assert.Equal(None, execute game action)

[<Fact>]
let ``Light player cannot place on E1 if E1 is occupied`` () =
    let action =
        { Source = None
          Destination = Junction "E1" }

    let player = { Shade = Light; Hand = 7 }
    let occupants = List.init 5 (fun i -> Junction $"E{i}", Dark) |> Map

    let game =
        { Board =
            { Occupants = occupants
              Player = player
              Opponent = { player with Shade = Dark } }
          History = [] }

    Assert.Equal(None, execute game action)

[<Fact>]
let ``Save action after first action is executed`` () =
    let action =
        { Source = None
          Destination = Junction "A1" }

    let game = execute initialGame action |> Option.get
    Assert.Equal<list<Action>>([ action ], game.History)

[<Fact>]
let ``Do not switch turns after dark player forms a mill`` () =
    [ "A1"; "R1"; "A2"; "R2"; "A3" ]
    |> List.fold
        (fun gameState junction ->
            Option.bind
                (fun game ->
                    execute
                        game
                        { Source = None
                          Destination = Junction junction })
                gameState)
        (Some initialGame)
    |> Option.get
    |> fun { Board = { Player = player } } -> Assert.Equal(Dark, player.Shade)

[<Fact>]
let ``Allow dark player to remove cow if they have a mill`` () =
    let target = "R1"

    [ "A1"; target; "A2"; "R2"; "A3"; target ]
    |> List.fold
        (fun gameState junction ->
            Option.bind
                (fun game ->
                    execute
                        game
                        { Source = None
                          Destination = Junction junction })
                gameState)
        (Some initialGame)
    |> Option.get
    |> fun { Board = { Occupants = occupants } } -> Assert.False(Map.containsKey (Junction target) occupants)

[<Fact>]
let ``Prevent light player from removing light cow`` () =
    let target = "R1"

    [ "A1"; target; "A2"; "R2"; "E3"; "R3"; target ]
    |> List.fold
        (fun gameState junction ->
            Option.bind
                (fun game ->
                    execute
                        game
                        { Source = None
                          Destination = Junction junction })
                gameState)
        (Some initialGame)
    |> fun gameOption -> Assert.Equal(None, gameOption)

[<Fact>]
let ``Light player cannot shoot an empty junction`` () =
    let target = "A8"

    [ "A1"; "R1"; "A2"; "R2"; "E3"; "R3"; target ]
    |> List.fold
        (fun gameState junction ->
            Option.bind
                (fun game ->
                    execute
                        game
                        { Source = None
                          Destination = Junction junction })
                gameState)
        (Some initialGame)
    |> fun gameOption -> Assert.Equal(None, gameOption)

[<Fact>]
let ``Dark player cannot shoot a light cow in a mill`` () =
    [ "E1"; "R4"; "A1"; "R1"; "A2"; "R2"; "A8"; "R3"; "A8"; "A3"; "R3" ]
    |> List.fold
        (fun gameState junction ->
            Option.bind
                (fun game ->
                    execute
                        game
                        { Source = None
                          Destination = Junction junction })
                gameState)
        (Some initialGame)
    |> fun gameOption -> Assert.Equal(None, gameOption)

[<Fact>]
let ``Dark player can shoot a light cow if all light cows are in a mill`` () =
    [ "A1"; "R1"; "A2"; "R2"; "A8"; "R3"; "A8"; "A3"; "R3" ]
    |> List.fold
        (fun gameState junction ->
            Option.bind
                (fun game ->
                    execute
                        game
                        { Source = None
                          Destination = Junction junction })
                gameState)
        (Some initialGame)
    |> fun gameOption -> Assert.NotEqual(None, gameOption)

[<Fact>]
let ``Dark player can only shoot with a newly formed mill`` () =
    [ "A1"; "R1"; "A2"; "R2"; "A3"; "R2"; "R2"; "A4"; "R2" ] // Mill "A1;A2;A3" cannot be reused
    |> List.fold
        (fun gameState junction ->
            Option.bind
                (fun game ->
                    execute
                        game
                        { Source = None
                          Destination = Junction junction })
                gameState)
        (Some initialGame)
    |> fun gameOption -> Assert.Equal(None, gameOption)

let gameAfterMovementSimulation =
    [ "A1"; "R1"; "A2"; "R2"; "A3"; "R2"; "R2"; "A4"; "R3"; "A4"; "A4"; "R4" ]
    |> List.fold
        (fun gameState junction ->
            Option.bind
                (fun game ->
                    execute
                        game
                        { Source = None
                          Destination = Junction junction })
                gameState)
        (Some initialGame)
    |> Option.map (fun game ->
        { game with
            Board =
                { game.Board with
                    Player = { game.Board.Player with Hand = 0 } } })
    |> Option.bind (fun game ->
        execute
            game
            { Source = Some(Junction "A4")
              Destination = Junction "E4" })

[<Fact>]
let ``Movement removes source junction occupant, placing them on the destination`` () =
    match gameAfterMovementSimulation with
    | None -> Assert.Fail(sprintf "Failed to move cow. Current game state is %A" gameAfterMovementSimulation)
    | Some { Board = { Occupants = o } } ->
        let sourceJunctionOccupant = Map.tryFind (Junction "A4") o
        let destinationJunctionOccupant = Map.tryFind (Junction "E4") o
        Assert.Equal((None, Some Dark), (sourceJunctionOccupant, destinationJunctionOccupant))

[<Fact>]
let ``Movements are saved`` () =
    Option.bind (fun { History = h } -> List.tryHead h) gameAfterMovementSimulation
    |> fun action ->
        Assert.Equal(
            Some
                { Source = Some(Junction "A4")
                  Destination = Junction "E4" },
            action
        )
