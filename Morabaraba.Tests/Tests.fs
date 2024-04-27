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
