module Morabaraba.Tests

open System
open Xunit
open Morabaraba

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
