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
let ``Player has 12 cows initially`` () =
    let player = initialGame.Board.Player
    Assert.Equal(12, player.Hand)
