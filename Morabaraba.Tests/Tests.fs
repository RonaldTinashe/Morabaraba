module Morabaraba.Tests

open System
open Xunit
open Morabaraba

[<Fact>]
let ``Initial player's shade is Dark`` () =
    let initialBoard = initialGame.Board

    let playerShadeIsDark =
        initialBoard.Player.Shade = Dark && initialBoard.Opponent.Shade = Light

    Assert.True(playerShadeIsDark)
