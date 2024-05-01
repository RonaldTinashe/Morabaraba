module Morabaraba.ExecutionService

open Morabaraba
open ExecutorTree

let initialBoard =
    let player = { Shade = Dark; Hand = 12 }
    let opponent = { player with Shade = Light }

    { Player = player
      Opponent = opponent
      Occupants = Map.empty
      History = []
      Status = Playing }

let execute board action =

    let executionFolder boardOption ruleExecution =
        Option.bind (fun boardValue -> ruleExecution boardValue action) boardOption

    let boardAfterExecution =
        BinaryTree.fold executionFolder Option.isSome (Some board) root

    match boardAfterExecution with
    | Some b -> b
    | None -> { board with Status = Lost }
