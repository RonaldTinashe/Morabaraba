module Morabaraba.ExecutorTree

open Morabaraba
open ExecutorCollection

let shootExecution =
    BinaryTree.Node(
        shoot,
        BinaryTree.Node(saveAction, BinaryTree.Node(switchTurns, BinaryTree.Empty, BinaryTree.Empty), BinaryTree.Empty),
        BinaryTree.Empty
    )

let shoot' =
    BinaryTree.Node(
        checkShootingTargetShade,
        BinaryTree.Node(
            checkShootingTargetNotInMill,
            shootExecution,
            BinaryTree.Node(checkAllOpponentCowsAreInMills, shootExecution, BinaryTree.Empty)
        ),
        BinaryTree.Empty
    )

let checkMillOrSwitch =
    BinaryTree.Node(
        checkPlayerMillIsNew,
        BinaryTree.Empty,
        BinaryTree.Node(switchTurns, BinaryTree.Empty, BinaryTree.Empty)
    )

let place' =
    BinaryTree.Node(
        place,
        BinaryTree.Node(
            saveAction,
            BinaryTree.Node(decreaseHand, checkMillOrSwitch, BinaryTree.Empty),
            BinaryTree.Empty
        ),
        BinaryTree.Empty
    )

let moveExecution =
    BinaryTree.Node(
        move,
        BinaryTree.Node(
            winIfNoMovesForOpponent,
            BinaryTree.Node(saveAction, BinaryTree.Empty, BinaryTree.Empty),
            BinaryTree.Node(saveAction, checkMillOrSwitch, BinaryTree.Empty)
        ),
        BinaryTree.Empty
    )

let move' =
    BinaryTree.Node(
        checkMovingJunctions,
        BinaryTree.Node(checkLegalMillFormation, moveExecution, BinaryTree.Empty),
        BinaryTree.Node(checkCowCountAllowsFlying, moveExecution, BinaryTree.Empty)
    )

let placeOrMill =
    BinaryTree.Node(
        checkPlayerMillIsNew,
        shoot',
        BinaryTree.Node(checkPlacingDestination, BinaryTree.Node(checkPlacingHand, place', move'), BinaryTree.Empty)
    )

let root = placeOrMill
