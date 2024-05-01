module Morabaraba.ExecutorTree

open Morabaraba
open ExecutorCollection

let root =
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
        BinaryTree.Node(
            move,
            BinaryTree.Node(
                winIfNoMovesForOpponent,
                BinaryTree.Node(saveAction, BinaryTree.NoValue, BinaryTree.NoValue),
                BinaryTree.Node(saveAction, checkMillOrSwitch, BinaryTree.NoValue)
            ),
            BinaryTree.NoValue
        )

    let move' =
        BinaryTree.Node(
            checkMovingJunctions,
            BinaryTree.Node(checkLegalMillFormation, moveExecution, BinaryTree.NoValue),
            BinaryTree.Node(checkCowCountAllowsFlying, moveExecution, BinaryTree.NoValue)
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
