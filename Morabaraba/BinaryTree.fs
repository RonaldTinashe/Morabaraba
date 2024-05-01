[<RequireQualifiedAccess>]
module BinaryTree

type Node<'T> =
    | Node of value: 'T * left: Node<'T> * right: Node<'T>
    | Empty

let rec fold folder leftBranchDiscriminant state tree =
    match tree with
    | Empty -> state
    | Node(value, left, right) ->
        let state2 = folder state value

        if leftBranchDiscriminant state2 then
            let state3 = fold folder leftBranchDiscriminant state2 left
            state3
        else
            match right with
            | Empty -> state2
            | Node _ ->
                let state4 = fold folder leftBranchDiscriminant state right
                state4
