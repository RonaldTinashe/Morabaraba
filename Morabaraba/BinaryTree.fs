[<RequireQualifiedAccess>]
module BinaryTree

type Node<'T> =
    | Node of value: 'T * left: Node<'T> * right: Node<'T>
    | NoValue

let rec fold folder shouldSelectLeft state tree =
    match tree with
    | NoValue -> state
    | Node(value, left, right) ->
        let state2 = folder state value

        if shouldSelectLeft state2 then
            let state3 = fold folder shouldSelectLeft state2 left
            state3
        else
            match right with
            | NoValue -> state2
            | Node _ ->
                let state4 = fold folder shouldSelectLeft state right
                state4
