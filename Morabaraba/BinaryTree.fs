[<RequireQualifiedAccess>]
module BinaryTree

type Node<'T> =
    | Node of value: 'T * left: Node<'T> * right: Node<'T>
    | Empty

let rec fold folder shouldSelectLeft state tree =
    match tree with
    | Empty -> state
    | Node(value, left, right) ->
        let state2 = folder state value

        if shouldSelectLeft state2 then
            let state3 = fold folder shouldSelectLeft state2 left
            state3
        else
            match right with
            | Empty -> state2
            | Node _ ->
                let state4 = fold folder shouldSelectLeft state right
                state4
