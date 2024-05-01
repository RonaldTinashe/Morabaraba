module Morabaraba.OccupationService

let createJunction (letter: char) (number: int) = Junction $"{letter}{number}"
let boardNumbers = [ 1..8 ]
let boardLetters = [ 'E'; 'A'; 'R' ]

let lines =
    let flip a b c = a c b

    let sameLetterLines =
        let numbersLines = [ [ 1; 2; 3 ]; [ 7; 6; 5 ]; [ 1; 8; 7 ]; [ 3; 4; 5 ] ]
        List.collect (fun l -> List.map (fun nl -> List.map (createJunction l) nl) numbersLines) boardLetters

    let sameNumberLines =
        boardNumbers
        |> List.map (fun number -> List.map ((flip createJunction) number) [ 'E'; 'A'; 'R' ])

    sameLetterLines @ sameNumberLines

let neighbours =
    let sameLetterNeighbours =
        let numbers = [ 1, 2; 2, 3; 7, 6; 6, 5; 1, 8; 8, 7; 3, 4; 4, 5 ]
        let letters = [ 'E'; 'A'; 'R' ]

        List.collect
            (fun letter -> List.map (fun (n1, n2) -> (createJunction letter n1), (createJunction letter n2)) numbers)
            letters

    let sameNumberNeigbours =
        let letterNeighbours = [ 'E', 'A'; 'A', 'R' ]

        List.collect
            (fun (letter1, letter2) ->
                List.map (fun n -> (createJunction letter1 n), (createJunction letter2 n)) boardNumbers)
            letterNeighbours

    sameLetterNeighbours @ sameNumberNeigbours

let isAMill shade occupants line =
    List.forall (fun junction -> Map.tryFind junction occupants = Some shade) line

let occupantsByShade competitorShade occupants =
    Map.filter (fun _ shade -> shade = competitorShade) occupants

let junctionsInMillsByShade competitorShade occupants =
    let competitorOccupants = occupantsByShade competitorShade occupants

    List.filter (isAMill competitorShade competitorOccupants) lines
    |> List.concat
    |> List.distinct
