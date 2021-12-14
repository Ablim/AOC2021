module Day9

open Utils.ArrayUtils

let isLocalMin (point: int) (neighbors: int list) =
    neighbors |> List.forall (fun x -> x > point)


let solve (data: string list) =
    let map = array2D data |> Array2D.map (fun x -> x |> string |> int)
    let rows = Array2D.length1 map
    let cols = Array2D.length2 map
    let mutable lowPoints = []

    for i in [0..rows - 1] do
        for j in [0..cols - 1] do
            let point = map.[i, j]
            let neighbors =
                [
                    tryGetValue2D map (i - 1) j;
                    tryGetValue2D map (i + 1) j;
                    tryGetValue2D map i (j - 1);
                    tryGetValue2D map i (j + 1)
                ]
                |> List.filter (fun x -> x.IsSome)
                |> List.map (fun x -> x.Value)
            
            if isLocalMin point neighbors then
                lowPoints <- point :: lowPoints
    
    lowPoints
    |> List.map (fun x -> x + 1)
    |> List.sum


let solve2 (data: string list) =
    0