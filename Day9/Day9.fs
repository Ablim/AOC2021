module Day9

open Utils.ArrayUtils

let isLocalMin (point: int) (neighbors: int list) =
    neighbors |> List.forall (fun x -> x > point)


let getLowPoints (map: int[,]) =
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
                lowPoints <- (point, i, j) :: lowPoints
    
    lowPoints


let solve (data: string list) =
    let map = array2D data |> Array2D.map (fun x -> x |> string |> int)
    getLowPoints map
    |> List.map (fun (x, _, _) -> x + 1)
    |> List.sum


let getBasin (map: int[,]) (point: int * int * int) =
    let mutable basin = []
    let mutable candidates = [ point ]

    while candidates.Length > 0 do
        let mutable newCandidates = []

        for (value, row, col) in candidates do
            basin <- (value, row, col) :: basin
            let neighbors =
                [
                    (tryGetValue2D map (row - 1) col, row - 1, col);
                    (tryGetValue2D map (row + 1) col, row + 1, col);
                    (tryGetValue2D map row (col - 1), row, col - 1);
                    (tryGetValue2D map row (col + 1), row, col + 1)
                ]
                |> List.filter (fun (some, _, _) -> some.IsSome)
                |> List.map (fun (some, r, c) -> (some.Value, r, c))
                |> List.filter (fun p -> basin |> List.contains p |> not)
                |> List.filter (fun p -> newCandidates |> List.contains p |> not)
                |> List.filter (fun (v, _, _) -> v < 9 && v >= value)
            
            newCandidates <- List.append newCandidates neighbors

        candidates <- newCandidates
    
    basin


let solve2 (data: string list) =
    let map = array2D data |> Array2D.map (fun x -> x |> string |> int)
    let lowPoints = getLowPoints map
    let basins = lowPoints |> List.map (fun x -> getBasin map x)
    basins
    |> List.sortByDescending (fun x -> x.Length)
    |> List.take 3
    |> List.fold (fun product list -> product * list.Length) 1
