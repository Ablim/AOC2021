module Day11

open Utils

let rec executeFlashes (toFlash: (int * int) list) (flashes: int) (flashMap: bool[,]) (map: int[,]) =
    match toFlash with
    | [] -> (map |> Array2D.mapi (fun row col fish -> if flashMap.[row, col] then 0 else fish), flashes)
    | nextFlash :: tail ->
        let (row, col) = nextFlash
        flashMap.[row, col] <- true

        let mutable newFlashes = []
        let flashesCandidates =
            [
                (ArrayUtils.tryGetValue2D map (row - 1) (col - 1), row - 1, col - 1);
                (ArrayUtils.tryGetValue2D map (row - 1) col, row - 1, col);
                (ArrayUtils.tryGetValue2D map (row - 1) (col + 1), row - 1, col + 1);
                (ArrayUtils.tryGetValue2D map row (col - 1), row, col - 1);
                (ArrayUtils.tryGetValue2D map row (col + 1), row, col + 1);
                (ArrayUtils.tryGetValue2D map (row + 1) (col - 1), row + 1, col - 1);
                (ArrayUtils.tryGetValue2D map (row + 1) col, row + 1, col);
                (ArrayUtils.tryGetValue2D map (row + 1) (col + 1), row + 1, col + 1)
            ]
            |> List.filter (fun (x, _, _) -> x.IsSome)
            |> List.map (fun (x, y, z) -> (x.Value, y, z))
            |> List.iter (fun (fish, row, col) ->
                map.[row, col] <- fish + 1
                
                if map.[row, col] > 9 && flashMap.[row, col] |> not && tail |> List.contains (row, col) |> not then
                    newFlashes <- (row, col) :: newFlashes)

        executeFlashes (List.append newFlashes tail) (flashes + 1) flashMap map


let runSteps (steps: int) (map: int[,]) =
    let mutable flashes = 0
    let mutable tempMap = map

    for i in [1..steps] do
        tempMap <- tempMap |> Array2D.map (fun x -> x + 1)
        let nextFlashes =
            tempMap
            |> Array2D.mapi (fun row col fish -> if fish > 9 then Some (row, col) else None)
            |> ArrayUtils.flatten2D
            |> List.filter (fun x -> x.IsSome)
            |> List.map (fun x -> x.Value)
        let flashMap = Array2D.zeroCreate (Array2D.length1 tempMap) (Array2D.length2 tempMap)
        
        let (newMap, newFlashes) = tempMap |> executeFlashes nextFlashes 0 flashMap
        tempMap <- newMap
        flashes <- flashes + newFlashes

    flashes


let findSyncStep (map: int[,]) =
    let mutable steps = 0
    let mutable tempMap = map
    let mutable run = true

    while run do
        steps <- steps + 1
        tempMap <- tempMap |> Array2D.map (fun x -> x + 1)
        let nextFlashes =
            tempMap
            |> Array2D.mapi (fun row col fish -> if fish > 9 then Some (row, col) else None)
            |> ArrayUtils.flatten2D
            |> List.filter (fun x -> x.IsSome)
            |> List.map (fun x -> x.Value)
        let flashMap = Array2D.zeroCreate (Array2D.length1 tempMap) (Array2D.length2 tempMap)
        
        let (newMap, newFlashes) = tempMap |> executeFlashes nextFlashes 0 flashMap
        tempMap <- newMap
        if newFlashes = tempMap.Length then run <- false

    steps


let solve (data: string list) = 
    data
    |> array2D
    |> Array2D.map (fun x -> x |> string |> int)
    |> runSteps 100 


let solve2 (data: string list) = 
    data
    |> array2D
    |> Array2D.map (fun x -> x |> string |> int)
    |> findSyncStep