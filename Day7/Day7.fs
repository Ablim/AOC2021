module Day7

open System

let rec moveTo (target: int) (crabs: int list) (cost: int) (lowestCost: int) (expensive: bool) =
    if cost > lowestCost then
        None
    else
        match crabs with
        | [] -> Some cost
        | crab :: _ ->
            let moveCost =
                if expensive then
                    let steps = Math.Abs (crab - target)
                    [0..steps] |> List.sum
                else
                    Math.Abs (crab - target)
            moveTo target crabs.Tail (cost + moveCost) lowestCost expensive


let solveHelper (data: string list) (expensive: bool) = 
    let crabs =
        data.Head.Split ','
        |> Array.map (fun x -> x |> int)
        |> Array.toList
    let minPos = List.min crabs
    let maxPos = List.max crabs
    let targets = [minPos..maxPos]

    let mutable bestPos = 0
    let mutable bestCost = Int32.MaxValue
    
    for t in targets do
        let cost = moveTo t crabs 0 bestCost expensive

        if cost.IsSome && cost.Value < bestCost then
            bestCost <- cost.Value
            bestPos <- t

    bestCost


let solve (data: string list) = 
    solveHelper data false


let solve2 (data: string list) = 
    solveHelper data true