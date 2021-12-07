module Day5

open Utils.Printer



let getMaxValue (rows: string list) (index: int) =
    rows
    |> Seq.map (fun x -> x.Split " -> ")
    |> Seq.collect (fun x -> x)
    |> Seq.map (fun x -> (x.Split ',').[index] |> int)
    |> Seq.max



let getCoordinates (rows: string list) =
    []



let solve (data: string list) =
    let minX = 0
    let minY = 0
    let maxX = getMaxValue data 0
    let maxY = getMaxValue data 1
    let map = Array2D.zeroCreate<int> (maxX + 1) (maxY + 1)
    print2D (Array2D.zeroCreate<int> 10 10)
    0



let solve2 (data: string list) = 
    0