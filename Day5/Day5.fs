module Day5



let getMaxValue (rows: string list) (index: int) =
    rows
    |> Seq.map (fun x -> x.Split " -> ")
    |> Seq.collect (fun x -> x)
    |> Seq.map (fun x -> (x.Split ',').[index] |> int)
    |> Seq.max



let createLine (p1: int * int) (p2: int * int) (diagonal: bool) =
    let (x1, y1) = p1
    let (x2, y2) = p2

    if x1 = x2 then
        if y1 < y2 then [y1..y2] else [y2..y1] |> List.rev
        |> List.map (fun y -> (x1, y))
    elif y1 = y2 then
        if x1 < x2 then [x1..x2] else [x2..x1] |> List.rev
        |> List.map (fun x -> (x, y1))
    else
        if diagonal then
            let xs = if x1 < x2 then [x1..x2] else [x2..x1] |> List.rev
            let ys = if y1 < y2 then [y1..y2] else [y2..y1] |> List.rev
            List.zip xs ys
        else
            []



let getLines (rows: string list) (diagonal: bool) =
    seq {
        for row in rows do
            let points =
                row.Split " -> "
                |> Array.map (fun x ->
                    let p = x.Split ',' |> Array.map (fun y -> y |> int)
                    (p.[0], p.[1]))

            yield createLine points.[0] points.[1] diagonal
    }
    |> Seq.filter (fun x -> x = [] |> not)
    |> Seq.toList



let countOverlaps (map: int[,]) =
    let mutable sum = 0
    map
    |> Array2D.iter (fun x -> if x >= 2 then sum <- sum + 1)
    sum



let solve (data: string list) =
    let minX = 0
    let minY = 0
    let maxX = getMaxValue data 0
    let maxY = getMaxValue data 1
    let map = Array2D.zeroCreate<int> (maxX + 1) (maxY + 1)
    let lines = getLines data false

    lines
    |> List.collect (fun x -> x)
    |> List.iter (fun (x, y) -> 
        let value = map[x, y]
        map.[x, y] <- value + 1)
    
    countOverlaps map



let solve2 (data: string list) = 
    let minX = 0
    let minY = 0
    let maxX = getMaxValue data 0
    let maxY = getMaxValue data 1
    let map = Array2D.zeroCreate<int> (maxX + 1) (maxY + 1)
    let lines = getLines data true

    lines
    |> List.collect (fun x -> x)
    |> List.iter (fun (x, y) -> 
        let value = map[x, y]
        map.[x, y] <- value + 1)
    
    countOverlaps map