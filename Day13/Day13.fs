namespace Day13

module Solver =
    let createSheet (data: string list) =
        let splitIndex = data |> List.findIndex (fun x -> x = "")
        let (coordinates, folding) =
            data
            |> List.filter (fun x -> x = "" |> not)
            |> List.splitAt splitIndex 
        let separated = coordinates |> List.map (fun x ->
            let result = x.Split ','
            (result.[0] |> int, result.[1] |> int))
        let maxX =
            separated
            |> List.map (fun (x, _) -> x)
            |> List.max
        let maxY =
            separated
            |> List.map (fun (_, y) -> y)
            |> List.max
        let sheet = Array2D.zeroCreate<int> (maxY + 1) (maxX + 1)
        separated |> List.iter (fun (x, y) -> sheet.[y, x] <- 1)
        (sheet, folding)


    let foldUp (sheet: int[,]) (position: int) =
        sheet


    let foldLeft (sheet: int[,]) (position: int) =
        sheet


    let fold (sheet: int[,]) (instruction: string) =
        let parts = (instruction.Split ' ').[2].Split '='
        let (direction, position) = (parts.[0], parts.[1] |> int)
        
        match direction with
        | "x" -> foldLeft sheet position
        | _ -> foldUp sheet position
    

    let solve (data: string list) =
        let (sheet, folding) = createSheet data
        let mutable points = 0

        let foldOnce = fold sheet folding.Head
        foldOnce |> Array2D.iter (fun x -> if x > 0 then points <- points + 1)
        points


    let solve2 (data: string list) =
        0