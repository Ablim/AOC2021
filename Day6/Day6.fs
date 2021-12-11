module Day6

let rec reproduce (fishes: int list) (days: int) =
    if days = 0 then
        fishes
    else
        let mutable newFishes = []
        
        for f in fishes do
            if (f = 0) then
                newFishes <- 6 :: newFishes
                newFishes <- 8 :: newFishes
            else
                newFishes <- (f - 1) :: newFishes

        reproduce newFishes (days - 1)



let solve (data: string list) =
    let fishes =
        data
        |> List.map (fun x -> x.Split ',' |> Array.toList)
        |> List.collect (fun x -> x)
        |> List.map (fun x -> x |> int)
    reproduce fishes 80 |> List.length



let rec reproduce2 (record: bigint[]) (days: int) =
    if (days = 0) then
        record
    else
        let newSpawns = record.[0]

        for i in [0..7] do
            record.[i] <- record.[i + 1]

        record.[6] <- record.[6] + newSpawns
        record.[8] <- newSpawns
        reproduce2 record (days - 1)



let solve2 (data: string list) (days: int) =
    let fishes =
        data
        |> List.map (fun x -> x.Split ',' |> Array.toList)
        |> List.collect (fun x -> x)
        |> List.map (fun x -> x |> int)
    
    let mutable record = Array.zeroCreate<bigint> 9
    for i in [0..8] do
        record.[i] <- 
            fishes
            |> List.filter (fun x -> x = i)
            |> List.length
            |> bigint

    let result = reproduce2 record days
    Array.sum result