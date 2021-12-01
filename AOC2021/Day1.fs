module Day1

let rec private scan (first: string) (rest: List<string>) (output: List<string>) =
    if List.length rest = 0 then
        output |> List.rev
    else
        match first with
        | "" -> scan rest.Head rest.Tail ("N/A" :: output)
        | _ ->
            let next = rest.Head |> int
            let parsedFirst = first |> int

            if parsedFirst < next then
                scan rest.Head rest.Tail ("inc" :: output)
            else if parsedFirst > next then
                scan rest.Head rest.Tail ("dec" :: output)
            else
                scan rest.Head rest.Tail ("flat" :: output)

let rec private groupBy3 (depths: List<string>) (output: List<string>) =
    if List.length depths < 3 then
        output |> List.rev
    else
        let fst = depths[0] |> int
        let snd = depths[1] |> int
        let thrd = depths[2] |> int
        let sum = fst + snd + thrd
        groupBy3 depths.Tail (sum.ToString() :: output)

let solve (data: List<string>) =
    let distances = scan "" data []
    let grouping = List.countBy (fun x -> x) distances
    let (key, value) = List.find (fun (k, _) -> k = "inc") grouping
    value

let solve2 (data: List<string>) =
    let groupedBy3 = groupBy3 data []
    let distances = scan "" groupedBy3 []
    let grouping = List.countBy (fun x -> x) distances
    let (key, value) = List.find (fun (k, _) -> k = "inc") grouping
    value