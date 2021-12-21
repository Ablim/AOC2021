namespace Day12

module Solver =
    let isSmall (cave: string) =
        cave.ToLower () = cave


    let canAdd (cave: string) (path: string list) (allowedPairs: int) =
        let pairs =
            cave :: path
            |> List.filter (fun x -> x |> isSmall)
            |> List.groupBy (fun x -> x)
            |> List.filter (fun (_, y) -> y.Length > 1)
            |> List.length
        pairs <= allowedPairs

    
    let rec buildPaths (cave: string) (path: string list) (smallOnce: bool) (lookup: Map<string, string list>) =
        if (cave = "end") then
            [ cave :: path ] |> List.rev
        else
            if smallOnce && canAdd cave path 0 |> not then
                []
            elif smallOnce |> not && canAdd cave path 1 |> not then
                []
            else
                let newPath = cave :: path
                let nextCaves = lookup.[cave]
                seq {
                    for c in nextCaves do
                        yield buildPaths c newPath smallOnce lookup
                }
                |> Seq.collect (fun x -> x)
                |> Seq.toList
    

    let buildLookup (data: string list) =
        let mutable lookup = Map.empty<string, string list>
        
        for row in data do
            let parts = row.Split '-'
            let a = parts.[0]
            let b = parts.[1]
            let aList = b :: if lookup.TryFind a |> Option.isSome then lookup.[a] else []
            lookup <- lookup.Add (a, aList)
            let bList = a :: if lookup.TryFind b |> Option.isSome then lookup.[b] else []
            lookup <- lookup.Add (b, bList)

        lookup <- lookup.Remove "end"
        lookup
     
     
    let clearStartDestinations (map: Map<string, string list>) =
        let mutable newMap = Map.empty<string, string list>

        for key in map.Keys do
            let filtered = map.[key] |> List.filter (fun x -> x = "start" |> not)
            newMap <- newMap.Add (key, filtered)

        newMap

    
    let solve (data: string list) = 
        buildLookup data
        |> buildPaths "start" [] true
        |> List.length


    let solve2 (data: string list) =
        data
        |> buildLookup
        |> clearStartDestinations
        |> buildPaths "start" [] false
        |> List.length