namespace Day14

open System

module Solver =
    let rec insert (polymer: char list) (lookup: Map<string, char>) (newPolymer: char list) =
        match polymer with
        | [] -> newPolymer |> List.rev
        | first :: tail ->
            match tail with
            | [] -> first :: newPolymer |> List.rev
            | second :: _ ->
                let key = new String([| first; second |])
                let value = lookup.TryFind key
                
                if (value.IsSome) then
                    insert tail lookup (value.Value :: first :: newPolymer)
                else
                    insert tail lookup (first :: newPolymer)

    
    let solve (data: string list) = 
        let template =
            data.Head.ToCharArray ()
            |> Array.toList
        let lookpup =
            data.Tail.Tail
            |> List.map (fun x ->
                let parts = x.Split " -> "
                (parts.[0], parts.[1] |> char))
            |> Map.ofList

        let mutable result = template
        for i in [0..9] do
            result <- insert result lookpup []

        let groups =
            result
            |> List.groupBy (fun x -> x)
            |> List.sortByDescending (fun (_, xs) -> xs.Length)
            |> List.toArray
        let (_, longest) = groups.[0]
        let (_, shortest) = groups.[groups.Length - 1]
        longest.Length - shortest.Length


    let solve2 (data: string list) = 
        0