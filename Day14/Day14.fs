namespace Day14

module Solver =
    let rec insert (polymer: char list) (lookup: Map<string, char>) (newPolymer: char list) =
        match polymer with
        | [] -> newPolymer |> List.rev
        | first :: tail ->
            match tail with
            | [] -> first :: newPolymer |> List.rev
            | second :: _ ->
                let key = $"{first}{second}"
                let value = lookup.TryFind key
                
                if (value.IsSome) then
                    insert tail lookup (value.Value :: first :: newPolymer)
                else
                    insert tail lookup (first :: newPolymer)

    
    let parse (data: string list) =
        let template =
            data.Head.ToCharArray()
            |> Array.toList
        let lookpup =
            data.Tail.Tail
            |> List.map (fun x ->
                let parts = x.Split " -> "
                (parts.[0], parts.[1] |> char))
            |> Map.ofList
        (template, lookpup)
        

    let solve (data: string list) = 
        let (template, lookup) = parse data
        let mutable result = template
        
        for i in [0..9] do
            result <- insert result lookup []

        let groups =
            result
            |> List.groupBy (fun x -> x)
            |> List.sortByDescending (fun (_, xs) -> xs.Length)
            |> List.toArray
        let (_, longest) = groups.[0]
        let (_, shortest) = groups.[groups.Length - 1]
        longest.Length - shortest.Length


    let rec seedPolymer (template: char list) (map: Map<string, bigint>) =
        match template with
        | [] -> map
        | h :: t when t = [] ->
            seedPolymer t (map.Add ($"{h}_", 1))
        | h :: t ->
            let key = $"{h}{t.Head}"
            let oldValue = map.TryFind key
            let newMap = map.Add (key, if oldValue.IsSome then oldValue.Value + 1I else 1)
            seedPolymer t newMap


    let countElemets (polymer: Map<string, bigint>) = 
        polymer
        |> Map.toList
        |> List.map (fun (key, value) -> (key.ToCharArray().[0], value))
        |> List.groupBy (fun (key, _) -> key)
        |> List.map (fun (key, list) ->
            let sum = list |> List.fold (fun totalCount (_, count) -> totalCount + count) 0I
            (key, sum))
        |> List.sortByDescending (fun (_, count) -> count)
        |> List.toArray


    let insert2 (polymer: Map<string, bigint>) (rules: (string * char) list) =
        let mutable newPolymer = polymer

        for (rule, newChar) in rules do
            let somePolymer = polymer.TryFind rule

            if (somePolymer.IsSome) then
                newPolymer <- newPolymer.Add (rule, newPolymer.[rule] - somePolymer.Value)
                let ruleChars = rule.ToCharArray()
                
                let newA = $"{ruleChars.[0]}{newChar}"
                let someA = newPolymer.TryFind newA
                newPolymer <- newPolymer.Add (newA, somePolymer.Value + (if someA.IsSome then someA.Value else 0I))
                
                let newB = $"{newChar}{ruleChars.[1]}"
                let someB = newPolymer.TryFind newB
                newPolymer <- newPolymer.Add (newB, somePolymer.Value + (if someB.IsSome then someB.Value else 0I))

        newPolymer


    let solve2 (data: string list) = 
        let (template, lookup) = parse data
        let lookupList = lookup |> Map.toList
        let seed = seedPolymer template Map.empty
        
        let mutable result = seed
        for i in [0..39] do
            result <- insert2 result lookupList
        
        let counted = countElemets result
        let (_, longest) = counted.[0]
        let (_, shortest) = counted.[counted.Length - 1]
        longest - shortest