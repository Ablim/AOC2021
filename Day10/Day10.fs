module Day10

let rec checkLineLoop (line: char list) (lookup: char list) =
    match line with
    | [] -> (None, lookup)
    | head :: tail -> 
        match head with
        | '(' -> checkLineLoop tail (head :: lookup)
        | '[' -> checkLineLoop tail (head :: lookup)
        | '{' -> checkLineLoop tail (head :: lookup)
        | '<' -> checkLineLoop tail (head :: lookup)
        | ')' -> if lookup.Head = '(' then checkLineLoop tail lookup.Tail else (Some head, [])
        | ']' -> if lookup.Head = '[' then checkLineLoop tail lookup.Tail else (Some head, [])
        | '}' -> if lookup.Head = '{' then checkLineLoop tail lookup.Tail else (Some head, [])
        | '>' -> if lookup.Head = '<' then checkLineLoop tail lookup.Tail else (Some head, [])
        | _ -> (Some head, [])


let checkLine (line: string) =
    checkLineLoop (line.ToCharArray () |> Array.toList) [] 


let getScore (c: char) =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> 0


let solve (data: string list) =
    data
    |> List.map checkLine
    |> List.filter (fun (error, _) -> error.IsSome)
    |> List.map (fun (error, _) -> getScore error.Value)
    |> List.sum


let rec closeBlocks (openBlocks: char list) (closed: char list) =
    match openBlocks with
    | [] -> closed |> List.rev
    | head :: tail ->
        match head with
        | '(' -> closeBlocks tail (')' :: closed)
        | '[' -> closeBlocks tail (']' :: closed)
        | '{' -> closeBlocks tail ('}' :: closed)
        | '<' -> closeBlocks tail ('>' :: closed)
        | _ -> []


let getScore2 (c: char) =
    match c with
    | ')' -> 1I
    | ']' -> 2I
    | '}' -> 3I
    | '>' -> 4I
    | _ -> 0I


let solve2 (data: string list) =
    let scores =
        data
        |> List.map checkLine
        |> List.filter (fun (error, _) -> error.IsNone)
        |> List.map (fun (_, openBlocks) -> openBlocks)
        |> List.map (fun x -> closeBlocks x [])
        |> List.map (fun x -> x |> List.fold (fun t c -> t * 5I + getScore2 c) 0I)
        |> List.sort
        |> List.toArray
    let index = scores.Length / 2
    scores.[index]