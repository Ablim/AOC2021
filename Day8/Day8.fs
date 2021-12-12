module Day8

open System
open SegmentUtils

let guessByLength (word: string) =
    match word.Length with
    | 2 -> Some [ 1 ]
    | 3 -> Some [ 7 ]
    | 4 -> Some [ 4 ]
    | 5 -> Some [ 2; 3; 5 ]
    | 6 -> Some [ 0; 6; 9 ]
    | 7 -> Some [ 8 ]
    | _ -> None


let solve (data: string list) =
    let mutable sum = 0
    
    for row in data do
        let splitted =
            row.Split " | "
            |> Array.map (fun x -> x.Split ' ' |> Array.toList)
        let output = splitted.[1]
        
        output |> List.map guessByLength
        |> List.iter (fun x ->
            match x with
            | None -> ()
            | Some values -> if values.Length = 1 then sum <- sum + 1)
    
    sum


let decode (numbers: string list) =
    //  aaa
    // b   c
    //  ddd
    // e   f
    //  ggg
    
    // Setup
    let one = numbers |> List.filter (fun x -> x.Length = 2) |> List.head
    let seven = (numbers |> List.filter (fun x -> x.Length = 3) |> List.head).ToCharArray()
    let a = (seven |> Array.filter (fun x -> one.Contains x |> not)).[0]
    let aList = [ a ]
    let cfList = [| one.[0]; one.[1] |]

    let four = (numbers |> List.filter (fun x -> x.Length = 4) |> List.head).ToCharArray()
    let bd = four |> Array.filter (fun x -> one.Contains x |> not)
    let bdList = [| bd.[0]; bd.[1] |]

    let eight = (numbers |> List.filter (fun x -> x.Length = 7) |> List.head).ToCharArray()
    let eg = eight |> Array.filter (fun x -> Array.contains x seven |> not && Array.contains x four |> not)
    let egList = [| eg.[0]; eg.[1] |]

    let twoThreeFive = numbers |> List.filter (fun x -> x.Length = 5)
    let zeroSixNine = numbers |> List.filter (fun x -> x.Length = 6)

    // Decode
    let mutable result = Map.empty

    for i in [0..1] do
        for j in [0..1] do
            for k in [0..1] do
                let (c, f) = if i = 0 then (cfList.[0], cfList.[1]) else (cfList.[1], cfList.[0])
                let (b, d) = if j = 0 then (bdList.[0], bdList.[1]) else (bdList.[1], bdList.[0])
                let (e, g) = if k = 0 then (egList.[0], egList.[1]) else (egList.[1], egList.[0])
                let testDisplay = [| a; b; c; d; e; f; g |]

                if (twoThreeFive |> List.exists (fun x -> canPrint2 testDisplay x)
                    && twoThreeFive |> List.exists (fun x -> canPrint3 testDisplay x)
                    && twoThreeFive |> List.exists (fun x -> canPrint5 testDisplay x)
                    && zeroSixNine |> List.exists (fun x -> canPrint0 testDisplay x)
                    && zeroSixNine |> List.exists (fun x -> canPrint6 testDisplay x)
                    && zeroSixNine |> List.exists (fun x -> canPrint9 testDisplay x)) then
                    result <- Map [ (a, 'a'); (b, 'b'); (c, 'c'); (d, 'd'); (e, 'e'); (f, 'f'); (g, 'g') ]

    result


let displayPrint (segments: string) =
    match segments with
    | "abcefg" -> Some 0
    | "cf" -> Some 1
    | "acdeg" -> Some 2
    | "acdfg" -> Some 3
    | "bcdf" -> Some 4
    | "abdfg" -> Some 5
    | "abdefg" -> Some 6
    | "acf" -> Some 7
    | "abcdefg" -> Some 8
    | "abcdfg" -> Some 9
    | _ -> None


let displayPrintWithKey (key: Map<char, char>) (numbers: string list) =
    let mutable decimals = []
    
    for n in numbers do
        let someValue =
            n.ToCharArray()
            |> Array.map (fun x -> key.[x])
            |> Array.sort
            |> String
            |> displayPrint
        decimals <- someValue.Value :: decimals
    
    List.rev decimals
    |> List.map (fun x -> x.ToString())
    |> String.concat ""
    |> int


let solve2 (data: string list) =
    let mutable sum = 0
    
    for row in data do
        let splitted =
            row.Split " | "
            |> Array.map (fun x -> x.Split ' ' |> Array.toList)
        let scrambled = splitted.[0]
        let output = splitted.[1]
        
        let key = decode scrambled
        let decimalValue = displayPrintWithKey key output
        sum <- sum + decimalValue
    
    sum