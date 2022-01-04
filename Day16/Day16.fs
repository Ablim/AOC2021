namespace Day16

open System

module Solver =
    let fromHex (value: char) =
        match value with
        | '0' -> [| 0; 0; 0; 0|]
        | '1' -> [| 0; 0; 0; 1|]
        | '2' -> [| 0; 0; 1; 0|]
        | '3' -> [| 0; 0; 1; 1|]
        | '4' -> [| 0; 1; 0; 0|]
        | '5' -> [| 0; 1; 0; 1|]
        | '6' -> [| 0; 1; 1; 0|]
        | '7' -> [| 0; 1; 1; 1|]
        | '8' -> [| 1; 0; 0; 0|]
        | '9' -> [| 1; 0; 0; 1|]
        | 'A' -> [| 1; 0; 1; 0|]
        | 'B' -> [| 1; 0; 1; 1|]
        | 'C' -> [| 1; 1; 0; 0|]
        | 'D' -> [| 1; 1; 0; 1|]
        | 'E' -> [| 1; 1; 1; 0|]
        | 'F' -> [| 1; 1; 1; 1|]
        | _ -> [||]


    let getVersion (data: int[]) =
        [| data.[0]; data.[1]; data.[2] |]


    let getTypeId (data: int[]) =
        [| data.[3]; data.[4]; data.[5] |]


    let getPacketType (typeId: int) =
        match typeId with
        | 4 -> PacketType.Literal
        | _ -> PacketType.Operator


    let toDecimal (bits: int[]) =
        let maxIndex = bits.Length - 1
        let bitList = Array.toList bits
        let rec loop (index: int) (sum: int) (remaining: int list) =
            match remaining with
            | [] -> sum
            | _ ->
                let current = remaining.Head * pown 2 index
                loop (index - 1) (sum + current) remaining.Tail
        loop maxIndex 0 bitList

    
    let solve (data: string list) =
        let apa = 
            "D2FE28".ToCharArray()
            |> Array.map (fun x -> fromHex x)
            |> Array.collect (fun x -> x)
        let bepa = String (apa |> Array.map (fun x -> x |> string |> char))
        printfn "%s" bepa
        let version = (getVersion >> toDecimal) apa
        printfn "Version %i" version
        let typeId = (getTypeId >> toDecimal) apa
        printfn "Type id %i" typeId
        0


    let solve2 (data: string list) =
        0