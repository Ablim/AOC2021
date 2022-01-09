﻿namespace Day16

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


    let getLengthTypeId (data: int[]) =
        data.[6]


    let parseLengthTypeId (lengthTypeId: int) (data: int[]) =
        match lengthTypeId with
        | 0 -> Array.sub data 7 15
        | 1 -> Array.sub data 7 11
        | _ -> [||]


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


    let parseLiteral (data: int[]) =
        let mutable i = 6
        while data.[i] = 1 do
            i <- i + 5
        i <- i + 5
            
        Array.splitAt i data


    let rec parsePacketsByCount (packages: int[] list) (count: int) (data: int[]) =
        let parseOperator (_data: int[]) =
            let lengthType = getLengthTypeId _data
            let length = parseLengthTypeId lengthType _data |> toDecimal
            match lengthType with
            | 0 ->
                Array.splitAt (7 + 15 + length) _data
            | 1 ->
                let subPacketsLength =
                    parsePacketsByCount [] length (Array.sub _data 18 (_data.Length - 18))
                    |> Seq.collect (fun x -> x)
                    |> Seq.length
                Array.splitAt (7 + 11 + subPacketsLength) _data
            | _ -> ([||], [||])

        if packages.Length = count then
            packages
        else
            let packetType = (getTypeId >> toDecimal >> getPacketType) data
            
            match packetType with
            | PacketType.Literal ->
                let (newPackage, remaining) = parseLiteral data
                parsePacketsByCount (newPackage :: packages) count remaining
            | PacketType.Operator ->
                let (newPackage, remaining) = parseOperator data
                parsePacketsByCount (newPackage :: packages) count remaining
            | _ -> []


    let rec parsePacketsByLength (packages: int[] list) (data: int[]) =
        let parseOperator (_data: int[]) =
            let lengthType = getLengthTypeId _data
            let length = parseLengthTypeId lengthType _data |> toDecimal
            match lengthType with
            | 0 ->
                Array.splitAt (7 + 15 + length) _data
            | 1 ->
                let subPacketsLength =
                    parsePacketsByCount [] length (Array.sub _data 18 (_data.Length - 18))
                    |> Seq.collect (fun x -> x)
                    |> Seq.length
                Array.splitAt (7 + 11 + subPacketsLength) _data
            | _ -> ([||], [||])
        
        if (data.Length = 0) then
            packages
        else
            let packetType = (getTypeId >> toDecimal >> getPacketType) data
        
            match packetType with
            | PacketType.Literal ->
                let (newPackage, remaining) = parseLiteral data
                parsePacketsByLength (newPackage :: packages) remaining
            | PacketType.Operator -> 
                let (newPackage, remaining) = parseOperator data
                parsePacketsByLength (newPackage :: packages) remaining
            | _ -> []


    let rec countVersions (data: int[]) =
        let version = (getVersion >> toDecimal) data
        let packetType = (getTypeId >> toDecimal >> getPacketType) data
            
        match packetType with
        | PacketType.Literal -> version
        | PacketType.Operator ->
            let lengthType = getLengthTypeId data
            let subLength = parseLengthTypeId lengthType data |> toDecimal
            let subPackets = 
                match lengthType with
                | 0 ->
                    Array.sub data 22 subLength
                    |> parsePacketsByLength []
                | 1 -> 
                    Array.sub data 18 (data.Length - 18)
                    |> parsePacketsByCount [] subLength
                | _ -> []

            let subPacketSum =
                seq {
                    for packet in subPackets do
                        yield countVersions packet
                }
                |> Seq.sum
            version + subPacketSum

        | _ -> 0

    
    let solve (data: string) =
        let bits = 
            data.ToCharArray()
            |> Array.map (fun x -> fromHex x)
            |> Array.collect (fun x -> x)
        countVersions bits


    let solve2 (data: string) =
        0