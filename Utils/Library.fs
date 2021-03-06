namespace Utils

module Printer =
    let print2D (array: int[,]) =
        let inner = Array2D.length1 array
        let outer = Array2D.length2 array
        for i in [0..inner-1] do
            for j in [0..outer-1] do
                printf "%i " array.[i, j]
            
            printfn ""

        printfn ""



// module for reading input via http



module ListUtils = 
    let rec private loop (list: 'a list) (counter: bigint) =
        match list with
        | [] -> counter
        | _ -> loop list.Tail (counter + 1I)
    
    let count (list: 'a list) =
        loop list 0I


module ArrayUtils =
    let tryGetValue2D (array: 'a[,]) (row: int) (column: int) =
        try
            Some array.[row, column]
        with
            | :? System.IndexOutOfRangeException -> None


    let flatten2D (array: 'a[,]) =
        let mutable result = []
        let length = array |> Array2D.length1
        for i in [0..length - 1] do
            let row = array.[i, *] |> Array.toList
            result <- result |> List.append row
        result


module DictionaryUtils =
    open System.Collections.Generic
    
    let addOne (collection: Dictionary<'a, int>) (key: 'a) =
        if collection.ContainsKey key then
            collection.[key] <- collection.[key] + 1
        else
            collection.Add (key, 1)
        collection