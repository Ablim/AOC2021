namespace Utils

module Printer =
    let print2D (array: int[,]) =
        let inner = Array2D.length1 array
        let outer = Array2D.length2 array
        for i in [0..inner-1] do
            for j in [0..outer-1] do
                printf "%i" array.[i, j]
            
            printfn ""

        printfn ""



// module for reading input via http



module ListHelper = 
    let rec private loop (list: 'a list) (counter: bigint) =
        match list with
        | [] -> counter
        | _ -> loop list.Tail (counter + 1I)
    
    let count (list: 'a list) =
        loop list 0I