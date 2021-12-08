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