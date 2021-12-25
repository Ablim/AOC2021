namespace Test

module Day13Test = 
    open Xunit
    open System.IO
    open Day13

    let filename = "Data/Day13Part1.txt"

    [<Fact>]
    let ``Part 1`` () =
        let result =  
            File.ReadLines filename
            |> Seq.toList
            |> Solver.solve
        Assert.Equal(17, result)

    [<Fact>]
    let ``Part 2`` () =
        let result = 
            File.ReadLines filename 
            |> Seq.toList 
            |> Solver.solve2
        
        let part2Result =
            File.ReadLines "Data/Day13Part2.txt"
            |> Seq.toList
            |> array2D
            |> Array2D.map (fun x -> if x = '.' then 0 else 1)
        
        Assert.Equal(part2Result, result)