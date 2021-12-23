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
        Assert.Equal(36, result)