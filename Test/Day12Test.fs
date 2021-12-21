namespace Test

module Day12Test = 
    open Xunit
    open System.IO
    open Day12

    let filename = "Data/Day12Part1.txt"

    [<Fact>]
    let ``Part 1`` () =
        let input = File.ReadLines filename |> Seq.toList
        let result = Solver.solve input
        Assert.Equal(10, result)

    [<Fact>]
    let ``Part 2`` () =
        let input = File.ReadLines filename |> Seq.toList
        let result = Solver.solve2 input
        Assert.Equal(36, result)
