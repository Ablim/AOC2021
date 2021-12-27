namespace Test

module Day15Test = 
    open Xunit
    open System.IO
    open Day15

    let filename = "Data/Day15Part1.txt"

    [<Fact>]
    let ``Part 1`` () =
        let input = File.ReadLines filename |> Seq.toList
        let result = Solver.solve input
        Assert.Equal(40, result)


    [<Fact>]
    let ``Part 2`` () =
        let input = File.ReadLines filename |> Seq.toList
        let result = Solver.solve2 input
        Assert.Equal(1588, result)