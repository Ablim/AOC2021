namespace Test

module Day14Test = 
    open Xunit
    open System.IO
    open Day14

    let filename = "Data/Day14Part1.txt"

    [<Fact>]
    let ``Part 1`` () =
        let input = File.ReadLines filename |> Seq.toList
        let result = Solver.solve input
        Assert.Equal(1588, result)


    [<Fact>]
    let ``Part 2`` () =
        let input = File.ReadLines filename |> Seq.toList
        let result = Solver.solve2 input
        Assert.Equal(2188189693529I, result)