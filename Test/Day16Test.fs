namespace Test

module Day16Test = 
    open Xunit
    open System.IO
    open Day16

    let filename = "Data/Day16Part1.txt"

    [<Theory>]
    [<InlineDataAttribute("D2FE28", 6)>]
    [<InlineDataAttribute("38006F45291200", 9)>]
    [<InlineDataAttribute("EE00D40C823060", 14)>]
    [<InlineDataAttribute("8A004A801A8002F478", 16)>]
    [<InlineDataAttribute("620080001611562C8802118E34", 12)>]
    [<InlineDataAttribute("C0015000016115A2E0802F182340", 23)>]
    [<InlineDataAttribute("A0016C880162017C3686B18A3D4780", 31)>]
    let ``Part 1`` (packet: string) (versionSum: int) =
        let result = Solver.solve packet
        Assert.Equal(versionSum, result)


    [<Fact>]
    let ``Part 2`` () =
        let input = File.ReadLines filename |> Seq.head
        let result = Solver.solve2 input
        Assert.Equal(315, result)