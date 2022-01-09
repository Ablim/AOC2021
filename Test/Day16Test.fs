namespace Test

module Day16Test = 
    open Xunit
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


    [<Theory>]
    [<InlineDataAttribute("C200B40A82", 3)>]
    [<InlineDataAttribute("04005AC33890", 54)>]
    [<InlineDataAttribute("880086C3E88112", 7)>]
    [<InlineDataAttribute("CE00C43D881120", 9)>]
    [<InlineDataAttribute("D8005AC2A8F0", 1)>]
    [<InlineDataAttribute("F600BC2D8F", 0)>]
    [<InlineDataAttribute("9C005AC2F8F0", 0)>]
    [<InlineDataAttribute("9C0141080250320F1802104A08", 1)>]
    let ``Part 2`` (packet: string) (expected: int) =
        let result = Solver.solve2 packet
        Assert.Equal(expected |> bigint, result)