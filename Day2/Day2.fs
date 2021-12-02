module Day2

let solve (data: List<string>) =
    let mutable position = 0
    let mutable depth = 0
    
    for command in data do
        let parts = command.Split ' '
        let steps = parts[1] |> int

        match parts[0] with
        | "up" -> depth <- depth - steps
        | "down" -> depth <- depth + steps
        | "forward" -> position <- position + steps
        | _ -> ()

    position * depth

let solve2 (data: List<string>) =
    let mutable position = 0
    let mutable depth = 0
    let mutable aim = 0
    
    for command in data do
        let parts = command.Split ' '
        let steps = parts[1] |> int

        match parts[0] with
        | "up" -> aim <- aim - steps
        | "down" -> aim <- aim + steps
        | "forward" ->
            position <- position + steps
            depth <- depth + aim * steps
        | _ -> ()

    position * depth