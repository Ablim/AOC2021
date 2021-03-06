namespace Day15

open Utils
open System

[<CustomComparison; CustomEquality>]
type Path =
    struct
        val x: int
        val y: int
        val cost: int
        val path: Set<(int * int)>
        val length: int

        new (x, y, cost, path, length) =
            {
                x = x;
                y = y;
                cost = cost
                path = path
                length = length
            }

        interface IComparable with
            member this.CompareTo(other: obj): int = 
                match other with
                | :? Path as p ->
                    if this.cost < p.cost then
                        -1
                    elif this.cost > p.cost then
                        1
                    elif this.length > p.length then
                        -1
                    elif this.length < p.length then
                        1
                    elif this.x <> p.x then
                        this.x.CompareTo p.x
                    else
                        this.y.CompareTo p.y
                | _ -> -1
        
        interface IEquatable<Path> with
            member this.Equals other = this.GetHashCode() = other.GetHashCode()

        override this.Equals other =
            match other with
            | :? Path as p -> this.Equals p
            | _ -> false

        override this.GetHashCode () =
            this.x * 3 + this.y * 5 + this.cost * 11
            
    end


module Solver =
    let shortestPath (map: int[,]) (paths: Set<Path>) =
        let mutable pathSet = paths
        let mutable path = pathSet.MinimumElement
        let goal = (Array2D.length2 map - 1, Array2D.length1 map - 1)
        let costMap = Array2D.create (Array2D.length1 map) (Array2D.length2 map) Int32.MaxValue
        
        while (path.x, path.y) <> goal do
            let newSteps =
                [
                    (ArrayUtils.tryGetValue2D map (path.y - 1) path.x, path.x, (path.y - 1));
                    (ArrayUtils.tryGetValue2D map (path.y + 1) path.x, path.x, (path.y + 1));
                    (ArrayUtils.tryGetValue2D map path.y (path.x - 1), (path.x - 1), path.y);
                    (ArrayUtils.tryGetValue2D map path.y (path.x + 1), (path.x + 1), path.y)
                ]
                |> List.filter (fun (cost, x, y) ->
                    cost.IsSome
                    && path.path.Contains (x, y) |> not
                    && path.cost + cost.Value <= costMap.[y, x])
                |> List.map (fun (cost, x, y) -> Path(x, y, path.cost + cost.Value, path.path.Add (x, y), path.length + 1))
            
            pathSet <- pathSet.Remove path
            newSteps |> List.iter (fun x ->
                pathSet <- pathSet.Add x
                costMap.[x.y, x.x] <- x.cost)
            path <- pathSet.MinimumElement

        path


    let solve (data: string list) =
        let map =
            data
            |> array2D
            |> Array2D.map (fun x -> x |> string |> int)
        let seed =
            [
                Path(1, 0, map.[0, 1], Set.ofList [ (0, 0); (1, 0) ], 2);
                Path(0, 1, map.[1, 0], Set.ofList [ (0, 0); (0, 1) ], 2)
            ]
            |> Set.ofList
        let path = shortestPath map seed
        path.cost


    let create5By5 (map: int[,]) =
        let height = Array2D.length1 map
        let width = Array2D.length2 map
        let bigMoma = Array2D.zeroCreate<int> (height * 5) (width * 5)

        for y in [0..Array2D.length1 bigMoma - 1] do
            for x in [0..Array2D.length2 bigMoma - 1] do
                let transY = y % height
                let transX = x % width
                let value = map.[transY, transX]
                let extraY = y / height
                let extraX = x / width
                let newValue = value + extraX + extraY
                bigMoma.[y, x] <- if newValue > 9 then newValue % 9 else newValue

        bigMoma


    let solve2 (data: string list) =
        let map =
            data
            |> array2D
            |> Array2D.map (fun x -> x |> string |> int)
            |> create5By5
        let seed =
            [
                Path(1, 0, map.[0, 1], Set.ofList [ (0, 0); (1, 0) ], 2);
                Path(0, 1, map.[1, 0], Set.ofList [ (0, 0); (0, 1) ], 2)
            ]
            |> Set.ofList
        let path = shortestPath map seed
        path.cost