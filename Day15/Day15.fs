namespace Day15

open Utils
open System

type Path =
    struct
        val x: int
        val y: int
        val cost: int
        val path: (int * int) list

        new (x, y, cost, path) =
            {
                x = x;
                y = y;
                cost = cost
                path = path
            }
    end


module Solver =
    let shortestPath (map: int[,]) (paths: Path list) =
        let mutable path = paths.Head
        let mutable paths = paths
        let goal = (Array2D.length2 map - 1, Array2D.length1 map - 1)
        
        while (path.x, path.y) <> goal do
            let timestamp = DateTime.Now
            if timestamp.Second = 0 then printfn "Paths: %i, shortest: %i, x: %i, y: %i" paths.Length path.cost path.x path.y 

            let newSteps =
                [
                    //(ArrayUtils.tryGetValue2D map (path.y - 1) path.x, path.x, (path.y - 1));
                    (ArrayUtils.tryGetValue2D map (path.y + 1) path.x, path.x, (path.y + 1));
                    //(ArrayUtils.tryGetValue2D map path.y (path.x - 1), (path.x - 1), path.y);
                    (ArrayUtils.tryGetValue2D map path.y (path.x + 1), (path.x + 1), path.y)
                ]
                |> List.filter (fun (cost, x, y) ->
                    cost.IsSome
                    && path.path |> List.contains (x, y) |> not)
                |> List.map (fun (cost, x, y) -> Path(x, y, path.cost + cost.Value, (x, y) :: path.path))
            
            paths <-
                paths.Tail
                |> List.append newSteps
                |> List.sortBy (fun path -> path.cost)
            path <- paths.Head

        path


    let solve (data: string list) =
        let map =
            data
            |> array2D
            |> Array2D.map (fun x -> x |> string |> int)
        let seed =
            [
                Path(1, 0, map.[0, 1], [ (0, 0); (1, 0) ]);
                Path(0, 1, map.[1, 0], [ (0, 0); (0, 1) ])
            ]
        let shortestPath = shortestPath map seed
        shortestPath.cost


    let solve2 (data: string list) =
        0