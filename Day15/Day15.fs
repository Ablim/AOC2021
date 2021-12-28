﻿namespace Day15

open Utils
open System

[<CustomComparison; CustomEquality>]
type Path =
    struct
        val x: int
        val y: int
        val cost: int
        val path: Set<(int * int)>
        val pathLength: int

        new (x, y, cost, path) =
            {
                x = x;
                y = y;
                cost = cost
                path = path
                pathLength = path.Count
            }

        interface IComparable with
            member this.CompareTo(other: obj): int = 
                match other with
                | :? Path as p ->
                    if this.cost < p.cost then
                        -1
                    elif this.cost > p.cost then
                        1
                    elif this.pathLength > p.pathLength then
                        -1
                    elif this.pathLength < p.pathLength then
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
    let shortestPath (map: int[,]) (paths: Path list) =
        let mutable pathSet = Set.ofList paths
        let mutable path = pathSet.MinimumElement
        let goal = (Array2D.length2 map - 1, Array2D.length1 map - 1)
        let mutable timestamp = DateTime.Now.AddSeconds 30
        
        while (path.x, path.y) <> goal do
            if DateTime.Now.Second = timestamp.Second then
                printfn "Paths: %i, shortest: %i, x: %i, y: %i" pathSet.Count path.cost path.x path.y 
                timestamp <- DateTime.Now.AddSeconds 30

            let newSteps =
                [
                    (ArrayUtils.tryGetValue2D map (path.y - 1) path.x, path.x, (path.y - 1));
                    (ArrayUtils.tryGetValue2D map (path.y + 1) path.x, path.x, (path.y + 1));
                    (ArrayUtils.tryGetValue2D map path.y (path.x - 1), (path.x - 1), path.y);
                    (ArrayUtils.tryGetValue2D map path.y (path.x + 1), (path.x + 1), path.y)
                ]
                |> List.filter (fun (cost, x, y) ->
                    cost.IsSome
                    && path.path.Contains (x, y) |> not)
                |> List.map (fun (cost, x, y) -> Path(x, y, path.cost + cost.Value, path.path.Add (x, y)))
            
            pathSet <- pathSet.Remove path
            newSteps |> List.iter (fun x -> pathSet <- pathSet.Add x)
            path <- pathSet.MinimumElement

        path


    let solve (data: string list) =
        let map =
            data
            |> array2D
            |> Array2D.map (fun x -> x |> string |> int)
        let seed =
            [
                Path(1, 0, map.[0, 1], Set.ofList [ (0, 0); (1, 0) ]);
                Path(0, 1, map.[1, 0], Set.ofList [ (0, 0); (0, 1) ])
            ]
        let shortestPath = shortestPath map seed
        shortestPath.cost


    let solve2 (data: string list) =
        0